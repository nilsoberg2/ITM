// This file is part of the ITM model.
//
// Copyright 2009 University of Illinois at Urbana-Champaign
//
// Authors: Arturo S. Leon (Hydraulics), Nils Oberg (User interface)
//
// ITM is a free software; you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published
// by the Free Software Foundation; either version 2.0 of the
// License, or (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
// 02110-1301, USA.

#define _CRT_SECURE_NO_DEPRECATE


#include "headers.h"
#include "swmm5.h"
#include <string.h>
#include <math.h>
#include "text.h"
#include <stdlib.h>
#include "itm.h"
#include <stdio.h>


//FILE* xxxxxx = NULL;


double itm_get_inflow(int id, double currentStep)
//
//  Input:   currentDate = current date/time
//  Output:  returns the sum of the external inflows
//  Purpose: adds direct external inflows to nodes at current date.
//           Trimmed down version of addExternalInflows in routing.c
{
    int j;
    double  q;
    TExtInflow* inflow;
    double currentDate;

    j = id - 1;

    inflow = Node[j].extInflow;
    if ( !inflow ) return 0.0;
    
    currentDate = getDateTime(currentStep * 1000.0);

    // --- get flow inflow
    q = 0.0;
    while ( inflow )
    {
        if ( inflow->type == FLOW_INFLOW )
        {
            q = inflow_getExtInflow(inflow, currentDate);
            break;
        }
        else inflow = inflow->next;
    }
    if ( fabs(q) < FLOW_TOL ) q = 0.0;

	//if (xxxxxx == NULL)
	//	xxxxxx = fopen("c:/temp/xxx.txt", "w");
	//fprintf(xxxxxx, "%f\t%f\n", currentStep, q);

    return q;
}


double itm_get_gate_opening_swmm(int id, double currentStep)
//
//  Input:   currentStep = current date/time
//  Output:  returns the current gate opening
{
    int j, ts;
    double opening;
    double currentDate;

    j = Node[id - 1].subIndex;

	if (Divider[j].tsCurve < 0)
		return -1.0; // error
	ts = Divider[j].tsCurve;

    currentDate = getDateTime(currentStep * 1000.0);

    // --- get flow inflow
	opening = table_tseriesLookup(&Tseries[ts], currentDate, TRUE);
	Divider[j].curOpening = opening;

	return opening;
}


double itm_get_gate_coeff_swmm(int id, double opening)
//
//  Input:   opening = the percentage (between 0 and 100) that the gate is open
//  Output:  returns the corresponding loss coefficient
{
    int j, cid;
    double c;

    j = Node[id - 1].subIndex;

	if (Divider[j].lossCurve < 0)
		return -1.0; // error
	cid = Divider[j].lossCurve;

    // --- get flow inflow
	c = table_lookup(&Curve[cid], opening);

	return c;
}


double itm_get_max_rating_head_swmm(int id)
//
//  Input:   opening = the percentage (between 0 and 100) that the gate is open
//  Output:  returns the corresponding loss coefficient
{
  int j;
  double x1,y1,x2,y2;
  double max;
  TTable* table;

  j = Node[id - 1].subIndex;

	if (RatingUnit[j].qCurve < 0)
		return -1.0; // error
	
	table = &Curve[RatingUnit[j].qCurve];

  table_getFirstEntry(table, &x1, &y1);
  max = x1;
  while ( table_getNextEntry(table, &x2, &y2) )
  {
    if (x2 > max)
      max = x2;
  }
  
  return max;
}


double itm_get_curve_area(int id, double currentStep)
//
//  Input:   currentDate = current date/time
//  Output:  returns the area under the hydrograph curve for the node
{
    TExtInflow* inflow;
    double currentDate;
    int    k;
    double cf;
    double sf;
    double blv;
    double tsv;

    inflow = Node[id-1].extInflow;
    if ( !inflow ) return 0.0;
    
    currentDate = getDateTime(currentStep * 1000.0);

    k = inflow->tSeries;      // time series index
    cf = inflow->cFactor;     // units conversion factor
    sf = inflow->sFactor;     // scaling factor
    blv = inflow->baseline;   // baseline value
    tsv = 0.0;                // time series value

    if ( k >= 0 )
		tsv = table_getAreaFromSimStart(&Tseries[k], currentDate) * sf;
		//tsv = table_getArea(&Tseries[k], currentDate) * sf;

	return cf * (tsv + blv);
}


double itm_get_q_from_rat_curve(int id, double yb)
{
    int j, cid;
    double q;

    j = Node[id - 1].subIndex;

	if (RatingUnit[j].qCurve < 0)
		return -1.0; // error
	cid = RatingUnit[j].qCurve;

	q = table_lookup(&Curve[cid], yb);

	return q;
}


int itm_has_inflow_swmm(int id)
{
	id = id - 1; // to get to zero-based indexing

    if (Node[id].extInflow)
		return 1;
	else
		return 0;
}


double itm_node_get_volume(int j, double d)
{
    double volume;

    // fix the indexing
    j = j - 1;

    volume = node_getVolume(j, d);
    
    return volume;
}




int itm_get_step_count_swmm()
{
    return StepCount;
}


void itm_get_error_msg_swmm(int errorCode, char* errorStr, int errorStrMaxLen)
{
	char tempStr[1024];
	memset(tempStr, 0, 1024);
	strcpy(tempStr, error_getMsg(errorCode)); // DANGER! this is bad if the return string is > 1023 characters.
    
	if ((int)strlen(tempStr) > errorStrMaxLen)
		strncpy(errorStr, tempStr, errorStrMaxLen);
	else
		strcpy(errorStr, tempStr);
}


void itm_report_error_swmm(char* errorStr, int errorStrLen)
{
    if ( Frpt.file )
    {
        errorStr[errorStrLen-1] = 0; // null-terminate
        fprintf(Frpt.file, "\n  ERR ITM: %s", errorStr);
    }
}


void itm_get_swmm_id_swmm(int link, int idx, char* swmm_id, int swmm_id_len)
{
	DEBUGWRITE("start itm_get_swmm_id_swmm\n");
    idx--; // to get swmm 0-based id
    if (link == 1)
    {
        if (idx >= 0 && idx < Nobjects[LINK])
            strncpy(swmm_id, Link[idx].ID, swmm_id_len);
    }
    else
    {
        if (idx >= 0 && idx < Nobjects[NODE])
            strncpy(swmm_id, Node[idx].ID, swmm_id_len);
    }
	DEBUGWRITE("end itm_get_swmm_id_swmm\n");
}


int itm_get_num_links_swmm()
{
    return Nobjects[LINK];
}


int itm_get_num_nodes_swmm()
{
    return Nobjects[NODE];
}


int itm_get_starting_nodes_swmm(int* start, int start_count)
{
    int i, curS;

    // current starting node
    curS = 0;

    for (i = 0; i < Nobjects[NODE]; i++)
    {
        if (Node[i].type == STORAGE)
        {
            start[curS] = i + 1; //+1 because fortran indexing is 1-based
            curS++;
            if (curS > start_count-1)
                break;
        }
    }

    return curS;
}


int itm_get_min_num_grids()
{
    return MinNumGrids;
}

int itm_get_max_num_iterations()
{
    return MaxNumIterations;
}

int itm_get_max_num_cells()
{
    return MaxNumCells;
}


int itm_get_max_num_plot_cells()
{
    return MaxNumPlotCells;
}



double itm_get_pressurized_wave_celerity()
{
    return PressurizedWaveCelerity;
}

double itm_get_mixed_flow_wave_celerity()
{
    return MixedFlowWaveCelerity;
}

double itm_get_reference_depth_fraction()
{
    return RefDepthFraction;
}


double itm_get_max_simulation_time()
{
    return TotalDuration / 1000.0; // divide by 1000.0 because we want seconds
}

double itm_get_max_timestep()
{
    return RouteStep;
}

double itm_get_report_interval()
{
    return ReportTime / 1000.0; // divide by 1000.0 because we want seconds
}

double itm_get_report_start_time()
{
	return (ReportStart - StartDateTime) * 86400.0;
}

double itm_get_tol_normal()
{
    return ITMTolNormal;
}

double itm_get_tol_low()
{
    return ITMTolLow;
}

double itm_get_tol_very_low()
{
    return ITMTolVeryLow;
}

double itm_get_tol_high()
{
    return ITMTolHigh;
}

double itm_get_tol_transition()
{
    return ITMTolTransition;
}

double itm_get_water_init_elevation()
{
	return WaterInitElevation;
}

int itm_get_ini_cond() // for waterinitelevation
{
	if (WaterInitElevation < -99999.0 && WaterInitElevation > -100000.0) // -99999.5
		return 0;
	else
		return 1;
}

int itm_get_flow_type()
{
	if (ITMFlowType == FLOW_TYPE_FREE_SURFACE)
		return 1;
	else if (ITMFlowType == FLOW_TYPE_MIXED)
		return 2;
	else if (ITMFlowType == FLOW_TYPE_PRESSURIZED)
		return 3;
	else
		return 0;
}

int itm_hotstart_status()
{
	if (Fhotstart1.mode == USE_FILE)
		return 1;
	else if (Fhotstart2.mode == SAVE_FILE)
		return 2;
	else
		return 0;
}


double itm_get_max_curve_val_swmm(int xory, int nodeID)
{
	int j, m;
	double v1, v2, v;

	j = nodeID - 1;
	v = 0.0;
	
	if (Node[j].type == STORAGE)
	{
		m = Storage[Node[j].subIndex].aCurve;
		table_getLastEntry(&Curve[m], &v1, &v2);
		if (xory == 1)
			v = v1;
		else
			v = v2;
	}

	return v;
}


void itm_set_swmm_error_swmm(int err_code, double errTime)
{
	char *temp = (char*)malloc(sizeof(char)*50);
	sprintf(temp, "%f", errTime);
	ErrorCode = ERR_ITM_ENGINE;
	report_writeErrorMsg(ERR_ITM_ENGINE, temp);
	free(temp);
}
