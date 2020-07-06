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

#include "headers.h"
#include "swmm5.h"
#include <string.h>
#include <math.h>
#include "text.h"
#include <stdlib.h>
#include "itm.h"


extern TNodeStats*     NodeStats;
extern float           SysVolErrDiff;
extern float           SysVolErrPct;
//extern FILE* xxxxxx;


int itm_run_swmm(char* f1, char* f2, char* f3)
{
	return swmm_run(f1, f2, f3);
}


int itm_open_start_swmm(char* f1, char* f2, char* f3)
{
	int result, i;
    int* upCount;
    int* downCount;

    result = swmm_open(f1, f2, f3);

    // Check for an error condition.
    if (result > 0)
        return result;
    
    StepCount = 0;
    result = swmm_start(TRUE);

    if (result > 0)
        return result;

    upCount = (int*)calloc(Nobjects[NODE], sizeof(int));
    downCount = (int*)calloc(Nobjects[NODE], sizeof(int));

    // Initialize degree and upstream/downstream node occurrence counters to zero.
    for (i = 0; i < Nobjects[NODE]; i++)
    {
        Node[i].degree = 0;
        upCount[i] = 0;
        downCount[i] = 0;
    }

    // Look at every link and update the updatream/downstream node occurrence
    // counter as well as the node degree.
    for (i = 0; i < Nobjects[LINK]; i++)
    {
        upCount[ Link[i].node1 ]++;
        downCount[ Link[i].node2 ]++;
        Node[ Link[i].node1 ].degree ++;
        Node[ Link[i].node2 ].degree ++;
    }

    // Now check each upstream node occurrence to see if there's more than
    // one occurrence of the specified node.  This means that the pipes
    // split in the downstream direction.  If this is the case, then we
    // negate the degree to indicate a loop junction.
    // NOTE: we don't want to do this in the updated model because it
    // doesn't matter what type of junction it is; it's all the same.
    //for (i = 0; i < Nobjects[NODE]; i++)
    //{
    //    if (upCount[i] > 1)
    //        Node[i].degree = - Node[i].degree;
    //}

    free(upCount);
    free(downCount);

    // Now initialize the statistics.
    for (i = 0; i < Nobjects[NODE]; i++)
        NodeStats[i].maxOverflow = Node[i].fullDepth + Node[i].surDepth;

	DEBUGWRITE("end itm_open_start_swmm\n");

    return result;
}


int itm_close_swmm()
{
	DEBUGWRITE("ITM_CLOSE_SWMM");

	//fclose(xxxxxx);
	return swmm_close();
}


int itm_start_swmm()
{
    int result;

    result = swmm_start(TRUE);

    return result;
}


int itm_end_swmm()
{
    return swmm_end();
}


int itm_report_swmm()
{
    return swmm_report();
}


int itm_step_swmm()
{
    StepCount++;
    return 0;
}


int itm_report_step_swmm(double currentStep, int numRows, double* linkDepths, double* linkFlows,
						 double* linkVelocities, double* linkFroudes, double* nodeDepths,
						 double sysVolErrPct, double sysVolErrDiff)
{
    int i = 0;
    double q;
    double currentDate = getDateTime(currentStep * 1000.0);

    // do something here where we save the results to SWMM arrays
    int bound = (numRows > Nobjects[LINK] ? Nobjects[LINK] : numRows);

	DEBUGWRITE("start itm_report_step_swmm\n");
    
    for (i = 0; i < bound; i++)
    {
        Link[i].oldDepth = Link[i].newDepth;
		Link[i].newDepth = linkDepths[i];
        Link[i].oldFlow = Link[i].newFlow;
        Link[i].newFlow = linkFlows[i];
		Link[i].velocity = linkVelocities[i];
		Link[i].froude = linkFroudes[i];
    }

    bound = (numRows > Nobjects[NODE] ? Nobjects[NODE] : numRows);

    for (i = 0; i < bound; i++)
    {
        Node[i].oldDepth = Node[i].newDepth;
        Node[i].newDepth = nodeDepths[i];
        
        q = itm_get_inflow(i+1, currentStep);
        Node[i].newLatFlow += q;
        Node[i].inflow += q;

        if (Node[i].type == STORAGE)
            Node[i].newVolume = itm_node_get_volume(i+1, Node[i].newDepth);
    }

    // Update the statistics for the report file.
    stats_updateFlowStats(RouteStep, currentDate, 1, TRUE);

	SysVolErrDiff = (float)sysVolErrDiff;
	SysVolErrPct = (float)sysVolErrPct;

    // Output the results for the current time.
    output_saveResults(currentStep * 1000.0);

    currentStep += RouteStep * 1000.0;
    NewRoutingTime = currentStep;

	DEBUGWRITE("end itm_report_step_swmm\n");
	
	return 0;
}

