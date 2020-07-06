#define _CRT_SECURE_NO_DEPRECATE

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

//#define ZEBRA_X _stdcall
#define ZEBRA_X

#ifdef __cplusplus
extern "C" 
#endif
int ZEBRA_X ITM_OPEN_HOTSTART2(char* hsfile, int hsfile_len);

#ifdef __cplusplus
extern "C" 
#endif
int ZEBRA_X ITM_CLOSE_HOTSTART2();

#ifdef __cplusplus
extern "C" 
#endif
int ZEBRA_X ITM_LOAD_HOTSTART(char* hsfile, int hsfile_len);

#ifdef __cplusplus
extern "C" 
#endif
int ZEBRA_X ITM_SAVE_HOTSTART(char* hsfile, int hsfile_len);


int itm_copy(int maxNumPipes, int* node1, int* node2, double* length, double* d, double* nm, double* zb,
             double* init_flow, int* init_depth_type, double* init_depth,
             double* entrance_loss, double* exit_loss,
             int maxNumNodes, double* const_depth_flow,
             int drop_count, double* Adrop, double* hdrops_overf, int res_count,
             double* flowdepth_res, double* res_outflow, double* res_maxdepth, int bc_count, double* junc_elev,
             int* node_bc, int* open_bc)
{
    int i, subIdx, subType, curDrop, curRes;
    TExtInflow* inflow = 0;

    if (Nobjects[LINK] > maxNumPipes)
        return 1;

    // Copy the topology and physical pipe properties
    for (i = 0; i < Nobjects[LINK]; i++)
    {
        node1[i] = Link[i].node1 + 1; // +1 because fortran is 1-based
        node2[i] = Link[i].node2 + 1;

        // Inverts
        zb[i] = Node[ Link[i].node1 ].invertElev + Link[i].offset1;
        zb[maxNumPipes + i] = Node[ Link[i].node2 ].invertElev + Link[i].offset2;

        // Diameter
        d[i] = Link[i].xsect.yFull;
        
        // Get the sub-type and sub-index
        subIdx = Link[i].subIndex;
        subType = Link[i].type;

        // Copy length and roughness
        if (subType == CONDUIT)
        {
            length[i] = Conduit[subIdx].length;
            nm[i] = Conduit[subIdx].roughness;
        }

        entrance_loss[i] = Link[i].cLossInlet;
        exit_loss[i] = Link[i].cLossOutlet;

        // Copy initial conditions
        init_flow[i] = Link[i].q0;
        init_depth[i] = Link[i].initDepth;
        if (Link[i].initDepthCode == INIDEP_CRITICAL)
            init_depth_type[i] = 2;
        else if (Link[i].initDepthCode == INIDEP_NORMAL)
            init_depth_type[i] = 3;
        else if (Link[i].initDepthCode == INIDEP_CONSTANT)
            init_depth_type[i] = 1;
        else
            init_depth_type[i] = -1;
    }

    // Copy data to dropshafts and boundary conditions
    curDrop = 0;
    curRes = 0;
    for (i = 0; i < Nobjects[NODE]; i++)
    {
        // Determine the boundary conditions.
        if (i < bc_count)
        {
            junc_elev[i] = Node[i].invertElev;
            node_bc[i] = determineBoundaryCondition(i);

            // If we couldn't determine the boundary condition type, then return an error.
            if (node_bc[i] <= 0)
                return 1;

            open_bc[i] = Node[i].boundaryOpen;
        }

        if (Node[i].type == JUNCTION)
        {
            Adrop[i] = Node[i].pondedArea;
            hdrops_overf[i] = Node[i].fullDepth;
        }

        if (Node[i].type == STORAGE && i < res_count)
        {
            //drop_reser[curRes] = 0.0;
            res_outflow[i] = Storage[ Node[i].subIndex ].aConst; // constant outflow
            res_maxdepth[i] = Node[i].fullDepth;
            curRes++;
        }

		if (Node[i].type == RATINGUNIT)
		{
            hdrops_overf[i] = 1.0;
			res_maxdepth[i] = 1.0;
		}
        
        if (i < maxNumNodes)
        {
            // .initDepth stores either constant flow, constant depth, or initial depth,
            // depending on the boundary condition.
            const_depth_flow[i] = Node[i].initDepth;
            flowdepth_res[i] = Node[i].initDepth;
        }
    }

    return 0;
}


int determineBoundaryCondition(int idx)
{
    if (Node[idx].boundaryCond <= 0)
    {
        if (Node[idx].type == STORAGE)
            return 20; // reservoir
		else if (Node[idx].type == RATINGUNIT)
			return 30; // rating unit
		else if (Node[idx].type == DIVIDER)
			return 40; // divider
        else if (Node[idx].degree > 1) // internal boundary
            return 7; // junction
        else // external boundary
            return 4; // dropshaft
    }
    else
        return Node[idx].boundaryCond;
}


int itm_read_hotstart_swmm(TFile* file)
{
	int rescode;

	/*
	filename_len = strlen(file.name) + 5;
	filename = (char*)malloc(filename_len * sizeof(char));
	memset(filename, '\0', filename_len*sizeof(char));
	strncpy(filename, file.name, filename_len-5);
	strcat(filename, ".itm");
	*/

	rescode = 0;

	// --- try to open the file
    if ( file->mode != USE_FILE )
		return TRUE;
    if ( (file->file = fopen(file->name, "r+b")) == NULL)
    {
        report_writeErrorMsg(ERR_HOTSTART_FILE_OPEN, file->name);
        return FALSE;
    }
	fclose(file->file); // close since this is just a test

	rescode = ITM_LOAD_HOTSTART(file->name, (int)strlen(file->name));

	if (rescode)
	{
        report_writeErrorMsg(ERR_HOTSTART_FILE_FORMAT, "");
        return FALSE;
    }
	else
		return TRUE;
}


int itm_open_hotstart2_swmm(TFile* file)
{
	//int rescode;

    // --- try to open file
    if ( file->mode != SAVE_FILE ) return TRUE;
    if ( (file->file = fopen(file->name, "w+b")) == NULL)
    {
        report_writeErrorMsg(ERR_HOTSTART_FILE_OPEN, file->name);
        return FALSE;
    }
	fclose(file->file); // close since this is just a test

	//rescode = ITM_OPEN_HOTSTART2(file->name, (int)strlen(file->name));

	//if (rescode != 0)
	//{
	//	report_writeErrorMsg(ERR_HOTSTART_FILE_OPEN, "ITM was unable to open hotstart file");
	//	return FALSE;
	//}
	//else
	//{
		//file->file = 0x77; // I know this isn't valid but it's the fortran unit that this file is attached to.
	return TRUE;
	//}
}


int itm_close_hotstart2_swmm(TFile* file)
{
	int rescode;

	rescode = ITM_CLOSE_HOTSTART2();

	if (rescode != 0)
	{
		return FALSE;
	}
	else
	{
		file->file = NULL;
		return TRUE;
	}
}


int itm_write_hotstart_swmm(TFile* file)
{
	int rescode;

	rescode = ITM_SAVE_HOTSTART(file->name, (int)strlen(file->name));

	if (rescode)
	{
        report_writeErrorMsg(ERR_HOTSTART_FILE_FORMAT, "");
        return FALSE;
    }
	else
		return TRUE;
}
