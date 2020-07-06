//-----------------------------------------------------------------------------
//   statsrpt.c
//
//   Project:  EPA SWMM5
//   Version:  5.0
//   Date:     2/4/08   (Build 5.0.012)
//   Author:   L. Rossman
//
//   Report writing functions for summary statistics.
//-----------------------------------------------------------------------------
#define _CRT_SECURE_NO_DEPRECATE

#include <malloc.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include "headers.h"

//-----------------------------------------------------------------------------
//  Imported variables
//-----------------------------------------------------------------------------
extern TSubcatchStats* SubcatchStats;          // defined in STATS.C
extern TNodeStats*     NodeStats;
extern TLinkStats*     LinkStats;
extern TStorageStats*  StorageStats;
extern TOutfallStats*  OutfallStats;
extern TPumpStats*     PumpStats;
extern double          MaxOutfallFlow;
extern double          MaxRunoffFlow;
extern double*         NodeInflow;             // defined in MASSBAL.C

//-----------------------------------------------------------------------------
//  Local functions
//-----------------------------------------------------------------------------
void    writeSubcatchRunoff(void);
void    writeSubcatchLoads(void);
void    writeNodeDepths(void);
void    writeNodeFlows(void);
void    writeNodeSurcharge(void);
void    writeNodeFlooding(void);
void    writeStorageVolumes(void);
void    writeOutfallLoads(void);
void    writeLinkFlows(void);
void    writeFlowClass(void);
void    writeLinkSurcharge(void);
void    writePumpFlows(void);

#define WRITE(x) (report_writeLine((x)))
static char   FlowFmt[6];
static double Vcf;

//=============================================================================

void statsrpt_writeReport()
//
//  Input:   none
//  Output:  none
//  Purpose: reports simulation summary statistics.
//
{
    // --- set number of decimal places for reporting flow values
    if ( FlowUnits == MGD || FlowUnits == CFS ) strcpy(FlowFmt, "%9.3f");
    else strcpy(FlowFmt, "%9.2f");

    // --- volume conversion factor from ft3 to Mgal or Mliters
    if (UnitSystem == US) Vcf = 7.48 / 1.0e6;
    else                  Vcf = 28.317 / 1.0e6;

    //// --- report summary results for subcatchment runoff 
    //if ( Nobjects[SUBCATCH] > 0 )
    //{
    //    writeSubcatchRunoff();
    //    if ( Nobjects[POLLUT] > 0 ) writeSubcatchLoads();
    //}

    // --- report summary results for flow routing
    if ( Nobjects[LINK] > 0 && RouteModel != NO_ROUTING )
    {
        writeNodeDepths();
        //writeNodeFlows();
        //writeNodeSurcharge();
        //writeNodeFlooding();
        writeStorageVolumes();
        //writeOutfallLoads();
        writeLinkFlows();
        //writeFlowClass();
        writeLinkSurcharge();
        writePumpFlows();
    }
}

//=============================================================================

void writeSubcatchRunoff()
{
    int    j;
    double a, aSum, x, r;
    TSubcatchStats totals;

    if ( Nobjects[SUBCATCH] == 0 ) return;
    aSum = 0.0;
    totals.precip = 0.0;
    totals.runon  = 0.0;
    totals.evap   = 0.0;
    totals.infil  = 0.0;
    totals.runoff = 0.0;
    totals.maxFlow = MaxRunoffFlow * UCF(FLOW);
    WRITE("");
    WRITE("***************************");
    WRITE("Subcatchment Runoff Summary");
    WRITE("***************************");
    WRITE("");
    fprintf(Frpt.file,

"\n  --------------------------------------------------------------------------------------------"
"\n                          Total    Total    Total    Total    Total     Total     Peak  Runoff"
"\n                         Precip    Runon     Evap    Infil   Runoff    Runoff   Runoff   Coeff");
    if ( UnitSystem == US ) fprintf(Frpt.file,
"\n  Subcatchment               in       in       in       in       in     %5s      %3s",
        VolUnitsWords[UnitSystem], FlowUnitWords[FlowUnits]);
    else fprintf(Frpt.file,
"\n  Subcatchment               mm       mm       mm       mm       mm     %5s      %3s",
        VolUnitsWords[UnitSystem], FlowUnitWords[FlowUnits]);
    fprintf(Frpt.file,
"\n  --------------------------------------------------------------------------------------------");

    for ( j = 0; j < Nobjects[SUBCATCH]; j++ )
    {
        a = Subcatch[j].area;
        aSum += a;
        fprintf(Frpt.file, "\n  %-20s", Subcatch[j].ID);
        x = SubcatchStats[j].precip * UCF(RAINDEPTH);
        fprintf(Frpt.file, " %8.3f", x);
        totals.precip += x * a;
        x = SubcatchStats[j].runon * UCF(RAINDEPTH); 
        fprintf(Frpt.file, " %8.3f", x);
        totals.runon += x * a;
        x = SubcatchStats[j].evap * UCF(RAINDEPTH);
        fprintf(Frpt.file, " %8.3f", x);
        totals.evap += x * a;
        x = SubcatchStats[j].infil * UCF(RAINDEPTH); 
        fprintf(Frpt.file, " %8.3f", x);
        totals.infil += x * a;
        x = SubcatchStats[j].runoff * UCF(RAINDEPTH);
        fprintf(Frpt.file, " %8.3f", x);
        totals.runoff += x * a;
        x = SubcatchStats[j].runoff * a * Vcf;
		fprintf(Frpt.file, "%10.3f", x);
        x = SubcatchStats[j].maxFlow * UCF(FLOW);
        fprintf(Frpt.file, " %8.3f", x);
        r = SubcatchStats[j].precip + SubcatchStats[j].runon;
        if ( r > 0.0 ) r = SubcatchStats[j].runoff / r;
        fprintf(Frpt.file, "%8.3f", r);
    }
    if ( aSum > 0.0 )
    {
        fprintf(Frpt.file,
"\n  --------------------------------------------------------------------------------------------");
        fprintf(Frpt.file, "\n  System              ");
        fprintf(Frpt.file, " %8.3f", totals.precip / aSum);
        fprintf(Frpt.file, " %8.3f", totals.runon / aSum);
        fprintf(Frpt.file, " %8.3f", totals.evap / aSum);
        fprintf(Frpt.file, " %8.3f", totals.infil / aSum);
        fprintf(Frpt.file, " %8.3f", totals.runoff / aSum);
        x = totals.runoff / UCF(RAINDEPTH) * Vcf;
		fprintf(Frpt.file, "%10.3f", x);
        fprintf(Frpt.file, " %8.3f", totals.maxFlow);
        r = totals.precip + totals.runon;
        if ( r > 0.0 ) r = totals.runoff / r;
        fprintf(Frpt.file, "%8.3f", r);
    }
    WRITE("");
}

//=============================================================================

void writeSubcatchLoads()
{
    int i, j, p;
    double x;
    double* totals; 
    char  units[15];
    char  subcatchLine[] = "--------------------";
    char  pollutLine[]   = "--------------";

    // --- create an array to hold total loads for each pollutant
    totals = (double *) calloc(Nobjects[POLLUT], sizeof(double));
    if ( totals )
    {
        // --- print the table headings 
        WRITE("");
        WRITE("****************************");
        WRITE("Subcatchment Washoff Summary");
        WRITE("****************************");
        WRITE("");
        fprintf(Frpt.file, "\n  %s", subcatchLine);
        for (p = 0; p < Nobjects[POLLUT]; p++) fprintf(Frpt.file, "%s", pollutLine);
        fprintf(Frpt.file, "\n                      ");
        for (p = 0; p < Nobjects[POLLUT]; p++) fprintf(Frpt.file, "%14s", Pollut[p].ID);
        fprintf(Frpt.file, "\n  Subcatchment        ");
        for (p = 0; p < Nobjects[POLLUT]; p++)
        {
            i = UnitSystem;
            if ( Pollut[p].units == COUNT ) i = 2;
            strcpy(units, LoadUnitsWords[i]);
            fprintf(Frpt.file, "%14s", units);
            totals[p] = 0.0;
        }
        fprintf(Frpt.file, "\n  %s", subcatchLine);
        for (p = 0; p < Nobjects[POLLUT]; p++) fprintf(Frpt.file, "%s", pollutLine);

        // --- print the pollutant loadings from each subcatchment
        for ( j = 0; j < Nobjects[SUBCATCH]; j++ )
        {
            fprintf(Frpt.file, "\n  %-20s", Subcatch[j].ID);
            for (p = 0; p < Nobjects[POLLUT]; p++)
            {
                x = Subcatch[j].totalLoad[p];
                totals[p] += x;
                if ( Pollut[p].units == COUNT ) x = LOG10(x);
				fprintf(Frpt.file, "%14.3f", x); 
            }
        }

        // --- print the total loading of each pollutant
        fprintf(Frpt.file, "\n  %s", subcatchLine);
        for (p = 0; p < Nobjects[POLLUT]; p++) fprintf(Frpt.file, "%s", pollutLine);
        fprintf(Frpt.file, "\n  System              ");
        for (p = 0; p < Nobjects[POLLUT]; p++)
        {
            x = totals[p];
            if ( Pollut[p].units == COUNT ) x = LOG10(x);
			fprintf(Frpt.file, "%14.3f", x); 
        }
        free(totals);
        WRITE("");
    }
}

//=============================================================================

void writeNodeDepths()
//
//  Input:   none
//  Output:  none
//  Purpose: writes simulation statistics for nodes to report file.
//
{
    int j, days, hrs, mins;

    if ( Nobjects[LINK] == 0 ) return;
    WRITE("");
    WRITE("******************");
    WRITE("Node Depth Summary");
    WRITE("******************");
    WRITE("");

    fprintf(Frpt.file,
"\n  ---------------------------------------------------------------"
"\n                                   Average  Maximum  Time of Max "
"\n                                     Depth    Depth  Depth Occur.");
    if ( UnitSystem == US ) fprintf(Frpt.file,
"\n  Node                 Type           Feet     Feet  days hr:min ");
    else fprintf(Frpt.file,
"\n  Node                 Type         Meters   Meters  days hr:min ");
    fprintf(Frpt.file,
"\n  ---------------------------------------------------------------");

    for ( j = 0; j < Nobjects[NODE]; j++ )
    {
        fprintf(Frpt.file, "\n  %-20s", Node[j].ID);
        fprintf(Frpt.file, " %-11s ", NodeTypeWords[Node[j].type]);
        getElapsedTime(NodeStats[j].maxDepthDate, &days, &hrs, &mins);
        fprintf(Frpt.file, "%7.2f  %7.2f  %4d  %02d:%02d",
            NodeStats[j].avgDepth / StepCount * UCF(LENGTH),
            NodeStats[j].maxDepth * UCF(LENGTH),
            days, hrs, mins);
    }
    WRITE("");
}

//=============================================================================

void writeNodeFlows()
//
//  Input:   none
//  Output:  none
//  Purpose: writes flow statistics for nodes to report file.
//
{
    int j;
    int days1, hrs1, mins1;

    WRITE("");
    WRITE("*******************");
    WRITE("Node InFlow Summary");
    WRITE("*******************");
    WRITE("");

    fprintf(Frpt.file,
"\n  ---------------------------------------------------------------------------------"
"\n                                  Maximum  Maximum                Lateral     Total"
"\n                                  Lateral    Total  Time of Max    Inflow    Inflow"
"\n                                   Inflow   Inflow   Occurrence    Volume    Volume"
"\n  Node                 Type           %3s      %3s  days hr:min     %5s     %5s",
        FlowUnitWords[FlowUnits], FlowUnitWords[FlowUnits], VolUnitsWords[UnitSystem],
        VolUnitsWords[UnitSystem]);
    fprintf(Frpt.file,
"\n  ---------------------------------------------------------------------------------");

    for ( j = 0; j < Nobjects[NODE]; j++ )
    {
        fprintf(Frpt.file, "\n  %-20s", Node[j].ID);
        fprintf(Frpt.file, " %-9s", NodeTypeWords[Node[j].type]);
        getElapsedTime(NodeStats[j].maxInflowDate, &days1, &hrs1, &mins1);
        fprintf(Frpt.file, FlowFmt, NodeStats[j].maxLatFlow * UCF(FLOW));
        fprintf(Frpt.file, FlowFmt, NodeStats[j].maxInflow * UCF(FLOW));
        fprintf(Frpt.file, "  %4d  %02d:%02d", days1, hrs1, mins1);
		fprintf(Frpt.file, "%10.3f", NodeStats[j].totLatFlow * Vcf);
		fprintf(Frpt.file, "%10.3f", NodeInflow[j] * Vcf);
    }
    WRITE("");
}

//=============================================================================

void writeNodeSurcharge()
{
    int    j, n = 0;
    double t, d1, d2;

    WRITE("");
    WRITE("**********************");
    WRITE("Node Surcharge Summary");
    WRITE("**********************");
    WRITE("");

    for ( j = 0; j < Nobjects[NODE]; j++ )
    {
        if ( Node[j].type == OUTFALL ) continue;
        if ( NodeStats[j].timeSurcharged == 0.0 ) continue;
        t = MAX(0.01, (NodeStats[j].timeSurcharged / 3600.0));
        if ( n == 0 )
        {
            WRITE("Surcharging occurs when water rises above the top of the highest conduit."); 
            fprintf(Frpt.file, 
"\n  --------------------------------------------------------"
"\n                                               Max. Height "
"\n                                   Hours       Above Crown ");
    if ( UnitSystem == US ) fprintf(Frpt.file,
"\n  Node                 Type      Surcharged           Feet ");
    else fprintf(Frpt.file,
"\n  Node                 Type      Surcharged         Meters ");
    fprintf(Frpt.file,
"\n  --------------------------------------------------------");
//"\n  ---------------------------------------------------------------------"
//"\n                                               Max. Height   Min. Depth"
//"\n                                   Hours       Above Crown    Below Rim");
//    if ( UnitSystem == US ) fprintf(Frpt.file,
//"\n  Node                 Type      Surcharged           Feet         Feet");
//    else fprintf(Frpt.file,
//"\n  Node                 Type      Surcharged         Meters       Meters");
//    fprintf(Frpt.file,
//"\n  ---------------------------------------------------------------------");
            n = 1;
        }
        fprintf(Frpt.file, "\n  %-20s", Node[j].ID);
        fprintf(Frpt.file, " %-9s", NodeTypeWords[Node[j].type]);
        d1 = NodeStats[j].maxDepth + Node[j].invertElev - Node[j].crownElev;
        if ( d1 < 0.0 ) d1 = 0.0;
        d2 = Node[j].fullDepth + Node[j].surDepth - NodeStats[j].maxDepth;
        if ( d2 < 0.0 ) d2 = 0.0;
        fprintf(Frpt.file, "  %9.2f      %9.3f",
                t, d1*UCF(LENGTH));
        //fprintf(Frpt.file, "  %9.2f      %9.3f    %9.3f",
        //        t, d1*UCF(LENGTH), d2*UCF(LENGTH));
    }
    if ( n == 0 ) WRITE("No nodes were surcharged.");
    WRITE("");
}

//=============================================================================

void writeNodeFlooding()
{
    int    j, n = 0;
    int    days, hrs, mins;
    double t;

    WRITE("");
    WRITE("*********************");
    WRITE("Node Flooding Summary");
    WRITE("*********************");
    WRITE("");

    for ( j = 0; j < Nobjects[NODE]; j++ )
    {
        if ( Node[j].type == OUTFALL ) continue;
        if ( NodeStats[j].timeFlooded == 0.0 ) continue;
        t = MAX(0.01, (NodeStats[j].timeFlooded / 3600.0));
        if ( n == 0 )
        {
            //WRITE("Flooding refers to all water that overflows a node, whether it ponds or not.");
            //            fprintf(Frpt.file, 
            //"\n  ------------------------------------------------------------------------"
            //"\n                                                           Total   Maximum"
            //"\n                                 Maximum   Time of Max     Flood    Ponded"
            //"\n                        Hours       Rate    Occurrence    Volume    Volume");
            //            fprintf(Frpt.file,
            //"\n  Node                 Flooded       %3s   days hr:min     %5s   %7s",
            //                FlowUnitWords[FlowUnits], VolUnitsWords[UnitSystem],
            //                PondingUnitsWords[UnitSystem]);
            //            fprintf(Frpt.file,
            //"\n  ------------------------------------------------------------------------");

            WRITE("Flooding refers to all water that exceeds the global overflow height.");
            fprintf(Frpt.file, 
"\n  ----------------------------------------------------"
"\n                                 Time of Max   Maximum"
"\n                        Hours     Occurrence    Height");
            if (UnitSystem == US)
                fprintf(Frpt.file,
"\n  Node                 Flooded   days hr:min      Feet");
            else
                fprintf(Frpt.file,
"\n  Node                 Flooded   days hr:min    Meters");

            fprintf(Frpt.file,
"\n  ----------------------------------------------------");
            n = 1;
        }
        getElapsedTime(NodeStats[j].maxDepthDate, &days, &hrs, &mins);
        fprintf(Frpt.file, "\n  %-20s", Node[j].ID);
        fprintf(Frpt.file, " %7.2f ", t);
        fprintf(Frpt.file, "   %4d  %02d:%02d", days, hrs, mins);
        fprintf(Frpt.file, "  %7.2f", (NodeStats[j].maxDepth + Node[j].invertElev) * UCF(LENGTH));
        //fprintf(Frpt.file, FlowFmt, NodeStats[j].maxOverflow * UCF(FLOW));
		//fprintf(Frpt.file, "%10.3f", NodeStats[j].volFlooded * Vcf);
        //fprintf(Frpt.file, " %9.2f",
        //      NodeStats[j].maxPondedVol * UCF(LANDAREA) * UCF(RAINDEPTH));
    }
    if ( n == 0 ) WRITE("No nodes were flooded.");
    WRITE("");
}

//=============================================================================

void writeStorageVolumes()
//
//  Input:   none
//  Output:  none
//  Purpose: writes simulation statistics for storage units to report file.
//
{
    int    j, k, days, hrs, mins;
    double avgVol, maxVol;
	//double pctAvgVol, pctMaxVol;

    if ( Nnodes[STORAGE] > 0 )
    {
        WRITE("");
        WRITE("**********************");
        WRITE("Storage Volume Summary");
        WRITE("**********************");
        WRITE("");

        fprintf(Frpt.file,
"\n  ---------------------------------------------------------"
"\n                         Average      Maximum   Time of Max"
"\n                          Volume       Volume    Vol Occur.");
        if ( UnitSystem == US ) fprintf(Frpt.file,
"\n  Storage Unit          1000 ft3     1000 ft3   days hr:min");
        else fprintf(Frpt.file,
"\n  Storage Unit           1000 m3      1000 m3   days hr:min");
        fprintf(Frpt.file,
"\n  ---------------------------------------------------------");
        for ( j = 0; j < Nobjects[NODE]; j++ )
        {
            if ( Node[j].type != STORAGE ) continue;
            k = Node[j].subIndex;
            fprintf(Frpt.file, "\n  %-20s", Node[j].ID);
            avgVol = StorageStats[k].avgVol / (double)StepCount;
            maxVol = StorageStats[k].maxVol;
            //pctMaxVol = 0.0;
            //pctAvgVol = 0.0;
            //if ( Node[j].fullVolume > 0.0 )
            //{
            //    pctAvgVol = avgVol / Node[j].fullVolume * 100.0;
            //    pctMaxVol = maxVol / Node[j].fullVolume * 100.0;
            //}
            fprintf(Frpt.file, "%10.3f   %10.3f", avgVol/1000.0, maxVol/1000.0);
            getElapsedTime(StorageStats[k].maxVolDate, &days, &hrs, &mins);

            fprintf(Frpt.file, "   %4d  %02d:%02d", days, hrs, mins);
            //fprintf(Frpt.file, FlowFmt, StorageStats[k].maxFlow*UCF(FLOW));
        }
        WRITE("");
    }
}

//=============================================================================

void writeOutfallLoads()
//
//  Input:   node
//  Output:  none
//  Purpose: writes simulation statistics for outfall nodess to report file.
//
{
    char    units[15];
    int     i, j, k, p;
    double  x;
    double  outfallCount, flowCount;
    double  flowSum, freqSum, volSum;
    double* totals;

    if ( Nnodes[OUTFALL] > 0 )
    {
        // --- initial totals
        totals = (double *) calloc(Nobjects[POLLUT], sizeof(double));
        for (p=0; p<Nobjects[POLLUT]; p++) totals[p] = 0.0;
        flowSum = 0.0;
        freqSum = 0.0;
		volSum  = 0.0;

        // --- print table title
        WRITE("");
        WRITE("***********************");
        WRITE("Outfall Loading Summary");
        WRITE("***********************");
        WRITE("");

        // --- print table column headers
        fprintf(Frpt.file,
 "\n  ---------------------------------------------------------");
        for (p = 0; p < Nobjects[POLLUT]; p++) fprintf(Frpt.file, "--------------");
        fprintf(Frpt.file,
 "\n                        Flow       Avg.      Max.     Total");
        for (p=0; p<Nobjects[POLLUT]; p++) fprintf(Frpt.file,"         Total");
        fprintf(Frpt.file,
 "\n                        Freq.      Flow      Flow    Volume");  
        for (p = 0; p < Nobjects[POLLUT]; p++) fprintf(Frpt.file, "%14s", Pollut[p].ID);
        fprintf(Frpt.file,
 "\n  Outfall Node          Pcnt.       %3s       %3s     %5s",
            FlowUnitWords[FlowUnits], FlowUnitWords[FlowUnits],
			VolUnitsWords[UnitSystem]);
        for (p = 0; p < Nobjects[POLLUT]; p++)
        {
            i = UnitSystem;
            if ( Pollut[p].units == COUNT ) i = 2;
            strcpy(units, LoadUnitsWords[i]);
            fprintf(Frpt.file, "%14s", units);
        }
        fprintf(Frpt.file,
 "\n  ---------------------------------------------------------");
        for (p = 0; p < Nobjects[POLLUT]; p++) fprintf(Frpt.file, "--------------");

        // --- identify each outfall node
        for (j=0; j<Nobjects[NODE]; j++)
        {
            if ( Node[j].type != OUTFALL ) continue;
            k = Node[j].subIndex;
            flowCount = OutfallStats[k].totalPeriods;

            // --- print node ID, flow freq., avg. flow, max. flow & flow vol.
            fprintf(Frpt.file, "\n  %-20s", Node[j].ID);
            x = 100.*flowCount/(double)StepCount;
            fprintf(Frpt.file, "%7.2f", x);
            freqSum += x;
            if ( flowCount > 0 )
                x = OutfallStats[k].avgFlow*UCF(FLOW)/flowCount;
            else
                x = 0.0;
            flowSum += x;

            fprintf(Frpt.file, " ");
            fprintf(Frpt.file, FlowFmt, x);
            fprintf(Frpt.file, " ");
            fprintf(Frpt.file, FlowFmt, OutfallStats[k].maxFlow*UCF(FLOW));
			fprintf(Frpt.file, "%10.3f", NodeInflow[j] * Vcf);
			volSum += NodeInflow[j];

            // --- print load of each pollutant for outfall
            for (p=0; p<Nobjects[POLLUT]; p++)
            {
                x = OutfallStats[k].totalLoad[p];
                totals[p] += x;
                if ( Pollut[p].units == COUNT ) x = LOG10(x);
				fprintf(Frpt.file, "%14.3f", x); 
            }
        }

        // --- print total outfall loads
        outfallCount = Nnodes[OUTFALL];
        fprintf(Frpt.file,
 "\n  ---------------------------------------------------------");
        for (p = 0; p < Nobjects[POLLUT]; p++) fprintf(Frpt.file, "--------------");

        fprintf(Frpt.file, "\n  System              %7.2f ",
            freqSum/outfallCount);
        fprintf(Frpt.file, FlowFmt, flowSum);
        fprintf(Frpt.file, " ");
        fprintf(Frpt.file, FlowFmt, MaxOutfallFlow*UCF(FLOW));
		fprintf(Frpt.file, "%10.3f", volSum * Vcf);

        for (p = 0; p < Nobjects[POLLUT]; p++)
        {
            x = totals[p];
            if ( Pollut[p].units == COUNT ) x = LOG10(x);
			fprintf(Frpt.file, "%14.3f", x); 
        }
        WRITE("");
        free(totals);
    } 
}

//=============================================================================

void writeLinkFlows()
//
//  Input:   none
//  Output:  none
//  Purpose: writes simulation statistics for links to report file.
//
{
    int    j, k, days, hrs, mins;
    //double v, fullDepth;

    if ( Nobjects[LINK] == 0 ) return;
    WRITE("");
    WRITE("********************");
    WRITE("Link Flow Summary");
    WRITE("********************");
    WRITE("");

    fprintf(Frpt.file,
"\n  -----------------------------------------------------------"
"\n                                 Maximum  Time of Max        "
"\n                                    Flow  Flow Occur.     Max");
    
    if ( UnitSystem == US )
        fprintf(Frpt.file,
"\n  Link                 Type          %3s  days hr:min   Depth",
            FlowUnitWords[FlowUnits]);
    else
        fprintf(Frpt.file,
"\n  Link                 Type          %3s  days hr:min   Depth",
            FlowUnitWords[FlowUnits]);

    fprintf(Frpt.file, "\n  -----------------------------------------------------------");

    for ( j = 0; j < Nobjects[LINK]; j++ )
    {
        // --- print link ID
        k = Link[j].subIndex;
        fprintf(Frpt.file, "\n  %-20s", Link[j].ID);

        // --- print link type
        if ( Link[j].xsect.type == DUMMY ) fprintf(Frpt.file, " DUMMY   ");
        else if ( Link[j].xsect.type == IRREGULAR ) fprintf(Frpt.file, " CHANNEL ");
        else fprintf(Frpt.file, " %-7s ", LinkTypeWords[Link[j].type]);

        // --- print max. flow & time of occurrence
        getElapsedTime(LinkStats[j].maxFlowDate, &days, &hrs, &mins);
        fprintf(Frpt.file, FlowFmt, LinkStats[j].maxFlow*UCF(FLOW));
        fprintf(Frpt.file, "  %4d  %02d:%02d", days, hrs, mins);

        // --- print max flow / flow capacity for pumps
        if ( Link[j].type == PUMP && Link[j].qFull > 0.0)
        {
            fprintf(Frpt.file, "          ");
            fprintf(Frpt.file, "  %6.2f",
                LinkStats[j].maxFlow / Link[j].qFull);
            continue;
        }

        // --- stop printing for dummy conduits
        if ( Link[j].xsect.type == DUMMY ) continue;

        // --- stop printing for outlet links (since they don't have xsections)
        if ( Link[j].type == OUTLET ) continue;

        //// --- print max velocity & max/full flow for conduits
        //if ( Link[j].type == CONDUIT )
        //{
        //    v = LinkStats[j].maxVeloc*UCF(LENGTH);
        //    if ( v > 50.0 ) fprintf(Frpt.file, "    >50.00");
        //    else fprintf(Frpt.file, "   %7.2f", v);
        //    fprintf(Frpt.file, "  %6.2f", LinkStats[j].maxFlow / Link[j].qFull /
        //                                  (double)Conduit[k].barrels);
        //}
        //else fprintf(Frpt.file, "                  ");

        // --- print max/full depth
        fprintf(Frpt.file, "  %6.2f", LinkStats[j].maxDepth);
        //fullDepth = Link[j].xsect.yFull;
        //if ( Link[j].type == ORIFICE &&
        //     Orifice[k].type == BOTTOM_ORIFICE ) fullDepth = 0.0;
        //if ( fullDepth > 0.0 )
        //{
        //    fprintf(Frpt.file, "  %6.2f", LinkStats[j].maxDepth / fullDepth); 
        //}
        //else fprintf(Frpt.file, "        ");
    }
    WRITE("");
}

//=============================================================================

void writeFlowClass()
//
//  Input:   none
//  Output:  none
//  Purpose: writes flow classification fro each conduit to report file.
//
{
    int   i, j, k;

    if ( RouteModel != DW ) return;

    WRITE("");
    WRITE("***************************");
    WRITE("Flow Classification Summary");
    WRITE("***************************");
    WRITE("");
    fprintf(Frpt.file,
"\n  -----------------------------------------------------------------------------------------"
"\n                      Adjusted    --- Fraction of Time in Flow Class ----   Avg.     Avg.  "
"\n                       /Actual         Up    Down  Sub   Sup   Up    Down   Froude   Flow  "
"\n  Conduit               Length    Dry  Dry   Dry   Crit  Crit  Crit  Crit   Number   Change"
"\n  -----------------------------------------------------------------------------------------");
    for ( j = 0; j < Nobjects[LINK]; j++ )
    {
        if ( Link[j].type != CONDUIT ) continue;
        if ( Link[j].xsect.type == DUMMY ) continue;
        k = Link[j].subIndex;
        fprintf(Frpt.file, "\n  %-20s", Link[j].ID);
        fprintf(Frpt.file, "  %6.2f ", Conduit[k].modLength / Conduit[k].length);
        for ( i=0; i<MAX_FLOW_CLASSES; i++ )
        {
            fprintf(Frpt.file, "  %4.2f",
                LinkStats[j].timeInFlowClass[i] /= StepCount);
        }
        fprintf(Frpt.file, "   %6.2f", LinkStats[j].avgFroude / StepCount);
        fprintf(Frpt.file, "   %6.4f", LinkStats[j].avgFlowChange /
                                       Link[j].qFull / StepCount);
    }
    WRITE("");
}

//=============================================================================

void writeLinkSurcharge()
{
    int    i, j, n = 0;
    double t[5];

    WRITE("");
    WRITE("*************************");
    WRITE("Conduit Surcharge Summary");
    WRITE("*************************");
    WRITE("");
    for ( j = 0; j < Nobjects[LINK]; j++ )
    {
        if ( Link[j].type != CONDUIT ) continue;
        t[0] = LinkStats[j].timeSurcharged / 3600.0;
        t[1] = LinkStats[j].timeFullUpstream / 3600.0;
        t[2] = LinkStats[j].timeFullDnstream / 3600.0;
        t[3] = LinkStats[j].timeFullFlow / 3600.0;
        if ( t[0] + t[1] + t[2] + t[3] == 0.0 ) continue;
        t[4] = LinkStats[j].timeCapacityLimited / 3600.0;
        for (i=0; i<5; i++) t[i] = MAX(0.01, t[i]);
        if (n == 0)
        {
            fprintf(Frpt.file, 
"\n  ----------------------------------------------------"
"\n                                                       "
"\n                         --------- Hours Full -------- "
"\n  Conduit                Both Ends  Upstream  Dnstream "
"\n  ----------------------------------------------------");
//"\n  ----------------------------------------------------------------------------"
//"\n                                                           Hours        Hours "
//"\n                         --------- Hours Full --------   Above Full   Capacity"
//"\n  Conduit                Both Ends  Upstream  Dnstream   Normal Flow   Limited"
//"\n  ----------------------------------------------------------------------------");
            n = 1;
        }
        fprintf(Frpt.file, "\n  %-20s", Link[j].ID);
        //fprintf(Frpt.file, "    %8.2f  %8.2f  %8.2f  %8.2f     %8.2f",
        //        t[0], t[1], t[2], t[3], t[4]);
        fprintf(Frpt.file, "    %8.2f  %8.2f  %8.2f",
                t[0], t[1], t[2]);
    }
    if ( n == 0 ) WRITE("No conduits were surcharged.");
    WRITE("");
}

//=============================================================================

void writePumpFlows()
//
//  Input:   none
//  Output:  none
//  Purpose: writes simulation statistics for pumps to report file.
//
{
    int    j, k;
    double avgFlow, pctUtilized, pctOffCurve, totalSeconds;

    if ( Nlinks[PUMP] == 0 ) return;

    WRITE("");
    WRITE("***************");
    WRITE("Pumping Summary");
    WRITE("***************");
    WRITE("");

    fprintf(Frpt.file,
"\n  ------------------------------------------------------------------------------"
"\n                                      Max       Avg      Total     Power  %% Time"
"\n                        Percent      Flow      Flow     Volume     Usage    Off "
"\n  Pump                 Utilized       %3s       %3s      %5s     Kw-hr   Curve"
"\n  ------------------------------------------------------------------------------",
        FlowUnitWords[FlowUnits], FlowUnitWords[FlowUnits],
        VolUnitsWords[UnitSystem]);
    for ( j = 0; j < Nobjects[LINK]; j++ )
    {
        if ( Link[j].type != PUMP ) continue;
        k = Link[j].subIndex;
        fprintf(Frpt.file, "\n  %-20s", Link[j].ID);
        totalSeconds = NewRoutingTime / 1000.0;
        pctUtilized = PumpStats[k].utilized / totalSeconds * 100.0;
        pctOffCurve = PumpStats[k].offCurve;
        if ( PumpStats[k].utilized > 0.0 )
            pctOffCurve = pctOffCurve / PumpStats[k].utilized * 100.0;
        avgFlow = PumpStats[k].avgFlow;
        if ( PumpStats[k].totalPeriods > 0 )
            avgFlow /=  PumpStats[k].totalPeriods;
        fprintf(Frpt.file, " %8.2f %9.2f %9.2f %10.3f %9.2f %7.2f",
            pctUtilized, PumpStats[k].maxFlow*UCF(FLOW), avgFlow*UCF(FLOW),
            PumpStats[k].volume*Vcf, PumpStats[k].energy, pctOffCurve);
    }
    WRITE("");
}
