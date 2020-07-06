//-----------------------------------------------------------------------------
//   project.c
//
//   Project:  EPA SWMM5
//   Version:  5.0
//   Date:     6/19/07   (Build 5.0.010)
//             2/4/08    (Build 5.0.012)
//   Author:   L. Rossman
//
//   Project management functions.
//
//   This module provides project-related services such as:
//   o opening a new project and reading its input data
//   o allocating and freeing memory for project objects
//   o setting default values for object properties and options
//   o initializing the internal state of all objects
//   o managing hash tables for identifying objects by ID name
//-----------------------------------------------------------------------------
#define _CRT_SECURE_NO_DEPRECATE

#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include "headers.h"
#include "hash.h"
#include "mempool.h"

//-----------------------------------------------------------------------------
//  Shared variables
//-----------------------------------------------------------------------------
static HTtable* Htable[MAX_OBJ_TYPES]; // Hash tables for object ID names
static char     MemPoolAllocated;      // TRUE if memory pool allocated 

//-----------------------------------------------------------------------------
//  External Functions (declared in funcs.h)
//-----------------------------------------------------------------------------
//  project_open           (called from swmm_open in swmm5.c)
//  project_close          (called from swmm_close in swmm5.c)
//  project_readInput      (called from swmm_open in swmm5.c)
//  project_readOption     (called from readOption in input.c)
//  project_validate       (called from swmm_open in swmm5.c)
//  project_init           (called from swmm_start in swmm5.c)
//  project_addObject      (called from addObject in input.c)
//  project_createMatrix   (called from openFileForInput in iface.c)
//  project_freeMatrix     (called from iface_closeRoutingFiles)
//  project_findObject
//  project_findID

//-----------------------------------------------------------------------------
//  Function declarations
//-----------------------------------------------------------------------------
static void initPointers(void);
static void setDefaults(void);
static void openFiles(char *f1, char *f2, char *f3);
static void createObjects(void);
static void deleteObjects(void);
static void createHashTables(void);
static void deleteHashTables(void);


//=============================================================================

void project_open(char *f1, char *f2, char *f3)
//
//  Input:   f1 = pointer to name of input file
//           f2 = pointer to name of report file
//           f3 = pointer to name of binary output file
//  Output:  none
//  Purpose: opens a new SWMM project.
//
{
    initPointers();
    setDefaults();
    openFiles(f1, f2, f3);
}

//=============================================================================

void project_readInput()
//
//  Input:   none
//  Output:  none
//  Purpose: retrieves project data from input file.
//
{
	double temp1, temp2;

    // --- create hash tables for fast retrieval of objects by ID names
    createHashTables();

    // --- count number of objects in input file and create them
    input_countObjects();
    createObjects();

    // --- read project data from input file
    input_readData();
    if ( ErrorCode ) return;

    // --- establish starting & ending date/time
    StartDateTime = StartDate + StartTime;
    EndDateTime   = EndDate + EndTime;
    ReportStart   = ReportStartDate + ReportStartTime;
    ReportStart   = MAX(ReportStart, StartDateTime);
	temp1         = (((double)ReportStep + RouteStepMS) * 1000.0);
	ReportStep    = (int)temp1;
	temp2         = temp1 - (double)((int)temp1);
	
	// If temp1 == 2009.9999999999 then ReportStep is = 2009
	// instead of the 2010 it should be.  This corrects that
	// problem.
	if (temp2 >= 0.5)
		ReportStep++;

    // --- check for valid starting & ending date/times
    if ( EndDateTime <= StartDateTime )
    {
        report_writeErrorMsg(ERR_START_DATE, "");
    }
    else if ( EndDateTime <= ReportStart )
    {
        report_writeErrorMsg(ERR_REPORT_DATE, "");
    }
    else
    {
        // --- compute total duration of simulation in milliseconds
        //     (add on 1 msec to account for any roundoff)
        TotalDuration = (EndDateTime - StartDateTime) * MSECperDAY;
        TotalDuration += 1.0;

        // --- reporting step must be <= total duration
        if ( (double)ReportStep > TotalDuration )
        {
            ReportStep = (int)(TotalDuration);
        }
        if ( (double)ReportStep < (RouteStep * 1000.0) )
        {
            report_writeErrorMsg(ERR_REPORT_STEP, "");
        }
    }
}

//=============================================================================

void project_validate()
//
//  Input:   none
//  Output:  none
//  Purpose: checks validity of project data.
//
{
    int i;
    int j = 0;                                                                 //(5.0.010 - LR)

    climate_validate();
    for ( i=0; i<Nobjects[AQUIFER]; i++)   gwater_validateAquifer(i);
    for ( i=0; i<Nobjects[SUBCATCH]; i++ ) subcatch_validate(i);
    for ( i=0; i<Nobjects[SNOWMELT]; i++ ) snow_validateSnowmelt(i);           //(5.0.012 - LR)

    // --- validate each shape curve (do this before validating links)         //(5.0.010 - LR)
    for ( i=0; i<Nobjects[CURVE]; i++ )                                        //(5.0.010 - LR)
    {                                                                          //(5.0.010 - LR)
        if ( Curve[i].curveType == SHAPE_CURVE )                               //(5.0.010 - LR)
        {                                                                      //(5.0.010 - LR)
            Curve[i].refersTo = j;                                             //(5.0.010 - LR)
            Shape[j].curve = i;                                                //(5.0.010 - LR)
            if ( !shape_validate(&Shape[j], &Curve[i]) )                       //(5.0.010 - LR)
                report_writeErrorMsg(ERR_CURVE_SEQUENCE, Curve[i].ID);         //(5.0.010 - LR)
            j++;                                                               //(5.0.010 - LR)
        }                                                                      //(5.0.010 - LR)
    }                                                                          //(5.0.010 - LR)

    // --- validate links before nodes, since the latter can
    //     result in adjustment of node depths
    for ( i=0; i<Nobjects[LINK]; i++) link_validate(i);
    for ( i=0; i<Nobjects[NODE]; i++) node_validate(i);

    for ( i=0; i<Nobjects[CURVE]; i++ )
    {
        if ( !table_validate(&Curve[i]) )
            report_writeErrorMsg(ERR_CURVE_SEQUENCE, Curve[i].ID);
    }
    for ( i=0; i<Nobjects[TSERIES]; i++ )
    {
        if ( !table_validate(&Tseries[i]) )
            report_writeErrorMsg(ERR_TIMESERIES_SEQUENCE, Tseries[i].ID);
    }
}

//=============================================================================

void project_close()
//
//  Input:   none
//  Output:  none
//  Purpose: closes a SWMM project.
//
{
    deleteObjects();
    deleteHashTables();
}

//=============================================================================

int  project_init(void)
//
//  Input:   none
//  Output:  returns an error code
//  Purpose: initializes the internal state of all objects.
//
{
    int j;
    climate_initState();
    for (j=0; j<Nobjects[TSERIES]; j++)  table_tseriesInit(&Tseries[j]);
    for (j=0; j<Nobjects[GAGE]; j++)     gage_initState(j);
    for (j=0; j<Nobjects[SUBCATCH]; j++) subcatch_initState(j);
    for (j=0; j<Nobjects[NODE]; j++)     node_initState(j);
    for (j=0; j<Nobjects[LINK]; j++)     link_initState(j);
    return ErrorCode;
}

//=============================================================================

int   project_addObject(int type, char *id, int n)
//
//  Input:   type = object type
//           id   = object ID string
//           n    = object index
//  Output:  returns 0 if object already added, 1 if not, -1 if hashing fails
//  Purpose: adds an object ID to a hash table
//
{
    int  result;
    int  len;
    char *newID;

    // --- do nothing if object already placed in hash table
    if ( project_findObject(type, id) >= 0 ) return 0;

    // --- use memory from the hash tables' common memory pool to store
    //     a copy of the object's ID string
    len = (int)strlen(id) + 1;
    newID = (char *) Alloc(len*sizeof(char));
    strcpy(newID, id);

    // --- insert object's ID into the hash table for that type of object
    result = HTinsert(Htable[type], newID, n);
    if ( result == 0 ) result = -1;
    return result;
}

//=============================================================================

int   project_findObject(int type, char *id)
//
//  Input:   type = object type
//           id   = object ID
//  Output:  returns index of object with given ID, or -1 if ID not found
//  Purpose: uses hash table to find index of an object with a given ID.
//
{
    return HTfind(Htable[type], id);
}

//=============================================================================

char  *project_findID(int type, char *id)
//
//  Input:   type = object type
//           id   = ID name being sought
//  Output:  returns pointer to location where object's ID string is stored
//  Purpose: uses hash table to find address of given string entry.
//
{
    return HTfindKey(Htable[type], id);
}

//=============================================================================

double ** project_createMatrix(int nrows, int ncols)
//
//  Input:   nrows = number of rows (0-based)
//           ncols = number of columns (0-based)
//  Output:  returns a pointer to a matrix
//  Purpose: allocates memory for a matrix of doubles.
//
{
    int i,j;
    double **a;

    // --- allocate pointers to rows
    a = (double **) malloc(nrows * sizeof(double *));
    if ( !a ) return NULL;
    
    // --- allocate rows and set pointers to them
    a[0] = (double *) malloc (nrows * ncols * sizeof(double));
    if ( !a[0] ) return NULL;
    for ( i = 1; i < nrows; i++ ) a[i] = a[i-1] + ncols;

    for ( i = 0; i < nrows; i++)
    {
        for ( j = 0; j < ncols; j++) a[i][j] = 0.0;
    }
    
    // --- return pointer to array of pointers to rows
    return a;
}

//=============================================================================

void project_freeMatrix(double **a)
//
//  Input:   a = matrix of floats
//  Output:  none
//  Purpose: frees memory allocated for a matrix of doubles.
//
{
    if ( a != NULL )
    {
        if ( a[0] != NULL ) free( a[0] );
        free( a );
    }
}

//=============================================================================

int project_readOption(char* s1, char* s2)
//
//  Input:   s1 = option keyword
//           s2 = string representation of option's value
//  Output:  returns error code
//  Purpose: reads a project option from a pair of string tokens.
//
//  NOTE:    all project options have default values assigned in setDefaults().
//
{
    int      k, m, h, s;
    double   tStep;
    char     strDate[25];
    DateTime aTime;
    DateTime aDate;

    // --- determine which option is being read
    k = findmatch(s1, OptionWords);
    if ( k < 0 ) return error_setInpError(ERR_KEYWORD, s1);
    switch ( k )
    {
      // --- choice of flow units
      case FLOW_UNITS:
        //m = findmatch(s2, FlowUnitWords);
        //if ( m < 0 ) return error_setInpError(ERR_KEYWORD, s2);
        OutputFlowUnits = FlowUnits = CMS; // Always use CMS
        //if ( FlowUnits <= MGD )
        UnitSystem = SI; // Always use SI
        //else                    UnitSystem = SI;
        break;

      // --- choice of infiltration modeling method
      case INFIL_MODEL:
        m = findmatch(s2, InfilModelWords);
        if ( m < 0 ) return error_setInpError(ERR_KEYWORD, s2);
        InfilModel = m;
        break;

      // --- choice of flow routing method
      case ROUTE_MODEL:
        m = findmatch(s2, RouteModelWords);
        if ( m < 0 ) m = findmatch(s2, OldRouteModelWords);
        if ( m < 0 ) return error_setInpError(ERR_KEYWORD, s2);
        RouteModel = m;
        if ( RouteModel == EKW ) RouteModel = KW;                              //(5.0.010 - LR)
        break;

      // --- simulation start date
      case START_DATE:
        if ( !datetime_strToDate(s2, &StartDate) )
            return error_setInpError(ERR_DATETIME, s2);
        break;

      // --- simulation start time of day
      case START_TIME:
        if ( !datetime_strToTime(s2, &StartTime) )
            return error_setInpError(ERR_DATETIME, s2);
        break;

      // --- simulation ending date
      case END_DATE:
        if ( !datetime_strToDate(s2, &EndDate) ) 
            return error_setInpError(ERR_DATETIME, s2);
        break;

      // --- simulation ending time of day
      case END_TIME:
        if ( !datetime_strToTime(s2, &EndTime) )
            return error_setInpError(ERR_DATETIME, s2);
        break;

      // --- reporting start date
      case REPORT_START_DATE:
        if ( !datetime_strToDate(s2, &ReportStartDate) )
            return error_setInpError(ERR_DATETIME, s2);
        break;

      // --- reporting start time of day
      case REPORT_START_TIME:
        if ( !datetime_strToTime(s2, &ReportStartTime) )
            return error_setInpError(ERR_DATETIME, s2);
        break;

      // --- day of year when street sweeping begins or when it ends
      //     (year is arbitrarily set to 1947 so that the dayOfYear
      //      function can be applied)
      case SWEEP_START:
      case SWEEP_END:
        strcpy(strDate, s2);
        strcat(strDate, "/1947");
        if ( !datetime_strToDate(strDate, &aDate) )
            return error_setInpError(ERR_DATETIME, s2);
        m = datetime_dayOfYear(aDate);
        if ( k == SWEEP_START ) SweepStart = m;
        else SweepEnd = m;
        break;

      // --- number of antecedent dry days
      case START_DRY_DAYS:
        StartDryDays = atof(s2);
        if ( StartDryDays < 0.0 )
            return error_setInpError(ERR_NUMBER, s2);
        break;

      // --- runoff or reporting time steps
      //     (input is in hrs:min:sec format, time step saved as seconds)
      case WET_STEP:
      case DRY_STEP:
      case REPORT_STEP:
        if ( !datetime_strToTime(s2, &aTime) )
            return error_setInpError(ERR_DATETIME, s2);
        datetime_decodeTime(aTime, &h, &m, &s);
        h += 24*(int)aTime;
        s = s + 60*m + 3600*h;
        if ( s < 0 ) return error_setInpError(ERR_NUMBER, s2); // can be zero since we allow fractional seconds
        switch ( k )
        {
          case WET_STEP:     WetStep = s;     break;
          case DRY_STEP:     DryStep = s;     break;
          case REPORT_STEP:  ReportStep = s;  break;
        }
        break;

      // --- type of damping applied to inertial terms of dynamic wave routing
      case INERT_DAMPING:
        m = findmatch(s2, InertDampingWords);
        if ( m < 0 ) return error_setInpError(ERR_KEYWORD, s2);
        else InertDamping = m;
        break;

      // --- Yes/No options to allow ponding of flooded water above nodes,
      //     use slope weighting with dynamic wave flow routing,               //(5.0.010 - LR)
      //     or skip flow routing during steady state periods.                 //(5.0.010 - LR)
      //     (NO = 0, YES = 1)
      case ALLOW_PONDING:
      case SLOPE_WEIGHTING:
      case SKIP_STEADY_STATE:
      case IGNORE_RAINFALL:
        m = findmatch(s2, NoYesWords);
        if ( m < 0 ) return error_setInpError(ERR_KEYWORD, s2);
        switch ( k )
        {
          case ALLOW_PONDING:     AllowPonding    = m;  break;
          case SLOPE_WEIGHTING:   SlopeWeighting  = m;  break;
          case SKIP_STEADY_STATE: SkipSteadyState = m;  break;
          case IGNORE_RAINFALL:   IgnoreRainfall  = m;  break;
        }
        break;

      case NORMAL_FLOW_LTD:                                                    //(5.0.010 - LR)
        m = findmatch(s2, NormalFlowWords);                                    //(5.0.010 - LR)
        if ( m < 0 ) m = findmatch(s2, NoYesWords);                            //(5.0.010 - LR)
        if ( m < 0 ) return error_setInpError(ERR_KEYWORD, s2);                //(5.0.010 - LR)
        NormalFlowLtd = m;                                                     //(5.0.010 - LR)
        break;                                                                 //(5.0.010 - LR)

      case FORCE_MAIN_EQN:                                                     //(5.0.010 - LR)
        m = findmatch(s2, ForceMainEqnWords);                                  //(5.0.010 - LR)
        if ( m < 0 ) return error_setInpError(ERR_KEYWORD, s2);                //(5.0.010 - LR)
        ForceMainEqn = m;                                                      //(5.0.010 - LR)
        break;                                                                 //(5.0.010 - LR)

      case LINK_OFFSETS:                                                       //(5.0.012 - LR)
        m = findmatch(s2, LinkOffsetWords);                                    //(5.0.012 - LR)
        if ( m < 0 ) return error_setInpError(ERR_KEYWORD, s2);                //(5.0.012 - LR)
        LinkOffsets = m;                                                       //(5.0.012 - LR)
        break;                                                                 //(5.0.012 - LR)
     

      // --- compatibility option for selecting solution method for
      //     dynamic wave flow routing (NOT CURRENTLY USED)
      case COMPATIBILITY:
        if      ( strcomp(s2, "3") ) Compatibility = SWMM3;
        else if ( strcomp(s2, "4") ) Compatibility = SWMM4;
        else if ( strcomp(s2, "5") ) Compatibility = SWMM5;
        else return error_setInpError(ERR_KEYWORD, s2);
        break;

      // --- routing or lengthening time step (in decimal seconds)
      //     (lengthening time step is used in Courant stability formula
      //     to artificially lengthen conduits for dynamic wave flow routing
      //     (a value of 0 means that no lengthening is used))
      case ROUTE_STEP:
      case LENGTHENING_STEP:
        if ( !getDouble(s2, &tStep) )
        {
            if ( !datetime_strToTime(s2, &aTime) )
            {
                return error_setInpError(ERR_NUMBER, s2);
            }
            else
            {
                datetime_decodeTime(aTime, &h, &m, &s);
                h += 24*(int)aTime;
                s = s + 60*m + 3600*h;
                tStep = s;
            }
        }
        if ( k == ROUTE_STEP )
        {
            if ( tStep <= 0.0 ) return error_setInpError(ERR_NUMBER, s2);
            RouteStep = tStep;
        }
        else LengtheningStep = MAX(0.0, tStep);
        break;

      // --- safety factor applied to variable time step estimates under
      //     dynamic wave flow routing (value of 0 indicates that variable
      //     time step option not used)
      case VARIABLE_STEP:
        if ( !getDouble(s2, &CourantFactor) )
            return error_setInpError(ERR_NUMBER, s2);
        if ( CourantFactor < 0.0 || CourantFactor > 2.0 )
            return error_setInpError(ERR_NUMBER, s2);
        break;

      // --- minimum surface area (ft2 or sq. meters) associated with nodes
      //     under dynamic wave flow routing 
      case MIN_SURFAREA:
        MinSurfArea = atof(s2);
        break;

      case TEMPDIR: // Temporary Directory
        sstrncpy(TmpDir, s2, MAXFNAME);
        break;

////BEGIN ITM CODE
      // ITM OPTIONS
      case MIN_NUM_GRIDS:
        MinNumGrids = atoi(s2);
        break;

      case MAX_NUM_ITERATIONS:
        MaxNumIterations = atoi(s2);
        break;

      case MAX_NUM_CELLS:
        MaxNumCells = atoi(s2);
        break;

      case PRESSURIZED_WAVE_CELERITY:
        PressurizedWaveCelerity = atof(s2);
        break;

      case MIXED_FLOW_WAVE_CELERITY:
        MixedFlowWaveCelerity = atof(s2);
        break;

      case REF_DEPTH_FRACTION:
        RefDepthFraction = atof(s2);
        break;

      case MAX_NUM_PLOT_CELLS:
        MaxNumPlotCells = atoi(s2);
        break;

      case ITM_TOL_NORMAL:
        ITMTolNormal = atof(s2);
        break;

      case ITM_TOL_LOW:
        ITMTolLow = atof(s2);
        break;

      case ITM_TOL_VERY_LOW:
        ITMTolVeryLow = atof(s2);
        break;

      case ITM_TOL_HIGH:
        ITMTolHigh = atof(s2);
        break;

      case ITM_TOL_TRANSITION:
        ITMTolTransition = atof(s2);
        break;

	  case ROUTE_STEP_MS:
		RouteStepMS = atof(s2) / 1000.0;
		break;

	  case WATER_INIT_ELEVATION:
		WaterInitElevation = atof(s2);
		break;

	  case ITM_FLOW_TYPE:
        m = findmatch(s2, FlowTypeWords);                                      //(5.0.012 - LR)
        if ( m < 0 ) return error_setInpError(ERR_KEYWORD, s2);                //(5.0.012 - LR)
        ITMFlowType = m;                                                       //(5.0.012 - LR)
        break;                                                                 //(5.0.012 - LR)

////END ITM CODE
    }
    return 0;
}

//=============================================================================

void initPointers()
//
//  Input:   none
//  Output:  none
//  Purpose: assigns NULL to all dynamic arrays for a new project.
//
{
    Gage     = NULL;
    Subcatch = NULL;
    Node     = NULL;
    Outfall  = NULL;
    Divider  = NULL;
    Storage  = NULL;
    Link     = NULL;
    Conduit  = NULL;
    Pump     = NULL;
    Orifice  = NULL;
    Weir     = NULL;
    Outlet   = NULL;
    Pollut   = NULL;
    Landuse  = NULL;
    Pattern  = NULL;
    Curve    = NULL;
    Tseries  = NULL;
    Transect = NULL;
    Shape    = NULL;                                                           //(5.0.010 - LR)  
    HortInfil  = NULL;
    GAInfil    = NULL;
    CNInfil    = NULL;
    Aquifer    = NULL;
    UnitHyd    = NULL;
    Snowmelt   = NULL;
    MemPoolAllocated = FALSE;
}

//=============================================================================

void setDefaults()
//
//  Input:   none
//  Output:  none
//  Purpose: assigns default values to project variables.
//
{
   int i, j;

   // Project title & temp. file path
   for (i = 0; i < MAXTITLE; i++) strcpy(Title[i], "");
   strcpy(TmpDir, "");

   // Interface files
   Frain.mode      = SCRATCH_FILE;     // Use scratch rainfall file
   Fclimate.mode   = NO_FILE; 
   Frunoff.mode    = NO_FILE;
   Frdii.mode      = NO_FILE;
   Fhotstart1.mode = NO_FILE;
   Fhotstart2.mode = NO_FILE;
   Finflows.mode   = NO_FILE;
   Foutflows.mode  = NO_FILE;
   Frain.file      = NULL;
   Fclimate.file   = NULL;
   Frunoff.file    = NULL;
   Frdii.file      = NULL;
   Fhotstart1.file = NULL;
   Fhotstart2.file = NULL;
   Finflows.file   = NULL;
   Foutflows.file  = NULL;
   Fout.file       = NULL;
   Fout.mode       = NO_FILE;

   // Analysis options
   UnitSystem      = SI;               // SI unit system
   FlowUnits       = CMS;              // CMS flow units
   OutputFlowUnits = CMS;

   InfilModel      = HORTON;           // Horton infiltration method
   RouteModel      = ITM;              // ITM model
   AllowPonding    = FALSE;            // No ponding at nodes
   InertDamping    = SOME;             // Partial inertial damping
   NormalFlowLtd   = BOTH;             // Default normal flow limitation       //(5.0.010 - LR)
   ForceMainEqn    = H_W;              // Hazen-Williams eqn. for force mains  //(5.0.010 - LR)
   LinkOffsets     = DEPTH_OFFSET;     // Use depth for link offsets           //(5.0.012 - LR)
   LengtheningStep = 0;                // No lengthening of conduits
   CourantFactor   = 0.0;              // No variable time step 
   MinSurfArea     = 0.0;              // Use default min. nodal surface area
   SkipSteadyState = FALSE;            // Do flow routing in steady state periods 
   IgnoreRainfall  = FALSE;            // Analyze rainfall/runoff
   WetStep         = 300;              // Runoff wet time step (secs)
   DryStep         = 3600;             // Runoff dry time step (secs)
   RouteStep       = 300.0;            // Routing time step (secs)
   ReportStep      = 900;              // Reporting time step (secs)
   StartDryDays    = 0.0;              // Antecedent dry days

////BEGIN ITM CODE
   // ITM Default options
   MinNumGrids = 50;
   MaxNumCells = 5000;
   MaxNumIterations = 800;
   MaxNumPlotCells = 10;
   PressurizedWaveCelerity = 1000.0;
   MixedFlowWaveCelerity = 50.0;
   RefDepthFraction = 0.95;
   WaterInitElevation = -99999.5;
   ITMTolNormal = 0.0000001;
   ITMTolLow = 0.01;
   ITMTolVeryLow = 0.01;
   ITMTolHigh = 0.0000000000001;
   ITMTolTransition = 0.001;
   ITMFlowType		= -1;
////END ITM CODE

   // Deprecated options                                                       //(5.0.010 - LR)
   SlopeWeighting  = TRUE;             // Use slope weighting                  //(5.0.010 - LR)
   Compatibility   = SWMM4;            // Use SWMM 4 up/dn weighting method    //(5.0.010 - LR)

   // Starting & ending date/time
   StartDate       = datetime_encodeDate(2004, 1, 1);
   StartTime       = datetime_encodeTime(0,0,0);
   StartDateTime   = StartDate + StartTime;
////BEGIN ITM MODIFICATION
   //EndDate         = StartDate;
   EndDate         = datetime_encodeDate(2004, 2, 1);
////END ITM MODIFICATION
   EndTime         = 0.0;                                                      //(5.0.012 - LR)
   ReportStartDate = NO_DATE;
   ReportStartTime = NO_DATE;
   SweepStart      = 1;
   SweepEnd        = 365;

   // Reporting options
   RptFlags.input         = FALSE;
   RptFlags.continuity    = TRUE;
   RptFlags.flowStats     = TRUE;
   RptFlags.controls      = FALSE;
   RptFlags.subcatchments = FALSE;
   RptFlags.nodes         = FALSE;
   RptFlags.links         = FALSE;
   RptFlags.nodeStats     = FALSE;

   // Temperature data
   Temp.dataSource  = NO_TEMP;
   Temp.tSeries     = -1;
   Temp.ta          = 70.0;
   Temp.elev        = 0.0;
   Temp.anglat      = 40.0;
   Temp.dtlong      = 0.0;
   Temp.tmax        = MISSING;

   // Wind speed data
   Wind.type = MONTHLY_WIND;
   for ( i=0; i<12; i++ ) Wind.aws[i] = 0.0;

   // Snowmelt parameters
   Snow.snotmp      = 34.0;
   Snow.tipm        = 0.5;
   Snow.rnm         = 0.6;

   // Snow areal depletion curves for pervious and impervious surfaces
   for ( i=0; i<2; i++ )
   {
       for ( j=0; j<10; j++) Snow.adc[i][j] = 1.0;
   }

   // Evaporation rates
   Evap.type = CONSTANT_EVAP;
   for (i=0; i<12; i++)
   {
       Evap.monthlyEvap[i] = 0.0;
       Evap.panCoeff[i]    = 1.0;
   }
}

//=============================================================================

void openFiles(char *f1, char *f2, char *f3)
//
//  Input:   f1 = name of input file
//           f2 = name of report file
//           f3 = name of binary output file
//  Output:  none
//  Purpose: opens a project's input and report files.
//
{
    // --- initialize file pointers to NULL
    Finp.file = NULL;
    Frpt.file = NULL;
    Fout.file = NULL;

    // --- save file names
    sstrncpy(Finp.name, f1, MAXFNAME);
    sstrncpy(Frpt.name, f2, MAXFNAME);
    sstrncpy(Fout.name, f3, MAXFNAME);

    // --- check that file names are not identical
    if (strcomp(f1, f2) || strcomp(f1, f3) || strcomp(f2, f3))
    {
        writecon(FMT11);
        ErrorCode = ERR_FILE_NAME;
        return;
    }

    // --- open input and report files
    if ((Finp.file = fopen(f1,"rt")) == NULL)
    {
        writecon(FMT12);
        writecon(f1);
        ErrorCode = ERR_INP_FILE;
        return;
    }
    if ((Frpt.file = fopen(f2,"wt")) == NULL)
    {
       writecon(FMT13);
       ErrorCode = ERR_RPT_FILE;
       return;
    }
}

//=============================================================================

void createObjects()
//
//  Input:   none
//  Output:  none
//  Purpose: allocates memory for project's objects.
//
//  NOTE: number of each type of object has already been determined in
//        project_readInput().
//
{
    int j, k;

    // --- allocate memory for each category of object
    if ( ErrorCode ) return;
    Gage     = (TGage *)     calloc(Nobjects[GAGE],     sizeof(TGage));
    Subcatch = (TSubcatch *) calloc(Nobjects[SUBCATCH], sizeof(TSubcatch));
    Node     = (TNode *)     calloc(Nobjects[NODE],     sizeof(TNode));
    Outfall  = (TOutfall *)  calloc(Nnodes[OUTFALL],    sizeof(TOutfall));
    Divider  = (TDivider *)  calloc(Nnodes[DIVIDER],    sizeof(TDivider));
    Storage  = (TStorage *)  calloc(Nnodes[STORAGE],    sizeof(TStorage));
	RatingUnit = (TRatingUnit *) calloc(Nnodes[RATINGUNIT], sizeof(TRatingUnit));
    Link     = (TLink *)     calloc(Nobjects[LINK],     sizeof(TLink));
    Conduit  = (TConduit *)  calloc(Nlinks[CONDUIT],    sizeof(TConduit));
    Pump     = (TPump *)     calloc(Nlinks[PUMP],       sizeof(TPump));
    Orifice  = (TOrifice *)  calloc(Nlinks[ORIFICE],    sizeof(TOrifice));
    Weir     = (TWeir *)     calloc(Nlinks[WEIR],       sizeof(TWeir));
    Outlet   = (TOutlet *)   calloc(Nlinks[OUTLET],     sizeof(TOutlet));
    Pollut   = (TPollut *)   calloc(Nobjects[POLLUT],   sizeof(TPollut));
    Landuse  = (TLanduse *)  calloc(Nobjects[LANDUSE],  sizeof(TLanduse));
    Pattern  = (TPattern *)  calloc(Nobjects[TIMEPATTERN],  sizeof(TPattern));
    Curve    = (TTable *)    calloc(Nobjects[CURVE],    sizeof(TTable));
    Tseries  = (TTable *)    calloc(Nobjects[TSERIES],  sizeof(TTable));
    Aquifer  = (TAquifer *)  calloc(Nobjects[AQUIFER],  sizeof(TAquifer));
    UnitHyd  = (TUnitHyd *)  calloc(Nobjects[UNITHYD],  sizeof(TUnitHyd));
    Snowmelt = (TSnowmelt *) calloc(Nobjects[SNOWMELT], sizeof(TSnowmelt));
    Shape    = (TShape *)    calloc(Nobjects[SHAPE],    sizeof(TShape));       //(5.0.010 - LR)

    // --- create control rules
    ErrorCode = controls_create(Nobjects[CONTROL]);
    if ( ErrorCode ) return;

    // --- create cross section transects
    ErrorCode = transect_create(Nobjects[TRANSECT]);
    if ( ErrorCode ) return;

    // --- allocate memory for infiltration data
    if ( InfilModel == HORTON )
    {
        HortInfil = (THorton *) calloc(Nobjects[SUBCATCH], sizeof(THorton));
    }
    if ( InfilModel == GREEN_AMPT )
    {
        GAInfil = (TGrnAmpt *) calloc(Nobjects[SUBCATCH], sizeof(TGrnAmpt));
    }
    if ( InfilModel == CURVE_NUMBER )
    {
        CNInfil = (TCurveNum *) calloc(Nobjects[SUBCATCH], sizeof(TCurveNum));
    }

    // --- allocate memory for water quality state variables
    for (j = 0; j < Nobjects[SUBCATCH]; j++)
    {
        Subcatch[j].initBuildup =
                              (double *) calloc(Nobjects[POLLUT], sizeof(double));
        Subcatch[j].oldQual = (double *) calloc(Nobjects[POLLUT], sizeof(double));
        Subcatch[j].newQual = (double *) calloc(Nobjects[POLLUT], sizeof(double));
        Subcatch[j].pondedQual = (double *) calloc(Nobjects[POLLUT], sizeof(double));
        Subcatch[j].totalLoad  = (double *) calloc(Nobjects[POLLUT], sizeof(double));
    }
    for (j = 0; j < Nobjects[NODE]; j++)
    {
        Node[j].oldQual = (double *) calloc(Nobjects[POLLUT], sizeof(double));
        Node[j].newQual = (double *) calloc(Nobjects[POLLUT], sizeof(double));
        Node[j].extInflow = NULL;
        Node[j].dwfInflow = NULL;
        Node[j].rdiiInflow = NULL;
        Node[j].treatment = NULL;
    }
    for (j = 0; j < Nobjects[LINK]; j++)
    {
        Link[j].oldQual = (double *) calloc(Nobjects[POLLUT], sizeof(double));
        Link[j].newQual = (double *) calloc(Nobjects[POLLUT], sizeof(double));
    }

    // --- allocate memory for land use buildup/washoff functions
    for (j = 0; j < Nobjects[LANDUSE]; j++)
    {
        Landuse[j].buildupFunc =
            (TBuildup *) calloc(Nobjects[POLLUT], sizeof(TBuildup));
        Landuse[j].washoffFunc =
            (TWashoff *) calloc(Nobjects[POLLUT], sizeof(TWashoff));
    }

    // --- allocate memory for subcatchment landuse factors
    for (j = 0; j < Nobjects[SUBCATCH]; j++)
    {
        Subcatch[j].landFactor =
            (TLandFactor *) calloc(Nobjects[LANDUSE], sizeof(TLandFactor));
        for (k = 0; k < Nobjects[LANDUSE]; k++)
        {
            Subcatch[j].landFactor[k].buildup =
                (double *) calloc(Nobjects[POLLUT], sizeof(double));
        }
    }

    // --- initialize buildup & washoff functions
    for (j = 0; j < Nobjects[LANDUSE]; j++)
    {
        for (k = 0; k < Nobjects[POLLUT]; k++)
        {
            Landuse[j].buildupFunc[k].funcType = NO_BUILDUP;
            Landuse[j].buildupFunc[k].normalizer = PER_AREA;
            Landuse[j].washoffFunc[k].funcType = NO_WASHOFF;
        }
    }

    // --- initialize rain gage properties
    for (j = 0; j < Nobjects[GAGE]; j++)
    {
        Gage[j].tSeries = -1;
        strcpy(Gage[j].fname, "");
    }

    // --- initialize subcatchment properties
    for (j = 0; j < Nobjects[SUBCATCH]; j++)
    {
        Subcatch[j].outSubcatch = -1;
        Subcatch[j].outNode     = -1;
        Subcatch[j].infil       = -1;
        Subcatch[j].groundwater = NULL;
        Subcatch[j].snowpack    = NULL;
        for (k = 0; k < Nobjects[POLLUT]; k++)
        {
            Subcatch[j].initBuildup[k] = 0.0;
        }
    }

    // --- initialize RDII unit hydrograph properties
    for ( j = 0; j < Nobjects[UNITHYD]; j++ ) rdii_initUnitHyd(j);

    // --- initialize snowmelt properties
    for ( j = 0; j < Nobjects[SNOWMELT]; j++ ) snow_initSnowmelt(j);

    // --- initialize link properties
    for (j = 0; j < Nobjects[LINK]; j++)
    {
        Link[j].xsect.type   = -1;
        Link[j].cLossInlet   = 0.5;
        Link[j].cLossOutlet  = 0.5;
        Link[j].cLossAvg     = 0.0;
        Link[j].hasFlapGate  = FALSE;
    }
    for (j = 0; j < Nlinks[PUMP]; j++) Pump[j].pumpCurve  = -1;

    // --- initialize reporting flags
    for (j = 0; j < Nobjects[SUBCATCH]; j++) Subcatch[j].rptFlag = FALSE;
    for (j = 0; j < Nobjects[NODE]; j++) Node[j].rptFlag = FALSE;
    for (j = 0; j < Nobjects[LINK]; j++) Link[j].rptFlag = FALSE;

    //  --- initialize curves, time series, and time patterns
    for (j = 0; j < Nobjects[CURVE]; j++)   table_init(&Curve[j]);
    for (j = 0; j < Nobjects[TSERIES]; j++) table_init(&Tseries[j]);
    for (j = 0; j < Nobjects[TIMEPATTERN]; j++) inflow_initDwfPattern(j);
}

//=============================================================================

void deleteObjects()
//
//  Input:   none
//  Output:  none
//  Purpose: frees memory allocated for a project's objects.
//
//  NOTE: care is taken to first free objects that are properties of another
//        object before the latter is freed (e.g., we must free a
//        subcatchment's land use factors before freeing the subcatchment).
//
{
    int j, k;

    // --- free memory for landuse factors & groundwater
    if ( Subcatch ) for (j = 0; j < Nobjects[SUBCATCH]; j++)
    {
        for (k = 0; k < Nobjects[LANDUSE]; k++)
        {
            FREE(Subcatch[j].landFactor[k].buildup);
        }
        FREE(Subcatch[j].landFactor);
        FREE(Subcatch[j].groundwater);
    }

    // --- free memory for buildup/washoff functions
    if ( Landuse ) for (j = 0; j < Nobjects[LANDUSE]; j++)
    {
        FREE(Landuse[j].buildupFunc);
        FREE(Landuse[j].washoffFunc)
    }

    // --- free memory for water quality state variables
    if ( Subcatch ) for (j = 0; j < Nobjects[SUBCATCH]; j++)
    {
        FREE(Subcatch[j].initBuildup);
        FREE(Subcatch[j].oldQual);
        FREE(Subcatch[j].newQual);
        FREE(Subcatch[j].pondedQual);
        FREE(Subcatch[j].totalLoad);
    }
    if ( Node ) for (j = 0; j < Nobjects[NODE]; j++)
    {
        FREE(Node[j].oldQual);
        FREE(Node[j].newQual);
    }
    if ( Link ) for (j = 0; j < Nobjects[LINK]; j++)
    {
        FREE(Link[j].oldQual);
        FREE(Link[j].newQual);
    }

    // --- free memory used for nodal inflows & treatment functions
    if ( Node ) for (j = 0; j < Nobjects[NODE]; j++)
    {
        inflow_deleteExtInflows(j);
        inflow_deleteDwfInflows(j);
        rdii_deleteRdiiInflow(j);
        treatmnt_delete(j);
    }

    // --- delete table entries for curves and time series
    if ( Tseries ) for (j = 0; j < Nobjects[TSERIES]; j++)
        table_deleteEntries(&Tseries[j]);
    if ( Curve ) for (j = 0; j < Nobjects[CURVE]; j++)
        table_deleteEntries(&Curve[j]);

    // --- delete cross section transects
    transect_delete();

    // --- delete control rules
    controls_delete();

    // --- now free each major category of object
    FREE(Gage);
    FREE(Subcatch);
    FREE(Node);
    FREE(Outfall);
    FREE(Divider);
    FREE(Storage);
    FREE(Link);
    FREE(Conduit);
    FREE(Pump);
    FREE(Orifice);
    FREE(Weir);
    FREE(Outlet);
    FREE(Pollut);
    FREE(Landuse);
    FREE(Pattern);
    FREE(Curve);
    FREE(Tseries);
    FREE(HortInfil);
    FREE(GAInfil);
    FREE(CNInfil);
    FREE(Aquifer);
    FREE(UnitHyd);
    FREE(Snowmelt);
    FREE(Shape);                                                               //(5.0.010 - LR)
}

//=============================================================================

void createHashTables()
//
//  Input:   none
//  Output:  returns error code
//  Purpose: allocates memory for object ID hash tables
//
{   int j;
    MemPoolAllocated = FALSE;
    for (j = 0; j < MAX_OBJ_TYPES ; j++)
    {
         Htable[j] = HTcreate();
         if ( Htable[j] == NULL ) report_writeErrorMsg(ERR_MEMORY, "");
    }

    // --- initialize memory pool used to store object ID's
    if ( AllocInit() == NULL ) report_writeErrorMsg(ERR_MEMORY, "");
    else MemPoolAllocated = TRUE;
}

//=============================================================================

void deleteHashTables()
//
//  Input:   none
//  Output:  none
//  Purpose: frees memory allocated for object ID hash tables
//
{
    int j;
    for (j = 0; j < MAX_OBJ_TYPES; j++)
    {
        if ( Htable[j] != NULL ) HTfree(Htable[j]);
    }

    // --- free object ID memory pool
    if ( MemPoolAllocated ) AllocFreePool();
}

//=============================================================================
