//-----------------------------------------------------------------------------
//   rdii.c
//
//   Project:  EPA SWMM5
//   Version:  5.0
//   Date:     6/19/07   (Build 5.0.010)
//             2/4/08    (Build 5.0.012)
//   Author:   L. Rossman (EPA)
//             R. Dickinson (CDM)
//
//   RDII processing functions.
//
//   Note: RDII means rainfall dependent infiltration/inflow,
//         UH means unit hydrograph.
//
////////////////////////////////////////////////////////////////
// This module was heavily modified for Build 5.0.010 to include
// Initial Abstraction in the evaluation of RDII flows. - LR
////////////////////////////////////////////////////////////////
//-----------------------------------------------------------------------------
#define _CRT_SECURE_NO_DEPRECATE

#include <math.h>
#include <string.h>
#include <malloc.h>
#include "headers.h"

//-----------------------------------------------------------------------------                  
// Constants
//-----------------------------------------------------------------------------
const double ZERO_RDII = 0.0001;       // Minimum non-zero RDII inflow (cfs)
const int    IA_MAX    = 0;            // Max. initial abstraction index
const int    IA_REC    = 1;            // Initial abstraction recovery index
const int    IA_INIT   = 2;            // Initial stored abstraction index

//-----------------------------------------------------------------------------                  
// Data Structures
//-----------------------------------------------------------------------------                  
typedef struct                         // Processing data for each UH group
{                                      // ----------------------------------
   double    area;                     // sewered area covered by UH's gage (ft2)
   double*   pastRain;                 // array of past rainfall values
   char*     pastMonth;                // month in which past rainfall occurred
   int       period;                   // current UH time period 
   int       hasPastRain;              // true if > 0 past periods with rain
   int       maxPeriods;               // max. past rainfall periods
   long      drySeconds;               // time since last nonzero rainfall
   DateTime  gageDate;                 // calendar date of rain gage period
   int       isUsed;                   // true if UH group used by any nodes
   double    iaUsed;                   // initial abstraction used (inches or mm)
   double    rdii;                     // rdii flow (in rainfall units)
   DateTime  lastDate;                 // date of last rdii computed
}  TUHData;


//-----------------------------------------------------------------------------                  
// Shared Variables
//-----------------------------------------------------------------------------                  
static TUHData*   UHData;              // processing data for each UH group
static int        RdiiStep;            // RDII time step (sec)
static int        NumRdiiNodes;        // number of nodes w/ RDII data
static int*       RdiiNodeIndex;       // indexes of nodes w/ RDII data
static double*    RdiiNodeFlow;        // inflows for nodes with RDII 
static int        RdiiFlowUnits;       // RDII flow units code
static DateTime   RdiiStartDate;       // start date of RDII inflow period
static DateTime   RdiiEndDate;         // end date of RDII inflow period 
static double     TotalRainVol;        // total rainfall volume (ft3)
static double     TotalRdiiVol;        // total RDII volume (ft3)

//-----------------------------------------------------------------------------
// Imported Variables
//-----------------------------------------------------------------------------
extern double     Qcf[];               // flow units conversion factors
                                       // (see swmm5.c)
//-----------------------------------------------------------------------------
//  External functions (declared in funcs.h)
//-----------------------------------------------------------------------------
//  rdii_readRdiiInflow     (called from parseLine in input.c)
//  rdii_deleteRdiiInflow   (called from deleteObjects in project.c)
//  rdii_initUnitHyd        (called from createObjects in project.c)
//  rdii_readUnitHydParams  (called from parseLine in input.c)
//  rdii_openRdii           (called from rain_open)
//  rdii_closeRdii          (called from rain_close)
//  rdii_getNumRdiiFlows    (called from addRdiiInflows in routing.c)
//  rdii_getRdiiFlow        (called from addRdiiInflows in routing.c)

//-----------------------------------------------------------------------------
// Function Declarations
//-----------------------------------------------------------------------------
// --- functions used to create a RDII file
static void   createRdiiFile(void);
static void   setUnitHydParams(int j, int m, double x[]);
static int    getNumRdiiNodes(void);
static void   validateRdii(void);
static void   openRdiiProcessor(void);
static int    allocRdiiMemory(void);
static int    getMaxPeriods(int i);
static void   initGageData(void);
static void   initUnitHydData(void);
static int    openNewRdiiFile(void);
static void   getRainfall(DateTime currentDate);
static double applyIA(int j, DateTime aDate, double dt, double rainDepth);
static void   getUnitHydRdii(DateTime currentDate);
static double getUnitHydConvol(int unithyd);
static double getUnitHydOrd(int unithyd, int month, int k, double t);
static int    getNodeRdii(void);
static void   saveRdiiFlows(DateTime currentDate);
static void   closeRdiiProcessor(void);
static void   freeRdiiMemory(void);

// --- functions used to read an existing RDII file
static int   readRdiiFileHeader(void);
static void  readRdiiFlows(void);


//=============================================================================
//                   Management of RDII-Related Data
//=============================================================================

int rdii_readRdiiInflow(char* tok[], int ntoks)
//
//  Input:   tok[] = array of string tokens
//           ntoks = number of tokens
//  Output:  returns an error code
//  Purpose: reads properties of an RDII inflow from a line of input.
//
{
    int    j, k;
    double a;
    TRdiiInflow* inflow;

    // --- check for proper number of items
    if ( ntoks < 3 ) return error_setInpError(ERR_ITEMS, "");

    // --- check that node receiving RDII exists
    j = project_findObject(NODE, tok[0]);
    if ( j < 0 ) return error_setInpError(ERR_NAME, tok[0]);
    
    // --- check that RDII unit hydrograph exists
    k = project_findObject(UNITHYD, tok[1]);
    if ( k < 0 ) return error_setInpError(ERR_NAME, tok[1]);

    // --- read in sewer area value
    if ( !getDouble(tok[2], &a) || a < 0.0 )
        return error_setInpError(ERR_NUMBER, tok[2]);

    // --- create the RDII inflow object if it doesn't already exist
    inflow = Node[j].rdiiInflow;
    if ( inflow == NULL )
    {
        inflow = (TRdiiInflow *) malloc(sizeof(TRdiiInflow));
        if ( !inflow ) return error_setInpError(ERR_MEMORY, "");
    }

    // --- assign UH & area to inflow object
    inflow->unitHyd = k;
    inflow->area = a / UCF(LANDAREA);

    // --- assign inflow object to node
    Node[j].rdiiInflow = inflow;
    return 0;
}

//=============================================================================

void rdii_initUnitHyd(int j)
//
//  Input:   j = UH group index
//  Output:  none
//  Purpose: initializes properties of a unit hydrograph group.
//
{
    int i;                             // individual UH index
    int m;                             // month index

    for ( m=0; m<12; m++)
    {
        for (i=0; i<3; i++)
        {
            UnitHyd[j].ia[m][i] = 0.0;
            UnitHyd[j].r[m][i] = 0.0;
            UnitHyd[j].tPeak[m][i] = 0;
            UnitHyd[j].tBase[m][i] = 0;
        }
    }
}

//=============================================================================

int rdii_readUnitHydParams(char* tok[], int ntoks)
//
//  Input:   tok[] = array of string tokens
//           ntoks = number of tokens
//  Output:  returns an error code
//  Purpose: reads parameters of an RDII unit hydrograph from a line of input.
//
{
    int i, j, m, g;
    double x[12];

    // --- check that RDII UH object exists in database
    j = project_findObject(UNITHYD, tok[0]);
    if ( j < 0 ) return error_setInpError(ERR_NAME, tok[0]);
   
    // --- assign UH ID to name in hash table
    if ( UnitHyd[j].ID == NULL )
        UnitHyd[j].ID = project_findID(UNITHYD, tok[0]);

    // --- line has 2 tokens; assign rain gage to UH object
    if ( ntoks == 2 )
    {
        g = project_findObject(GAGE, tok[1]);
        if ( g < 0 ) return error_setInpError(ERR_NAME, tok[1]);
        UnitHyd[j].rainGage = g;
        return 0;
    }

    // --- line has >= 11 tokens; retrieve & save UH params.
    if ( ntoks >= 11 )
    {
        // --- find which month UH params apply to
        m = datetime_findMonth(tok[1]);
        if ( m == 0 )
        {
            if ( !match(tok[1], w_ALL) )
                return error_setInpError(ERR_KEYWORD, tok[1]);
        }

        // --- read 3 sets of r-t-k values
        for ( i = 0; i < 9; i++ )
        {
            if ( ! getDouble(tok[i+2], &x[i]) )
                return error_setInpError(ERR_NUMBER, tok[i+2]);
        }
    
        // --- read initial abstraction parameters
        for (i = 9; i < 12; i++)
        {
            x[i] = 0.0;
            if ( ntoks >= i+3 )
            {
                if ( ! getDouble(tok[i+2], &x[i]) )
                    return error_setInpError(ERR_NUMBER, tok[i+2]);
            }
        }

        // --- save UH params
        setUnitHydParams(j, m, x);
        return 0;
    }
    else return error_setInpError(ERR_ITEMS, "");
}

//=============================================================================

void setUnitHydParams(int j, int m, double x[])
//
//  Input:   j = unit hydrograph index
//           m = month of year (0 = all months)
//           x = array of parameters for set of 3 unit hydrographs + Abstraction
//  Output:  none
//  Purpose: assigns parameters to unit hydrographs for specified month of year.
//
{
    int    i,                          // individual UH index
           m1, m2,                     // start/end month indexes
           n;                          // index into parameter array x
    double t,                          // UH time to peak (hrs)
           k,                          // UH k-value
           tBase;                      // UH base time (hrs)

    // --- find range of months that share same parameter values
    if ( m == 0 )
    {
        m1 = 0;
        m2 = 11;
    }
    else
    {
        m1 = m-1;
        m2 = m1;
    }

    // --- for each month in the range
    for (m=m1; m<=m2; m++)
    {
        // --- for each of 3 unit hydrographs
        for (i=0; i<3; i++)
        {
            // --- set UH response ratio, time to peak, & base time
            n = i*3;
            UnitHyd[j].r[m][i] = x[n];
            t = x[n+1];
            k = x[n+2];
            tBase = t * (1.0 + k);                              // hours
            UnitHyd[j].tPeak[m][i] = (long)(t * 3600.);         // seconds
            UnitHyd[j].tBase[m][i] = (long)(tBase * 3600.);     // seconds

            // -- set initial abstraction parameters
            UnitHyd[j].ia[m][i] = x[9+i];
        }
    }
}

//=============================================================================

void rdii_deleteRdiiInflow(int j)
//
//  Input:   j = node index
//  Output:  none
//  Purpose: deletes the RDII inflow object for a node.
//
{
    if ( Node[j].rdiiInflow )
    {
        free(Node[j].rdiiInflow);
        Node[j].rdiiInflow = NULL;
    }
}


//=============================================================================
//                 Reading Inflow Data From a RDII File
//=============================================================================

void rdii_openRdii()
//
//  Input:   none
//  Output:  none
//  Purpose: opens an exisiting RDII interface file or creates a new one.
//
{
    // --- initialize shared RDII variables
    RdiiNodeIndex = NULL;
    RdiiNodeFlow = NULL;
    NumRdiiNodes = 0;
    RdiiStartDate = NO_DATE;

    // --- create the RDII file if existing file not being used
    if ( Frdii.mode != USE_FILE ) createRdiiFile();
    if ( Frdii.mode == NO_FILE || ErrorCode ) return;

    // --- open the RDII file
    Frdii.file = fopen(Frdii.name, "rt");
    if ( Frdii.file == NULL)
    {
        if ( Frdii.mode == SCRATCH_FILE )
        {
            report_writeErrorMsg(ERR_RDII_FILE_SCRATCH, "");
        }
        else
        {
            report_writeErrorMsg(ERR_RDII_FILE_OPEN, Frdii.name);
        }
        return;
    }

    // --- read header records from file
    ErrorCode = readRdiiFileHeader();
    if ( ErrorCode )
    {
        report_writeErrorMsg(ErrorCode, Frdii.name);
    }
    else readRdiiFlows();
}

//=============================================================================

void rdii_closeRdii()
//
//  Input:   none
//  Output:  none
//  Purpose: closes the RDII interface file.
//
{
    if ( Frdii.file ) fclose(Frdii.file);
    if ( Frdii.mode == SCRATCH_FILE ) remove(Frdii.name);
    FREE(RdiiNodeIndex);
    FREE(RdiiNodeFlow);
}

//=============================================================================

int rdii_getNumRdiiFlows(DateTime aDate)
//
//  Input:   aDate = current date/time
//  Output:  returns 0 if no RDII flow or number of nodes with RDII inflows
//  Purpose: finds number of RDII inflows at a specified date.
//
{
    // --- default result is 0 indicating no RDII inflow at specified date
    if ( NumRdiiNodes == 0 ) return 0;
    if ( !Frdii.file ) return 0;

    // --- keep reading RDII file as need be
    while ( !feof(Frdii.file) )
    {
        // --- return if date of current RDII inflow not reached yet
        if ( RdiiStartDate == NO_DATE ) return 0;
        if ( aDate < RdiiStartDate ) return 0;

        // --- return RDII node count if specified date falls 
        //     within time interval of current RDII inflow 
        if ( aDate < RdiiEndDate ) return NumRdiiNodes;

        // --- otherwise get next date and RDII flow values from file
        else readRdiiFlows();
    }
    return 0;
}

//=============================================================================

void rdii_getRdiiFlow(int i, int* j, double* q)
//
//  Input:   i = RDII node index
//           j = pointer to project node index
//           q = pointer to RDII flow rate
//  Output:  returns node index and RDII inflow for node
//  Purpose: finds index and current RDII inflow for an RDII node.
//
{
    if ( i >= 0 && i < NumRdiiNodes )
    {
        *j = RdiiNodeIndex[i];
        *q = RdiiNodeFlow[i];
    }
}

//=============================================================================

int readRdiiFileHeader()
//
//  Input:   none
//  Output:  returns error code
//  Purpose: reads header information from RDII file.
//
{
    int   i;
    char  line[MAXLINE+1];             // line from RDII data file
    char  s1[MAXLINE+1];               // general string variable
    char  s2[MAXLINE+1];         

    // --- check for correct file type
    fgets(line, MAXLINE, Frdii.file);
    sscanf(line, "%s", s1);
    if ( strcmp(s1, "SWMM5") != 0 ) return ERR_RDII_FILE_FORMAT;

    // --- skip title line
    fgets(line, MAXLINE, Frdii.file);

    // --- read RDII UH time step interval (sec)
    RdiiStep = 0;
    fgets(line, MAXLINE, Frdii.file);
    sscanf(line, "%d", &RdiiStep);
    if ( RdiiStep <= 0 ) return ERR_RDII_FILE_FORMAT;

    // --- skip over line with number of constituents (= 1 for RDII)
    fgets(line, MAXLINE, Frdii.file);

    // --- read flow units
    fgets(line, MAXLINE, Frdii.file);
    sscanf(line, "%s %s", s1, s2);
    RdiiFlowUnits = findmatch(s2, FlowUnitWords);
    if ( RdiiFlowUnits < 0 ) return ERR_RDII_FILE_FORMAT;

    // --- read number of RDII nodes
    fgets(line, MAXLINE, Frdii.file);
    if ( sscanf(line, "%d", &NumRdiiNodes) < 1 ) return ERR_RDII_FILE_FORMAT;

    // --- allocate memory for RdiiNodeIndex & RdiiNodeFlow arrays
    RdiiNodeIndex = (int *) calloc(NumRdiiNodes, sizeof(int));
    if ( !RdiiNodeIndex ) return ERR_MEMORY;
    RdiiNodeFlow = (double *) calloc(NumRdiiNodes, sizeof(double));
    if ( !RdiiNodeFlow ) return ERR_MEMORY;

    // --- read names of RDII nodes from file & save their indexes
    for ( i=0; i<NumRdiiNodes; i++ )
    {
        if ( feof(Frdii.file) ) return ERR_RDII_FILE_FORMAT;
        fgets(line, MAXLINE, Frdii.file);
        sscanf(line, "%s", s1);
        RdiiNodeIndex[i] = project_findObject(NODE, s1);
    }

    // --- skip column heading line
    if ( feof(Frdii.file) ) return ERR_RDII_FILE_FORMAT;
    fgets(line, MAXLINE, Frdii.file);
    return 0;
}

//=============================================================================

void readRdiiFlows()
//
//  Input:   none
//  Output:  none
//  Purpose: reads date and flow values of next RDII inflows from RDII file.
//
{
    int    i, n;
    int    yr = 0, mon = 0, day = 0,
		   hr = 0, min = 0, sec = 0;   // year, month, day, hour, minute, second
    float  x;                          // RDII flow in original units
    char   line[MAXLINE+1];            // line from RDII data file
    char   s[MAXLINE+1];               // node ID label (not used)

    RdiiStartDate = NO_DATE;
    for (i=0; i<NumRdiiNodes; i++)
    {
        if ( feof(Frdii.file) ) return;
        fgets(line, MAXLINE, Frdii.file);
        n = sscanf(line, "%s %d %d %d %d %d %d %f",
            s, &yr, &mon, &day, &hr, &min, &sec, &x);
        if ( n < 8 ) return;
        RdiiNodeFlow[i] = x / Qcf[RdiiFlowUnits];
    }
    RdiiStartDate = datetime_encodeDate(yr, mon, day) +
                    datetime_encodeTime(hr, min, sec);
    RdiiEndDate = datetime_addSeconds(RdiiStartDate, RdiiStep);
}


//=============================================================================
//                   Creation of a RDII Interface File
//=============================================================================

void createRdiiFile()
//
//  Input:   none
//  Output:  none
//  Purpose: computes time history of RDII inflows and saves them to file.
//
{
    int      hasRdii;                  // true when total RDII > 0 
    double   elapsedTime;              // current elapsed time (sec)
    double   duration;                 // duration being analyzed (sec)
    DateTime currentDate;              // current calendar date/time

    // --- set RDII reporting time step to Runoff wet step
    RdiiStep = WetStep;

    // --- count nodes with RDII data
    NumRdiiNodes = getNumRdiiNodes();

    // --- if no RDII nodes then re-set RDII file usage to NO_FILE
    if ( NumRdiiNodes == 0 )
    {
        Frdii.mode = NO_FILE;
        return;
    }

    // --- otherwise set file usage to SCRATCH if originally set to NO_FILE
    else if ( Frdii.mode == NO_FILE ) Frdii.mode = SCRATCH_FILE;

    // --- validate RDII data
    validateRdii();
    if ( ErrorCode ) return;

    // --- open RDII processing system
    openRdiiProcessor();
    if ( !ErrorCode )
    {
        // --- initialize rain gage & UH processing data
        initGageData();
        initUnitHydData();

        // --- convert total simulation duration from millisec to sec
        duration = TotalDuration / 1000.0;
    
        // --- examine rainfall record over each RdiiStep time step
        elapsedTime = 0.0;
        while ( elapsedTime <= duration && !ErrorCode )
        {
            // --- compute current calendar date/time 
            currentDate = StartDateTime + elapsedTime / SECperDAY;

            // --- update rainfall at all rain gages
            getRainfall(currentDate);

            // --- compute convolutions of past rainfall with UH's
            getUnitHydRdii(currentDate);
    
            // --- find RDII at all nodes
            hasRdii = getNodeRdii();
    
            // --- save RDII at all nodes to file for current date
            if ( hasRdii ) saveRdiiFlows(currentDate);
    
            // --- advance one time step
            elapsedTime += RdiiStep; 
        }
    }

    // --- close RDII processing system
    closeRdiiProcessor();
} 

//=============================================================================

int  getNumRdiiNodes()
//
//  Input:   none
//  Output:  returns node count
//  Purpose: counts number of nodes that receive RDII inflow.
//
{
    int j,                             // node index
        n;                             // node count

    n = 0;
    for (j=0; j<Nobjects[NODE]; j++)
    {
        if ( Node[j].rdiiInflow ) n++;
    }
    return n;
}

//=============================================================================

void validateRdii()
//
//  Input:   none
//  Output:  none
//  Purpose: validates UH and RDII inflow object data.
//
{
    int    i,                          // node index
           j,                          // UH group index
           k,                          // individual UH index
           m;                          // month index
    double rsum;                       // sum of UH r-values

    // --- check each unit hydrograph for consistency
    for (j=0; j<Nobjects[UNITHYD]; j++)
    {
        for (m=0; m<12; m++)
        {
            rsum = 0.0;
            for (k=0; k<3; k++)
            {
                // --- if no base time then UH doesn't exist
                if ( UnitHyd[j].tBase[m][k] == 0 ) continue;

                // --- can't have negative UH parameters
                if ( UnitHyd[j].tPeak[m][k] < 0.0 ) 
                {
                    report_writeErrorMsg(ERR_UNITHYD_TIMES, UnitHyd[j].ID);
                }

                // --- can't have negative UH response ratio
                if ( UnitHyd[j].r[m][k] < 0.0 )
                {
                    report_writeErrorMsg(ERR_UNITHYD_RATIOS, UnitHyd[j].ID);
                }
                else rsum += UnitHyd[j].r[m][k];
            }
            if ( rsum > 1.01 )
            {
                report_writeErrorMsg(ERR_UNITHYD_RATIOS, UnitHyd[j].ID);
            }
        }
    }

    // --- check each node's RDII inflow object
    for (i=0; i<Nobjects[NODE]; i++)
    {
        if ( Node[i].rdiiInflow )
        {
            // --- check that sewer area is non-negative
            if ( Node[i].rdiiInflow->area < 0.0 )
            {
                report_writeErrorMsg(ERR_RDII_AREA, Node[i].ID);
            }
        }
    }
}

//=============================================================================

void openRdiiProcessor()
//
//  Input:   none
//  Output:  none
//  Purpose: opens RDII processing system.
//
{
    int j;                             // object index
    int n;                             // RDII node count

    // --- set RDII processing arrays to NULL
    UHData   = NULL;
    RdiiNodeIndex = NULL;
    RdiiNodeFlow = NULL;
    TotalRainVol = 0.0;
    TotalRdiiVol = 0.0;

    // --- allocate memory used for RDII processing
    if ( !allocRdiiMemory() )
    {
        report_writeErrorMsg(ERR_MEMORY, "");
        return;
    }

    // --- open & initialize RDII file
    if ( !openNewRdiiFile() )
    {
        report_writeErrorMsg(ERR_RDII_FILE_SCRATCH, "");
        return;
    }

    // --- identify index of each node with RDII inflow
    n = 0;    
    for (j=0; j<Nobjects[NODE]; j++)
    {
        if ( Node[j].rdiiInflow )
        {
            RdiiNodeIndex[n] = j;
            n++;
        }
    }
}

//=============================================================================

int  allocRdiiMemory()
//
//  Input:   none
//  Output:  returns TRUE if successful, FALSE if not
//  Purpose: allocates memory used for RDII processing .
//
//
{
    int i;                             // UH group index
    int n;                             // number of past rain periods

    // --- allocate memory for RDII processing data for UH groups
    UHData = (TUHData *) calloc(Nobjects[UNITHYD], sizeof(TUHData));
    if ( !UHData ) return FALSE;

    // --- allocate memory for past rainfall data for each UH group
    for (i=0; i<Nobjects[UNITHYD]; i++)
    {
        UHData[i].pastRain = NULL;
        UHData[i].pastMonth = NULL;
        UHData[i].maxPeriods = getMaxPeriods(i);
        n = UHData[i].maxPeriods;
        if ( n > 0 )
        {
            UHData[i].pastRain = (double *) calloc(n, sizeof(double));
            if ( !UHData[i].pastRain ) return FALSE;
            UHData[i].pastMonth = (char *) calloc(n, sizeof(char));
            if ( !UHData[i].pastMonth ) return FALSE;
        }
    }

    // --- allocate memory for RDII indexes & inflow at each node w/ RDII data
    RdiiNodeIndex = (int *) calloc(NumRdiiNodes, sizeof(int));
    if ( !RdiiNodeIndex ) return FALSE;
    RdiiNodeFlow = (double *) calloc(NumRdiiNodes, sizeof(double));
    if ( !RdiiNodeFlow ) return FALSE;
    return TRUE;
}

//=============================================================================

int  getMaxPeriods(int i)
//
//  Input:   i = UH group index
//  Output:  returns number of past rainfall values
//  Purpose: finds number of past rainfall values to save for a UH group.
//
{
    int   g,                           // rain gage index
          k,                           // individual UH index
          m,                           // month index
          n,                           // number of time periods
          nMax,                        // maximum number of time periods
          gageInterval;                // rainfall recording interval (sec)

    // --- get the UH's rain gage recording interval
    g = UnitHyd[i].rainGage;
    gageInterval = Gage[g].rainInterval;

    // --- examine each monthly set of UHs
    nMax = 0;
    for (m=0; m<12; m++)
    {
        // --- examine each UH in the set
        for (k=0; k<3; k++)
        {
            // --- compute number of time periods in UH base
            n = (UnitHyd[i].tBase[m][k] / gageInterval) + 1;

            // --- update number of time periods to be saved at gage
            nMax = MAX(n, nMax);
        }
    }
    return nMax;
}

//=============================================================================

void initGageData()
//
//  Input:   none
//  Output:  none
//  Purpose: initializes rain gage processing data.
//
{
    int i;                             // unit hyd. index
	int m;                             // month index
    int g;                             // rain gage index

    // --- for each unit hydrograph group
    for (i=0; i<Nobjects[UNITHYD]; i++)
    {
        // --- see if UH's rain gage not already initialized
        g = UnitHyd[i].rainGage;
        if ( g >= 0 && Gage[g].isUsed == FALSE )
        {
            // --- initialize the gage's rainfall time series & its state
            if ( Gage[g].tSeries >= 0 )
            {
                table_tseriesInit(&Tseries[Gage[g].tSeries]);
            }
            gage_initState(g);
            Gage[g].isUsed = TRUE;
        }

        // --- make the first recorded rainfall begin a new RDII event
        // --- (new RDII event occurs when dry period > base of longest UH)
        UHData[i].drySeconds =
            (UHData[i].maxPeriods * Gage[g].rainInterval) + 1;
        UHData[i].period = UHData[i].maxPeriods + 1;
        UHData[i].hasPastRain = FALSE;

        // --- initialize gage date to simulation start date
        UHData[i].gageDate = StartDateTime;
        UHData[i].area = 0.0;

        // --- assign initial abstraction used
        m = datetime_monthOfYear(StartDateTime) - 1;
        UHData[i].iaUsed = UnitHyd[i].ia[m][IA_INIT];
    }
}

//=============================================================================

void initUnitHydData()
//
//  Input:   none
//  Output:  none
//  Purpose: initializes unit hydrograph processing data.
//
{
    int i,                             // UH group index
        j,                             // node index
        n;                             // RDII node index

    // --- assume each UH group is not used
    for (i=0; i<Nobjects[UNITHYD]; i++) UHData[i].isUsed = FALSE;

    // --- look at each node with RDII inflow 
    for (n=0; n<NumRdiiNodes; n++)
    {
        // --- mark as used the UH group associated with the node
        j = RdiiNodeIndex[n];
        i = Node[j].rdiiInflow->unitHyd;
        UHData[i].isUsed = TRUE;

        // --- add node's sewer area to UH group's area
        UHData[i].lastDate = StartDateTime;
        UHData[i].area += Node[j].rdiiInflow->area;
    }
}

//=============================================================================

int openNewRdiiFile()
//
//  Input:   none
//  Output:  returns TRUE if successful, FALSE if not
//  Purpose: opens a new RDII interface file.
//
{
    int j;                             // node index

    // --- create a temporary file name if scratch file being used
    if ( Frdii.mode == SCRATCH_FILE ) getTmpName(Frdii.name);

    // --- open the RDII file as a formatted text file
    Frdii.file = fopen(Frdii.name, "wt");
    if ( Frdii.file == NULL )
    {
        return FALSE;
    }

    // --- initialize the contents of the file with header line,
    //     flow units, RDII time step (sec), number of RDII nodes,
    //     and name of each node 
    fprintf(Frdii.file, "SWMM5 Interface File");
    fprintf(Frdii.file, "\n%s", Title[0]);
    fprintf(Frdii.file, "\n%d - reporting time step in sec", RdiiStep);
    fprintf(Frdii.file, "\n1 - number of constituents as listed below:");
    fprintf(Frdii.file, "\nFLOW %s", FlowUnitWords[FlowUnits]);
    fprintf(Frdii.file, "\n%d - number of nodes as listed below:",
        NumRdiiNodes);
    for (j=0; j<Nobjects[NODE]; j++)
    {
        if ( Node[j].rdiiInflow )
        {
            fprintf(Frdii.file, "\n%s", Node[j].ID);
        }
    }

    // --- write column headings
    fprintf(Frdii.file,"\nNode             Year Mon Day Hr  Min Sec FLOW");
    return TRUE;
}

//=============================================================================

void getRainfall(DateTime currentDate)
//
//  Input:   currentDate = current calendar date/time
//  Output:  none
//  Purpose: determines rainfall at current RDII processing date.
//
//
{
    int      j;                        // UH group index
    int      g;                        // rain gage index
    int      i;                        // past rainfall index
    int      month;                    // month of current date
    int      gageInterval;             // gage recording interval (sec)
    double   rainDepth;                // rainfall depth (inches or mm)
    DateTime gageDate;                 // calendar date for rain gage
    
    // --- examine each UH group
    for (g = 0; g < Nobjects[GAGE]; g++) Gage[g].isCurrent = FALSE;            //(5.0.012 - RD)
    for (j = 0; j < Nobjects[UNITHYD]; j++)
    {
        // --- repeat until gage's date reaches or exceeds current date
        g = UnitHyd[j].rainGage;
        while ( UHData[j].gageDate < currentDate )
        {
            // --- get rainfall volume over gage's recording interval
            //     at gage'a current date (in original depth units)
            gageDate = UHData[j].gageDate;
            gageInterval = Gage[g].rainInterval;
            if (!Gage[g].isCurrent)                                            //(5.0.012 - RD)
            {                                                                  //(5.0.012 - RD)
                gage_setState(g, gageDate);                                    //(5.0.012 - RD)
                Gage[g].isCurrent = TRUE;                                      //(5.0.012 - RD)
            }                                                                  //(5.0.012 - RD)
            rainDepth = Gage[g].rainfall * (double)gageInterval / 3600.0;

            // --- adjust rainfall volume for any initial abstraction
            rainDepth = applyIA(j, gageDate, gageInterval, rainDepth);

            // --- if rainfall occurs
            if ( rainDepth > 0.0 )
            {
                // --- if previous dry period long enough then begin
                //     new RDII event with time period index set to 0
                //     (new RDII event occurs when dry period > base of longest UH)
                if ( UHData[j].drySeconds >= gageInterval * UHData[j].maxPeriods  )
                {
                    for (i=0; i<UHData[j].maxPeriods; i++)
                    {
                        UHData[j].pastRain[i] = 0.0;
                    }
                    UHData[j].period = 0;
                }
                UHData[j].drySeconds = 0;
                UHData[j].hasPastRain = TRUE;

                // --- update count of total rainfall volume (ft3)
                TotalRainVol += rainDepth / UCF(RAINDEPTH) * UHData[j].area;
            }

            // --- if no rainfall, update duration of dry period
            else
            {
                UHData[j].drySeconds += gageInterval;
                if ( UHData[j].drySeconds >= 
                    gageInterval * UHData[j].maxPeriods )
                {
                    UHData[j].hasPastRain = FALSE;
                }
                else UHData[j].hasPastRain = TRUE;

            }
        
            // --- add rainfall to list of past values,
            //     wrapping array index if necessary
            if ( UHData[j].period < UHData[j].maxPeriods )
            {
                i = UHData[j].period;
            }
            else i = 0;
            UHData[j].pastRain[i] = rainDepth;
            month = datetime_monthOfYear(currentDate) - 1;
            UHData[j].pastMonth[i] = (char)month;
            UHData[j].period = i + 1;

            // --- advance rain date by gage recording interval
            UHData[j].gageDate = datetime_addSeconds(gageDate, gageInterval);
        }
    }
}

//=============================================================================

double  applyIA(int j, DateTime aDate, double dt, double rainDepth)
//
//  Input:   j = UH group index
//           aDate = current date/time
//           dt = time interval (sec)
//           rainDepth = unadjusted rain depth (in or mm)
//  Output:  returns rainfall adjusted for initial abstraction (IA)
//  Purpose: adjusts rainfall for any initial abstraction and updates the 
//           amount of available initial abstraction actually used.
//
{
    int m;
    double ia, netRainDepth;

    // --- determine amount of unused IA
    m = datetime_monthOfYear(aDate) - 1;
    ia = UnitHyd[j].ia[m][IA_MAX] - UHData[j].iaUsed;
    ia = MAX(ia, 0.0);

    // --- case where there's some rainfall
    if ( rainDepth > 0.0 )
    {
        // --- reduce rain depth by unused IA
        netRainDepth = rainDepth - ia;
        netRainDepth = MAX(netRainDepth, 0.0);

        // --- update amount of IA used up
        ia = rainDepth - netRainDepth;
        UHData[j].iaUsed += ia;
    }

    // --- case where there's no rainfall
    else
    {
        // --- recover a portion of the IA already used
        UHData[j].iaUsed -= dt / 86400. * UnitHyd[j].ia[m][IA_REC];
        UHData[j].iaUsed = MAX(UHData[j].iaUsed, 0.0);
        netRainDepth = 0.0;
    }
    return netRainDepth;
}

//=============================================================================

void getUnitHydRdii(DateTime currentDate)
//
//  Input:   currentDate = current calendar date/time
//  Output:  none
//  Purpose: computes RDII generated by past rainfall for each UH group.
//
{
    int   j;                           // UH group index

    // --- examine each UH group
    for (j=0; j<Nobjects[UNITHYD]; j++)
    {
        // --- skip calculation if UH not used by any RDII node or if
        //     current date hasn't reached last date RDII was computed
        if ( !UHData[j].isUsed ) continue;
        if ( currentDate < UHData[j].lastDate ) continue;

        // --- update date RDII last computed
        UHData[j].lastDate = UHData[j].gageDate;

        // --- perform convolution only if some past rainfall exists
        if ( UHData[j].hasPastRain )
        {
            UHData[j].rdii = getUnitHydConvol(j);
        }
        else UHData[j].rdii = 0.0;
    }
}

//=============================================================================

double getUnitHydConvol(int j)
//
//  Input:   j = UH group index
//  Output:  returns a RDII flow value
//  Purpose: computes convolution of Unit Hydrographs with past rainfall.
//
{
    int    g;                          // rain gage index
    int    i;                          // previous rainfall period index
    int    m;                          // month of year index
    int    p;                          // UH time period index
    int    pMax;                       // max. number of periods
    int    k;                          // individual UH index
    int    gageInterval;               // rainfall recording interval (sec)
    double t;                          // UH time value (sec)
    double u;                          // UH ordinate
    double v;                          // rainfall volume
    double rdii;                       // RDII flow

    // --- initialize RDII, rain period index and UH period index
    rdii = 0.0;
    g = UnitHyd[j].rainGage;
    gageInterval = Gage[g].rainInterval;
    i = UHData[j].period - 1;
    if ( i < 0 ) i = UHData[j].maxPeriods - 1;
    pMax = UHData[j].maxPeriods;
    p = 1;

    // --- evaluate each time period of UH's
    while ( p < pMax )
    {
        // --- if rain period has rainfall
        v = UHData[j].pastRain[i];
        m = UHData[j].pastMonth[i];
        if ( v > 0.0 )
        {
            // --- find mid-point time of UH period in seconds
            t = ((double)(p) - 0.5) * (double)gageInterval;

            // --- convolute rain volume with UH ordinate for each UH
            for (k=0; k<3; k++)
            {
                u = getUnitHydOrd(j, m, k, t) * UnitHyd[j].r[m][k];
                rdii += u * v;
            }
        }

        // --- move to next UH period & previous rainfall period
        p = p + 1;
        i = i - 1;
        if ( i < 0 ) i = UHData[j].maxPeriods - 1;
    }
    return rdii;
}

//=============================================================================

double getUnitHydOrd(int h, int m, int k, double t)
//
//  Input:   h = index of UH group
//           m = month index
//           k = individual UH index
//           t = UH time (sec)
//  Output:  returns ordinate of a unit hydrograph
//  Purpose: gets ordinate of a particular unit hydrograph at specified time.
//
{
    double qPeak;                      // peak flow of unit hydrograph 
    double f;                          // fraction of time to/from peak on UH
    double t1;                         // time to peak on UH (sec)
    double t2;                         // time after peak on UH (sec)
    double tBase;                      // base time of UH (sec)

    // --- return 0 if past end of UH time base
    tBase = UnitHyd[h].tBase[m][k];
    if ( t >= tBase ) return 0.0;

    // --- compute peak value of UH in original rainfall units (in/hr or mm/hr)
    qPeak = 2. / tBase * 3600.0;
    
    // --- break UH base into times before & after peak flow
    t1 = UnitHyd[h].tPeak[m][k];
    t2 = tBase - t1;

    // --- find UH flow at time t
    if ( t <= t1 ) f = t / t1;
    else           f = 1.0 - (t - t1) / t2;
    return MAX(f, 0.0) * qPeak;                  
}

//=============================================================================

int getNodeRdii()
//
//  Input:   none
//  Output:  returns TRUE if any node has RDII inflow, FALSE if not
//  Purpose: computes current RDII inflow at each node.
//
{
    int   hasRdii = FALSE;             // true if any node has some RDII
    int   i;                           // UH group index
    int   j;                           // node index
    int   n;                           // number of nodes w/ RDII
    double rdii;                       // RDII flow (cfs)

    // --- examine each node w/ RDII data
    for (n = 0; n < NumRdiiNodes; n++)
    {
        // --- identify node's index in project's data base
        j = RdiiNodeIndex[n];
        
        // --- apply node's sewer area to UH RDII to get node RDII in CMS
        i = Node[j].rdiiInflow->unitHyd;
        rdii = UHData[i].rdii * Node[j].rdiiInflow->area / UCF(RAINFALL);
        if ( rdii < ZERO_RDII ) rdii = 0.0;
        else hasRdii = TRUE;

        // --- update total RDII volume
        RdiiNodeFlow[n] = rdii;
        if ( rdii > 0.0 )
        {
            TotalRdiiVol += rdii * (double)RdiiStep;
        }
    }
    return hasRdii;
}

//=============================================================================

void saveRdiiFlows(DateTime currentDate)
//
//  Input:   currentDate = current calendar date/time
//  Output:  none
//  Purpose: saves current set of RDII inflows in current flow units to file.
//
{
    int i, j, yr, mon, day, hr, min, sec;

    // --- write year, month, day, hour, minute of current date to string
    datetime_decodeDate(currentDate, &yr, &mon, &day);
    datetime_decodeTime(currentDate, &hr, &min, &sec);
    
    // --- write RDII inflow at each RDII node to file
    for (i = 0; i < NumRdiiNodes; i++)
    {
        j = RdiiNodeIndex[i];
        fprintf(Frdii.file,
            "\n%-16s  %04d %02d  %02d  %02d  %02d  %02d %-10f",
            Node[j].ID, yr, mon, day, hr, min, sec,
            RdiiNodeFlow[i]*Qcf[FlowUnits]);
    }
}

//=============================================================================

void  closeRdiiProcessor()
//
//  Input:   none
//  Output:  none
//  Purpose: closes RDII processing system.
//
{
    // --- write rainfall & RDII totals to report file
    if ( !ErrorCode )
    {
        report_writeRdiiStats(TotalRainVol, TotalRdiiVol);
    }

    // --- free allocated memory and close RDII file
    freeRdiiMemory();
    if ( Frdii.file ) fclose(Frdii.file);
}

//=============================================================================

void freeRdiiMemory()
//
//  Input:   none
//  Output:  none
//  Purpose: frees memory used for RDII processing.
//
{
    int i;
    if ( UHData )
    {
        for (i = 0; i < Nobjects[UNITHYD]; i++)
        {
            FREE(UHData[i].pastRain);
            FREE(UHData[i].pastMonth);
        }
        FREE(UHData);
    }
    FREE(RdiiNodeIndex);
    FREE(RdiiNodeFlow);
}
