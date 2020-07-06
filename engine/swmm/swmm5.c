//-----------------------------------------------------------------------------
//   swmm5.c
//
//   Project:  EPA SWMM5
//   Version:  5.0
//   Date:     6/19/07   (Build 5.0.010)
//             7/16/07   (Build 5.0.011)
//             2/4/08    (Build 5.0.012)
//   Author:   L. Rossman
//
//   This is the main module of the computational engine for Version 5 of
//   the U.S. Environmental Protection Agency's Storm Water Management Model
//   (SWMM). It contains functions that control the flow of computations.
//
//   Depending on how it is compiled, this engine can be executed either as
//   a command line executable or through a series of calls made to functions
//   in a dynamic link library.
//
//-----------------------------------------------------------------------------
#define _CRT_SECURE_NO_DEPRECATE

// Un-comment this line to compile as a DLL
#define DLL

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <float.h>

// --- define WINDOWS
#undef WINDOWS
#ifdef _WIN32
  #define WINDOWS
#endif
#ifdef __WIN32__
  #define WINDOWS
#endif

// --- headers for exception handling
#ifdef WINDOWS
#include <windows.h>
#include <excpt.h>
#endif

//-----------------------------------------------------------------------------
//  SWMM's header files
//
//  Note: the directives listed below are also contained in headers.h which
//        is included at the start of most of SWMM's other code modules.
//-----------------------------------------------------------------------------
#include "consts.h"                    // defined constants
#include "macros.h"                    // macros used throughout SWMM
#include "enums.h"                     // enumerated variables
#include "error.h"                     // error message codes
#include "datetime.h"                  // date/time functions
#include "objects.h"                   // definitions of SWMM's data objects
#include "funcs.h"                     // declaration of all global functions
#include "text.h"                      // listing of all text strings 
#define  EXTERN                        // defined as 'extern' in headers.h
#include "globals.h"                   // declaration of all global variables
#include "debug.h"

#include "swmm5.h"                     // declaration of exportable functions
                                       //   callable from other programs
#define  MAX_EXCEPTIONS 100            // max. number of exceptions handled


#ifdef DEBUGFILE
FILE* DEBUGFH;
#endif

//-----------------------------------------------------------------------------
//  Unit conversion factors
//-----------------------------------------------------------------------------
const double Ucf[10][2] =                                                      //(5.0.010 - LR)
      {//  US      SI
      {1097280.0 ,   43200.0},         // RAINFALL (in/hr, mm/hr --> ft/sec)
      {304.8     ,      12.0},         // RAINDEPTH (in, mm --> ft)
      {26334720.0, 1036800.0},         // EVAPRATE (in/day, mm/day --> ft/sec)
      {0.3048    ,       1.0},         // LENGTH (ft, m --> ft)
      {0.92903e-5, 2.2956e-5},         // LANDAREA (ac, ha --> ft2)
      {0.02832   ,       1.0},         // VOLUME (ft3, m3 --> ft3)
      {1.608     ,       1.0},         // WINDSPEED (mph, km/hr --> mph)
      {1.8       ,       1.0},         // TEMPERATURE (deg F, deg C --> deg F)
      {1.0e-6    ,  2.203e-6},         // MASS (lb, kg --> mg)
      {3048.0    ,   43560.0}          // GWFLOW (cfs/ac, cms/ha --> ft/sec)   //(5.0.010 - LR)
      };
const double Qcf[6] =                  // Flow Conversion Factors:
      { 0.02832, 28.317,  2.4466,      // cms, lps, mld --> cfs
      1.0,     448.831, 0.64632};      // cfs, gpm, mgd --> cfs

//-----------------------------------------------------------------------------
//  Shared variables
//-----------------------------------------------------------------------------
static int  IsOpenFlag;           // TRUE if a project has been opened
static int  IsStartedFlag;        // TRUE if a simulation has been started
static int  SaveResultsFlag;      // TRUE if output to be saved to binary file
static int  ExceptionCount;       // number of exceptions handled
static int  DoRunoff;             // TRUE if project has subcatchments
static int  DoRouting;            // TRUE if project has drainage links

//-----------------------------------------------------------------------------
//  External functions (prototyped in swmm5.h)
//-----------------------------------------------------------------------------
//  swmm_run
//  swmm_open
//  swmm_start
//  swmm_step
//  swmm_end
//  swmm_report
//  swmm_close
//  swmm_getMassBalErr
//  swmm_getVersion

//-----------------------------------------------------------------------------
//  Local functions
//-----------------------------------------------------------------------------
static void execRouting(DateTime elapsedTime);

// Exception filtering function
#ifdef WINDOWS
static int  xfilter(int xc, DateTime elapsedTime, long step);
#endif

//-----------------------------------------------------------------------------
//  Entry point used to compile a stand-alone executable.
//-----------------------------------------------------------------------------
#ifndef DLL
int  main(int argc, char *argv[])
//
//  Input:   argc = number of command line arguments
//           argv = array of command line arguments
//  Output:  returns error status
//  Purpose: processes command line arguments.
//
//  Command line for stand-alone operation is: swmm5 f1  f2  f3
//  where f1 = name of input file, f2 = name of report file, and
//  f3 = name of binary output file if saved (or blank if not saved).
//
{
    char *inputFile;
    char *reportFile;
    char *binaryFile;
    char blank[] = "";
    time_t start;
    double runTime;

    // --- initialize flags
    IsOpenFlag = FALSE;
    IsStartedFlag = FALSE;
    SaveResultsFlag = TRUE;

    // --- check for proper number of command line arguments
    start = time(0);
    if (argc < 3) writecon(FMT01);
    else
    {
        // --- extract file names from command line arguments
        inputFile = argv[1];
        reportFile = argv[2];
        if (argc > 3) binaryFile = argv[3];
        else          binaryFile = blank;
        writecon(FMT02);

        // --- run SWMM
        swmm_run(inputFile, reportFile, binaryFile);

        // Display closing status on console
        runTime = difftime(time(0), start);
        sprintf(Msg, "\n\n... EPA-SWMM completed in %.2f seconds.", runTime);
        writecon(Msg);
        if      ( ErrorCode   ) writecon(FMT03);
        else if ( WarningCode ) writecon(FMT04);
        else                    writecon(FMT05);
    }

// --- Use the code below if you need to keep the console window visible
/* 
    writecon("    Press Enter to continue...");
    getchar();
*/

    return 0;
}                                      /* End of main */
#endif

//=============================================================================

int DLLEXPORT  swmm_run(char* f1, char* f2, char* f3)
//
//  Input:   f1 = name of input file
//           f2 = name of report file
//           f3 = name of binary output file
//  Output:  returns error code
//  Purpose: runs a SWMM simulation.
//
{
    long newHour, oldHour = 0;
    long theDay, theHour;
    DateTime elapsedTime = 0.0;

    // --- open the files & read input data
    ErrorCode = 0;
    swmm_open(f1, f2, f3);

    // --- run the simulation if input data OK
    if ( !ErrorCode )
    {
        // --- initialize values
        swmm_start(TRUE);

        // --- execute each time step until elapsed time is re-set to 0
        if ( !ErrorCode )
        {
            writecon("\n o  Simulating day: 0     hour:  0");
            do
            {
                swmm_step(&elapsedTime);
                newHour = (long)(elapsedTime * 24.0);
                if ( newHour > oldHour )
                {
                    theDay = (long)elapsedTime;
                    theHour = (long)((elapsedTime - floor(elapsedTime)) * 24.0);
                    writecon("\b\b\b\b\b\b\b\b\b\b\b\b\b\b");
                    sprintf(Msg, "%-5d hour: %-2d", theDay, theHour);
                    writecon(Msg);
                    oldHour = newHour;
                }
            } while ( elapsedTime > 0.0 && !ErrorCode );
            writecon("\b\b\b\b\b\b\b\b\b\b\b\b\b\b"
                     "\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b");
            writecon("Simulation complete           ");
        }

        // --- clean up
        swmm_end();
    }

    // --- report results
    swmm_report();

    // --- close the system
    swmm_close();
    return ErrorCode;
}

//=============================================================================

int DLLEXPORT swmm_open(char* f1, char* f2, char* f3)
//
//  Input:   f1 = name of input file
//           f2 = name of report file
//           f3 = name of binary output file
//  Output:  returns error code
//  Purpose: opens a SWMM project.
//
{
#ifdef DLL
   _fpreset();              
#endif

#ifdef WINDOWS
    // --- begin exception handling here
    __try
#endif
    {
#ifdef DEBUGFILE
        DEBUGFH = fopen(DEBUGFILE_PATH, "w");
#endif
        DEBUGWRITE("Start\n");

        // --- initialize error & warning codes
        datetime_setDateFormat(M_D_Y);
        ErrorCode = 0;
        WarningCode = 0;
        IsOpenFlag = FALSE;
        IsStartedFlag = FALSE;
        ExceptionCount = 0;

        DEBUGWRITE("project_open\n");
        // --- open a SWMM project
        project_open(f1, f2, f3);      
        DEBUGWRITE("project_open\n");

        if ( ErrorCode ) return ErrorCode;
        IsOpenFlag = TRUE;
        report_writeLogo();
        writecon(FMT06);

        // --- retrieve project data from input file
        DEBUGWRITE("project_readInput\n");
        project_readInput();
        DEBUGWRITE("project_readInput\n");
        if ( ErrorCode ) return ErrorCode;

/*** This is already done in project_readInput() ***/                          //(5.0.012 - LR)
        // --- assign date/time values if not supplied by user
        //if ( EndDate == NO_DATE ) EndDate = StartDate;                       //(5.0.012 - LR)
        //if ( ReportStartDate == NO_DATE ) ReportStartDate = StartDate;       //(5.0.012 - LR)
        //if ( ReportStartTime == NO_DATE ) ReportStartTime = StartTime;       //(5.0.012 - LR)

        // --- write project title to report file & validate data
        report_writeTitle();
        project_validate();

        // --- write input summary to report file if requested
        if ( RptFlags.input ) inputrpt_writeInput();                           //(5.0.012 - LR)
    }

#ifdef WINDOWS
    // --- end of try loop; handle exception here
    __except(xfilter(GetExceptionCode(), 0.0, 0))
    {
        ErrorCode = ERR_SYSTEM;
    }
#endif
    return ErrorCode;
}

//=============================================================================

int DLLEXPORT swmm_start(int saveResults)
//
//  Input:   saveResults = TRUE if simulation results saved to binary file 
//  Output:  returns an error code
//  Purpose: starts a SWMM simulation.
//
{
    // --- check that a project is open & no run started
    if ( ErrorCode ) return ErrorCode;
    if ( !IsOpenFlag || IsStartedFlag )
    {
        report_writeErrorMsg(ERR_NOT_OPEN, "");
        return ErrorCode;
    }
    ExceptionCount = 0;

#ifdef WINDOWS
    // --- begin exception handling loop here
    __try
#endif
    {
        // --- initialize runoff, routing & reporting time (in milliseconds)
        NewRunoffTime = 0.0;
        NewRoutingTime = 0.0;
        ReportTime =   (double)(ReportStep);
        StepCount = 0;
        IsStartedFlag = TRUE;

        // --- initialize global continuity errors
        RunoffError = 0.0;
        GwaterError = 0.0;
        FlowError = 0.0;
        QualError = 0.0;

        // --- open rainfall processor (creates/opens a rainfall
        //     interface file and generates any RDII flows)

        if ( !IgnoreRainfall ) rain_open();

        if ( ErrorCode ) return ErrorCode;

        // --- initialize state of each major system component
        DEBUGWRITE("project_init\n");
        project_init();
        DEBUGWRITE("project_init\n");

        // --- open all computing systems (order is important!)
        DEBUGWRITE("output_open\n");
        output_open();
        DEBUGWRITE("output_open\n");
        runoff_open();
        if ( RouteModel != NO_ROUTING ) routing_open(RouteModel);              //(5.0.010 - LR)
        massbal_open();
        stats_open();

        // --- see if runoff & routing needs to be computed
        if ( Nobjects[SUBCATCH] > 0 ) DoRunoff = TRUE;
        else DoRunoff = FALSE;
        if ( Nobjects[NODE] > 0 && RouteModel != NO_ROUTING )                  //(5.0.011 - LR)
            DoRouting = TRUE;
        else DoRouting = FALSE;

        // --- write Control Actions heading to report file
        if ( RptFlags.controls ) report_writeControlActionsHeading();

		DEBUGWRITE("end swmm_start\n");
    }

#ifdef WINDOWS
    // --- end of try loop; handle exception here
    __except(xfilter(GetExceptionCode(), 0.0, 0))
    {
        ErrorCode = ERR_SYSTEM;
    }
#endif

    // --- save saveResults flag to global variable
    SaveResultsFlag = saveResults;    
    return ErrorCode;
}

//=============================================================================

int DLLEXPORT swmm_step(DateTime* elapsedTime)
//
//  Input:   elapsedTime = current elapsed time in decimal days
//  Output:  updated value of elapsedTime,
//           returns error code
//  Purpose: advances the simulation by one routing time step.
//
{
    // --- check that simulation can proceed
    if ( ErrorCode ) return ErrorCode;
    if ( !IsOpenFlag || !IsStartedFlag  )
    {
        report_writeErrorMsg(ERR_NOT_OPEN, "");
        return ErrorCode;
    }

#ifdef WINDOWS
    // --- begin exception handling loop here
    __try
#endif
    {
		DEBUGWRITE("swmm_step\n");
        // --- if routing time has not exceeded total duration                 //(5.0.010 - LR)
        if ( NewRoutingTime <= TotalDuration )                                 //(5.0.010 - LR)
        {
            // --- route flow & WQ through drainage system                     //(5.0.010 - LR)
            //     (runoff will be calculated as needed)                       //(5.0.010 - LR)
            DEBUGWRITE("execRouting\n");
            execRouting(*elapsedTime);                                         //(5.0.010 - LR)
            DEBUGWRITE("execRouting\n");

            // --- save results at next reporting time
            if ( NewRoutingTime >= ReportTime )                                //(5.0.010 - LR)
            {
                if ( SaveResultsFlag ) output_saveResults(ReportTime);
                ReportTime = ReportTime + (double)(ReportStep);
            }

            // --- update elapsed time (days)
            *elapsedTime = NewRoutingTime / MSECperDAY;
        }

        // --- otherwise end the simulation
        else *elapsedTime = 0.0;
    }

#ifdef WINDOWS
    // --- end of try loop; handle exception here
    __except(xfilter(GetExceptionCode(), *elapsedTime, StepCount))
    {
        ErrorCode = ERR_SYSTEM;
    }
#endif
    return ErrorCode;
}

//=============================================================================

void execRouting(DateTime elapsedTime)
//
//  Input:   none
//  Output:  none
//  Purpose: routes flow & WQ through drainage system over a single time step.
//
{
    double   nextRoutingTime;          // updated elapsed routing time (msec)
    double   routingStep;              // routing time step (sec)

#ifdef WINDOWS
    // --- begin exception handling loop here
    __try
#endif
    {
        // --- determine when next routing time occurs
        StepCount++;
        if ( !DoRouting ) routingStep = WetStep;
        else routingStep = routing_getRoutingStep(RouteModel, RouteStep);
        if ( routingStep <= 0.0 )
        {
            ErrorCode = ERR_TIMESTEP;
            return;
        }
        nextRoutingTime = NewRoutingTime + 1000.0 * routingStep;

        // --- compute runoff until next routing time reached or exceeded
        while ( NewRunoffTime < nextRoutingTime )
        {
            runoff_execute();
            if ( ErrorCode ) return;
        }
  
        // --- route flows through drainage system over current time step
        if ( DoRouting ) routing_execute(RouteModel, routingStep);             //(5.0.010 - LR)
        else NewRoutingTime = nextRoutingTime;                                 //(5.0.010 - LR)
    }

#ifdef WINDOWS
    // --- end of try loop; handle exception here
    __except(xfilter(GetExceptionCode(), elapsedTime, StepCount))
    {
        ErrorCode = ERR_SYSTEM;
        return;
    }
#endif
}

//=============================================================================

int DLLEXPORT swmm_end(void)
//
//  Input:   none
//  Output:  none
//  Purpose: ends a SWMM simulation.
//
{
    // --- check that project opened and run started
    if ( !IsOpenFlag )
    {
        report_writeErrorMsg(ERR_NOT_OPEN, "");
        return ErrorCode;
    }

    if ( IsStartedFlag )
    {
        // --- write ending records to binary output file
        if ( Fout.file ) output_end();

        // --- report mass balance results and system statistics
        if ( !ErrorCode )
        {
            //massbal_report();
            stats_report();
        }

        // --- close all computing systems
        stats_close();
        massbal_close();

        if ( !IgnoreRainfall ) rain_close();

        runoff_close();
        if ( RouteModel != NO_ROUTING ) routing_close(RouteModel);
        IsStartedFlag = FALSE;
    }
    return ErrorCode;
}

//=============================================================================

int DLLEXPORT swmm_report()
//
//  Input:   none
//  Output:  returns an error code
//  Purpose: writes simulation results to report file.
//
{
    if ( ErrorCode ) report_writeErrorCode();
    else
    {
        writecon(FMT07);
        report_writeReport();
    }
    return ErrorCode;
}

//=============================================================================

int DLLEXPORT swmm_close()
//
//  Input:   none
//  Output:  returns an error code
//  Purpose: closes a SWMM project.
//
{
	DEBUGWRITE("SWMM_CLOSE");

    if ( Fout.file ) output_close();
    if ( IsOpenFlag ) project_close();
    report_writeSysTime();
    if ( Finp.file != NULL ) fclose(Finp.file);
    if ( Frpt.file != NULL ) fclose(Frpt.file);
    if ( Fout.file != NULL )
    {
        fclose(Fout.file);
        if ( Fout.mode == SCRATCH_FILE ) remove(Fout.name);
    }
    IsOpenFlag = FALSE;
    IsStartedFlag = FALSE;
    return 0;
}

//=============================================================================

int  DLLEXPORT swmm_getMassBalErr(float* runoffErr, float* flowErr,
                                  float* qualErr)
//
//  Input:   none
//  Output:  runoffErr = runoff mass balance error (percent)
//           flowErr   = flow routing mass balance error (percent)
//           qualErr   = quality routing mass balance error (percent)
//           returns an error code
//  Purpose: reports a simulation's mass balance errors.
//
{
    *runoffErr = 0.0;
    *flowErr   = 0.0;
    *qualErr   = 0.0;

    if ( IsOpenFlag && !IsStartedFlag)
    {
        *runoffErr = (float)RunoffError;
        *flowErr   = (float)FlowError;
        *qualErr   = (float)QualError;
    }
    return 0;
}

//=============================================================================

int  DLLEXPORT swmm_getVersion(void)
//
//  Input:   none
//  Output:  returns SWMM engine version number
//  Purpose: retrieves version number of current SWMM engine which
//           uses a format of xyzzz where x = major version number,
//           y = minor version number, and zzz = build number.
//
{
	return VERSION;
}


//=============================================================================
//   General purpose functions
//=============================================================================

double UCF(int u)
//
//  Input:   u = integer code of quantity being converetd
//  Output:  returns a units conversion factor
//  Purpose: computes a conversion factor from SWMM's internal
//           units to user's units
//
{
    if ( u < FLOW ) return Ucf[u][UnitSystem];
    else if (FlowUnits == OutputFlowUnits) return 1.0;
    else return Qcf[FlowUnits];
}

//=============================================================================

char* sstrncpy(char *dest, const char *src, size_t maxlen)
//
//  Input:   dest = string to be copied to
//           src = string to be copied from
//           maxlen = number of characters to copy
//  Output:  returns a pointer to dest
//  Purpose: safe version of standard strncpy function
//
{
     strncpy(dest, src, maxlen);
     dest[maxlen] = '\0';
     return dest;
}

//=============================================================================

int  strcomp(char *s1, char *s2)
//
//  Input:   s1 = a character string
//           s2 = a character string
//  Output:  returns 1 if s1 is same as s2, 0 otherwise
//  Purpose: does a case insensitive comparison of two strings.
//
{
   int i;
   for (i=0; UCHAR(s1[i]) == UCHAR(s2[i]); i++)
     if (!s1[i+1] && !s2[i+1]) return(1);
   return(0);
}

//=============================================================================

char* getTmpName(char* fname)
//
//  Input:   fname = file name string
//  Output:  returns pointer to file name
//  Purpose: creates a temporary file name with path prepended to it.
//
{
    char name[MAXFNAME+1];
    int  n;

    // --- for Windows systems:
    #ifdef WINDOWS
      // --- use system function tmpnam() to create a temporary file name
      tmpnam(name);

      // --- if user supplied the name of a temporary directory,
      //     then make it be the prefix of the full file name
      n = (int)strlen(TmpDir);
      if ( n > 0 )
      {
          strcpy(fname, TmpDir);
          if ( fname[n-1] != '\\' ) strcat(fname, "\\");
      }

      // --- otherwise, use the relative path notation as the file name
      //     prefix so that the file will be placed in the current directory
      else
      {
          strcpy(fname, ".\\");
      }

      // --- now add the prefix to the file name
      strcat(fname, name);

    // --- for non-Windows systems:
    #else
      // --- use system function mkstemp() to create a temporary file name
      strcpy(fname, "swmmXXXXXX");
      mkstemp(fname);
    #endif
    return fname;
}

//=============================================================================

void getElapsedTime(DateTime aDate, int* days, int* hrs, int* mins)
//
//  Input:   aDate = simulation calendar date + time
//  Output:  days, hrs, mins = elapsed days, hours & minutes for aDate
//  Purpose: finds elapsed simulation time for a given calendar date
//
{
    DateTime x;
    int secs;
    x = aDate - StartDateTime;
    if ( x <= 0.0 )
    {
        *days = 0;
        *hrs  = 0;
        *mins = 0;
    }
    else
    {
        *days = (int)x;
        datetime_decodeTime(x, hrs, mins, &secs);
    }
}

//=============================================================================

DateTime getDateTime(double elapsedMsec)
//
//  Input:   elapsedMsec = elapsed milliseconds
//  Output:  returns date/time value
//  Purpose: finds calendar date/time value for elapsed milliseconds of
//           simulation time.
//
{
    return datetime_addSeconds(StartDateTime, elapsedMsec/1000.0);
}

//=============================================================================

void  writecon(char *s)
//
//  Input:   s = a character string
//  Output:  none
//  Purpose: writes string of characters to the console.
//
{
#ifndef DLL
   fprintf(stdout,s);
   fflush(stdout);
#endif
}

//=============================================================================

#ifdef WINDOWS
int xfilter(int xc, DateTime elapsedTime, long step)
//
//  Input:   xc          = exception code
//           elapsedTime = simulation time when exception occurred (days)
//           step        = step count at time when exception occurred
//  Output:  returns an exception handling code
//  Purpose: exception filtering routine for operating system exceptions
//           under Windows.
//
{
    int  rc;                           // result code
    long hour;                         // current hour of simulation
    char msg[40];                      // exception type text
    char xmsg[120];                    // error message text
    switch (xc)
    {
    case EXCEPTION_ACCESS_VIOLATION:
        sprintf(msg, "\n  Access violation ");
        rc = EXCEPTION_EXECUTE_HANDLER;
        break;
    case EXCEPTION_FLT_DENORMAL_OPERAND:
        sprintf(msg, "\n  Illegal floating point operand ");
        rc = EXCEPTION_CONTINUE_EXECUTION;
        break;
    case EXCEPTION_FLT_DIVIDE_BY_ZERO:
        sprintf(msg, "\n  Floating point divide by zero ");
        rc = EXCEPTION_CONTINUE_EXECUTION;
        break;
    case EXCEPTION_FLT_INVALID_OPERATION:
        sprintf(msg, "\n  Illegal floating point operation ");
        rc = EXCEPTION_CONTINUE_EXECUTION;
        break;
    case EXCEPTION_FLT_OVERFLOW:
        sprintf(msg, "\n  Floating point overflow ");
        rc = EXCEPTION_CONTINUE_EXECUTION;
        break;
    case EXCEPTION_FLT_STACK_CHECK:
        sprintf(msg, "\n  Floating point stack violation ");
        rc = EXCEPTION_EXECUTE_HANDLER;
        break;
    case EXCEPTION_FLT_UNDERFLOW:
        sprintf(msg, "\n  Floating point underflow ");
        rc = EXCEPTION_CONTINUE_EXECUTION;
        break;
    case EXCEPTION_INT_DIVIDE_BY_ZERO:
        sprintf(msg, "\n  Integer divide by zero ");
        rc = EXCEPTION_CONTINUE_EXECUTION;
        break;
    case EXCEPTION_INT_OVERFLOW:
        sprintf(msg, "\n  Integer overflow ");
        rc = EXCEPTION_CONTINUE_EXECUTION;
        break;
    default:
        sprintf(msg, "\n  Exception %d", xc);
        rc = EXCEPTION_EXECUTE_HANDLER;
    }
    hour = (long)(elapsedTime / 1000.0 / 3600.0);
    sprintf(xmsg, "%s at step %d, hour %d", msg, step, hour);
    if ( rc == EXCEPTION_EXECUTE_HANDLER ||
         ++ExceptionCount >= MAX_EXCEPTIONS )
    {
        strcat(xmsg, " --- execution halted.");
        rc = EXCEPTION_EXECUTE_HANDLER;
    }
    report_writeLine(xmsg);
    return rc;
}
#endif

//=============================================================================
    