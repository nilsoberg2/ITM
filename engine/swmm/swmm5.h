//-----------------------------------------------------------------------------
//   swmm5.h
//
//   Project: EPA SWMM5
//   Version: 5.0
//   Date:    6/19/07   (Build 5.0.010)
//   Author:  L. Rossman
//
//   Prototypes for SWMM5 functions exported to swmm5.dll.
//-----------------------------------------------------------------------------

#ifdef DLL
  #ifdef __cplusplus
  #define DLLEXPORT extern "C" __declspec(dllexport)
  #else
  #define DLLEXPORT __declspec(dllexport)
  #endif
#else
#define DLLEXPORT
#endif

DLLEXPORT   int  swmm_run(char* f1, char* f2, char* f3);
DLLEXPORT   int  swmm_open(char* f1, char* f2, char* f3);
DLLEXPORT   int  swmm_start(int saveFlag);
DLLEXPORT   int  swmm_step(double* elapsedTime);
DLLEXPORT   int  swmm_end(void);
DLLEXPORT   int  swmm_report(void);
DLLEXPORT   int  swmm_getMassBalErr(float* runoffErr, float* flowErr,
                 float* qualErr);
DLLEXPORT   int  swmm_close(void);
DLLEXPORT   int  swmm_getVersion(void);
