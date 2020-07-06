#ifndef DEBUG_H__
#define DEBUG_H__

#include <stdio.h>


#define DEBUGFILE

#ifdef DEBUGFILE
#define DEBUGFILE_PATH "c:\\temp\\itm.log"
extern FILE* DEBUGFH;
#define DEBUGWRITE(X) {fprintf(DEBUGFH, X);fflush(DEBUGFH);}
#define DEBUGWRITE2(X, Y) {fprintf(DEBUGFH, X, Y);fflush(DEBUGFH);}
#else
#define DEBUGWRITE(X)
#define DEBUGWRITE2(X, Y)
#endif



#endif//DEBUG_H__
