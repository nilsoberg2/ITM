//-----------------------------------------------------------------------------
//   error.h
//
//   Project: EPA SWMM5
//   Version: 5.0
//   Date:    6/19/07   (Build 5.0.010)
//            2/4/08    (Build 5.0.012)
//   Author:  L. Rossman
//
//   Error codes
//-----------------------------------------------------------------------------

enum  ErrorType {
      ERR_NONE,                 //     0
      ERR_MEMORY,               //101  1
      ERR_KINWAVE,              //103  2
      ERR_ODE_SOLVER,           //105  3
      ERR_TIMESTEP,             //107  4
      ERR_SUBCATCH_OUTLET,      //108  5
      ERR_AQUIFER_PARAMS,       //109  6
      ERR_LENGTH,               //111  7
      ERR_ROUGHNESS,            //113  8
      ERR_BARRELS,              //114  9
      ERR_SLOPE,                //115  10
      ERR_NO_XSECT,             //117  11
      ERR_XSECT,                //119  12
      ERR_NO_CURVE,             //121  13
      ERR_LOOP,                 //131  14
      ERR_MULTI_OUTLET,         //133  15
      ERR_MULTI_DUMMY_OUTLET,   //134  16
      ERR_DIVIDER,              //135  17
      ERR_DIVIDER_LINK,         //136  18
      ERR_WEIR_DIVIDER,         //137  19
      ERR_NODE_DEPTH,           //138  20
      ERR_REGULATOR,            //139  21
      ERR_OUTFALL,              //141  22
      ERR_REGULATOR_SHAPE,      //143  23
      ERR_NO_OUTLETS,           //145  24
      ERR_UNITHYD_TIMES,        //151  25
      ERR_UNITHYD_RATIOS,       //153  26
      ERR_RDII_AREA,            //155  27
      ERR_CYCLIC_TREATMENT,     //161  28
      ERR_CURVE_SEQUENCE,       //171  29
      ERR_TIMESERIES_SEQUENCE,  //173  30
      ERR_START_DATE,           //191  31
      ERR_REPORT_DATE,          //193  32
      ERR_REPORT_STEP,          //195  33

      ERR_INPUT,                //200  34
      ERR_LINE_LENGTH,          //201  35
      ERR_ITEMS,                //203  36
      ERR_KEYWORD,              //205  37
      ERR_DUP_NAME,             //207  38
      ERR_NAME,                 //209  39
      ERR_NUMBER,               //211  40
      ERR_DATETIME,             //213  41
      ERR_RULE,                 //217  42
      ERR_TRANSECT_UNKNOWN,     //219  43
      ERR_TRANSECT_SEQUENCE,    //221  44
      ERR_TRANSECT_TOO_FEW,     //223  45
      ERR_TRANSECT_TOO_MANY,    //225  46
      ERR_TRANSECT_MANNING,     //227  47
      ERR_TRANSECT_OVERBANK,    //229  48
      ERR_TRANSECT_NO_DEPTH,    //231  49
      ERR_TREATMENT_EXPR,       //233  50

      ERR_FILE_NAME,            //301  51
      ERR_INP_FILE,             //303  52
      ERR_RPT_FILE,             //305  53
      ERR_OUT_FILE,             //307  54
      ERR_OUT_WRITE,            //309  55
      ERR_OUT_READ,             //311  56

      ERR_RAIN_FILE_SCRATCH,    //313  57
      ERR_RAIN_FILE_OPEN,       //315  58
      ERR_RAIN_FILE_DATA,       //317  59
      ERR_RAIN_FILE_FORMAT,     //319  60
      ERR_RAIN_FILE_GAGE,       //321  61

      ERR_RUNOFF_FILE_OPEN ,    //323  62
      ERR_RUNOFF_FILE_FORMAT,   //325  63
      ERR_RUNOFF_FILE_END,      //327  64
      ERR_RUNOFF_FILE_READ,     //329  65

      ERR_HOTSTART_FILE_NAMES,  //330  66
      ERR_HOTSTART_FILE_OPEN,   //331  67
      ERR_HOTSTART_FILE_FORMAT, //333  68
      ERR_HOTSTART_FILE_READ,   //335  69

      ERR_NO_CLIMATE_FILE,      //336  70
      ERR_CLIMATE_FILE_OPEN,    //337  71
      ERR_CLIMATE_FILE_READ,    //338  72
      ERR_CLIMATE_END_OF_FILE,  //339  73

      ERR_RDII_FILE_SCRATCH,    //341  74
      ERR_RDII_FILE_OPEN,       //343  75
      ERR_RDII_FILE_FORMAT,     //345  76
      
      ERR_ROUTING_FILE_OPEN,    //351  77
      ERR_ROUTING_FILE_FORMAT,  //353  78
      ERR_ROUTING_FILE_NOMATCH, //355  79
      ERR_ROUTING_FILE_NAMES,   //357  80

      ERR_SYSTEM,               //401  81
      ERR_NOT_CLOSED,           //402  82
      ERR_NOT_OPEN,             //403  83
      ERR_FILE_SIZE,            //405  84

      ERR_SNOWMELT_PARAMS,      //181  85                                      //(5.0.012 - LR)
      ERR_SNOWPACK_PARAMS,      //182  86                                      //(5.0.012 - LR)

	  ERR_ITM_ENGINE};			//901  87

char* error_getMsg(int i);
int   error_getCode(int i);
int   error_setInpError(int errcode, char* s);
