#ifndef __ITM_H__
#define __ITM_H__


double itm_get_inflow(int id, double currentStep);
int itm_has_inflow_swmm(int id);
double itm_node_get_volume(int j, double d);
int itm_get_step_count_swmm();
void itm_get_error_msg_swmm(int errorCode, char* errorStr, int errorStrMaxLen);
void itm_report_error_swmm(char* errorStr, int errorStrLen);
void itm_get_swmm_id_swmm(int link, int idx, char* swmm_id, int swmm_id_len);
int itm_get_num_links_swmm();
int itm_get_num_nodes_swmm();
int itm_get_starting_nodes_swmm(int* start, int start_count);
int itm_get_min_num_grids();
int itm_get_max_num_iterations();
int itm_get_max_num_cells();
int itm_get_max_num_plot_cells();
double itm_get_pressurized_wave_celerity();
double itm_get_mixed_flow_wave_celerity();
double itm_get_reference_depth_fraction();
double itm_get_max_simulation_time();
double itm_get_max_timestep();
double itm_get_report_interval();
double itm_get_report_start_time();
double itm_get_tol_normal();
double itm_get_tol_low();
double itm_get_tol_very_low();
double itm_get_tol_high();
double itm_get_tol_transition();
double itm_get_water_init_elevation();
int itm_get_ini_cond();
int itm_get_flow_type();
double itm_get_gate_opening_swmm(int id, double currentStep);
double itm_get_gate_coeff_swmm(int id, double opening);
double itm_get_max_curve_val_swmm(int xory, int nodeID);
double itm_get_q_from_rat_curve(int id, double yb);

int itm_run_swmm(char* f1, char* f2, char* f3);
int itm_open_start_swmm(char* f1, char* f2, char* f3);
int itm_close_swmm();
int itm_start_swmm();
int itm_end_swmm();
int itm_report_swmm();
int itm_step_swmm();
//int itm_report_step_swmm(double currentStep, int numRows, double* linkDepths, double* linkFlows, double* nodeDepths);
int itm_report_step_swmm(double currentStep, int numRows, double* linkDepths, double* linkFlows, double* linkVelocities, double* linkFroudes, double* nodeDepths, double sysVolErrPct, double sysVolErrDiff);

int itm_copy(int maxNumPipes, int* node1, int* node2, double* length, double* d, double* nm, double* zb,
             double* init_flow, int* init_depth_type, double* init_depth,
             double* entrance_loss, double* exit_loss,
             int maxNumNodes, double* const_depth_flow,
             int drop_count, double* Adrop, double* hdrops_overf, int res_count,
             double* flowdepth_res, double* res_outflow, double* res_maxdepth, int bc_count, double* junc_elev,
             int* node_bc, int* open_bc);
int determineBoundaryCondition(int idx);

int itm_open_hotstart2_swmm(TFile* file);
int itm_close_hotstart2_swmm(TFile* file);
int itm_write_hotstart_swmm(TFile* file);
int itm_read_hotstart_swmm(TFile* file);
int itm_hotstart_status();


#endif//__ITM_H__
