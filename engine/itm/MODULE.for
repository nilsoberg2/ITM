! This file is part of the ITM model.
!
! Copyright 2009 University of Illinois at Urbana-Champaign
! Copyright 2011 Oregon State University, Corvallis
!
! Authors: Arturo S. Leon (Hydraulics), Nils Oberg (User interface)
!
! ITM is a free software; you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published
! by the Free Software Foundation; either version 2.0 of the
! License, or (at your option) any later version.
! 
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
! 02110-1301, USA.

	MODULE COMMON_MODULE	
	implicit none 
c	General parameters						
	double precision, ALLOCATABLE :: S0(:)			!sewer bottom slope	
	double precision, ALLOCATABLE :: z0(:,:)		!elevations of cells (mid point)	
	double precision, ALLOCATABLE :: zb(:,:) 		!elevations 	
	Integer, ALLOCATABLE :: IdFlow(:,:)	
	double precision, ALLOCATABLE :: d(:)
	double precision, ALLOCATABLE :: Length(:)
	double precision, ALLOCATABLE :: Dx(:)

	!Init
	Integer, ALLOCATABLE :: inf(:,:)
	Integer, ALLOCATABLE :: oufl(:,:) 
	Integer, ALLOCATABLE :: Ninf(:)
	Integer, ALLOCATABLE :: Noufl(:)
      Integer, ALLOCATABLE :: Number_of_zero_drops(:)
      Integer, ALLOCATABLE :: ID_Number_of_zero_drops(:,:)
      
	double precision, ALLOCATABLE :: Qcrit_maxIA(:)
	double precision, ALLOCATABLE :: Qnor_maxIA(:)
	double precision, ALLOCATABLE :: ycrit_min(:)
      double precision, ALLOCATABLE :: ycrit_max(:)
      double precision, ALLOCATABLE :: Ecrit_max(:)
      
      
	double precision, ALLOCATABLE :: d1_min(:)
	double precision, ALLOCATABLE :: d2_min(:)
      
      
c     Boundaries		
	Integer, ALLOCATABLE :: Nnod(:)
	Integer, ALLOCATABLE :: line_elem(:,:)
	double precision, ALLOCATABLE :: const_depth_flow(:)
	Integer, ALLOCATABLE :: min_water_pipeID(:)
	double precision, ALLOCATABLE :: Del_Y_res(:)
	double precision, ALLOCATABLE :: V_head_reser(:)

	!for hydrographs and reserv. 	
	double precision, ALLOCATABLE :: Abound(:,:)
	double precision, ALLOCATABLE :: Qbound(:,:) !to store flow 
	double precision, ALLOCATABLE :: ybound(:,:)
	Integer, ALLOCATABLE :: Idflow_bound(:,:)
	Integer, ALLOCATABLE :: Idflow_bound_reser(:,:)
	
	!variables from the boundaries	
	Integer, ALLOCATABLE :: BCnode(:)
	Integer, ALLOCATABLE :: sum_dry_bed_node(:)
	Integer, ALLOCATABLE :: open_closed_bound(:)
	
c	Roughness
	double precision, ALLOCATABLE :: nm(:)		!nm = Manning roughness coefficient (gravity flow)
	double precision, ALLOCATABLE :: fd(:)		!fd = Darcy friction coefficient
	Integer, ALLOCATABLE :: Node1(:)
	Integer, ALLOCATABLE :: Node2(:)


c	Parameters Star region 
      Integer CODE_STAR_REGION  !Parameter to compute or not variables at star region
      !CODE_STAR_REGION = 1. Variabels at star region are computed. Otherwise, no. 

c	Parameters open channel/pressurized flow
	double precision, ALLOCATABLE :: h0(:,:)	!flow depth
	double precision, ALLOCATABLE :: h0_Rec(:,:)	!Water depth for reconstruction	
	double precision, ALLOCATABLE :: A0(:,:)
	double precision, ALLOCATABLE :: Q0(:,:)
	double precision, ALLOCATABLE :: AREA_FULL(:)
	double precision, ALLOCATABLE :: Area_for_pressur(:)
	double precision, ALLOCATABLE :: y_for_pressur(:)


	double precision, ALLOCATABLE :: Aref(:)
	double precision, ALLOCATABLE :: Phiref(:)
	double precision, ALLOCATABLE :: Yref(:)
	double precision, ALLOCATABLE :: haver_ref(:)
	double precision, ALLOCATABLE :: celer_ref(:) 
	double precision, ALLOCATABLE :: P_pho_ref(:) 
	double precision, ALLOCATABLE :: P_pho_dry(:) 
	

	double precision, ALLOCATABLE :: pc1(:)	
	Integer, ALLOCATABLE :: fully_pressuri(:)
c	pref	=	Reference pressure
c	RoRef	=	Reference density
c	pc = pressurized flow wave speed
	double precision RoRef
	double precision PI
	double precision pc_air
	double precision pc_press
	double precision pc_mixed

	double precision g						!@ITM@IGNORE
	parameter (g = 9.8) !gravity (m/s2)		!@ITM@IGNORE
	
	!values at adjacent cells and at boundaries at old time step	
	!NodeNS(r) = Number of pipes connected to the node (1, 2, 3 ....)
	!NodeID(r,j) = pipe ID(1, 2, 3, ...) 
	!Nodetype = inflowing or outflowing
	!maximum 10 pipes can be connected to each node
	integer, ALLOCATABLE :: NodeNS(:)
	integer, ALLOCATABLE :: Nodetype(:,:)
	
c	Parameters air pocket
	!air pressure head expressed in equivalent water head
	double precision, ALLOCATABLE :: ha(:,:) 
	!X minimum and maximum cell IDS for air pockets
	Integer, ALLOCATABLE :: XapMIN(:,:)
	integer, ALLOCATABLE :: XapMAX(:,:) !new
	Integer, ALLOCATABLE :: XapMINold(:,:)
	integer, ALLOCATABLE :: XapMAXold(:,:) !old
	!Number of air pocket
	Integer, ALLOCATABLE :: Nap(:)
	integer, ALLOCATABLE :: Napold(:)
	!volume of air pocket
	double precision, ALLOCATABLE :: VOLap(:,:)
	double precision, ALLOCATABLE :: VOLap_old(:,:) 
	!air pressure in water column 
	double precision, ALLOCATABLE :: hap(:,:)
	double precision, ALLOCATABLE :: hapold(:,:)
	!politropic exponent for air 
	!ka = 1.0, 1.4 or somewhere between
	!Cd = air discharge coefficient 
	double precision ka
	double precision Cd

	!air density
	double precision, ALLOCATABLE :: dens_old(:,:)
	double precision, ALLOCATABLE :: dens(:,:)

c	Various	
	double precision, ALLOCATABLE :: Atemp1(:)
	double precision, ALLOCATABLE :: htemp1(:)
	double precision, ALLOCATABLE :: Qtemp1(:)
	Integer, ALLOCATABLE :: IdFlow1(:)	
	Integer, ALLOCATABLE :: IdFlow_REC_L(:) !Flow types for reconstruction (LEFT)
	Integer, ALLOCATABLE :: IdFlow_REC_R(:) !Flow types for reconstruction (RIGHT)	
	double precision, ALLOCATABLE :: A0L(:)
	double precision, ALLOCATABLE :: A0R(:)
	double precision, ALLOCATABLE :: Q0L(:)
	double precision, ALLOCATABLE :: Q0R(:) 
	integer pressurized
      
      Integer, ALLOCATABLE :: NoConvergence_Junction_GLOBAL(:)

	!## Upstream IDs
	

	!## Junction
	Integer, ALLOCATABLE :: Junct(:,:)
	Integer, ALLOCATABLE :: max_crown_pipe(:)	
	double precision, ALLOCATABLE :: Drop(:,:)
	double precision, ALLOCATABLE :: Ares_junct(:)
	double precision, ALLOCATABLE :: yres_jun_old(:)	
	double precision, ALLOCATABLE :: height_jun(:)
	double precision, ALLOCATABLE :: max_elev_crown(:) 
	
		
	!## Reservoir 	
	
	double precision, ALLOCATABLE :: flowdepth_res(:)	
	double precision, ALLOCATABLE :: yres_up(:)
	double precision, ALLOCATABLE :: Reser_outflow(:)
      double precision, ALLOCATABLE :: Outflow_limited(:) !allowed outflow
      
	double precision, ALLOCATABLE :: reser_maxdepth(:)

	!## Dropshaft
	double precision, ALLOCATABLE :: Adrop(:)
	double precision, ALLOCATABLE :: hdrops_overf(:)		
	double precision, ALLOCATABLE :: junct_elev(:)

	double precision, ALLOCATABLE :: V_over(:)
	Integer, ALLOCATABLE :: nodeID(:,:)	

	!(len=1024)

		
	double precision, ALLOCATABLE :: yudrop_n(:)
	double precision, ALLOCATABLE :: yudrop_n_1(:)
	double precision, ALLOCATABLE :: ydropmin(:)
	double precision, ALLOCATABLE :: dropmin(:)
	double precision, ALLOCATABLE :: Areser_min(:)
			
	!## Variables at interfaces (i+1/2)	
	double precision, ALLOCATABLE :: Fupst(:,:)
	double precision, ALLOCATABLE :: Fdownst(:,:)
      double precision, ALLOCATABLE :: Flux_all(:,:,:)	!Flux of the entire domain
	double precision, ALLOCATABLE :: Pres_pho_Bound(:,:)	
	double precision, ALLOCATABLE :: F(:,:) !Fluxes
	double precision, ALLOCATABLE :: P_pho_interf(:) !Pressure_Pho at interfaces
	double precision, ALLOCATABLE :: y_interf(:) !Y at interfaces			
	double precision, ALLOCATABLE :: A_interf(:) !A at interfaces			
	
	!constants	
	!Hb = Atmospheric pressure in m of water
	!RoRef_air =  air density
	double precision dt2
	double precision Hb
	double precision RoRef_air	
	Integer number_steps
	Integer NR	!NR = number of reaches
	Integer, ALLOCATABLE :: NX(:)
	Integer Num_max_cells !maximum number of cells as a 
	!result of discretization

	!Dry bed 
	double precision, ALLOCATABLE :: ydry(:)
	double precision, ALLOCATABLE :: Adry(:)
	double precision, ALLOCATABLE :: phi_dry(:)	
	double precision, ALLOCATABLE :: Celer_dry(:)
	double precision, ALLOCATABLE :: ydry_CFL(:)
	double precision, ALLOCATABLE :: Adry_CFL(:) !For Courant stab. 
	double precision, ALLOCATABLE :: fluxdry(:) !Flux due to minimum water depth 
	
	!sloped pipe
	double precision Min_Slope_for_Sloped_pipe
	
	double precision, ALLOCATABLE :: A_cell_dry_sloped(:)
	double precision, ALLOCATABLE :: A_open_sloped_pipe(:)
	double precision, ALLOCATABLE :: b2_max(:)  !maximum depth of pipe in the vertical axis
	double precision, ALLOCATABLE :: A2_max(:)  !maximum area of pipe in the vertical axis
      !b2_min and A2_min are for iteration purposes
      double precision, ALLOCATABLE :: b2_min(:)  !minimum depth of pipe in the vertical axis
	double precision, ALLOCATABLE :: A2_min(:)  !minimum area of pipe in the vertical axis	
      double precision, ALLOCATABLE :: P_pho_min(:)  !minimum P_pho for iteration purp
	double precision, ALLOCATABLE :: P_pho_max(:)  !maximum P_pho for iteration purp
      double precision, ALLOCATABLE :: phi_max(:)  !minimum phi for iteration purp
	double precision, ALLOCATABLE :: phi_min(:)  !maximum phi for iteration purp	
	double precision, ALLOCATABLE :: y_for_phi_max(:)  !maximum depth for phi in iteration
	
	!Dtmax
	double precision DtMax	
	
      !Internal Tolerances
      double precision Tol_int_10_1
      double precision Tol_int_10_2
      double precision Tol_int_10_3
      double precision Tol_int_10_4
      double precision Tol_int_10_5
      double precision Tol_int_10_6
      double precision Tol_int_10_7
      double precision Tol_int_10_8
      double precision Tol_int_10_9
      double precision Tol_int_10_10
      double precision Tol_int_10_12
      double precision Tol_int_10_14

	!stored volume
	double precision Vol_entered_system
      
      
      !Inflows volume
	double precision Vol_inflows      
	
	!Volumes
	double precision Vol_lost_system
      double precision vol_reservoirs_time_step
      double precision vol_rating_time_step
      double precision vol_dropshafts_time_step 
      double precision vol_junctions_time_step
      double precision vol_pipes_time_step
      double precision vol_inflows_time_step
      double precision vol_const_bound_time_step      
      

	!SWMM
	Integer Nnodes
	
	!various
	Integer SUM_VAR
	Integer sum_no_converg
	double precision, ALLOCATABLE :: Klocal(:,:)
	
	!Rating curve
	double precision, ALLOCATABLE :: Max_flow_rating_curve(:) 
	!Maximum flow specified at the rating curve. This is used to check if the water level in the rating curve is exceeded. 
      
      !cross-section area of weir (bottom to weir crest). 
      double precision, ALLOCATABLE :: area_weir(:) 
      
      
	double precision, ALLOCATABLE :: Max_Head_rating_curve(:) 
	!Maximum flow specified at the rating curve. This is used to check if the water level in the rating curve is exceeded. 
	
	!Gates
	double precision, ALLOCATABLE :: Cd_gate(:)	
	double precision, ALLOCATABLE :: Hgate_open(:) !Gate opening in percentage
	double precision, ALLOCATABLE :: h_gate_m(:) !Height of gate opening in m
		
	!Volume check       
      double precision vol_reserv_outflow_time_step
      double precision balance_volume_time_step
      double precision vol_lost_time_step
      double precision Vol_stored_old_time
	double precision Volume_stored_current_step
	double precision Error_volume
	double precision balance_volume
	double precision Q1(10)
	double precision A1(10)
	double precision u1(10)
	double precision y1(10)
	double precision c1(10)
	double precision P_pho1(10)	
	double precision dropIA(10)
	double precision Ab_oldIA(10)
	double precision Qb_oldIA(10)
	double precision yb_oldIA(10)
	double precision phi11(10)
	double precision phi1IA(10)

	
	double precision yw01(10)
	double precision Aw01(10)
	double precision Qw01(10)
	double precision y_drybed(10)
	double precision Q_drybed(10)
	
	integer IDf1(10)
	integer IDfbIA(10)
	integer IDfbIA_old(10)
	integer drybed(10) 
	integer flow_regIA(10)
	integer flowcaseIA(10)
	integer SumIDFIA(10)
	integer init_volume_counter
	integer cond_mixed1(10)
	integer Idf01(10)

	double precision, allocatable :: EntranceLoss(:)
	double precision, allocatable :: ExitLoss(:)

	integer temp9

	Integer Istor	
	double precision T_GLOBAL
	double precision DT_GLOBAL
	double precision T_NEXT_REPORT
	double precision TIME_BEGIN
	double precision TIME_END

	integer GLOBAL_STATUS_FLAG
	integer MaxNumPlotCells
	double precision tol
	double precision tol_lower
	double precision tol_very_low
	double precision tol_higher
	double precision tol_crit
	double precision convergen

      integer NxMax
	integer numitera
	integer min_num_grids
	double precision pc
	double precision pcm
	double precision yfree_press
	double precision Tmax
	double precision Dtmax1
	double precision Tstor

	double precision Initial_volume_stored !was added

	character (len=1024) :: INPUT_FILE_NAME	!@ITM@IGNORE
	character (len=1024) :: INPUT_DIRECTORY	!@ITM@IGNORE
	integer INPUT_FILES_OPEN				!@ITM@IGNORE

	character*1000 error_message			!@ITM@IGNORE
	integer error_message_len				!@ITM@IGNORE
	parameter (error_message_len = 1000)	!@ITM@IGNORE

	integer NumVolErrPoints !@ITM@IGNORE
	
	!Parameters used for solving non-linear equations
	double precision paramOP1
	double precision paramOP2
	double precision paramOP3
	double precision paramOP4 
	double precision paramOP5
	double precision paramOP6
	double precision paramOP7
	double precision paramOP8
	double precision paramOP9
	double precision param1
	double precision param2
	double precision param3
	double precision param4 
	double precision param5
	double precision param6
	double precision param7
	double precision param8 
	double precision param9
	double precision param10
	double precision param11
	double precision param12 
	double precision param13
	double precision param14
	double precision param15
	double precision param16 
	double precision param17
	double precision param18
	double precision param19
	double precision param20 	
	double precision param21
	double precision param22
	double precision param23
	double precision param24 
	double precision param25
	double precision param26
	double precision param27
	double precision param28 
	double precision param29
	double precision param30
	double precision param31
	double precision param32 
	double precision param33
	double precision param34
	double precision param35
	double precision param36
	double precision param_ener
		
	Integer parintOP1
	Integer parintOP2
	Integer parint1
	Integer parint2
	Integer parint3
	Integer parint4
	Integer parint5
	Integer parint6
	Integer parint7
	Integer parint8
	Integer parint9
	Integer parint10
	Integer parint11
	Integer parint12
	Integer parint13
	Integer parint14
	Integer parint15
	Integer parint16
	Integer parint17
	Integer parint18
	Integer parint1000
      Integer Counter_printing

	Integer sumIA
	Integer sumIB
	Integer sumIC
	double precision, ALLOCATABLE :: Qmin(:)
	Integer code_vol_bal
	double precision Vol_bal
	double precision water_init_elevation  !for initial constant water level
	Integer ini_cond !Initial condition = constant water depth
	Integer sum_temp !Initial condition = constant water depth
	double precision temp_outflow !temporal value

	!Pumping rates at selected nodes. 
	double precision, ALLOCATABLE :: Qpump(:)
	double precision, ALLOCATABLE :: t_begin_pump(:)
	Integer, ALLOCATABLE :: IDpump(:)

	integer CURRENT_REVISION			!@ITM@IGNORE
	parameter (CURRENT_REVISION = 213)	!@ITM@IGNORE
	
	!!!!!! Parameters thtat don't need to get saved to hotstart file go here.
	double precision, ALLOCATABLE :: h0L(:)
	double precision, ALLOCATABLE :: h0R(:) 
	

	!Pressurization and Depressurization of the system
	Integer system_pressur 
	double precision vol_factor
	Integer type_of_flow

	!To relax time steps
	Integer Relax_condit

	END MODULE COMMON_MODULE 
