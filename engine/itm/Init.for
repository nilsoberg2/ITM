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

      Subroutine Init(input_file, report_file, output_file)
	!## Purpose: Read data and set up constants	    
	use common_module 
	implicit none  
      integer i,j,k,p,q,Time_hydro
	integer nod
	integer Npipes,R,S,T	
	integer maxi
	integer num_max_hydr,pipe_max_crown
	
	double precision teta
      double precision P_pho,conver,area,discharge,TH, p1  
	double precision RH,yy,htemp,crown_elev_max,diamax
	double precision Ynormal,Ycrit,Yconjugate,dxtemp,s_temp,ScIA	
	double precision temp100,temp101
	character*200  TITLE, Variab
	Integer Node(1000,10)
	Integer Reser_dropsh_ID(1000),hydrog_ID(1000)
	Integer Pip(1000),nodID(1000)	
	Integer L1,L2,L3,L4,IdFlow_temp
	double precision Q2,dh1,dh2,A,Ts,dmin,temcel,Vmin,Qb,Dt
	double precision Init_disch(1000),Init_depth(1000)
	double precision sum_temp1,sum_temp2,sum_temp3,Stora_new 
	double precision dx1max, dx2max,dx3max,max_dia,drop_min
	Integer Init_depth_type(1000)
	Integer R1,R2
	double precision Drop1,Drop2,P_phoIA,h0b1,A0b1,value_reser
	double precision temp1,temp2,temp3,temp4,dz,teta1,teta2
	double precision temp5,temp6,temp7,temp77
	double precision const1,b,ALF,y_downs,Vol,y_reconR_dry
	double precision y_for_phi_min,Area_reconR_dry
	double precision x(1),x2(1)
	double precision dr, dry_diameter_fraction
	integer j_1,j_2,diam_differ
	character(*) input_file, report_file, output_file
	character*25 temp_id	
	integer itm_has_inflow,sum_drop,sum_drop2
	integer swmmRes
	character*1024 swmmMsg

	integer itm_check_hotstart_status ! function
	

	! OPEN THE SWMM FILE AND GET THE NUMBER OF OBJECTS
	call itm_open_start(input_file // CHAR(0), report_file // CHAR(0),
     &	output_file // CHAR(0), swmmRes)
	if (swmmRes > 0) then
		write(99,*), 'SWMM ERROR DETECTED'
		call itm_get_error_message(swmmRes, swmmMsg)
		write(99,*), swmmMsg
		call itm_finish_report
		call itm_end
cDEC$ IF DEFINED (ITM_USE_DLL)
		GLOBAL_STATUS_FLAG = 1
		return
cDEC$ ELSE
		stop
cDEC$ ENDIF
	endif
	
	call itm_get_globals(nxmax, numitera, min_num_grids, pc, pcm,
     &	yfree_press, Tmax, Dtmax1, Tstor, T_NEXT_REPORT,
     &    MaxNumPlotCells,
     &	tol, tol_lower, tol_very_low, tol_higher, tol_crit,
     &	water_init_elevation, type_of_flow)	

      NumVolErrPoints = 0
           

	if (itm_check_hotstart_status() == 1) then		
		write(99,*),'hotstart file'
		DtMax = Min(DtMax1,Tstor,Tmax) !This is important when using hotstart file
		return
	endif

	ALLOCATE (BCnode(1001))
	ALLOCATE (open_closed_bound(1001))	 
	ALLOCATE (Adrop(1000),hdrops_overf(1000),junct_elev(1001))
	ALLOCATE (flowdepth_res(1001))
	ALLOCATE (Length(1000))
	ALLOCATE (d(1000),zb(1000,2))			
	ALLOCATE (EntranceLoss(1000), ExitLoss(1000))
	ALLOCATE (nm(1000),fd(1000),Node1(1000),Node2(1000))
	ALLOCATE (Reser_outflow(1001),Klocal(1000,2))
      ALLOCATE (Outflow_limited(1001))
	ALLOCATE (Reser_maxdepth(1001))
	ALLOCATE (nodeID(1001,10),NodeNS(1001))
	ALLOCATE (Nodetype(1001,10))
	ALLOCATE (const_depth_flow(1001))	
     
	
      !Internal Tolerances
      Tol_int_10_1 = 1.d-1
      Tol_int_10_2 = 1.d-2
      Tol_int_10_3 = 1.d-3
      Tol_int_10_4 = 1.d-4
      Tol_int_10_5 = 1.d-5
      Tol_int_10_6 = 1.d-6
      Tol_int_10_7 = 1.d-7
      Tol_int_10_8 = 1.d-8
      Tol_int_10_9 = 1.d-9
      Tol_int_10_10 = 1.d-10
      Tol_int_10_12 = 1.d-12
      Tol_int_10_14 = 1.d-14
	
	parint1000 = 0	
	Nnodes = 0
	Npipes = 0
	init_volume_counter = 0 !for initial volume

	call itm_get_num_links(Npipes)
	call itm_get_num_nodes(Nnodes)
	!call itm_get_starting_nodes(Nstart, start, 10)

	NR = Npipes	!Number of pipes in the system

	write(99,*), 'Reading the input file now'
	call readswmm(1000, Node1, Node2, length, d, nm, zb, Init_disch,
     &    Init_depth_type, Init_depth, EntranceLoss, ExitLoss,
     &	1000, const_depth_flow,
     &	NR+2, Adrop, hdrops_overf, 1000,
     &    flowdepth_res, reser_outflow, reser_maxdepth, 1000,
     &	junct_elev, BCnode, open_closed_bound, swmmRes)	
	
	if (swmmRes > 0) then
		write(99,*), 'SWMM ERROR DETECTED'
		call itm_get_error_message(swmmRes, swmmMsg)
		write(99,*), swmmMsg
		call itm_close(swmmRes)
		call itm_finish_report
		call itm_end
cDEC$ IF DEFINED (ITM_USE_DLL)
		GLOBAL_STATUS_FLAG = 1
		return
cDEC$ ELSE
		stop
cDEC$ ENDIF
	endif
	
	
	if (water_init_elevation < -99999.51 .or.
     &	 water_init_elevation > -99999.49)then
		ini_cond = 1
	else
		ini_cond = 0
	endif


	! Write the ITM and SWMM ID of the pipes to the debug file
	write(99,*) 'PIPES'
	write(99,*) ' ITM_ID SWMM_ID'
	do j = 1, Npipes
		temp_id = ''
		call itm_get_swmm_id(1, j, temp_id) ! 1 for pipes
		write(99,*), j, trim(temp_id)
	enddo

	! Write the ITM and SWMM ID of the nodes to the debug file
	write(99,*) 'NODES'
	write(99,*) 'ITM_ID SWMM_ID'
	do R = 1, Nnodes
		temp_id = ''
		call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
		write(99,*), R, trim(temp_id)
	enddo

	!!!!END ITM CODE

	pc_press = pc
	pc_mixed = pc

	!Read data file
	!## zb(j,1),zb(j,2) = upst. and downst. invert elevations 
	!## Length(j) = Length of pipes
	!## d(j) = Diameter of pipes at every reach 
	!## n(j) = Manning roughness coefficient (gravity flow. It may be changed 
	!## with f darcy for gravity and pressurized flows)	

!	variables SWMM  varaiables ITM
!	Elev				junct_elev
!	Node1				Node1
!	Node2				Node2
!     Length				Length
!	N					nm
!	Z1 					zb(i,1)
!	Z					zb(i,2)
!	Q0					Init_disch(i)

	!open(UNIT=10,FILE='Reservoirs.csv',STATUS='Unknown')	
	!open(UNIT=15,FILE='Dropshafts.csv',STATUS='Unknown')
	
	!Enter boundary condition and type of dropshaft, No of hydrograph and 
	!specify if the boundary conditions it is open or pressurized. 

	!data Required ends here
	!##########################################################################################
	!Insert nodes and Lines
	!Npipes = maxval(pip) !NOO
	!Nnodes = maxval(nodID) !NOO
	ALLOCATE (Nnod(Nnodes))	
	ALLOCATE (min_water_pipeID(Nnodes))
	ALLOCATE (inf(Nnodes,3),oufl(Nnodes,3))
	ALLOCATE (Ninf(Nnodes), Noufl(Nnodes)) 
	ALLOCATE (V_over(Nnodes))
	ALLOCATE (Dx(NR),NX(NR))
	ALLOCATE (A_cell_dry_sloped(NR))
	ALLOCATE (A_open_sloped_pipe(NR))
	ALLOCATE (b2_max(NR))
	ALLOCATE (A2_max(NR))
	ALLOCATE (b2_min(NR))
	ALLOCATE (A2_min(NR))
	ALLOCATE (P_pho_min(NR))
	ALLOCATE (P_pho_max(NR))
	ALLOCATE (phi_max(NR))
	ALLOCATE (phi_min(NR))
	ALLOCATE (y_for_phi_max(NR))
	ALLOCATE (Qmin(NR))
		
	!Pressurization and Depressurization of the system	
	!Pumping rates at selected nodes.
	ALLOCATE (Qpump(Nnodes),t_begin_pump(Nnodes))
	ALLOCATE (IDpump(Nnodes)) !IDpump (0 No pumping, 1 pumping)
	system_pressur = 1 !1 System is only allowed to get pressurized
	!2 system is allow to get pressurized and depressurized
	IDpump(:) = 0
	!Read pumpling flow rates

	Nnod(:) = 0	
	inf(:,:) = 0; oufl(:,:) = 0	
	Noufl(:) = 0; Ninf(:) = 0
	Qpump(Nnodes) = 0d0
      Outflow_limited(Nnodes) = 0d0
	NodeID(:,:) = 0
	Node(:,:) = 0
      
	!To determine number of inflowing and outflowing pipes 
	! and to count the number of pipes at each node
	dmin = 10000d0
	do k=1,Npipes
		i = Node1(k) !Upstream node
          Nnod(i) = Nnod(i)+1
		R = Nnod(i); Node(i,R) = k; Noufl(i) = Noufl(i)+1
		NodeID(i,R) = Node(i,R)
		Nodetype(i,R) = 2 !outflowing
		Klocal(k,1) = EntranceLoss(k) !Entrance losses
		R = Noufl(i); oufl(i,R)=k		
		
          i = Node2(k) !downstream node
          Nnod(i) = Nnod(i)+1
		R = Nnod(i);Node(i,R) = k; Ninf(i) = Ninf(i)+1
		NodeID(i,R) = Node(i,R)
		Nodetype(i,R) = 1 !inflowing
		R = Ninf(i); inf(i,R)=k	
		Klocal(k,2) = ExitLoss(k) !Exit losses
		dmin = min(d(k),dmin)				
	enddo
	do R = 1, Nnodes
		NodeNS(R) = Nnod(R) !Number of pipes connected to each node		
	enddo

	!Preprocessing of input data
	!## Grid sizes for every reach
	dxtemp = 1000d0
	maxi = 4  !let this as it is
	min_num_grids = max(min_num_grids,4) !Minimum number of grids can not smaller than 4
	do j=1,NR
		dxtemp = min(Length(j)/min_num_grids,dxtemp)
	enddo

	do j=1,NR		
		Nx(j) = Int(Length(j)/dxtemp)
		Dx(j) = Length(j)/Nx(j)	
		maxi = max(NX(j),maxi) 
	enddo	
	maxi = maxi + 4 !4(to make second order the numerical scheme)
		
	ALLOCATE (S0(NR))	
		
      !Boundaries			
	ALLOCATE (Abound(NR,2),Qbound(NR,2),Idflow_bound(NR,2))
	ALLOCATE (Idflow_bound_reser(NR,2))
	ALLOCATE (ybound(NR,2))
	ALLOCATE (Fupst(NR,2),Fdownst(NR,2))
	ALLOCATE (Pres_pho_Bound(NR,2))

	!dry bed 
	ALLOCATE (ydry(NR),Adry(NR),fluxdry(NR))      
	ALLOCATE (Celer_dry(NR),phi_dry(NR))	
	ALLOCATE (ydry_CFL(NR),Adry_CFL(NR))
		
	!Junction	
	ALLOCATE (Drop(Nnodes,10),Ares_junct(Nnodes),height_jun(Nnodes),
     &	yres_jun_old(Nnodes))	
	ALLOCATE (max_elev_crown(Nnodes),max_crown_pipe(Nnodes))
	ALLOCATE (Del_Y_res(Nnodes),V_head_reser(Nnodes))
      ALLOCATE (Number_of_zero_drops(Nnodes))
      ALLOCATE (ID_Number_of_zero_drops(Nnodes,10))
      
      		
	!Gates
	ALLOCATE (Cd_gate(Nnodes))	
	ALLOCATE (Hgate_open(Nnodes))	
	ALLOCATE (h_gate_m(Nnodes))
	
	!Rating curve
	ALLOCATE (Max_flow_rating_curve(Nnodes))
	!Maximum flow specified at the rating curve. This is used to check if the water level in the rating curve is exceeded. 	     
      
	ALLOCATE (Max_Head_rating_curve(Nnodes))
	!Maximum Head specified at the rating curve. This is used to check if the water level in the rating curve is exceeded. 	
      
      ALLOCATE (area_weir(Nnodes))
	!cross-section area of weir (bottom to weir crest). 
      
	!Reservoir	
	ALLOCATE (yres_up(50))	
	
	!Dropshafts
	ALLOCATE(yudrop_n(NR+2),yudrop_n_1(NR+2))	
	ALLOCATE(ydropmin(Nnodes),dropmin(Nnodes))
	ALLOCATE(Areser_min(Nnodes))
	ALLOCATE(sum_dry_bed_node(Nnodes))
           
      
      ALLOCATE(NoConvergence_Junction_GLOBAL(Nnodes))
	
	!Air pockets 
	ALLOCATE (ha(NR,maxi),VOLap(NR,50),hap(NR,50),hapold(NR,50))
	ALLOCATE (VOLap_old(NR,50), dens_old(NR,50),dens(NR,50))

	ALLOCATE (XapMIN(NR,50),XapMAX(NR,50),Nap(NR))
	ALLOCATE (XapMINold(NR,50),XapMAXold(NR,50),Napold(NR))		

	!Parameters 
      ALLOCATE (Flux_all(NR,maxi,2))	!Flux of the entire domain
	ALLOCATE (h0(NR,maxi),IdFlow(NR,maxi))
	ALLOCATE (h0_Rec(NR,maxi))
      ALLOCATE (IdFlow_REC_L(maxi),IdFlow_REC_R(maxi))
	ALLOCATE (A0(NR,maxi),Q0(NR,maxi))
	ALLOCATE (z0(NR,maxi))
	ALLOCATE (AREA_FULL(NR),Area_for_pressur(NR),y_for_pressur(NR))
	ALLOCATE (Aref(NR),Phiref(NR),Yref(NR),haver_ref(NR),celer_ref(NR))
	ALLOCATE (P_pho_ref(NR))
	ALLOCATE (P_pho_dry(NR))	
	ALLOCATE (A0L(maxi),A0R(maxi),Q0L(maxi),Q0R(maxi))
	ALLOCATE (h0L(maxi),h0R(maxi))
	ALLOCATE (Qcrit_maxIA(NR),Qnor_maxIA(NR),ycrit_min(NR))
      ALLOCATE (ycrit_max(NR),Ecrit_max(NR))
	ALLOCATE (d1_min(NR),d2_min(NR))
	ALLOCATE (pc1(NR)) 
	ALLOCATE (fully_pressuri(NR))	
	
	!air pockets
	RoRef_air = 1.225 ! (kg/m3) at atmosph. pressure and T = 15 degrees C 
	hap(:,:) = 0d0	
	hapold(:,:) = 0d0
	volap(:,:) = 0d0
	dens(:,:) = RoRef_air

	!constants
	number_steps = 0
	sum_no_converg = 0	
	!k = politropic exponent for air: k = 1.0, 1.4 or somewhere between
	ka = 1.4
	
	CODE_STAR_REGION = 0 !Parameter to compute or not variables at star region
	Relax_condit = 1 !Initial condition for relax of time step (Please don't change)	
	
	!Constants for pressurized flow
	RoRef =  1000d0		!kg/m3 (Reference density)	
	pc_air = 340d0
	Cd = 0.65 !air discharge coefficient
	Hb = 10.33 !Atmospheric pressure in meters of water
	PI = ATAN(1d0)		!ATAN(1) =  PI/4
	PI = 4d0*PI			!VALUE OF PI	
		
	Error_volume = 0d0 
	balance_volume = 0d0 !To balance conservation of volume problems
	Vol_lost_system = 0d0  !Volume lost in the system
	
	!Sloped pipe
	!Slope to be considered sloped pipe
	Min_Slope_for_Sloped_pipe = 0.08 !This could be moved to the GUI	
		
	!write(99,*),'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
	!write(99,*),' Boundary Condition type'
	!write(99,*),' 4: Dropshaft with inflow hydrog.' !okay
	!write(99,*),' 7: Junction of 2 or more pipes'	!okay
	!write(99,*),' 10: Discharge Q constant'	!okay
	!write(99,*),' 11: Water depth constant'	
	!write(99,*),' 20: Reserv. BC' !okay
	!write(99,*),' 24: junction of 2 pipes without inflow 	
	!write(99,*),' 30: Rating curve BC
	!write(99,*),' 40: Gate Boundary condition (Two pipes)
	!write(99,*),' 41: Gate Boundary condition (one pipe)
      !write(99,*),'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
	
	Drop(:,:) = 10000d0  !to be used for computing 
	!mimimum water depth in dropshafts
	!Computing drop heights at inlets and outlets 
	!write(99,*),'Nnodes',Nnodes	
	sum_drop2 = 0
		
	do R =1,Nnodes
	      if (BCnode(R) .ne. 4 .and. BCnode(R) .ne. 7)then !Only for dropshafts and junctions
	          if (itm_has_inflow(R) == 1)then
	              call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
                 write(98,*),'Only dropshafts and junctions can have'
                 write(98,*),'inflow hydrographs. There is an inflow'
                 write(98,*),'hydrograph at node (SWMM) =',trim(temp_id)
                 write(99,*),'Only dropshafts and junctions can have'
                 write(99,*),'inflow hydrographs. There is an inflow'
                 write(99,*),'hydrograph at node (SWMM) =',trim(temp_id)
                    call endprog
                endif 
            endif 
	enddo
	
	
	!To be used in sloped pipes for computing minimum y_dry
      y_reconR_dry = 10000d0
	
	!Pipe slopes
	do j=1,NR	
		    S0(j) = (zb(j,1) - zb(j,2))/Length(j)
		    write(99,*),'s0',j,S0(j)
              
              if (S0(j) <= -1.d-14)then  !Negative slope
		        call itm_get_swmm_id(1, j, temp_id) ! 1 for pipes
		        write(99,*),'pipe ID = ',temp_id,'slope = ',S0(j)
		        write(99,*),'Pipe slope must be positive' 
                  write(99,*),'Upstream node must have higher elevation'
		        write(99,*),'Change direction of flow'                  
                  write(98,*),'pipe ID = ',temp_id,'slope = ',S0(j)
		        write(98,*),'Pipe slope must be positive' 
                  write(98,*),'Upstream node must have higher elevation'
		        write(98,*),'Change direction of flow'
			    call endprog
		    endif		    
		    
		    if (S0(j) <= 1.d-7)then  !This should be modified later to acount for horizontal slopes
                  S0(j) = 1.d-7
		        call itm_get_swmm_id(1, j, temp_id) ! 1 for pipes
		        write(98,*),'pipe ID = ',trim(temp_id),'slope = ',S0(j)
		        write(98,*),'Pipe slope must be at least 0.0000001 (10^-7)' 
		        write(98,*),'for plotting purposes'
			    write(98,*),'Change slightly the invert of pipe inlet or outlet'
			    write(99,*),'pipe ID = ',trim(temp_id),'slope = ',S0(j)
			    write(99,*),'Pipe slope must be at least 0.0000001 (10^-7)' 
		        write(99,*),'for plotting purposes'
			    write(99,*),'Change slightly the invert of pipe inlet or outlet' 
			    !call endprog
		    endif
		    
		    if (S0(j) > Min_Slope_for_Sloped_pipe)then
		        y_reconR_dry = min(y_reconR_dry,abs(s0(j)*dx(j)))
		    endif
      enddo
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	y_reconR_dry = y_reconR_dry/2d0  !y_reconR_dry/2d0 works great
	dry_diameter_fraction = 1000d0  !1000 works very well	
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
	!Dry bed and free surface and pressurized flow limits
	do j=1,NR
		    if (S0(j) > Min_Slope_for_Sloped_pipe)then		    
    		        !Full area
    		        ALF = abs(ATan(S0(j)))
	          b = 1d0/Cos(ALF)*(d(j)/2d0)
	          const1 = b*d(j)/2d0
		        Area_full(j) = PI*const1	
	          
	          b2_max(j) = (1d0-1.d-12)*2d0*b !maximum water depth for open channel (for searches)	          
		        call Area_from_H(j,b2_max(j),A2_max(j),Ts,RH,0)	
		        	        
		        
		        !dry bed 
		        !ydry(j) = max(1.d-3, 1d0/200d0*dmin) !Max (1mm, 1/200 dmin)
		        ydry(j) = 1d0/dry_diameter_fraction*dmin !Max (1mm, 1/200 dmin)
		        
		        !Minimum water depth and area (for iteration purposes)
		        b2_min(j) = 1.d-1*ydry(j)
		        call Area_from_H(j,b2_min(j),A2_min(j),Ts,RH,0)		      
		        
	          call Pressure_Pho(j,b2_max(j),P_pho_max(j),0) !0 for free surface flow
	          call Pressure_Pho(j,b2_min(j),P_pho_min(j),0) !0 for free surface flow
		        		        		        
		        call Area_from_H(j,ydry(j),Adry(j),Ts,RH,0)
		        Celer_dry(j) = sqrt(g*Adry(j)/Ts) !Celerity for dry conditions		
		        
		        !Modify Phi For ellipse
		        call Phi1(j,ydry(j),phi_dry(j)) !Phi for dry conditions		
		        		        
		        Vmin = sqrt(g*Adry(j)/Ts)
		        Qmin(j) = 5.d-1*Adry(j)*Vmin	
		        
		        !CFL for dry bed
		        ydry_CFL(j) = ydry(j)
		        call Area_from_H(j,ydry_CFL(j),Adry_CFL(j),Ts,RH,0)
		        !Reference depth
		        Yref(j) = yfree_press*2d0*b
		        call Phi1(j,Yref(j),Phiref(j))
		        
		        call Area_from_H(j,Yref(j),Aref(j),Ts,RH,0)		        
		        celer_ref(j) = sqrt(g*Aref(j)/Ts)		        		
		        call Pressure_Pho(j,Yref(j),P_pho,0) !0 for free surface flow
		        P_pho_ref(j) = P_pho		        
		        haver_ref(j) = P_pho/(Aref(j)*g)
		        call Pressure_Pho(j,ydry(j),P_pho,0) !0 for free surface flow
		        P_pho_dry(j) = P_pho	
		        fluxdry(j) = P_pho
		        
		        !To make sure that Volume_reconR_dry is larger than Volume dry   		        
		        y_downs = y_reconR_dry
                  Call Volume_sloped(j,y_downs,Vol) !this is minimum
                  Area_reconR_dry = Vol/dx(j)
                  if (Area_reconR_dry <= Adry(j))then
                      Area_reconR_dry = Adry(j)                    
                  !write(98,*),'Decrease minimum # of cells in GUI OR increase the'
	            !write(98,*),' "Min_Slope_for_Sloped_pipe" in Subr. INIT, OR'
	            !write(98,*),' "decrease the value of y_dry, in Subr. INIT'	      
	            !write(99,*),'Decrease min # of cells in GUI OR increase the'
	            !write(99,*),'"Min_Slope_for_Sloped_pipe" in Subr. INIT, OR'
	            !write(99,*),'"decrease the value of y_dry, in Subr. INIT'
	            !call endprog
	          endif                
	      else
    		        b2_max(j) = (1d0-1.d-12)*d(j)  !maximum water depth for open channel (for searches)
		        call Area_from_H(j,b2_max(j),A2_max(j),Ts,RH,0)	!maximum area for open channel     
		        
    		        !Full area
		        Area_full(j) = PI/4d0*d(j)*d(j)		
		        !dry bed 
		        !ydry(j) = max(1.d-3, 1d0/200d0*dmin) !Max (1mm, 1/200 dmin)
		        
		        !y_reconR_dry is added to make it homogeneous with steeper slopes
		        ydry(j) = 1d0/dry_diameter_fraction*dmin !Max (1mm, 1/200 dmin)
		        b2_min(j) = 1.d-1*ydry(j)
		        
		        call Area_from_H(j,b2_min(j),A2_min(j),Ts,RH,0)		      
	            call Pressure_Pho(j,b2_max(j),P_pho_max(j),0) !0 for free surface flow
	            call Pressure_Pho(j,b2_min(j),P_pho_min(j),0) !0 for free surface flow
		       
		        call Area_from_H(j,ydry(j),Adry(j),Ts,RH,0)
		        Celer_dry(j) = sqrt(g*Adry(j)/Ts) !Celerity for dry conditions		
		        call Phi1(j,ydry(j),phi_dry(j)) !Phi for dry conditions		
		        Vmin = sqrt(g*Adry(j)/Ts)
		        Qmin(j) = 5.d-1*Adry(j)*Vmin	
		        	        
		        !CFL for dry bed
		        ydry_CFL(j) = ydry(j)
		        teta = 2d0*ACOS(1d0-2d0*ydry_CFL(j)/d(j))
		        	
		        Adry_CFL(j) = 1d0/8d0*(teta-SIN(teta))*d(j)*d(j)
		        		        		        	
		        !Reference depth
		        Yref(j) = yfree_press*d(j)
		        call Phi1(j,Yref(j),Phiref(j))		        
		        teta = 2d0*ACOS(1d0-2d0*Yref(j)/d(j))					
		        Aref(j) = 1d0/8d0*(teta-SIN(teta))*d(j)*d(j)
		        call Area_from_H(j,Yref(j),A,Ts,RH,0)
		        celer_ref(j) = sqrt(g*A/Ts)		
		        call Pressure_Pho(j,Yref(j),P_pho,0) !0 for free surface flow
		        P_pho_ref(j) = P_pho	
		        haver_ref(j) = P_pho/(Aref(j)*g)
		        call Pressure_Pho(j,ydry(j),P_pho,0) !0 for free surface flow
		        P_pho_dry(j) = P_pho
		        fluxdry(j) = 0d0 !P_pho
	      endif	
	      y_for_phi_max(j) = (1d0-1.d-2)*d(j)
	      call Phi1(j,y_for_phi_max(j),phi_max(j)) !Phi_max for iteration
	      y_for_phi_min = b2_min(j)
	      call Phi1(j,y_for_phi_min,phi_min(j)) !Phi_max for iteration
	enddo		
	
		!call endprog
      ID_Number_of_zero_drops(:,:) = 2
	do R =1,Nnodes
		!write(99,*),max_elev_crown (R),max_crown_pipe(R)
		Ares_junct(R) = Adrop(R)
		height_jun(R) = hdrops_overf(R)	
		crown_elev_max = -1000d0
		max_dia = -1d0
		drop_min = 10000000d0	
          Number_of_zero_drops(R) = 0

		do j=1,NodeNS(R)
			L3 = Node(R,j)
			if(Nodetype(R,j) == 1)then !inflowing
				Drop(R,j) = zb(L3,2)-junct_elev(R)
			elseif(Nodetype(R,j) == 2)then !outflowing		
				Drop(R,j) = zb(L3,1)-junct_elev(R)				
			else 
				write(98,*),'Nodetype .ne. 1,2' 
				write(98,*),'Subroutine Init2_1'
				write(99,*),'Nodetype .ne. 1,2' 
				write(99,*),'Subroutine Init2_1'
				call endprog
				GLOBAL_STATUS_FLAG = 1
				return 
              endif		
              
              !Number_of_zero_drops = Number of zero drops 
              if (dabs(Drop(R,j)) < 0.02*d(j))then
                  Number_of_zero_drops(R) =  Number_of_zero_drops(R) +1
                  ID_Number_of_zero_drops(R,j) = 1 !1 means that the drop is zero, 2 means that the drop is not flat. 
              endif  
              
			if (Drop(R,j) < 0d0 .and. BCnode(R) .ne. 30)then
			    call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
				write(98,*),'Drop height must be always positive except' 
				write(98,*),'for a rating curve boundary'
				write(98,*),'Error in input data. Subr. INIT' 				
				write(98,*),'Node = ',trim(temp_id)
				write(99,*),'Drop height must be always positive except' 
				write(99,*),'for a rating curve boundary'
				write(99,*),'Error in input data. Subr. INIT' 				
				write(99,*),'Node = ',trim(temp_id)
				call endprog
			endif
			if (Drop(R,j)+(1d0 + Tol_int_10_8)*Yref(L3) > 
     &			crown_elev_max)then
				crown_elev_max = Drop(R,j)+(1d0 + Tol_int_10_8)*Yref(L3)
				pipe_max_crown = L3
			endif
			max_dia = max(max_dia,d(L3)) 
			if(drop_min > Drop(R,j))then
				drop_min = Drop(R,j)
				!pipe_min_point = Node(R,j)
				!smin = S0(pipe_min_point)
              endif
		enddo
		dropmin(R) = drop_min 
          
		Areser_min(R) =  PI*max_dia*max_dia/4d0
		sum_drop = 0
		do j=1,NodeNS(R)
			L3 = Node(R,j)			
			area = 0.01*PI*max_dia*max_dia/4d0	
			if (BCnode(R) == 4	.or. BCnode(R) == 7)then	
		        if (itm_has_inflow(R) == 1 .and. Adrop(R) < area)then
				    if(sum_drop < 1)then
					    call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes					
                     write(98,*),'There is an inflow hydrograph at node 
     &				(SWMM) =',trim(temp_id), 'but the area at this node is zero.'	
				    write(98,*),'Add an area for the pond at this drophaft'
				    write(99,*),'There is an inflow hydrograph at node 
     &				(SWMM) =',trim(temp_id), 'but the area at this node is zero.'
     	           write(99,*),'Add an area for the pond at this drophaft'
				        call endprog
				    endif
				    sum_drop = sum_drop+1
				    sum_drop2 = sum_drop2+1
		        endif		        
              endif              
          enddo
          
		max_elev_crown (R) = crown_elev_max
		max_crown_pipe(R) = pipe_max_crown	
		!write(99,*),'R,max_elev_crown (R)',R,max_elev_crown (R)
      enddo      
      
      do R =1,Nnodes	              
          if (BCnode(R) == 4.or. BCnode(R)==7)then
              if (Ares_junct(R) >= 10000.5)then !(100^2)  If Area is larger use a reservoir boundary
                  call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
                  write(98,*),'Area of node ',temp_id
                  write(98,*),'is larger than 10^4.'
                  write(98,*),'Use a reservoir boundary for this node'  
                  write(99,*),'Area of node ',temp_id
                  write(99,*),'is larger than 10^4.'
                  write(99,*),'Use a reservoir boundary for this node'  
                  call endprog    
              endif
              if (open_closed_bound(R) == 0)then 
		            !To make sure that the pond area at the ventilated junction is not zero or extremely small
                      area = 0.01*PI*max_dia*max_dia/4d0	
		            if (Ares_junct(R) < area)then  			           
		                call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
	 write(98,*),'Node =',trim(temp_id), 'is ventilated but the area of
     & dropsh. is zero or near-zero. Increase the area'
       write(99,*),'Node =',trim(temp_id), 'is ventilated but the area of
     & dropsh. is zero or near-zero. Increase the area'
				    call endprog
				    endif
              elseif(open_closed_bound(R) == 1)then 
                  if (Ares_junct(R) >= Areser_min(R))then  
                      !If Ares_junct(R) > Areser_min(R), system can get depressurized from the dropshaft
                      open_closed_bound(R) = 0 !if dropshaft area is not small, system can get depressurized from the dropshaft
                      call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
      write(99,1025),'Warning: Node =',
     & trim(temp_id),'is NOT ventilated but 
     & the area of the dropshat is significant, hence it
     & will allow depressurization from dropshaft'
 1025   format(A15,A10,A100) 
 1030   format(A15,A10,A30)   
   
      write(99,1030),'Warning: Node =',
     & trim(temp_id), 'was changed to ventilated node'
                  endif 
                  if (Ares_junct(R) < Areser_min(R))then  
		                Ares_junct(R) = Areser_min(R) !open_closed_bound(R) is maintaned equal to 1, but area is kept at minimum
                  endif
              else
		            call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
		            write(98,*),'Problem in node = ',trim(temp_id)
				    write(98,*),'open_closed_bound is unknown (neither "0" or 1)' 
				    write(99,*),'Problem in node = ',trim(temp_id)
				    write(99,*),'open_closed_bound is unknown (neither "0" or 1)' 
				    call endprog
              endif
          endif
      enddo
	
	!Assigning BCNode = 41 to gates that connects to one pipe only		
	do R =1,Nnodes	      
			if (BCnode(R) == 40)then !gate boundary
	          If (NodeNS(R) == 1)then
	              BCnode(R) = 41
	          endif
            endif
            write(99,*),'R,BCnode(R),NodeNS(R)',R,BCnode(R),NodeNS(R)
      enddo
      	
	
	do R =1,Nnodes	      
			If(BCnode(R) == 30)then !Rating curve
			    L3 = Node(R,1)	
			    if(Drop(R,1) >= Tol_int_10_4 .or. Drop(R,1) < -yref(L3))then
			        call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
			        write(98,*),'There is a problem with the crest elevation'
				    write(98,*),'of the rating curve node. The crest elevation '
				   write(98,*),'must be between the invert and the crown of the '
				   write(98,*),'connecting pipe. Error in input data. Subr. INIT' 
				   write(98,*),'Problem in node = ',trim(temp_id)
				   write(99,*),'There is a problem with the crest elevation'
				   write(99,*),'of the rating curve node. The crest elevation '
				   write(99,*),'must be between the invert and the crown of the '
				   write(99,*),'connecting pipe. Error in input data. Subr. INIT' 
				   write(99,*),'Problem in node = ',trim(temp_id)				   
				   call endprog
			    endif	
			elseif (BCnode(R) == 40)then !gate boundary
		        if (itm_has_inflow(R) == 1)then 
		            call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
		            write(98,*),'A gate boundary cannot have a hydrograph'
		            write(98,*),'Check gate at node ',trim(temp_id)
		            write(99,*),'A gate boundary cannot have a hydrograph'
		            write(99,*),'Check gate at node ',trim(temp_id)
				    call endprog
		        elseif (NodeNS(R) .ne. 2) then !Number of pipes connected to each node
		            call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
		            write(98,*),'A gate boundary must have one or two' 
		            write(98,*),'pipes connected to the boundary' 
		            write(98,*),'Check gate at node ',trim(temp_id)
		            write(99,*),'A gate boundary must have one or two' 
		            write(99,*),'pipes connected to the boundary' 		            
		            write(99,*),'Check gate at node ',trim(temp_id)
		            call endprog
		        else 
		            call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
		            temp1 = d(NodeID(R,1))-d(NodeID(R,2))
		            k = NodeID(R,1)
		            if (abs(temp1) > 0.01*d(k))then
			            write(98,*), 'Pipes connected to gate at node',
     &                 trim(temp_id)
			            write(98,*), 'must have the same diameter'
			            write(99,*), 'Pipes connected to gate at node',
     &                 trim(temp_id)
			            write(99,*), 'must have the same diameter'
			            call endprog
		            endif
		            
		            if (abs(Drop(R,1)) > 1.d-5*d(k) .or. 
     &		            abs(Drop(R,2)) > 1.d-5*d(k))then
			            write(98,*),'The elevations of the inverts of the
     &                  pipes that connect a gate boundary'
                        write(98,*),'must be the same as that of the 
     &                  invert of the gate. This means that'
                        write(98,*),'pipe offsets at the gate must be
     &                  zero. Check gate at node ',trim(temp_id)
                 
                        write(99,*),'The elevations of the inverts of 
     &                  the pipes that connect a gate boundary'
                        write(99,*),'must be the same as that of the 
     &                  invert of the gate. This means that'
                        write(99,*),'pipe offsets at the gate must be
     &                  zero. Check gate at node ',trim(temp_id)
			            call endprog
		            endif       
		        endif
		    endif						
	enddo
	if (sum_drop2 > 0)then !To stop if there are no areas at dropshafts				
		write(98,*),'Add dropshaft areas for nodes listed above' 
		write(98,*),'Nodes are based on SWMM notation'
		write(99,*),'Add dropshaft areas for nodes listed above' 
		write(99,*),'Nodes are based on SWMM notation'
		call endprog
	endif
	
	      
		
	!Crown of converging pipes at every node MUST be perfectly aligned or
	!have a discontinuity in elevation of at least 5% of the largest pipe diameter 
	do R =1,Nnodes	
		if(R==7)then
			do j=1,NodeNS(R)-1
				L3 = Node(R,j)
				do k=1,NodeNS(R)
					L4 = Node(R,k)
					call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
					if(L3 .ne. L4)then
						dh1 = Drop(R,j)+Yref(L3)
						dh2 = Drop(R,k)+Yref(L4)
						diamax = max(d(L3),d(L4))
						if(abs(dh1-dh2)>Tol_int_10_8*diamax .and. 
     &						abs(dh1-dh2)<0.05*diamax)then
					write(99,*),'Warning. Crowns of converging pipes 
     &					are not aligned'
                    write(98,*),'Warning. Crowns of converging pipes 
     &					are not aligned' 
    ! 					write(99,*),'aligned or have a 
    ! &					discontinuity in elevation of at least 5% '
    !					write(99,*),'of the largest pipe diameter'
					write(99,*),'Node',trim(temp_id)					
					write(98,*),'Node',trim(temp_id)					
					!call endprog
						endif
					endif
				enddo
			enddo
		endif		
	enddo
	
	!slopes	
	sum_drop = 0
	
	do j=1,NR	
		    !To make sure that cell size is not too big
		    s_temp = max(S0(j),1.d-7)		
		    dx1max = 100d0
		    dx2max = Yref(j)/(8d0*s_temp) !To avoid to touch the wall downstream 
		    !dx2max = 1.5d0*ydry(j)/s_temp !1.5 to compensate the difference in area at the sides of the symmetric line 
		    dx3max = min(dx1max,dx2max)
		    if (Dx(j) >= dx3max)then
			    sum_drop = sum_drop +1			
			    temp_id = ''
			    call itm_get_swmm_id(1, j, temp_id) ! 1 for pipes
			    write(98,*),'dx(cell size) in pipe (SWMM)',trim(temp_id),
     &            'is too large'
			    write(98,*),'max. cell size in this pipe should be at most',
     &			dx3max,'m'
		        write(98,*),'current cell size in this pipe is',
     &			dx(j),'m'
                write(99,*),'dx(cell size) in pipe (SWMM)',
     &            trim(temp_id),
     &            'is too large'
			    write(99,*),'max. cell size in this pipe should be at most',
     &			dx3max,'m'
		        write(99,*),'current cell size in this pipe is',
     &			dx(j),'m'					
		    endif
	enddo
	
	!It is divided by 10 because y_dry has to be much smaller
	!than y_reconR_dry to be able to track wet-dry interfaces 
	
	if (sum_drop > 0)then				
		write(98,*),'Increase the miminum number of grids in the user'
		write(98,*),'interface according with the recommendations'
		write(98,*),'indicated above'
		write(99,*),'Increase the miminum number of grids in the user'
		write(99,*),'interface according with the recommendations'
		write(99,*),'indicated above'
		call endprog
	endif
	
	!To make sure that boundary junctions have two or more pipes and that dropshafts
	!and reservoirs have only one pipe 
	do R=1,Nnodes
		If(BCnode(R) == 7)then
			if (Nnod(R) < 2)then
			    call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
				write(98,*),'Node',trim(temp_id),'can not be a junction boundary'
				write(98,*),'Node',trim(temp_id),'has',Nnod(R),'pipes'
				write(98,*),'Node',trim(temp_id),'has less than 2 pipes'
				write(99,*),'Node',trim(temp_id),'can not be a junction boundary'
				write(99,*),'Node',trim(temp_id),'has',Nnod(R),'pipes'
				write(99,*),'Node',trim(temp_id),'has less than 2 pipes'
				call endprog
			endif
		endif
	
		!To differentiate between a general node and the one that has two pipes with the same diameter and no inflows 
		If(BCnode(R) == 7)then !junction for pipes >= 2
              If(Nnod(R) == 2)then  !If Number of nodes = 2
                  dr = abs(Drop(R,1)-Drop(R,2))
				j_1 = NodeID(R,1); j_2 = NodeID(R,2)
				diam_differ = abs(d(j_1)-d(j_2))                   
                  If (itm_has_inflow(R) ==1)then !there is inflow
                      temp_id = ''
			        call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
			        write(99,*), 'reach for junction_general',R,temp_id 
                  else  !there is NO inflow
                      If(diam_differ < 0.02*d(j_1) .and. 
     &			        dr < 0.02*d(j_1))then
                          BCnode(R) = 24  !To this type of boundary (Two pipes with no inflow)
                          !we assign internally the ID of 24.	    					
					    temp_id = ''
				        call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
				        write(99,*), 'reach for junct2pipes_same_diam',R,temp_id
                      else
				        temp_id = ''
				        call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
				        write(99,*), 'reach for junction_general',R,temp_id  
                      endif    
                  endif 
		    else
			    temp_id = ''
			    call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
			    write(99,*), 'reach for junction_general',R,trim(temp_id)  	    			
		    endif					   
		endif		
!		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
				
		If(BCnode(R) == 4)then !Dropshaft boundary
			if (Nnod(R) > 1)then
			    call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
				write(98,*),'Node',trim(temp_id),'can not be a dropshaft boundary'
				write(98,*),'Node',trim(temp_id),'has',Nnod(R),'pipes'
				write(98,*),'Node',trim(temp_id),'has more than one pipe'
				write(99,*),'Node',trim(temp_id),'can not be a dropshaft boundary'
				write(99,*),'Node',trim(temp_id),'has',Nnod(R),'pipes'
				write(99,*),'Node',trim(temp_id),'has more than one pipe'
				call endprog
			endif
		endif
		
		
		If(BCnode(R) == 10 .or. BCnode(R) == 11)then !Constant boundary
		    write(99,*),'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
		    write(99,*),'WARNING!.WARNING!WARNING!WARNING!WARNING!'
		    write(99,*),'Current version of ITM doesnot support'
			write(99,*),'dry-wet interfaces for Constant boundaries (Constant'
			write(99,*),'Flow and Constant water depth). If your scenario may'
			write(99,*),'have these features, use the dropshaft(junction)'
			write(99,*),' boundary using a small horizontal area for'
			write(99,*),'the dropshaft'	  
			write(99,*),'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
			write(98,*),'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
		    write(98,*),'WARNING!.WARNING!WARNING!WARNING!WARNING!'
		    write(98,*),'Current version of ITM doesnot support'
			write(98,*),'dry-wet interfaces for Constant boundaries (Constant'
			write(98,*),'Flow and Constant water depth). If your scenario may'
			write(98,*),'have these features, use the dropshaft(junction)'
			write(98,*),' boundary using a small horizontal area for'
			write(98,*),'the dropshaft'	      
	      write(98,*),'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
			
			if (Nnod(R) > 1)then
			    call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
	    write(98,*),'Node',trim(temp_id),'has more than one pipe ',
     &     'connected to BC'
				write(98,*),'Node',trim(temp_id),'can not be a constant boundary'
				write(98,*),'Node',trim(temp_id),'has',Nnod(R),'pipes'	
				write(99,*),'Node',trim(temp_id),'has more than one pipe ',
     &      'connected to BC'
				write(99,*),'Node',trim(temp_id),'can not be a constant boundary'
				write(99,*),'Node',trim(temp_id),'has',Nnod(R),'pipes'			
				call endprog
			endif
		endif
		
		If(BCnode(R) == 30)then !Rating curve
			if (Nnod(R) > 1)then
			    call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
			  write(98,*),'Node',trim(temp_id),'has more than one pipe ',
     &        ' to BC'
				write(98,*),'Node',trim(temp_id),'can not be a rating curve ',
     &        'boundary'
				write(98,*),'Node',trim(temp_id),'has',Nnod(R),'pipes'
				write(99,*),'Node',trim(temp_id),'has more than one pipe ',
     &        'connected to BC'
				write(99,*),'Node',trim(temp_id),'can not be a rating curve ',
     &        'boundary'
				write(99,*),'Node',temp_id,'has',Nnod(R),'pipes'						
				call endprog
			endif
		endif
		
		If(BCnode(R) == 20)then
			!To check that Reservoir BC is connected to only one pipe
			!Later I will add more pipes (Arturo Leon)
			if (Nnod(R) > 1)then
			    call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
				write(98,*),'Node',trim(temp_id),'can not be a reservoir boundary'
				write(98,*),'Node',trim(temp_id),'has',Nnod(R),'pipes'
				write(98,*),'Node',trim(temp_id),'has more than one pipe'
				write(99,*),'Node',trim(temp_id),'can not be a reservoir boundary'
				write(99,*),'Node',trim(temp_id),'has',Nnod(R),'pipes'
				write(99,*),'Node',trim(temp_id),'has more than one pipe'
				call endprog
			endif

			!To check that the maximum reservoir depth is not zero
			L3 = NodeID(R,1)

			!call itm_get_max_curve_val_x(R, VALUE)
			call itm_get_max_curve_val_x(R, value_reser)			
			reser_maxdepth(R)  = value_reser
			write(99,*),'R,reser_maxdepth(R)',R, reser_maxdepth(R)		
			
			if(reser_maxdepth(R) <= 0.01*d(L3))then
				write(98,*),'Reservoir located at node',R,'has a maximum 
     &				reservoir depth equal to zero'
				write(98,*),'Please modify the value of the maximum
     &				 reservoir depth'
                write(99,*),'Reservoir located at node',R,'has a maximum 
     &				reservoir depth equal to zero'
				write(99,*),'Please modify the value of the maximum
     &				 reservoir depth'
				call endprog
			endif
		endif
	enddo	
		
	!Initial water depth for reservoirs and junctions	
	do R=1,Nnodes		
		If(BCnode(R) == 20.or.BCnode(R) == 7.or.BCnode(R) == 4 
     &		.or.BCnode(R) == 40)then !reservoirs and junctions						
			do j=1,NodeNS(R)
				if(Drop(R,j) == dropmin(R))then
					L3 = NodeID(R,j)
					goto 200
				endif

				if(j == NodeNS(R))then
					write(98,*), 'Drop(R,j) .ne. dropmin'
					write(98,*), 'Subr. INIT'
					write(99,*), 'Drop(R,j) .ne. dropmin'
					write(99,*), 'Subr. INIT'
					call endprog
				endif
              enddo
              
              !Reservoirs and dropshaft can not have drops
200           If(BCnode(R) == 4 .or. BCnode(R) == 20)then
                  if (abs(dropmin(R)) > 0.01*d(L3))then
                      call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes				
       write(98,*),'dropshaft boundaries cannot have
     & drops.'
       write(98,*),'If a drop is needed at these boundaries, use a 
     & new pipe' 
       write(98,*),'to connect the original pipe with the bottom of the' 
       write(98,*),'dropshaft'
       write(98,*),'The drop occurs at node ',temp_id				    
				    call endprog 
                  endif                  
              endif
              
              !ydropmin(R) = 1.5*ydry(L3) !before it was 3*ydry 
              
              !If(BCnode(R) == 20)then !Reservoir boundary
              !    ydropmin(R) = ydry(L3) !before it was 3*ydry  
              !else
                  ydropmin(R) = 1.5*ydry(L3) + dropmin(R) !before it was 3*ydry   
              !endif
			
			if (ini_cond == 1)then  !When constant water depth is used as initial condition
				!water_init_elevation = -10d0
				yres_jun_old(R) = water_init_elevation 
     &				- junct_elev(R)
			else
				yres_jun_old(R) = flowdepth_res(R)  !Later we can add this
				!write (99,*),'flow_depth_reser 1',flowdepth_res(R)
              endif            
              
			!if flow depth is smaller than the minimum elevation of the pipe invert
			!that connects wit the node, the initial water elevation is assumed to be at 
			!the elevation of this minimum invert
			if (yres_jun_old(R) < ydropmin(R))then
				yres_jun_old(R) = ydropmin(R)
				!write(99,*), 'yres_jun_old(R)',R,yres_jun_old(R)
			endif	
		endif		
	enddo
     
	!Determining the maximum and minimum critical and normal flow at the inflow 
	!that can be conveyed in gravity flow regime
	do j=1,NR 
              ycrit_max(j) = min(yref(j),0.80*d(j))
		    call Area_from_H(j,ycrit_max(j),A,Ts,RH,0)
		    Qcrit_maxIA(j) = A*sqrt(g*A/Ts)
              temp1 = 1d0/(2d0*g*A**2d0)*Qcrit_maxIA(j)**2d0
              Ecrit_max(j) = ycrit_max(j) + temp1 
              
              Qnor_maxIA(j) = 1d0/nm(j)*A*RH**(2d0/3d0)*sqrt(S0(j))              
		    ycrit_min(j) = Ycrit(j,Qmin(j))
    					
		    !Minimum conjugate depths (d1 and d2)				
		    call Area_from_H(j,ycrit_min(j),A,Ts,RH,0)
		    ScIA = ((Qmin(j)*nm(j))/(A*RH**(2d0/3d0)))**2d0
		    if (S0(j) > 0.001*ScIA)then !flow may be supercritical
			    d1_min(j) = Ynormal(j,Qmin(j))
			    d2_min(j) = Yconjugate(j,d1_min(j),Qmin(j))			    
		    endif
	enddo
	!Increasing number of cells by 4 (second-order)
	do j=1,NR 	
		    Nx(j) = Nx(j) + 4			
		    if (Nx(j) > nxmax-1) then
			    write(99,*)'INIT - too many cells in the model'
			    write(99,*)'Change Nxmax in "param1.inc" and recompile'
			    GLOBAL_STATUS_FLAG = 1
			    return
		    endif
	enddo

	Num_max_cells = maxval(Nx(:))

      !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
	!Initial conditions
	!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
	do j=1,NR		
			discharge = Init_disch(j)				
			Q0(j,:) = discharge
			if (Init_depth_type(j) == 1) then !constant
				h0(j,:) = Init_depth(j)				
			elseif (Init_depth_type(j) == 2)then !critical
				!critical depth	
				h0(j,:)  = Ycrit(j,discharge)				
			elseif (Init_depth_type(j) == 3)then  !normal								
				if (S0(j) < 1.d-7)then
				    call itm_get_swmm_id(1, j, temp_id) ! 1 for pipes
					write(99,*)'Normal depth can not be computed'	
					write(99,*)'Negative slope was found in pipe',trim(temp_id)
					GLOBAL_STATUS_FLAG = 1
					return
				endif
				h0(j,:) = Ynormal(j,discharge) 				
			else				
				write(99,*)'Type of initial flow depth'
				write(99,*)'not recognized. Subroutine INIT'
				GLOBAL_STATUS_FLAG = 1
				return
			endif	
	enddo	
	
      !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
	!This is only an internal boundary condition used for academic purposes.
	!This should be frozen when released to the public
	!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%			
	goto 215  !This always should be ON unless ITM is being used for academic purposes
	!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	j = 1
	water_init_elevation = 20d0
	discharge = 0d0
	Q0(j,:) = discharge
	R = Node1(j)
	do k=1,NodeNS(R)
	      L3 = NodeID(R,k)
	      if(L3 == j)then 
	          do i = 3,Nx(j)-2	            
	            	        h0_Rec(j,i) = water_init_elevation - 
     &                       junct_elev(R) - Drop(R,k) + 
     &                      (i-25d-1)*s0(j)*dx(j)
					        if (S0(j) > Min_Slope_for_Sloped_pipe)then
					            If (h0_Rec(j,i) <= yref(j))then
                                    IdFlow_temp = 0
                                else
                                    IdFlow_temp = 1
                                endif
					            If (IdFlow_temp == 0)then
                                    y_downs=h0_Rec(j,i)+5d-1*s0(j)*dx(j)
                                    Call Volume_sloped(j,y_downs,Vol)
	                              A0(j,i) = Vol/dx(j)           
	                              call H_from_Area(j,A0(j,i),h0(j,i),
     &	                                19,IdFlow_temp)
                                else
                                    h0(j,i) = h0_Rec(j,i)
                                endif
					        else
					            h0(j,i) = h0_Rec(j,i)
					        endif					    
		        enddo
	      endif
	 enddo
	 
	j = 2
	water_init_elevation = 3d0
	discharge = 0d0
	Q0(j,:) = discharge
	R = Node1(j)
	do k=1,NodeNS(R)
	      L3 = NodeID(R,k)
	      if(L3 == j)then 
	          do i = 3,Nx(j)-2	            
	            	        h0_Rec(j,i) = water_init_elevation - 
     &                       junct_elev(R) - Drop(R,k) + 
     &                      (i-25d-1)*s0(j)*dx(j)
					        if (S0(j) > Min_Slope_for_Sloped_pipe)then
					            If (h0_Rec(j,i) <= yref(j))then
                                    IdFlow_temp = 0
                                else
                                    IdFlow_temp = 1
                                endif
					            If (IdFlow_temp == 0)then
                                    y_downs=h0_Rec(j,i)+5d-1*s0(j)*dx(j)
                                    Call Volume_sloped(j,y_downs,Vol)
	                              A0(j,i) = Vol/dx(j)           
	                              call H_from_Area(j,A0(j,i),h0(j,i),
     &	                                19,IdFlow_temp)
                                else
                                    h0(j,i) = h0_Rec(j,i)
                                endif
					        else
					            h0(j,i) = h0_Rec(j,i)
					        endif					    
		        enddo
	      endif
	 enddo
		
	!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
	!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
	!Initial conditions when initial water depth is constant
	!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
215	if (ini_cond == 1)then 
		    do j=1,NR		
			    discharge = 0d0
			    Q0(j,:) = discharge
			    R = Node1(j)
			    do k=1,NodeNS(R)
				    L3 = NodeID(R,k)
				    if(L3 == j)then
					    do i = 3,Nx(j)-2
					        h0_Rec(j,i) = water_init_elevation - 
     &                       junct_elev(R) - Drop(R,k) + 
     &                      (i-25d-1)*s0(j)*dx(j)
					        if (S0(j) > Min_Slope_for_Sloped_pipe)then
					            If (h0_Rec(j,i) <= yref(j))then
                                    IdFlow_temp = 0
                                else
                                    IdFlow_temp = 1
                                endif
					            If (IdFlow_temp == 0)then
                                    y_downs=h0_Rec(j,i)+5d-1*s0(j)*dx(j)
                                    Call Volume_sloped(j,y_downs,Vol)
	                              A0(j,i) = Vol/dx(j)           
	                              call H_from_Area(j,A0(j,i),h0(j,i),
     &	                                19,IdFlow_temp) 
                                else
                                    h0(j,i) = h0_Rec(j,i)
                                endif
					        else
					            h0(j,i) = h0_Rec(j,i)
					        endif					    
					    enddo
				    endif
			    enddo
		    enddo
	endif
	
	
	!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
	!To determine if the flow is free surface or pressurized	
	!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
	do j=1,NR 
	  dz = abs(S0(j)*dx(j)) 			
		do i = 3,Nx(j)-2			
			if (h0(j,i) < ydry(j))then
				h0(j,i) = ydry(j)
				A0(j,i) = Adry(j)
				Q0(j,i) = 0d0
				IdFlow(j,i) = 0 !free surface flow
			elseif (h0(j,i) <= yref(j))then
		        IdFlow(j,i) = 0 !free surface flow			        	        		        
		        if (h0(j,i)-dz/2d0 <= ydry(j))then
		            h0(j,i) = ydry(j)
				    A0(j,i) = Adry(j)
				    Q0(j,i) = 0d0
			    else				    
		            call Area_from_H(j,h0(j,i),area,TH,RH,0)	
				    A0(j,i) = area            
				endif
			elseif (h0(j,i) > yref(j))then
				IdFlow(j,i) = 1 !pressurized flow
				!Area is computed below according with pc1
			else
				write(98,*),'IdFlow(j,i) .ne. 0,1 in Subr. INIT_101'
				write(99,*),'IdFlow(j,i) .ne. 0,1 in Subr. INIT_101'
				call endprog
			endif
		enddo				
	enddo
	
	!Areas: A_cell_dry_sloped and 	A_open_sloped_pipe(j)
	do j=1,NR 
	      y_downs = abs(S0(j)*dx(j))
	      Call Volume_sloped(j,y_downs,Vol)
	      A_cell_dry_sloped(j) = Vol/dx(j)	      
	      y_downs = yref(j)
	      Call Volume_sloped(j,y_downs,Vol)
	      A_open_sloped_pipe(j) = Vol/dx(j)
	enddo
	
	!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
	!To compute the water depth for variable reconstruction 
	!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
	do j=1,NR 	  
		    do i = 3,Nx(j)-2
			    if (S0(j) > Min_Slope_for_Sloped_pipe)then
		            call H_reconst_sloped(j,A0(j,i),h0_Rec(j,i),
     &		        10201,IdFlow(j,i)) 
		        else
		            h0_Rec(j,i) = h0(j,i)		        	       
		        endif		    
		    enddo				
	enddo	
	
			
	!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
	!Computing initial pressure wave
	!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
	do j=1,NR 
		fully_pressuri(j) = 2		
		do i = 3,Nx(j)-2			
			if (fully_pressuri(j) == 2)then !fully-pressurized conditions
				if(IDFlow(j,i) == 0)then
					fully_pressuri(j) = 1 !mixed flow conditions
					write(99,*),'System is in mixed flow conditions'
					!write(98,*),'System is in mixed flow conditions'	              
				endif
			endif			
		enddo	
	
		!Pressurized flow condition will occur only when the water depth in 
		!the dropsh. over the pipe inverts are higher than 1.5*Yref
		if (fully_pressuri(j) == 2)then !fully-pressurized conditions
			R1 = Node1(j); R2 = Node2(j)		
			Drop1 = zb(j,1)-junct_elev(R1)
			Drop2 = zb(j,2)-junct_elev(R2)
			if(R1 == 4 .or. R1 == 7 .or. R1 == 20)then
			    if (yres_jun_old(R1) < (1d0+Tol_int_10_8)*Yref(j) + Drop1)then 
				    fully_pressuri(j) = 1 !mixed flow conditions
			    endif
		    endif
		    if(R2 == 4 .or. R2 == 7 .or. R2 == 20)then
			    if (yres_jun_old(R2) < (1d0+Tol_int_10_8)*Yref(j) + Drop2)then    
				    fully_pressuri(j) = 1 !mixed flow conditions
			    endif
		    endif
		endif
	enddo	

	pressurized = 2 !(2: pressurized; 1: mixed)
	do j = 1,NR	
		If (pressurized == 2)then					
			If (fully_pressuri(j) == 1)then
				pressurized = 1
			endif
		endif		
	enddo	
	!Type_of_flow = 1: free surface, 2: mixed flow, 3: pressurized flow

	if (type_of_flow == 3)then
		if (pressurized .ne. 2)then
			write(98,*),'Type of flow specified on user interface is 
     &		pressurized, however the initial conditions do not meet the 
     &        criteria of pressurized flows.'
			write(98,*),'If flow is not pressurized, please change the
     &			type of flow (to Free surface or Mixed flow)
     &			on the user interface'
			write(98,*),'If depressurization is being simulated (flow is 
     &			initially pressurized but gradually becomes free surface flow),
     &			the type of flow specified must be "MIXED FLOW"'
            write(98,*),'Type of flow = 3'
            write(99,*),'Type of flow specified on user interface is 
     &		pressurized, however the initial conditions do not meet the 
     &        criteria of pressurized flows.'
			write(99,*),'If flow is not pressurized, please change the
     &			type of flow (to Free surface or Mixed flow)
     &			on the user interface'
			write(99,*),'If depressurization is being simulated (flow is 
     &			initially pressurized but gradually becomes free surface flow),
     &			the type of flow specified must be "MIXED FLOW"'
            write(99,*),'Type of flow = 3'
			call endprog
		endif
	endif

	!pc_mixed = max(pc_mixed,1.5*maxval(celer_ref(:)))
	!Pressure wave celerity to use
	if (pressurized == 2)then
		pc1(:) = pc_press
		temcel = 0.2*minval(Dx(:))/pc_press	
		fully_pressuri(:) = 2	
	else
		pc1(:) = pc_mixed
		temcel = 0.2*minval(Dx(:))/pc_mixed		
		fully_pressuri(:) = 1	
	endif

	!For defining Ypressurized the pc for mixed flows must be used
	do j=1,NR
		y_for_pressur(j) = (1d0 + 0.01)*Yref(j)  !It was 0.001
		Area_for_pressur(j) = Aref(j) + 
     &		g*Aref(j)*(y_for_pressur(j)-Yref(j))/(pc1(j)*pc1(j)) !this is very important
	enddo

	!To determine if the flow is free surface or pressurized
	do j=1,NR 			
		do i = 3,Nx(j)-2
			if (IdFlow(j,i) == 1)then
				A0(j,i) = Aref(j)+g*Aref(j)*(h0(j,i)-yref(j))/
     &				(pc1(j)*pc1(j))
			endif						
		enddo				
	enddo
	
	 
	      
	!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
	!To achieve second-order accurate boundary condition 
	!to be used in subrout. Junction
	!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
	do j=1,NR 
	  Dt = 0d0  !This is only for initial conditions and DT is not important
	  call ERP_combined(j,3,Dt,ybound(j,1),Abound(j,1),Qbound(j,1),
     &	    Idflow_bound(j,1))
	  call ERP_combined(j,Nx(j)-2,Dt,ybound(j,2),Abound(j,2),Qbound(j,2),
     &      Idflow_bound(j,2))		
		Idflow_bound_reser(j,1) = Idflow(j,3)
		Idflow_bound_reser(j,2) = Idflow(j,Nx(j)-2)
	enddo
	
	!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
	!Initial volume stored in tunnels, dropshafts and reservoirs
	!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	sum_temp1 = 0d0
	do j = 1,NR
	      z0(j,2) = zb(j,1) + 5d-1*S0(j)*dx(j)
	      z0(j,Nx(j)-1) = zb(j,2) - 5d-1*S0(j)*dx(j)
		    do i = 3,Nx(j)-2
		        z0(j,i) = zb(j,1)-(i-25d-1)*S0(j)*dx(j) !elevation of cell (mid-way)		
			    if(IdFlow(j,i) == 0)then
				    Area = A0(j,i)
				    if(Area > Aref(j))then
					    Area = Aref(j)
				    endif
				    sum_temp1 = sum_temp1+Area*dx(j)
			    elseif(IdFlow(j,i) == 1)then
				    sum_temp1 = sum_temp1+Area_full(j)*dx(j)
			    else
			        write(99,*), 'IdFlow(j,i) .ne. 0,1.Subr. INIT_102' 
			        write(98,*), 'IdFlow(j,i) .ne. 0,1.Subr. INIT_102' 
			        Call endprog
			    endif	
			    
			    !Writing initial conditions (Water stage and flow discharge)
                !write(99,*),'i,h+z(Water stage)', i, h0_Rec(j,i)+z0(j,i)
                !write(99,*),'i,Q', i, Q0(j,i)
		    enddo
	enddo	
		
	sum_temp2 = 0d0
	sum_temp3 = 0d0
      Vol_inflows = 0d0
	do R=1,Nnodes
		write(99,*),'Node, BCnode',R,BCnode(R)
		!Volume at dropshafts and junctions		
		If(BCnode(R) == 7.or.BCnode(R) == 4)then !junctions (no junction with two pipes of same diameter) and dropsh.
			sum_temp2 = sum_temp2 + Ares_junct(R)*yres_jun_old(R)	
		endif
		!Volume at reservoirs
		If(BCnode(R) == 20)then
			call get_storage(R,yres_jun_old(R),Stora_new)
			sum_temp3 = sum_temp3 + Stora_new	
          endif
		
		If(BCnode(R) == 30)then
	      p1 = abs(Drop(R,1)) !Drop height 
            j = NodeID(R,1) !Pipe Id of weir
            call Area_from_H(j,p1,area,TH,RH,0)
            area_weir(R) =  area             
            
	      call get_Q_from_rat_curve(R,p1,Qb)
	      if(abs(Qb) > Qmin(1)) then 
	        temp_id = ''
		      call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes		      
	        write(98,*),'There is a rating curve at node  ', trim(temp_id),
     &         ','
		      write(98,*),'which has a weir of depth of p1 =', p1,'m, '
		      write(98,*),'but, get_Q_from_rat_curve(R,p1,Qb) gives'
		      write(98,*),'a flow discharge (Qb [m3/s]) > 0 for water depths'
		      write(98,*),'smaller than p1. Qb [m3/s] =',Qb
		      write(98,*),'Check rating curve at node',trim(temp_id)
		      write(99,*),'There is a rating curve at node  ', trim(temp_id),
     &         ','
		      write(99,*),'which has a weir of depth of p1 =', p1,'m, '
		      write(99,*),'but, get_Q_from_rat_curve(R,p1,Qb) gives'
		      write(99,*),'a flow discharge (Qb [m3/s])>0 for water depths'
		      write(99,*),'smaller than p1. Qb [m3/s] =',Qb
		      write(99,*),'Check rating curve at node',trim(temp_id)
		      call endprog	
	      endif
	      !!Maximum flow specified at the rating curve. This is used to check if the water level in the rating curve is exceeded. 
	      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	      temp100 = 10.**14.
	      call get_Q_from_rat_curve(R,temp100,Qb)	      
            Max_flow_rating_curve(R) = abs(Qb)
            call itm_get_max_rating_head(R,Max_Head_rating_curve(R))            
	  endif			
      enddo
	
      If(BCnode(R) == 30)then !Rating curve 
              j = NodeID(R,1)
              If (Max_Head_rating_curve(R) <= yref(j))then
                  temp_id = ''
                  call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes	
                  write(98,*),'No enough data in rat.curve.Node',
     &             trim(temp_id)
                  write(98,*),'Please extend rating curve'
                  call endprog	
              endif
      endif

	Initial_volume_stored = sum_temp1 + sum_temp2 + sum_temp3
      Vol_stored_old_time = Initial_volume_stored
	Vol_entered_system = Initial_volume_stored !Vol_entered_system 
      Vol_entered_system = Initial_volume_stored !Vol_entered_system 
	!temp100 = 0d0	
	!call itm_conser_volume(T_GLOBAL,temp100)
	write(99,1005),'t (s)=',T_GLOBAL,' Initial_volume_stored (M3)=',
     &		 Vol_entered_system
      write(99,1005),'t (s)=',T_GLOBAL,' Initial volume lost (M3)=',
     &		 Vol_lost_system
      
      !&	' Vol. stored (M3) =',Volume_stored_current_step,
      !&	' Vol. outflow (M3) = ',Vol_lost_system,
      !&	' Error vol (%) = ',Error_volume

      
      !To determine if the boundary is open channel or pressurized      
      do R=1,Nnodes
            call Boundary_Open_Press(R)
      enddo	 
      
      
      !write(99,*),'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$' 
      !write(99,*),'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$' 
      !write(99,*),'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$' 
      !write(99,*),'t, reserv. depth2', T_global, yres_jun_old(18)
      !write(99,*),'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$' 
      !write(99,*),'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$' 
      write(99,*),'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$1' 
      
	!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
	!Determination of the initial time step
	!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
      DtMax = Min(DtMax1,Tstor,Tmax)
 980	format(I2,3f12.6)
 990	format(I2,2f12.6)
 994	format(A20,1000I20)
 995	format(3000f12.6)
 996	format(A20,1000A20) 
1005  FORMAT (A10,F10.2,A20,ES13.4,A20,ES13.4,A20,ES13.4,A20,F5.1) 
1010	format(A10,A10,A50)
1015	format(A50)
      return
      end
      
      
      