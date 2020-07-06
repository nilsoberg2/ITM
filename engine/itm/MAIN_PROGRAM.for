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


cDEC$ IF DEFINED (ITM_USE_DLL)
cDEC$ ELSE

      program MAINPROGRAM
	!This routine is the main module of the ITM model. 
      use common_module
	USE DFLIB
	USE DFPORT  
	implicit none 
	    

	character(1024) input_file, report_file, output_file, itm_file
	integer itm_status
	integer num_args

	num_args = iargc()

	if (num_args < 4) then
		write(98,*) 'Must pass input, report, and output files to program.'
		call endprog
	endif

	call getarg(1, input_file)
	call getarg(2, report_file)
	call getarg(3, output_file)
	call getarg(4, itm_file)

	ITM_DLL_INIT(input_file, report_file, output_file, itm_file,
     &             'debug.txt')
		
	call CPU_TIME (time_begin)
      !Loop on time				
      do while (T_GLOBAL.lt.TMAX)
		ITM_EXEC_STEP
      enddo		  
	!CPU results

	ITM_DLL_END

      end program
cDEC$ ENDIF




	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	function ITM_DLL_INIT(input_file, report_file, output_file,
     &                      itm_file, debug_file,
     &                      error_file, input_dir,
     &					    input_file_len, report_file_len, output_file_len,
     &						itm_file_len, debug_file_len,
     &						error_file_len, input_dir_len)

cDEC$ IF DEFINED (ITM_USE_DLL)
cDEC$ ATTRIBUTES DLLEXPORT :: ITM_DLL_INIT
cDEC$ ATTRIBUTES STDCALL :: ITM_DLL_INIT
cDEC$ ATTRIBUTES REFERENCE :: input_file, report_file, output_file
cDEC$ ATTRIBUTES REFERENCE :: itm_file, debug_file
cDEC$ ATTRIBUTES REFERENCE :: error_file, input_dir
cDEC$ ENDIF

      use common_module
	USE DFLIB
	implicit none 

	integer ITM_DLL_INIT
	INTEGER(2) control, holdcontrol, newcontrol
	integer input_file_len, report_file_len, output_file_len
	integer itm_file_len, debug_file_len, input_dir_len
	integer error_file_len, vol_err_file_len
	character (len=input_file_len) :: input_file
	character (len=report_file_len) :: report_file
	character (len=output_file_len) :: output_file
	character (len=itm_file_len) :: itm_file
	character (len=debug_file_len) :: debug_file
	character (len=error_file_len) :: error_file
	character (len=input_dir_len) :: input_dir
	character (len=1024) :: tecplot_file1
	character (len=1024) :: tecplot_file2
	character (len=1024) :: tecplot_file3
	character (len=1024) :: tecplot_file4
	character (len=1024) :: tecplot_file15
	integer itm_status
	integer itm_open_output_file

	!debug_file = 'debug.txt'
	tecplot_file1 = trim(input_dir) // '\Dambreak1.dat'
	tecplot_file2 = trim(input_dir) // '\depth.dat'
	tecplot_file3 = trim(input_dir) // '\flow_discharge.dat'
	tecplot_file4 = trim(input_dir) // '\energy.dat'
	tecplot_file15 = trim(input_dir) // '\vol_overflows.dat'

      open(99,file=debug_file,status='REPLACE')
      open(98,file=error_file,status='REPLACE')
	!open(101,file=tecplot_file15)
	!open(12,file=tecplot_file1,status='unknown')	
      !open(20,file=tecplot_file2)
      !open(30,file=tecplot_file3)
	!open(40,file=tecplot_file4)

	GLOBAL_STATUS_FLAG = 0

	CALL GETCONTROLFPQQ(control)
	 ! Clear any existing precision flags.
	holdcontrol = (control .AND. (.NOT. FPCW$MCW_PC))
	newcontrol = holdcontrol .OR. FPCW$64
	! Set precision to 64 bits.
	CALL SETCONTROLFPQQ(newcontrol)

	T_GLOBAL = 0
	DT_GLOBAL = 0
	temp9 = 0
	param_ener = 0	

      !Initialization: read data from file      
      call init(trim(input_file), trim(report_file), trim(output_file))
      
	
	! reset it in case it got read from the hotstart file
	T_GLOBAL = 0

	if (GLOBAL_STATUS_FLAG .NE. 0) then
		!TODO: write error message
		write(98,*) 'Error in GLOBAL_STATUS_FLAG. Routine Main_program'
		call endprog
		call deallocateALL
		return
	endif
	
	if (itm_open_output_file(trim(itm_file)) .ne. 0) then
		!TODO: write error message		
		write(98,*) 'Error in itm_open_output_file. Routine Main_program'
		call endprog
		call deallocateALL
		return
	endif

	call CPU_TIME(TIME_BEGIN)
	ITM_DLL_INIT = 0
		
      end function



	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	function ITM_EXEC_STEP(CurTime)

cDEC$ IF DEFINED (ITM_USE_DLL)
cDEC$ ATTRIBUTES DLLEXPORT :: ITM_EXEC_STEP
cDEC$ ATTRIBUTES STDCALL :: ITM_EXEC_STEP
cDEC$ ATTRIBUTES REFERENCE :: CurTime
cDEC$ ENDIF

      use common_module
	USE DFLIB
	implicit none 
	
	double precision curTime
	integer ITM_EXEC_STEP
	integer i,j,k,ss,R1,R2,sumID,CODL,Idf0,R,IDfBound_cell      
      integer Node_upst,Node_down,jj,kk
	double precision AL,AR,hL,hR,QL,QR,Wpred
      double precision Dt_Courant,FF1,FF2
	double precision temp1,temp2,h0b1,A0b1,Q0b1,Ts,RH
	double precision Aw0,Qw0,Pw0,Drop1,Drop2,m9,m10,m11,m12
      double precision y_temp,dr,TsIA,R_temp,Drop_junct
     
	
 40	if (GLOBAL_STATUS_FLAG .ne. 0) then
		ITM_EXEC_STEP = GLOBAL_STATUS_FLAG
		return
	endif	

 	number_steps = number_steps + 1		
	!Time step: Courant criteria      
      
      
	call CFL_time_step(T_GLOBAL,DT_GLOBAL,Dt_Courant)				
	DT_GLOBAL = Dt_Courant !time step for Courant criteria		

	! Determine if we should save the results at this point
	istor = 0											
	!temp1 = (T_GLOBAL+DT_GLOBAL)/Tstor								
	!temp2 = T_GLOBAL/Tstor	
	
	!write(99,50),'t, dt = ',T_GLOBAL,DT_GLOBAL
	if ((T_GLOBAL+DT_GLOBAL) > T_NEXT_REPORT) then
		istor = 1
		T_NEXT_REPORT = T_NEXT_REPORT + Tstor
		write(99,50),'t, dt = ',T_GLOBAL,DT_GLOBAL
	endif
	!if (int(temp1) > int(temp2)) istor = 1			
	!istor = 1
	!if (T_GLOBAL<3180) then !solely to check errors 
	!	istor = 0 					
	!else							
	!	istor = 1
	!endif 		
	
50	FORMAT (A10, F22.4,F11.4)

	!If Relax_condit == 2, no computations are performed until there are inflows
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!	
	!Relax_condit = 2 !Assigned on purpose. Delete later
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	if (Relax_condit == 2)then 
		write(99,*),'we are not computing. Relax_condit == 2. Subr. Main'
		goto 140
	endif

     
	!Flux computation at boundaries			
	!call air_pocket(DT_GLOBAL)
	
	CODE_STAR_REGION = 0  !0 Variables at star region will NOT be computed,1 will be computed
	                      !It is not necessary to use CODE_STAR_REGION = 1 here because all required variables at 
	                      !boundary are computed an retrieved below
	call boundaries(T_GLOBAL,DT_GLOBAL)
	
	!To determine if the boundary is open channel or pressurized      
      do R=1,Nnodes
            call Boundary_Open_Press(R)
	enddo	       
	       
	!Solve Riemann problem for all type of interfaces	
	!Variables reconstruction	
	!MUSCL-HANCOCK Method
	do j = 1,NR
	  !Solve Riemann problem 
	  CODE_STAR_REGION = 1    !Variables at star region ARE computed
		ALLOCATE (F(Nx(j),2))
		ALLOCATE (P_pho_interf(Nx(j)))
		ALLOCATE (y_interf(Nx(j)))
		ALLOCATE (A_interf(Nx(j)))
		ALLOCATE (Atemp1(Nx(j)+4),htemp1(Nx(j)+4),Qtemp1(Nx(j)+4),
     &			IdFlow1(Nx(j)+4))       
        
		do i = 3,Nx(j)-2 							
			call ERP_combined(j,i,DT_GLOBAL,h0b1,A0b1,Q0b1,IDfBound_cell)
		enddo		     
        !write(99,*),'boundary,3,FF1,FF2',Fupst(j,1),Fupst(j,2)      
		do i = 4,Nx(j)-2		    
			sumID = IdFlow_REC_L(i) + IdFlow_REC_R(i-1)	
			hL = h0R(i-1); hR = h0L(i)
			AL = A0R(i-1); AR = A0L(i)
			QL = Q0R(i-1); QR = Q0L(i)
			
			if(sumID == 0)then		
				call Riemann_open(j,i,hL,hR,QL,QR,FF1,FF2,Aw0,Qw0,Pw0)
				!write(99,*),'j,i,FF1,FF2',j,i,FF1,FF2
			else if (sumID == 2)then																	
				call Riemann_pressur(j,i,AL,AR,QL,QR,FF1,FF2,Aw0,Qw0,Pw0)	
			else if (sumID == 1)then	
				if(IdFlow_REC_R(i-1)==0)then 
					CODL = 0; !#left state is open channel flow
					call Riemann_pressur_open(j,i,hL,hR,QL,
     &				QR,CODL,Aw0,Qw0,Idf0,FF1,FF2,Wpred,Pw0)
				else 
					CODL = 1 !#left state is pressurized flow
					call Riemann_pressur_open(j,i,hL,hR,QL,
     &				QR,CODL,Aw0,Qw0,Idf0,FF1,FF2,Wpred,Pw0)													
				endif
			else
				write(98,*),'sumID .ne. 0,1,2'
				write(98,*),'routine Main_program' 
				call endprog
			endif	
			
			!computation of fluxes 
			F(i,1) = FF1	 								
			F(i,2) = FF2
		enddo
		!write(99,*),'boundary,Nx-2,FF1,FF2',Fdownst(j,1),Fdownst(j,2)
		CODE_STAR_REGION = 0 !0, Var. at star region will NOT be computed, 1 will be computed
	    !Fluxes at boundaries	
	    F(3,1) = Fupst(j,1)
	    F(3,2) = Fupst(j,2)	
	     F(Nx(j)-1,1) = Fdownst(j,1)
	    F(Nx(j)-1,2) = Fdownst(j,2)
	
	    !Variables at boundaries	
	    P_pho_interf(3) = Pres_pho_Bound(j,1) !Pressure_Pho at interfaces
	    y_interf(3) = ybound(j,1) !Water depth
	    A_interf(3) = Abound(j,1) !Hydraulic area
	    P_pho_interf(Nx(j)-1) = Pres_pho_Bound(j,2)
	    y_interf(Nx(j)-1) = ybound(j,2)
	    A_interf(Nx(j)-1) = Abound(j,2)
	  
		!update variables at end of time step
          Node_upst = Node1(j)
          Node_down = Node2(j)
		do i = 3,Nx(j)-2  
                  Atemp1(i)= A0(j,i) + DT_GLOBAL/Dx(j)*(F(i,1)-F(i+1,1))			
                  Qtemp1(i)= Q0(j,i) + DT_GLOBAL/Dx(j)*(F(i,2)-F(i+1,2))               
              !if (i==3 .or. i == Nx(j)-2)then
                  
                   !if (j == 1 .and. i==3)then
                       !if (BCnode(Node_upst) == 7)then
              !write(99,9999),'Flux:pipe j,i,F1,f2',j,i,F(i,1)*DT_GLOBAL !,F(i,2)
                      !endif
                   !endif
                   
                   !if (i==Nx(j)-2)then              
                          !if (BCnode(Node_down) == 7)then
             ! write(99,9999),'Flux:pipe j,i,F1,f2',j,i,F(i,1)*DT_GLOBAL !,F(i,2)
                          !endif
                   !endif
                   
                  !write(99,*),
              
              !write(99,9999),'Flux:pipe j,i,F1,f2',j,i,F(i+1,1),F(i+1,2)
 9999             FORMAT (A30,I6,I6,2F14.3)
			!endif
			
              
            If(ISNAN(Atemp1(i)) .or. ISNAN(Qtemp1(i)))then  
                  write(98,9999),'NaN is found in rout. Main_program12'                  
                  write(98,9999),'Atemp1(i)',Atemp1(i)
                  write(98,9999),'Qtemp1(i)',Qtemp1(i)     
                  write(99,9999),'NaN is found in rout. Main_program12'                  
                  write(99,9999),'Atemp1(i)',Atemp1(i)
                  write(99,9999),'i,j',i,j
                   write(99,9999),'t',Atemp1(i)
                  write(99,9999),'Flux:j,i,F1,f2',j,i,F(i,1),F(i+1,1)
                  write(99,9999),'Flux:j,i+1,F1,f2',j,i,F(i,2),F(i+1,2)
                      write(99,9999),'A0',A0(j,i)
                      write(99,9999),'Q0',Q0(j,i)
                   
                   
                  call endprog  
        !          if (IdFlow(j,i) == 0)then
        !              Atemp1(i) =Adry(j)
        !              Qtemp1(i) = 0d0
        !          else 
        !              write(98,*),'This occurs before source term'
				    !write(98,*),'A = ',Atemp1(i),'pipe No',j,'cell = ',i
				    !write(98,*),'Q = ',Qtemp1(i),'pipe No',j,'cell = ',i
				    !write(98,*),'NaN is found in routine Main_program1' 
        !          write(98,9999),'Flux:j,i,F1,f2',j,i,F(i,1),F(i+1,1)
        !          write(98,9999),'Flux:j,i+1,F1,f2',j,i,F(i,2),F(i+1,2)
        !              write(98,9999),'A0',A0(j,i)
        !              write(98,9999),'Q0',Q0(j,i)
				    !call endprog  
        !          endif
              endif
		enddo
		
		!update flow type and incorporate source term to the solution		
		call Source_term(j,DT_GLOBAL) 
		fully_pressuri(j) = 2
		do i = 3,Nx(j)-2						
			A0(j,i) = Atemp1(i); Q0(j,i) = Qtemp1(i)
			h0(j,i) = htemp1(i)
              
              !flow depths at boundaries can not be negative 
              
       !           if (i == 3 .or. i == Nx(j)-2)then
	      !              if (h0(j,i) < 0d0)then
       !                       if (i == 3)then
       !                           ss = Node1(j) !Upstream node                              
       !                           if (BCnode(ss) == 4)then  !dropshafts	
       !                               Drop_junct = zb(j,1)-junct_elev(ss)
       !                               Q0(j,i) = 0d0                      
       !               h0(j,i) = yres_jun_old(ss) - Drop_junct
       !               call Area_from_H(j,h0(j,i),A0(j,i),Ts,RH,IdFlow1(i))
       !                           endif
       !               
       !                       elseif (i == Nx(j)-2)then                          
       !                           ss = Node2(j) !downstream node 
       !                           if (BCnode(ss) == 4)then  !dropshafts	
       !                               Drop_junct = zb(j,2)-junct_elev(ss)
       !                               Q0(j,i) = 0d0 
       !               h0(j,i) = yres_jun_old(ss) - Drop_junct
       !               call Area_from_H(j,h0(j,i),A0(j,i),Ts,RH,IdFlow1(i))
       !                           endif
       !                       else
       !                           write(98,*),'i .ne. 3, Nx(j)-2'				       
			    !	            write(99,*),'i .ne. 3, Nx(j)-2'		
       !                           write(98,*),'Error in Main_program'
			    !	            call endprog  
       !                       endif
       !                   endif
       !               endif
       !           
	      !
	  !      if (j == 1 .and. i == 3)then
	  !        if (t_global >11100.0 .and. t_global<11150)then
	  !    !        write(99,*),'j = 15'
      !                   write(99,*),'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
      !                   write(99,*),'t,Aref',t_global,Aref(j)                      
		 !           write(99,*),'t,i,Q',t_global,i,Q0(j,i)
      !                   write(99,*),'t,i,A',t_global,i,A0(j,i)
			!        write(99,*),'t,i,h',t_global,i,h0(j,i)                      
			!        !write(99,*),'t,i,v',t_global,i,Qtemp1(i)/Atemp1(i)
			!        !write(99,*),'A_S,j,i,Qtemp1(i)',j,i,Qtemp1(i) 
			!    endif
			!endif			
			!write(99,*),'A_S,j,i,Atemp1(i)',j,i,Atemp1(i)
			!write(99,*),'A_S,j,i,Qtemp1(i)',j,i,Qtemp1(i) 
		    If(ISNAN(Qtemp1(i)))then 
		        write(98,*),'This occurs after source term'
				write(98,*),'Q = ',Qtemp1(i),'pipe No',j,'cell = ',i
				write(98,*),'NaN is found in routine Main_program1' 
				call endprog
			endif
						
			!write(99,*),'t,i,hrec+z',i,h0_Rec(j,i)+z0(j,i)		
			
			If (ISNAN(Atemp1(i)) .or. ISNAN(Qtemp1(i)) .or. ISNAN(htemp1(i)))then
				write(98,*),'NaN is found in Main_program1:','pipe No',j,'cell=',i
				call endprog
			endif			
			IDFlow(j,i) = IdFlow1(i)
			if (fully_pressuri(j) == 2)then !fully-pressurized conditions
				if (IDFlow(j,i) == 0)then
					fully_pressuri(j) = 1 !mixed flow conditions
				endif
			endif			
		enddo
		DEALLOCATE (Atemp1,htemp1,Qtemp1,IdFlow1,F)
		DEALLOCATE (P_pho_interf,y_interf,A_interf)
						
		if (fully_pressuri(j) == 2)then !fully-pressurized conditions
			if (Idflow_bound_reser(j,1) == 0 .or. 
     &			Idflow_bound_reser(j,2) == 0)then
				fully_pressuri(j) = 1 !mixed flow conditions
			endif
		endif
	
		!Pressurized flow condition will occur only when the water depth in 
		!the dropsh. over the pipe inverts are higher than 1.5*Yref. 
		!This doesn't apply for routine junct2pipes_same_diam.
		if (fully_pressuri(j) == 2)then !fully-pressurized conditions
			R1 = Node1(j); R2 = Node2(j)
			!No junction of pipes same diameter
			If(BCnode(R1) .ne. 24 .or. BCnode(R2) .ne. 24)then 
				Drop1 = zb(j,1)-junct_elev(R1)
				Drop2 = zb(j,2)-junct_elev(R2)
				if (yres_jun_old(R1) < 1.5*Yref(j) + Drop1 .or. 
     &			yres_jun_old(R2) < 1.5*Yref(j) + Drop2)then
					fully_pressuri(j) = 1 !mixed flow conditions
				endif
			endif
		endif
	enddo
	
      

	If (type_of_flow == 3)then
		type_of_flow = -1 !This was added intentionally
	!to overcome errors
	endif


	!If type of flow = 3 (fully-pressurized) we don't need to check depressurization
	!depressurization is checked only when type_of_flow .ne. 3
	If (type_of_flow == 3)then
		pressurized = 2
		goto 135
	endif 

	!If one pipe is simulated as fully pressurized, all the pipes that 
	!surround this pipe must also be pressurized
	do j = 1,NR				
		If (fully_pressuri(j) == 2)then
			R1 = Node1(j); R2 = Node2(j)
			do i = 1,Nnod(R1) 
				k = NodeID(R1,i) 
				if (fully_pressuri(k) == 1)then !mixed flow conditions
					fully_pressuri(j) = 1
					goto 120
				endif
			enddo
			do i = 1,Nnod(R2) 
				k = NodeID(R2,i) 
				if (fully_pressuri(k) == 1)then 
					fully_pressuri(j) = 1
					goto 120
				endif
			enddo			
		endif		
 120		continue
	enddo

	pressurized = 2 !(2: pressurized; 1: mixed)
	pressurized = 1 !(1: temporarilly added)
	!something is wrong when using pressurized = 2 and type_of_flow = 3
	!please check that

	do j = 1,NR	
		If (pressurized == 2)then					
			If (fully_pressuri(j) == 1)then
				pressurized = 1
				goto 135
			endif
		endif		
	enddo
	
	!Pressure wave celerity to use
 135	if (pressurized == 1)then !(2: pressurized; 1: mixed)
		pc1(:) = pc_mixed	
	elseif (pressurized == 2)then !(2: pressurized; 1: mixed)
		type_of_flow = 3  !added
		
		write(99,*),'Type of flow  = 3. Subr. Main program.'
		!If depressurization is allowed, type_of_flow must 
		!be changed to type_of_flow = 2 (mixed flow)
		pc1(:) = pc_press
		pressurized = 1  !delete this. It was added
	else
		write(98,*), 'Main Program'
		call endprog
      endif

      
      
      !Check cons. of volume
         call itm_conser_volume(T_GLOBAL,DT_GLOBAL)
         ! write(99,1005),'t (s)=',T_GLOBAL,' Vol. inflow (M3)=',
         !&		 Vol_inflows,
         !&	' Vol. stored (M3) =',Volume_stored_current_step,
         !&	' Vol. outflow (M3) = ',Vol_lost_system,
         !&	' Error vol (%) = ',Error_volume    
      
	!Plotting
140   if ((Istor == 1) .or. T_GLOBAL+DT_GLOBAL>=TMAX) then   
!140	if (T_GLOBAL < TMAX) then   
		!T_GLOBAL=TMAX was added only to compare to MOC
        write(99,141),'T, DT, No_convergence #',T_GLOBAL,DT_GLOBAL,
     &     sum_no_converg
 141    FORMAT (A25, 2F11.4, I20)
      !Maximum values
      !m9 = MAXVAL(Q0(1,:))       	
      !m12 = MINVAL(Q0(1,:))       	
      !m10 = MAXVAL(A0(1,:))       	
      !m11 = MAXVAL(h0(1,:))       	
		   ! write(99,*),'Qmax,Qmin,A, h', m9,m12 !,m10,m11
      !enddo
 
		!Check cons. of volume
        write(99,1004),'t(s)=',T_GLOBAL,
     &    ' Inflow total (M3)=',Vol_inflows,
     &	' Vol. stored at T (M3) =',Volume_stored_current_step,
     &	' Outflow total (M3) = ',Vol_lost_system,
     &	' Error vol (%) = ',Error_volume    
        write(99,1005),'t(s)=',T_GLOBAL,
     &	' Dropsh. at Delta T (M3)=',vol_dropshafts_time_step,
     &	' Junctions at Delta T (M3) =',vol_junctions_time_step,
     &	' Reservoirs at Delta T (M3) = ',vol_reservoirs_time_step,
     &    ' Pipes at Delta T (M3) = ',vol_pipes_time_step
      write(99,1006),'t(s)=',T_GLOBAL,
     &	' inflows at Delta T (M3)=',vol_inflows_time_step,
     &	' delt_storage_N+1-N(M3) =',Volume_stored_current_step 
     &	 - Vol_stored_old_time,
     &	' outflow at Delta T (M3) =',vol_lost_time_step,
     &	' vol_balance at Delta T (M3) =',balance_volume_time_step
     
1004  FORMAT (A10,F10.2,A30,F16.2,A30,F16.2,A30,F16.2,A30,F8.2)
1005  FORMAT (A10,F10.2,A30,F16.2,A30,F16.2,A30,F16.2,A30,F16.2)
1006  FORMAT (A10,F10.2,A30,F16.2,A30,F16.2,A30,F16.2,A30,F16.2)
          If (ISNAN(Error_volume))then 
			write(98,*),'NaN is found. Check data, computations' 
			write(98,*),'or contact Arturo Leon at artuleon@gmail.com'
			write(98,*),'Subr. Main program'
			call endprog
		elseif(abs(Error_volume) > 8d0)then			
              if (abs(Error_volume) > 50d0 .and. 
     &                abs(Error_volume) > 0.1*NR*Length(1)*Aref(1).and. 
     &            t_global > Tmax/10d0)then
			    write(98,*),'Fatal error. Error in volume exceeds 20%. '
     			    write(98,*),'Error in input data or in computations'
			    write(98,*),'Subr. Main program'
			    call endprog
			else
			    write(99,*),'Warning : Error in the volume exceeds 8 %'		
			endif			
		endif
		! Report results to SWMM/ITM
		call report_all(T_GLOBAL + DT_GLOBAL)		
		temp9 = temp9+1	   
      endif
      !Assign new storage as old storage 
      Vol_stored_old_time = Volume_stored_current_step      
      
	! Do the SWMM engine step thing (reports stuff to SWMM)
	call itm_step

      
	!Update time 
	T_GLOBAL = T_GLOBAL + DT_GLOBAL

	if (T_GLOBAL >= TMAX) then
		CurTime = 0d0
	else
		CurTime = T_GLOBAL
	endif
1200  format(200I2)
      
      if (GLOBAL_STATUS_FLAG .eq. 911) then
        CurTime = 0
        GLOBAL_STATUS_FLAG = 0
      endif
      
	ITM_EXEC_STEP = GLOBAL_STATUS_FLAG
	
	end function


	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	function ITM_DLL_END()

cDEC$ IF DEFINED (ITM_USE_DLL)
cDEC$ ATTRIBUTES DLLEXPORT :: ITM_DLL_END
cDEC$ ATTRIBUTES STDCALL :: ITM_DLL_END
cDEC$ ENDIF

	integer ITM_DLL_END
	integer itm_status
	integer itm_close_output_file
	
	call itm_end
	call itm_close(itm_status)
	
	itm_status = itm_close_output_file()
	
 	CALL CPU_TIME (time_end)
	!write(99,*),'Time of comput. was',time_end-time_begin,' seconds'
      !End of simulation: close files
	call deallocateALL
995	format(500I2)
998	format(4F20.2)

	close(99) ! debug file
	close(98)
      !close(12)
	close(20)
      close(30)
	close(40)

	ITM_DLL_END = 0
	
      end function



	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	function ITM_GET_MASS_BAL_ERROR(continuityError)

cDEC$ IF DEFINED (ITM_USE_DLL)
cDEC$ ATTRIBUTES DLLEXPORT :: ITM_GET_MASS_BAL_ERROR
cDEC$ ATTRIBUTES STDCALL :: ITM_GET_MASS_BAL_ERROR
cDEC$ ATTRIBUTES REFERENCE :: continuityError
cDEC$ ENDIF

      use common_module
	implicit none 
	
	double precision continuityError
	integer ITM_GET_MASS_BAL_ERROR
      
      continuityError = Error_Volume
      
      ITM_GET_MASS_BAL_ERROR = 0
      
      end function ITM_GET_MASS_BAL_ERROR

!!!!!!
!!!!!!	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!	integer function ITM_MAKE_HOTSTART()
!!!!!!	
!!!!!!cDEC$ IF DEFINED (ITM_USE_DLL)
!!!!!!cDEC$ ATTRIBUTES DLLEXPORT :: ITM_MAKE_HOTSTART
!!!!!!cDEC$ ATTRIBUTES STDCALL :: ITM_MAKE_HOTSTART
!!!!!!cDEC$ ENDIF
!!!!!!
!!!!!!      use common_module
!!!!!!
!!!!!!	integer s
!!!!!!
!!!!!!	ITM_MAKE_HOTSTART = make_hotstart()
!!!!!!	
!!!!!!	end function
!!!!!!
!!!!!!
!!!!!!
!!!!!!	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!	function ITM_GET_NUM_HOTSTART()
!!!!!!
!!!!!!cDEC$ IF DEFINED (ITM_USE_DLL)
!!!!!!cDEC$ ATTRIBUTES DLLEXPORT :: ITM_GET_NUM_HOTSTART
!!!!!!cDEC$ ATTRIBUTES STDCALL :: ITM_GET_NUM_HOTSTART
!!!!!!cDEC$ ENDIF
!!!!!!
!!!!!!      use common_module
!!!!!!
!!!!!!	integer ITM_GET_NUM_HOTSTART
!!!!!!
!!!!!!	ITM_GET_NUM_HOTSTART = NUM_HOTSTART
!!!!!!
!!!!!!	end function
