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

      Subroutine CFL_time_step (t,Dt,Dt_Courant)
	!This routine computes the time step according with the Courant criteria.
	!This routine is also used for relaxing the time step when there are no inflows.
      use common_module
	implicit none	    
      integer i,j,k,R,Relax_time_step
	double precision t,YY,Dt,Dt_Courant,Lambda,lambda1,lambda2
	double precision Ts,Area,Area1,Area2,RH,Phi,Cr,Crini,Qinf_new
	double precision Cr_open,Cr_mixed,Cr_pressur,Cr_dry,temcel
	double precision vel_cour,vel_max,vel
 5	Dt = Min(Tmax-t,DtMax)
 
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !Relax_condit = 1 !We are using this only for the well-balanced paper. Delete after paper is completed
      !Relax_time_step = 1	!We are using this only for the well-balanced paper. Delete after paper is completed
      !goto 10   !We are using this only for the well-balanced paper. Delete after paper is completed
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !Courant numbers
      Cr_pressur = 0.30 !Courant number for pressurized flow
      Cr_open = 0.30 !Courant number for open channel flow
	Cr_mixed = 0.30 !Courant number for mixed flow				
	Crini = 0.01 !Courant number for initial time steps	
      
	!To limit the value of DT when there are inflows
	vel_cour = 0.0	
	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	!This should be 2. Change to 1 for actual applications
	Relax_time_step = 1 !1 (NO), 2 (YES) [To relax time step] 
	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	do R=1,Nnodes
		k = NodeID(r,1)
		!Inflow hydrographs	
		call get_inflow(R,t+DT,Qinf_new)		
		if (Qinf_new > Qmin(k))then			
			vel_cour = 3d0
			Relax_time_step = 1	
			Relax_condit = 1		
			goto 10
		endif
	enddo
		
	if (Relax_condit == 1)then  !Do this only if at the previous time 
	!there was inflow or vel> 0.02 (2cm/s)
		do j = 1,NR	
			do i = 3,Nx(j)-2	
				vel = Q0(j,i)/A0(j,i)
				if (vel > 0.01)then
					Relax_condit = 1
					Relax_time_step = 1	
					goto 10
				endif
			enddo
		enddo
	endif

	if (Relax_time_step == 2)then		
		Relax_condit = 2  
		write(99,*),'Relax_time_step == 2. Solution is relaxed.'		
		goto 20
	endif
      
	
10	do j = 1,NR			
		if 	(fully_pressuri(j) == 2)then			
			Cr = Cr_pressur
			lambda = pc1(j) !This is a good approximation given that Cr_pressur doesn't exceed 0.5
			Dt = Min(Dt,Cr*Dx(j)/lambda)		
		else													
			do i = 3,Nx(j)-2
				Area = A0(j,i)
				vel = dabs(Q0(j,i)/A0(j,i))
				vel_max = max(vel_cour,vel)			
				!Cr to be used			
				if(Idflow(j,i) == 0)then  !Free surface flow
					yy = h0(j,i)
					call Area_from_H(j,YY,Area,Ts,RH,Idflow(j,i))					
					if(h0(j,i) > 0.70*yref(j))then					
						Cr = Cr_mixed  !mixed flow						
						lambda = vel_max + 
     &						max(sqrt(g*Area/Ts),pc_mixed)
					else
						Cr = Cr_open  !open channel flow											
						lambda = vel_max +
     &					sqrt(g*Area/Ts)
					endif
					! The first steps must have a low courant number because high
					! courant numbers can produce unreliable
					!estimates of the wave speed. 
					!For example use CFL = 0.2 for 20 time steps 
					if(number_steps < 200)then
						Cr = Crini
						if(number_steps == 1)then													
							if (type_of_flow == 3)then !Pressurized flow
								temcel = 0.3*Dx(j)/pc_press		
							else								
								temcel = 0.3*Dx(j)/pc_mixed		
							endif
							Dt = Min(Dt,temcel)
						endif		
					endif
     					Dt = Min(Dt,Cr*Dx(j)/lambda)		
				elseif(Idflow(j,i) == 1)then  !Pressurized flow
					Cr = Cr_mixed  !Press. side of mixed flow
					lambda = pc1(j) !This is a good approximation given that Cr_pressur doesn't exceed 0.5 						
					Dt = Min(Dt,Cr*Dx(j)/lambda)
				else						
					write(98,*),'Idflow(j,i) .ne. 0,1'
					write(98,*),'Idflow(j,i)',Idflow(j,i)
					write(98,*),'subrout. CFL_time_step'
					call endprog
				endif					
			enddo			
		endif					
	enddo
20	Dt_Courant = Dt
      end subroutine
