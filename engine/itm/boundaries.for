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

      subroutine boundaries(time,Dt)
	!This routine is the main module for computing fluxes at boundaries
      use common_module
	implicit none 
	integer j,k,r,IA,IC
      double precision time,Dt,F11,F12,F21,F22,F31,F32
	double precision Ab,Qb,yb,P_phob,Tb,y,RH,Ar,ycrit
	double precision dmin,Err_vol_allowed	
	character*25 temp_id	
	
	Vol_bal = 0d0
	code_vol_bal = 0 !1: Increase inflow volume
	Err_vol_allowed = 0.5  !Maximum volume error allowed
	!-1 decrease inflow volume, 0 no volume balance

	If(Error_volume < -Err_vol_allowed)then  !1% volume error
		code_vol_bal = -1 !(decrease inflow volume)
			!Correction for volume in case of unbalanced mass
			If(Error_volume < -Err_vol_allowed)then  !5% volume error
				vol_factor = 0.15
			else
				vol_factor = 0.05
			endif
	elseif(Error_volume > Err_vol_allowed)then  !1% volume error
		code_vol_bal = 1 !(increase inflow volume)
		!Correction for volume in case of unbalanced mass
		If(Error_volume > Err_vol_allowed)then  !5% volume error
			vol_factor = 0.15
		else
			vol_factor = 0.05
		endif
	else
		code_vol_bal = 0
		vol_factor = 0d0
	endif
	
	
	V_head_reser(:) = 0d0  !To initialize
	Del_Y_res(:)  = 0d0    !To initialize

	do R=1,Nnodes
			!If Error volume (entered-stored) is 1% or larger, balance of volume 
			!is performed until error is half of initial error
			If(code_vol_bal .ne. 0)then				
				If(Vol_bal > balance_volume)then
					code_vol_bal = 0
				endif
			endif			
				
			If(BCnode(R) == 7)then !junction for pipes >= 2
			    call junction_general(time,Dt,R)
              elseIf(BCnode(R) == 24)then  !Junction 2 pipes with same diameter (with and without inflows) 
		        !we assign internally the ID of 24.
                  call junct2pipes_same_diam(R)
			elseIf(BCnode(R) == 20)then  !Reservoirs				
				call Reservoirs(Dt,R)				 					    
			elseIf(BCnode(R) == 4)then  !dropshafts				    		    	
				call Dropshaft_general(time,Dt,R)			    
				!if (sum_temp == 2) then							
				!	temp_id = ''
				 !   call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
				    !write(99,*), 'Dropshaft_general',R,temp_id
				!endif
			elseIf(BCnode(R) == 10 .or. BCnode(R) == 11)then  !constant boundary			
				call const_bound(R)
			elseIf(BCnode(R) == 30)then !Rating curve boundary
				call Rating_curve(R)								
			elseIf(BCnode(R) == 40)then !Gate Boundary condition (Two pipes)
			    !It supports both open-channel and pressurized flow			
				call Gate_two_pipes(time,Dt,R)
	      elseIf(BCnode(R) == 41)then !Gate Boundary condition (one pipe)
			    !It supports both open-channel and pressurized flow			
				call Gate_one_pipe(time,Dt,R)				
			else
				write(98,*),'Boundary condition type not supported'
				write(98,*),'Subr. boundaries'
				call endprog
			endif	
	enddo	
      
	!For overflows
	sum_temp = sum_temp +1 
      end subroutine 