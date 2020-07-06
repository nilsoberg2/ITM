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

	subroutine junction_two_pipes_same_D_with_inflow(R)
	!This routine computes the fluxes at a junction boundary of two pipes 
	!that have the same diameter with no inflow hydrograph.
	use common_module
	implicit none 
	integer j,k,r,IA,IC,IH,IK,sum,CODL,Idf0,i
	double precision F11,F12,Aw0,Qw0,Ab,Qb,yb,Wpred	
	double precision yL,YR,AL,AR,QL,QR,h0b1,A0b1,Q0b1,Dt,Pw0
	double precision signflow(2),dh_junction1,dh_junction2
      double precision Qinf_old,Qinf_new,Qinflow,tim,Ts,RH
      double precision yjunction_temp, Q_temp
      double precision yser_temp, temp_sum
	     
	!values at adjacent cells
	j = 1 !left cell 
	k = NodeID(r,j)
	IA = k
	Dt = DT_GLOBAL
      tim = T_GLOBAL      
      
      !Inflow hydrographs
      !volume balance
      call get_inflow(R,tim,Qinf_old)
	call get_inflow(R,tim+Dt,Qinf_new)
	If(code_vol_bal == 1 .or. code_vol_bal == -1)then !Volume balance
		    Qinflow = (Qinf_old+Qinf_new)/2d0
		    Vol_bal = Vol_bal + vol_factor*Qinflow*Dt	
		    If(code_vol_bal == -1)then !Volume balance		
			    Qinf_old = (1d0-vol_factor)*Qinf_old
			    Qinf_new = (1d0-vol_factor)*Qinf_new
		    elseif(code_vol_bal == 1)then !Volume balance		
			    Qinf_old = (1d0+vol_factor)*Qinf_old
			    Qinf_new = (1d0+vol_factor)*Qinf_new
		    endif
	endif
	Qinflow = (Qinf_old+Qinf_new)/2d0
      
      if (Qinflow <= 0d0)then
          Qinflow = 0d0 
      endif
      
      if (Qinflow > 0d0)then   
          yjunction_temp = DT_GLOBAL*dabs(Qinflow)/Ares_junct(R)
      else 
          yjunction_temp = 0d0
      endif 
      
	if (Nodetype(r,j) == 1)then !inflowing
		call ERP_combined(k,Nx(k)-2,DT_GLOBAL,h0b1,A0b1,Q0b1,IDf1(j))		
		y1(j) = h0b1; QL = Q0b1; AL = A0b1; yL = h0b1          
		IH = Nx(k)-2  !cell for mixed interface
		signflow(j) = 1d0	!direction of flow discharge
		dh_junction1 = abs(S0(k)*dx(k)/2d0)
	elseif (Nodetype(r,j) == 2)then !outflowing
		call ERP_combined(k,3,DT_GLOBAL,h0b1,A0b1,Q0b1,IDf1(j))		
		y1(j) = h0b1; QL = -Q0b1; AL = A0b1; yL = h0b1          
		signflow(j) = -1d0	
		IH = 3
		dh_junction1 = -abs(S0(k)*dx(k)/2d0)
	else
		write(98,*),'Nodetype(r,j) unknown'
		write(98,*),'subrout. junct2pipes_same_diam'
		call endprog
	endif	

	j = 2 !right cell 
	k = NodeID(r,j)
	IC = k
	if (Nodetype(r,j) == 1)then !inflowing
		call ERP_combined(k,Nx(k)-2,DT_GLOBAL,h0b1,A0b1,Q0b1,IDf1(j))		
		y1(j) = h0b1; QR = -Q0b1; AR = A0b1; yR = h0b1   
		signflow(j) = -1d0	!direction of flow discharge
		Ik = Nx(k)-2  !cell for mixed interface
		dh_junction2 = abs(S0(k)*dx(k)/2d0)
	elseif (Nodetype(r,j) == 2)then !outflowing
		call ERP_combined(k,3,DT_GLOBAL,h0b1,A0b1,Q0b1,IDf1(j))		
		y1(j) = h0b1; QR = Q0b1; AR = A0b1; yR = h0b1  
		signflow(j) = 1d0	!direction of flow discharge
		Ik = 3  !cell for mixed interface
		dh_junction2 = -abs(S0(k)*dx(k)/2d0)
	else
		write(98,*),'Nodetype(r,j) unknown'
		write(98,*),'subrout. junct2pipes_same_diam'
		call endprog
	endif
	     
	sum = IDf1(1)+IDf1(2)

	!To check if we need to solve the equations at the boundary or not
	if (sum == 0)then
		if (yL <= (1d0+Tol_int_10_4)*ydry(IA) .and. 
     &		    yR <= (1d0+Tol_int_10_4)*ydry(IC))then			
			F11 = 0d0; F12 = 0d0
			Ab = Adry(IA); Qb = 0d0
			y1(1) = ydry(IA); y1(2) = ydry(IC)	
              if (Qinflow > 0d0)then                  
                  y1(1) = max(ydry(IA)+yjunction_temp, 
     &            ydry(IC)+yjunction_temp)
                  call Area_from_H(IA,y1(1),Ab,Ts,RH,0)     
                  y1(2) =  y1(1)   
                  F11 = 0d0 
                  Qb  = 0d0 
              endif              
			goto 10
		endif
	endif

	If (sum == 2)then
	  i = 1 !i is not important here	
		call Riemann_pressur(IA,i,AL,AR,QL,QR,F11,F12,Ab,Qb,Pw0)
		call H_from_Area(IA,Ab,yb,520,1)
		y1(1) = yb; y1(2) = yb		
	elseif (sum == 0)then
	  i = 1 !i is not important here	
		call Riemann_open(IA,i,yL,yR,QL,QR,F11,F12,Ab,Qb,Pw0)
		call H_from_Area(IA,Ab,yb,520,0)
		y1(1) = yb; y1(2) = yb          
	elseif (sum == 1)then
		if(IDf1(1)==0)then 
			CODL = 0; !#left state is open channel flow		
		else 
			CODL = 1 !#left state is pressurized flow								
		endif
		i = 1 !i is not important here	
		call Riemann_pressur_open(IA,i,yL,yR,QL,
     &		QR,CODL,Aw0,Qw0,Idf0,F11,F12,Wpred,Pw0)			
		    Ab = Aw0; Qb = Qw0
              call H_from_Area(IA,Ab,yb,520,Idf0)
	else	
		write(98,*),'Sum .ne. 0,1,2'
		write(98,*),'subrout. junct2pipes_same_diam'
		call endprog		
      endif	
 
 10	do j= 1,2	
		k = NodeID(r,j)		
		if (Nodetype(r,j) == 1)then !inflowing
			Fdownst(k,1) = F11*signflow(j)
			Fdownst(k,2) = F12
			Abound(k,2) = Ab; Qbound(k,2) = Qb*signflow(j)
			ybound(k,2) = yb
			Idflow_bound(k,2) = IDf1(j)
			Pres_pho_Bound(k,2) =  Pw0              
			if(j == 1)then
				Idflow_bound_reser(k,2) = IDf1(2)
			elseif(j == 2)then
				Idflow_bound_reser(k,2) = IDf1(1)
			else
				write(98,*),'j .ne. 1,2'
				write(98,*),'subrout. junct2pipes_same_diam'
				call endprog
              endif
              if (Qinflow > 0d0)then
                      Fdownst(k,1) = Fdownst(k,1) - Qinflow/2d0
                      Q_temp = Qbound(k,2) - Qinflow/2d0
         !              Fdownst(k,2) = F12 + Q_temp*Q_temp/Abound(k,2)-
         !&                - Qbound(k,2)*Qbound(k,2)/Abound(k,2)
                      Qbound(k,2) = Q_temp
              endif 
		elseif(Nodetype(r,j) == 2)then !outflowing
			Fupst(k,1) = F11*signflow(j)
			Fupst(k,2) = F12
			Abound(k,1) = Ab; Qbound(k,1) = Qb*signflow(j)
			ybound(k,1) = yb
			Idflow_bound(k,1) = IDf1(j)				
			Pres_pho_Bound(k,1) =  Pw0
			if(j == 1)then
				Idflow_bound_reser(k,1) = IDf1(2)
			elseif(j == 2)then
				Idflow_bound_reser(k,1) = IDf1(1)
			else
				write(98,*),'j .ne. 1,2'
				write(98,*),'subrout. junct2pipes_same_diam'
				call endprog
              endif
              if (Qinflow > 0d0)then
                  Fupst(k,1) = Fupst(k,1) + Qinflow/2d0
                  Q_temp = Qbound(k,1) + Qinflow/2d0        !          
         !              Fupst(k,2) = F12 + Q_temp*Q_temp/Abound(k,1)-
         !&                - Qbound(k,1)*Qbound(k,1)/Abound(k,1)
                  Qbound(k,1) = Q_temp
              endif 
		else
			write(98,*),'Nodetype(r,j) .ne. 1,2'
			write(98,*),'subrout. junct2pipes_same_diam'
			call endprog		
          endif               
      end do
      
      yres_jun_old(r) = 10d0**10d0
      temp_sum = 0d0
      do j= 1,2	
		k = NodeID(r,j)		
		if (Nodetype(r,j) == 1)then !inflowing
               yser_temp = h0(k,Nx(k)-4) 
		elseif(Nodetype(r,j) == 2)then !outflowing
              yser_temp = h0(k,5) 
		else
			write(98,*),'Nodetype(r,j) .ne. 1,2'
			write(98,*),'subrout. junct2pipes_same_diam'
			call endprog		
          endif
          temp_sum = temp_sum+yser_temp
      end do      
	yres_jun_old(r) = temp_sum /2d0
      
      !min(yres_jun_old(r),yser_temp)      
20    return	  
      end subroutine
