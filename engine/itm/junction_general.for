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

	subroutine junction_general(tf,Dt,R)
	!Purpose: This routine computes fluxes at a junction boundary of $N$ connecting pipes. 
	use common_module 
	implicit none 
	double precision temp1,temp2,temp3,temp4	
	integer i,j,prod,r,k,p,s,sum
	integer n  
	double precision fvec(10), x(10)
	double precision RHIA(10)
	double precision P_phoIA,Qsum
	double precision Qinflow,Qinf_new,Qinf_old,tf,tim,Dt
	double precision AA,TS,RH,Wpred,Area
	double precision y,A,TsIA,hIA_old,hIB_old,Ab,Qb
	double precision ytemp,Qtemp,F11,F12,yb
	double precision tempIA,Ares,h0b1,A0b1,Q0b1
	double precision Aw0,Qw0,yL,YR,AL,AR,QL,QR,FF1,FF2
	double precision fun,Dfun,yres,dely,av_y_conv
	double precision dr,inflowtemp,Ref_level,tol_local 
	Integer node_pressur_temp1(10),sum_press,SUM_DRY_BED
	double precision elev_temp,Q_temp,Error_y
	doubleprecision	temp_con1,temp_con2,temp_con3,temp_con4,temp_con5
	double precision Pw0,fvec_sum
      double precision delta_h,yc_11,Ac_temp,Q_allow,Qc_temp,con1
      double precision Ynormal      
      double precision E11_infl,E11_outf,Eres,delta_h_check
      double precision yres_Riemann,count,vmax,Q_max,sum_vec
      double precision Q_max1,Q_max2
	character*25 temp_id
	integer	CODL,IDF0,int_mult,S_write,itm_has_inflow
	integer conver_result,info,node_pressur
	external junction_solver !junction_volume_balance      
      integer mm,balan_junc,j_1,j_2
      double precision iterat_depth, vol,water_level_converg,y_water_min 
      double precision diam_differ, drop_differ

      !NodeNS(r) = Number of pipes connected to the node (1, 2, 3 ....)
	!NodeID(r,j) = pipe ID(1, 2, 3, ...) 
	!Nodetype = inflowing or outflowing


      !When node has two pipes with same diameter at same elevation, 
      ! it has an inflow and flow is open channel, the routine
      !junction_two_pipes_same_D_with_inflow(R) is used instead
      drop_differ = abs(Drop(R,1)-Drop(R,2))
      j_1 = NodeID(R,1); j_2 = NodeID(R,2)
      diam_differ = abs(d(j_1)-d(j_2)) 
      if (Nnod(R) == 2 .and. diam_differ < 0.02*d(j_1) .and. 
     &    drop_differ < 0.02*d(j_1) )then          
          do j = 1,2
 		    k = NodeID(r,j) 		    
		    if (Nodetype(r,j) == 1)then !inflowing
			    call ERP_combined(k,Nx(k)-2,DT_GLOBAL,y1(j),A1(j),Q1(j),IDf1(j))
		    elseif (Nodetype(r,j) == 2)then !outflowing
			    call ERP_combined(k,3,DT_GLOBAL,y1(j),A1(j),Q1(j),IDf1(j))
		    else
			    write(98,*),'Pipe is not inflowing or outflowing'
			    write(98,*),'Check the input data'
			    write(98,*),'subrout. junction_general'
			    call endprog
		    endif
          enddo
          !open channel flow and dropshaft water level is not close to pipe crown
          k = NodeID(r,1)
          if(IDf1(1) +  IDf1(2) == 0 .and. 
     &     yres_jun_old(R) - Drop(R,1) < 0.8*yref(k))then 
              call junction_two_pipes_same_D_with_inflow(R)
              goto 200
          endif
      endif

	!To enforce that the minimum water depth at pond is ydropmin(R)
	if (yres_jun_old(R) < 0.1*ydropmin(R))then !ydropmin(R))then
		yres_jun_old(R) = 0.1*ydropmin(R)
      endif	
      
      if (ydropmin(R) < 0d0)then 
          write(98,*),'Check the input data'
          write(98,*),'ydropmin(R) < 0, R, ydropmin(R)',R, ydropmin(R)
          write(98,*),'subrout. junction_general' 
          call endprog
      endif      
	
	tim = tf	
	Ares = Ares_junct(R)
		
	!Inflow hydrographs
	call get_inflow(R,tim,Qinf_old)
	call get_inflow(R,tim+Dt,Qinf_new)
	
	!volume balance
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

	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	!Velocity and pressure head at junction pond
	Del_Y_res(R) = Qinflow*Dt/Ares_junct(R)
	V_head_reser(R) = (Qinflow/Ares_junct(R))**2d0/(2d0*g)

	IDf1(:) = -1
	do j = 1, NodeNS(r)
 		    k = NodeID(r,j)
 		    dropIA(j) = Drop(R,j)
		    if (Nodetype(r,j) == 1)then !inflowing
			    call ERP_combined(k,Nx(k)-2,DT_GLOBAL,y1(j),A1(j),Q1(j),IDf1(j))
		    elseif (Nodetype(r,j) == 2)then !outflowing
			    call ERP_combined(k,3,DT_GLOBAL,y1(j),A1(j),Q1(j),IDf1(j))
		    else
			    write(98,*),'Pipe is not inflowing or outflowing'
			    write(98,*),'Check the input data'
			    write(98,*),'subrout. junction_general'
			    call endprog
		    endif
		    if(IDf1(j) == 0)then
			    if (y1(j) <= ydry(k))then
				    y1(j) = ydry(k); A1(j) = Adry(k); Q1(j) = 0d0		
			    endif 
		    endif	
		    u1(j) = Q1(j)/A1(j)	
		    call Pressure_Pho(k,y1(j),P_pho1(j),IDf1(j))

		    if(IDf1(j) == 0)then
			    call Phi1(k,y1(j),phi11(j))
			    call Area_from_H(k,y1(j),AA,TsIA,RH,IDf1(j))
			    c1(j) = sqrt(g*A1(j)/TsIA)
              endif		
    		
              !To set if the flow is open channel or pressurized at the boundary
              !if (yres_jun_old(R)-dropIA(j) < Yref(k))then 
              !    IDfbIA(j) = 0
	        !else 
              !    IDfbIA(j) = 1
              !Endif
              
		    !variables at boundaries
		    if (Nodetype(r,j) == 1)then !inflowing			
			    Ab_oldIA(j) = Abound(k,2); Qb_oldIA(j) = Qbound(k,2)
			    yb_oldIA(j)	= ybound(k,2)
			    IDfbIA(j) = IdFlow(k,Nx(k)-1) !This should be the case 
		    elseif (Nodetype(r,j) == 2)then !outflowing			
			    Ab_oldIA(j) = Abound(k,1); Qb_oldIA(j) = Qbound(k,1)
			    yb_oldIA(j)	= ybound(k,1)
			    IDfbIA(j) = IdFlow(k,2) !This should be the case
		    else
			    write(98,*),'Pipe is not inflowing or outflowing'
			    write(98,*),'Check the input data'
			    write(98,*),'subrout. junction_general'
			    call endprog
		    endif
	enddo	
 	
 5	sum_dry_bed = 0
	drybed(:) = 0  !Initializing for no dry bed
	sum = 0
	do j = 1, NodeNS(r)
 		k = NodeID(r,j)	
		dr = dropIA(j)              
          call Freesurface_flowregime(R,k,dr,IDf1(j),Q1(j),y1(j),A1(j),
     &		flow_regIA(j),flowcaseIA(j),Nodetype(r,j),cond_mixed1(j),
     &		SumIDFIA(j))
          sum = sum + SumIDFIA(j) 
          
          !For dry beds when the water level in the pipe is way above 
          !the water level in the pond
          
          !water level for y1 in drybed(j) = -1 
          
    		if (SumIDFIA(j) ==0)then
              y_water_min = max(3d0*ydry(k), yref(k)/10d0) 
		    if (y1(j) <= 2.0*ydry(k) .and. yres_jun_old(R)
     &            <= ydropmin(R)+yref(k)/100d0) then
     			    drybed(j) = 1 !Flow is dry bed, otherwise it is not 
			    sum_dry_bed = sum_dry_bed+1
      !elseif (y1(j) <=  (1d0 +Tol_int_10_6)*ydry(k) .and. 
              elseif (y1(j) <=  y_water_min .and. 
     &            yres_jun_old(R) < (dropIA(j) + yref(k)/10d0))then   
    				drybed(j) = -1 !Riemann problem is solved                  
              endif
          endif
      end do
	
	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	!To check if we need to solve the equations at the boundary or not
	if (sum_dry_bed == NodeNS(r))then
		do j = 1, NodeNS(r)
			k = NodeID(r,j)
			p=2*j-1
			x(p) = ydry(k); x(p+1) = 0d0
		enddo				
		temp3 = 5.d-1*(Qinf_new+Qinf_old)
		yres = yres_jun_old(R) + temp3*dt/Ares
		n = 2*NodeNS(r)+1
		x(n) = yres
		goto 50	
	endif	
	
	!Solving Riemann problem when the first cell adjacent to the pond 
	!is less or equal than ydry
	do j = 1, NodeNS(r)		
		k = NodeID(r,j)
		p=2*j-1
		if (drybed(j) == -1)then
			if (Nodetype(r,j) == 1)then !inflowing
				yL = y1(j); QL = Q1(j);
				yR = yres_jun_old(R)-dropIA(j); 
				QR = 0d0
			elseif(Nodetype(r,j) == 2)then !outflowing
				yL = yres_jun_old(R)-dropIA(j);
				QL = 0d0				
				yR = y1(j); QR = Q1(j);
              else
                  write(98,*),'Pipe is not inflowing or outflowing'
			    write(98,*),'Check the input data'
			    write(98,*),'subrout. junction_general'
			    call endprog
			endif
			i = 1 !i is not important here	
			call Riemann_open(k,i,YL,YR,QL,QR,F11,F12,Ab,Qb,Pw0)
			call H_from_Area(k,Ab,yb,118,0)
              yb = max(yb,yres_jun_old(R)-dropIA(j));
              if (yb > yref(k)) then
                  yb = yref(k) 
              endif                          
              call Area_from_H(k,yb,Ab,Ts,RH,0)
              
			y_drybed(j) = yb; Q_drybed(j) = Qb
			x(p) = yb; x(p+1) = Qb; IDfbIA(j) = 0
			!write(99,*),'Cell adjacent to junction pond is dry, Node', R			
			!write(99,*),'Routine junction'
		elseif(drybed(j) == 1)then
			x(p) = ydry(k); x(p+1) = 0d0; IDfbIA(j) = 0
		endif
	enddo
			
	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	!solving equations: free surface, pressurized and combined	
	!Pure free surface flow or combined	
	parint1000 = 0 
	param1 = Ares; param13 = dt ; param17 = Qinf_new; param18 = Qinf_old		  	     	      
	parint4 = r
				
	n = 2*NodeNS(r)+1
	do j = 1, NodeNS(r)
		if(drybed(j) == -1 .or. drybed(j) ==1)goto 15
		p=2*j-1
		!To start iteration use y1 and Q1. These values are very stable 
		!compared to those of yb_oldIA(j) and Qb_oldIA(j)
		x(p) = y1(j)  !yb_oldIA(j)		
		x(p+1) = Q1(j) !Qb_oldIA(j)
          
           if (flowcaseIA(j) == 2)then 
              !x(1) = yres - dr; x(2) = yres; x(3) = 0d0              
              x(p) = yres_jun_old(r) - dropIA(j)	
		    x(p+1) = Q1(j)
              !x(p+1) = 0d0
          else 
	        x(p) = y1(j)  !yb_oldIA(j)		
		    x(p+1) = Q1(j) !Qb_oldIA(j)
          endif          
          
15        continue
	enddo 
	x(n) = yres_jun_old(r)

	!Appropiate tolerance for solving equations	
	!tol_local = Tol_int_10_4	
	if (sum == 0)then
	    k = NodeID(r,1)
	    if (yres_jun_old(R) < 0.3*yref(k))then
		        tol_local = Tol_int_10_6	   
          else
		        tol_local = Tol_int_10_6	
          endif
      elseif(sum == 2*NodeNS(R))then
             tol_local = Tol_int_10_8
      elseif (sum > 0 .and. sum < 2*NodeNS(R))then
            tol_local = Tol_int_10_7
      else
          write(98,*),'Error in Sum.'
          write(98,*),'subrout. junction_general'
          call endprog
      endif
	Counter_printing = 0
	call hybrd1 (junction_solver,n,x,fvec,tol_local,info)
	call converg (conver_result, info)
      
	If(conver_result == 0 .and. parint1000 == 0)then	
          yres_jun_old(R) = x(n)	
	elseif(	conver_result == 1 .or. parint1000 == 1)then
		!No convergence. Riemann problem will be ved
		call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes          
		write(99,*),'t, No convergence at Node',T_GLOBAL,temp_id	     
          !write(99,*),'t, SumIDFIA(j)',T_GLOBAL,SumIDFIA(j)	     
          
          !yres_Riemann = yres_jun_old(R) !+ (temp3+Qsum)*dt/Ares
	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
		S_write = 0
		do j = 1, NodeNS(r)	
			k = NodeID(r,j)
			p=2*j-1				
              if (Nodetype(r,j) == 1)then !inflowing
				yL = y1(j); AL = A1(j); QL = Q1(j);
				yR = yres_jun_old(R)-dropIA(j); 
				QR = 0d0
                  if (SumIDFIA(j) == 0)then
                      call Area_from_H(k,yR,AR,Ts,RH,0)
                  elseif (SumIDFIA(j) == 2)then
                      call Area_from_H(k,yR,AR,Ts,RH,1)
                  elseif (SumIDFIA(j) == 1)then
                      if  (IDf1(j) ==0)then
                          call Area_from_H(k,yR,AR,Ts,RH,1)
                          CODL = 0
                      else 
                          call Area_from_H(k,yR,AR,Ts,RH,0)
                          CODL = 1
                      endif
                  else
                      write(98,*),'Uknown condition'
			        write(98,*),'subrout. junction_general'
			        call endprog 
                  endif
			elseif(Nodetype(r,j) == 2)then !outflowing
				yL = yres_jun_old(R)-dropIA(j);
				QL = 0d0                  
                  if (SumIDFIA(j) == 0)then
                      call Area_from_H(k,yL,AL,Ts,RH,0)
                  elseif (SumIDFIA(j) == 2)then
                      call Area_from_H(k,yL,AL,Ts,RH,1)
                  elseif (SumIDFIA(j) == 1)then
                      if  (IDf1(j) ==0)then
                          call Area_from_H(k,yL,AL,Ts,RH,1)
                          CODL = 1
                      else 
                          call Area_from_H(k,yL,AL,Ts,RH,0)
                          CODL = 0
                      endif                      
                  else
                      write(98,*),'Uknown condition'
			        write(98,*),'subrout. junction_general'
			        call endprog 
                  endif
				yR = y1(j); AR = A1(j); QR = Q1(j);
              else
                  write(98,*),'Pipe is not inflowing or outflowing'
			    write(98,*),'Check the input data'
			    write(98,*),'subrout. junction_general'
			    call endprog
              endif 
             
              Call Flux_junction(R,SumIDFIA(j),j,
     &        CODL,yL,yR,AL,AR,QL,QR,F11)
              x(p+1) = F11 
 20			continue
		enddo
 		Qsum = 0d0
		do j = 1, NodeNS(r)
              k = NodeID(r,j)
			p=2*j-1
			if (Nodetype(r,j) == 1)then !inflowing
				Qsum = Qsum + x(p+1)
			elseif (Nodetype(r,j) == 2)then !outflowing
				Qsum = Qsum - x(p+1)
              else 
                  write(98,*),'Pipe is not inflow. or outflow.'
	            write(98,*),'Subr. junction_general'
	            call endprog 
			endif
		end do
		temp3 = 5d-1*(Qinf_new+Qinf_old) 
		yres = yres_jun_old(R) + (temp3+Qsum)*dt/Ares
          yres_jun_old(R) = yres
          goto 200
      else 
          write(98,*),'Unkown condition in Convergence' 
		write(98,*),'Node',temp_id
		write(98,*),'Subr. junction_general'
		call endprog
	endif

50	If (ISNAN(x(n)))then 
		write(98,*),'NaN is found in x(n)' 
		write(98,*),'Node',trim(temp_id)
		write(98,*),'Yres',X(n)
		write(98,*),'Subr. junction_general'
		call endprog
	endif

	!Flux computation	
	do j = 1, NodeNS(r)
		k = NodeID(r,j)
		p=2*j-1
		If (ISNAN(x(p)) .or. ISNAN(x(p+1)))then 
			write(98,*),'NaN is found in junction_general' 
			write(98,*),'x(p)',x(p),'k',k
			write(98,*),'x(p+1)',x(p+1),'k',k
			write(98,*),'Subr. junction'
			call endprog
		endif

		if (Nodetype(r,j) == 1)then !inflowing
			If (IDfbIA(j) == 0)then
				If(x(p)<= (1d0 + Tol_int_10_6)*ydry(k))then
					Fdownst(k,1) = 0d0
					Fdownst(k,2) = fluxdry(k)
					Abound(k,2) = Adry(k); Qbound(k,2) = 0d0
					ybound(k,2) = ydry(k)					
					Pres_pho_Bound(k,2) =  fluxdry(k)
				elseif (x(p) > (1d0 + Tol_int_10_6)*ydry(k))then
					call Area_from_H(k,x(p),AA,TsIA,RH,0)
					call Pressure_Pho(k,x(p),P_phoIA,0)
					Fdownst(k,1) = x(p+1)
					Fdownst(k,2) = x(p+1)*x(p+1)/AA + P_phoIA
					Abound(k,2) = AA; Qbound(k,2) = x(p+1)
					ybound(k,2) = x(p)					
					Pres_pho_Bound(k,2) =  P_phoIA
                   else   
                      write(98,*),'x(p) <> ydry(k)'
			        call endprog
				endif
			elseif(IDfbIA(j) == 1)then
				call Area_from_H(k,x(p),AA,TsIA,RH,1)
				call Pressure_Pho(k,x(p),P_phoIA,1)
				Fdownst(k,1) = x(p+1)
				Fdownst(k,2) = x(p+1)*x(p+1)/AA + P_phoIA
				Abound(k,2) = AA; Qbound(k,2) = x(p+1)
				ybound(k,2) = x(p)				
				Pres_pho_Bound(k,2) =  P_phoIA
			else
				write(98,*),'Idflow_bound is not 0 or 1'
				write(98,*),'subrout. junction_general'
				call endprog
              endif
		elseif(Nodetype(r,j) == 2)then !outflowing
			If(IDfbIA(j) == 0)then				
				If(x(p)<= (1d0 + Tol_int_10_6)*ydry(k))then
					Fupst(k,1) = 0d0
					Fupst(k,2) = fluxdry(k)
					Abound(k,1) = Adry(k); Qbound(k,1) = 0d0
					ybound(k,1) = ydry(k)					
					Pres_pho_Bound(k,1) =  fluxdry(k)
				elseif (x(p) > (1d0 + Tol_int_10_6)*ydry(k))then
					call Area_from_H(k,x(p),AA,TsIA,RH,0)
					call Pressure_Pho(k,x(p),P_phoIA,0)
					Fupst(k,1) = x(p+1)
					Fupst(k,2) = x(p+1)*x(p+1)/AA + P_phoIA	
					Abound(k,1) = AA; Qbound(k,1) = x(p+1)
					ybound(k,1) = x(p)					
					Pres_pho_Bound(k,1) =  P_phoIA
                  else   
                      write(98,*),'x(p) <> ydry(k)'
			        call endprog
				endif
			elseif(IDfbIA(j)  == 1)then
				call Area_from_H(k,x(p),AA,TsIA,RH,1)
				call Pressure_Pho(k,x(p),P_phoIA,1)
				Fupst(k,1) = x(p+1)
				Fupst(k,2) = x(p+1)*x(p+1)/AA + P_phoIA	
				Abound(k,1) = AA; Qbound(k,1) = x(p+1)
				ybound(k,1) = x(p)
				Pres_pho_Bound(k,1) =  P_phoIA
			else 
				write(98,*),'Idflow_bound is not open channel '
				write(98,*),'or pressurized.'
				write(98,*),'subrout. junction_general'
				call endprog
              endif
		else
			write(98,*),'Pipe is not inflowing or outflowing'		
			write(98,*),'subrout. junction_general'
			call endprog
		endif
	end do		
	yres_jun_old(r) = x(n)	
 200  continue
      end subroutine

	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	subroutine junction_solver(n, x, fvec, iflag )
	use common_module
	implicit none
	integer n,iflag,r,j,k,p,itm_has_inflow
	double precision fvec(n),x(n)
	double precision temp1,temp2,temp3,temp4,temp8
	double precision Qinf_new,Qinf_old,dt,k1,Qsum
	double precision AIA(10),yIA(10),ubA(10),cbA(10),RHIA(10)
      double precision E1_infl(10),E1_outf(10)      
	double precision TsIA,Ares,Horif,Qorif
	double precision temp_con1,temp_con2,temp_con3,temp_con4,temp_con5
	doubleprecision	TS,Wpred
	double precision y,A,con1,con2,delta_h
	double precision yL,YR,AL,AR,QL,QR
      double precision Q_max1,Q_max2, Q_max, vmax
      double precision y_temp,A_temp,T_temp,R_temp,A_temp2
      integer :: seed
	double precision ran_number
	parint1000 = 0      
	Ares = param1
	dt = param13
	Qinf_new = param17; Qinf_old = param18
	r = parint4
      
	If (ISNAN(x(n)))then !To make sure that water depth at reservoir is not a NAN
		x(n) = yres_jun_old(R)
		parint1000 = 1 
		!goto 120
      endif
           
      vmax = 10.0 !max velocity = 10 m/s
	!compute Area and velocity
	do j = 1, NodeNS(r)		
		k = NodeID(r,j)
		p=2*j-1	
          
		If (drybed(j) == -1 .or. drybed(j) == 1)goto 90 !dry bed or Riemann solution
              
		If (ISNAN(x(p)))then 
			!x(p) = y1(j) 
              call random_seed(seed)  
              call random_number(ran_number)  
              
              if(SumIDFIA(j) == 0)then
                  x(p) = ran_number*yref(k)  !y1(j)  
              elseif(SumIDFIA(j) == 2)then
                  x(p) = yref(k) + ran_number*yref(k)     
              elseif (SumIDFIA(j) == 1)then
                  x(p) = ran_number*yref(k)  !y1(j)  
              else 
                  write(98,*),'SumIDFIA(j) == 2 .ne. 0,2'
				write(98,*),'subrout. junction_solver'
				call endprog
              endif              
			parint1000 = 1 
		endif

		If (ISNAN(x(p+1)))then 
              call random_seed(seed)  
              call random_number(ran_number)              
              x(p+1) = ran_number*Qcrit_maxIA(k) ! Q1(j)
			parint1000 = 1 
          endif          
				
		if(SumIDFIA(j) == 0 .or. SumIDFIA(j) == 2)then
			If (SumIDFIA(j) == 0)then
				!If (drybed(j) == 0)then  !No dry bed
					temp8 = (yref(k)+d(k))/2d0
					If (x(p) >= temp8)then			
						x(p) = temp8						
                      endif                      
                      
					If (x(p) < 0.01*b2_min(k))then			
						x(p) = 0.01*b2_min(k)
						x(p+1) = 0d0
					endif
					call Area_from_H(k,x(p),AIA(j),TsIA,RHIA(j),0)
					cbA(j) = sqrt(g*AIA(j)/TsIA)	
                      
                      y_temp = max(yres_jun_old(r)-dropIA(j),0d0)
                      call Area_from_H(k,y_temp,A_temp,T_temp,R_temp,0)
                      A_temp2 = max(A_temp,A1(j),AIA(j),Aref(k))
                      Q_max = vmax*A_temp2
                      If (abs(x(p+1)) > Q_max)then			
                          !x(p+1) = Q_max*SIGN(1d0,x(p+1))
                          x(p+1) = Q_max*SIGN(1d0,Q1(j))
                      endif
                      
				!else
				!	write(98,*),'drybed unknown. Subr. junction'
				!	Call endprog
				!endif				
				call Phi1(k,x(p),phi1IA(j))
			elseif (SumIDFIA(j) == 2)then		
				call Area_from_H(k,x(p),AIA(j),TsIA,RHIA(j),1)				
			else
				write(98,*),'SumIDFIA(j) == 2 .ne. 0,2'
				write(98,*),'subrout. junction_solver'
				call endprog
			endif
			ubA(j) = x(p+1)/AIA(j)
		endif
 90		continue
	end do	
		
	!Riemann invariants and freefall/constant state conditions
	do j = 1, NodeNS(r)
		k = NodeID(r,j)
		p=2*j-1
		If (SumIDFIA(j) == 0 .or. SumIDFIA(j) == 2) then	
              con1 = 1d0/(2d0*g)*u1(j)**2d0
              !con1 = 0d0
              E1_infl(j) = y1(j) !+ con1 !+ 0.5*abs(s0(k)*dx(k))
              E1_outf(j) = y1(j) !+ con1 !- 0.5*abs(s0(k)*dx(k))
			If (drybed(j) == -1)then !Riemann solution
				fvec(p) = x(p) - y_drybed(j)  
				fvec(p+1) = (x(p+1) - Q_drybed(j))/Aref(k)  !Normalization
				!fvec(p) = x(p) - x(n)
				!fvec(p+1) = x(p+1) - 0d0
				goto 100
			elseif (drybed(j) == 1)then
				!For dry beds when the invert of the pipe is above 
				!the water level in the pond			
				fvec(p) = x(p) - ydry(k)
				fvec(p+1) = x(p+1)-0d0
				goto 100
			endif

			if (Nodetype(r,j) == 1)then !inflowing	
				k1 =  Klocal(k,2)
			elseif(Nodetype(r,j) == 2)then !outflowing
				K1 = Klocal(k,1)
			else
				write(98,*),'Nodetype(r,j) undefined'
				write(98,*),'Subr. junction_solver'
				call endprog
			endif
		endif

		If (SumIDFIA(j) == 0) then
              !con2 = 1d0/(2d0*g)*ubA(j)**2d0
              !con2 = 0d0
			if (Nodetype(r,j) == 1)then !inflowing	
				fvec(p) = ubA(j) - u1(j) + (cbA(j) + c1(j))*
     &				(AIA(j) - A1(j))/(AIA(j) + A1(j))
				If (flowcaseIA(j) == 1)then	
					!free outfall or normal flow 
					if(flow_regIA(j) == 1)then	!Subcritical flow
                          !fvec(p)=x(p) - E1_infl(j)                          
						fvec(p+1) = ubA(j) - cbA(j) 
                      elseif(flow_regIA(j) == 2)then !Supercritical flow
                          !fvec(p)=x(p) - E1_infl(j)
						fvec(p+1) = ubA(j) - 
     &				        1d0/nm(k)*RHIA(j)**(2d0/3d0)*sqrt(S0(k))
					else
						write(98,*),'# of flow_regIA(j) is not supported'
						write(98,*),'Subr. junction_solver'
						call endprog
					endif
                  elseIf (flowcaseIA(j) == 2)then
                      if (Nodetype(r,j) == 2)then !outflowing
                          if (u1(j) - c1(j) >= 0d0)then !the characteristic doesn't connect the boundary 
                              fvec(p) = ubA(j) - cbA(j)
                          endif   
                      elseif (Nodetype(r,j) == 1)then !inflowing	
                          if (u1(j) + c1(j) <= 0d0)then !the characteristic doesn't connect the boundary 
                              fvec(p)  = ubA(j) + cbA(j)
                          endif  
                      else
				        write(98,*),'Pipe is not inflowing or outflowing'
				        write(98,*),'Check the input data'
				        write(98,*),'subrout. junction_solver'
				        call endprog
			        endif   
                      !fvec(p) = x(p) - E1_infl(j)					
                      fvec(p+1) = x(p) - (x(n)-dropIA(j))
				else
					write(98,*),'flowcaseIA(j)',flowcaseIA(j)
					write(98,*),'flowcaseIA1 undef. in subr. junction_solver'
					call endprog
                  endif
                  
              elseif(Nodetype(r,j) == 2)then !outflowing
				fvec(p) = ubA(j) - u1(j) - (cbA(j) + c1(j))*
     &				(AIA(j) - A1(j))/(AIA(j) + A1(j))
!				fvec(p) = u1(j) - phi11(j) - (ubA(j) - phi1IA(j))                        
				If(flowcaseIA(j) == 1)then				    
					If(flow_regIA(j) == 10)then
						!temp3 = y1(j)
						!fvec(p) = x(p) - max(ydry(k),temp3)
						!negative sign for critical depth (negative flow)
                          !fvec(p) = x(p) - E1_outf(j)
						fvec(p+1) = -ubA(j) - cbA(j)						
                      elseif(flow_regIA(j) == 11)then
						!Riemann invariants can not be used for outflowing 
						!pipes and supercritical flows
						!positive sign for critical depth (positive flow)
						fvec(p) = x(p) - 
     &                        (x(n)-dropIA(j))                          
                          fvec(p+1) = ubA(j) - cbA(j)
					else
						write(98,*),'# of flow_regIA(j) is not supported'
						write(98,*),'Subr. junction_solver'
						call endprog
					endif
				elseIf (flowcaseIA(j) == 2)then !Constant head
                      if (Nodetype(r,j) == 2)then !outflowing
                          if (u1(j) - c1(j) >= 0d0)then !the characteristic doesn't connect the boundary 
                              fvec(p) = ubA(j) - cbA(j)
                          endif   
                      elseif (Nodetype(r,j) == 1)then !inflowing	
                          if (u1(j) + c1(j) <= 0d0)then !the characteristic doesn't connect the boundary 
                              fvec(p)  = ubA(j) + cbA(j)
                          endif  
                      else
				        write(98,*),'Pipe is not inflowing or outflowing'
				        write(98,*),'Check the input data'
				        write(98,*),'subrout. junction_solver'
				        call endprog
			        endif                      
					fvec(p+1) = x(p) - 
     &                        (x(n)-dropIA(j))
				else
					write(98,*),'flowcaseIA(j)',flowcaseIA(j)
					write(98,*),'flowcaseIA1 undef. in subr. junction'
					call endprog
				endif
			else
				write(98,*),'Pipe is not inflowing or outflowing'
				write(98,*),'Check the input data'
				write(98,*),'subrout. junction_solver'
				call endprog
			endif
		elseif (SumIDFIA(j) == 2) then	
			if (Nodetype(r,j) == 1)then !inflowing	
				fvec(p) = ubA(j) + pc1(k)*LOG(AIA(j)) - 
     &				(u1(j) + pc1(k)*LOG(A1(j)))	
                  
                 
			elseif(Nodetype(r,j) == 2)then !outflowing
				fvec(p) = ubA(j) - pc1(k)*LOG(AIA(j)) - 
     &				(u1(j) - pc1(k)*LOG(A1(j)))
			else
				write(98,*),'Pipe is not inflowing or outflowing'
				write(98,*),'subrout. junction_solver'
				call endprog
			endif
			
			If(flowcaseIA(j) == 2)then
				!fvec(p+1) = dropIA(j)+x(p)-x(n)				
				if (Nodetype(r,j) == 1)then !inflowing	
					!fvec(p+1) = dropIA(j)+x(p)+con1-k1*con2 - x(n)	
					fvec(p+1) = dropIA(j) + x(p) - x(n)
				elseif(Nodetype(r,j) == 2)then !outflowing							
					!fvec(p+1) = dropIA(j)+x(p)+con1+k1*con2 - x(n)	
					fvec(p+1) = dropIA(j) + x(p) - x(n)
				else
					write(98,*),'Pipe is not inflowing or outflowing'
					write(98,*),'subrout. junction_solver'
					call endprog
				endif				
			else
				write(98,*),'flowcaseIA(j)',flowcaseIA(j)
				write(98,*),'flowcaseIA1 undef. in subr. junction'
				call endprog
			endif
		elseif (SumIDFIA(j) == 1) then	
              If(cond_mixed1(j) == 2)then
			    !flow in adjacent cell is pressurized, and water level at pond 
                   !is below pipe crown (pressur. flow)	
                  fvec(p) = x(p) - yref(k) !y1(j) 
			    delta_h = y1(j) - (yres_jun_old(r)-dropIA(j))
                  
                  if (delta_h<0d0)then 
                      delta_h = 0d0  
                  endif
                  temp_con1 = 0.6*Aref(k)*sqrt(2d0*g*delta_h) 
                  
                  vmax = 20d0
                  Q_max1 = vmax*Aref(k)                  
                  Q_max2 = (1d0/dt)*Aref(k)*dx(k)/2d0 
                  Q_max = min(Q_max1,Q_max2)
                  
                  if (temp_con1 > Q_max)then
                      temp_con1 = Q_max  
                  endif
                  
				!To make sure that as maximum 50% of the volume from the adjacent 
				!cell would enter the reservoir. 
                  
                  if (Nodetype(r,j) == 2)then !outflowing                      
                          fvec(p+1) = (x(p+1)+temp_con1)/Aref(k) !Normalization 
					!Negative sign because flow is entering the reservoir
                  elseif(Nodetype(r,j) == 1)then !inflowing                      
                          fvec(p+1) = (x(p+1)-temp_con1)/Aref(k) !Normalization 
                  else
                      write(98,*),'Nodetype(r,j) .ne. 1,2'
				    call endprog
                  endif
			elseIf(cond_mixed1(j) == 1)then  !WORK HERE
                  !flow in adjacent cell is free surface and 
				!water level at pond is above pipe crown (pressur. flow)
				temp_con1 = yres_jun_old(R)-dropIA(j)-y1(j)/2d0
				!temp_con2 = Ares_junct(R)*temp_con1/(Aref(k)*dt)				
				if(temp_con1 < 0d0)then
                      temp_con1 = 0d0
                  endif
                  
                  vmax = 20d0
                  Q_max1 = vmax*Aref(k)                  
                  Q_max2 = (1d0/dt)*Ares*temp_con1                  
                  Q_max = min(Q_max1,Q_max2)
                  
				!To make sure that as maximum 50% of the volume from the adjacent 
				!cell would enter the reservoir. 					
				temp_con3 = 0.6*Aref(k)*sqrt(2d0*g*temp_con1)	
                  
                  if (temp_con3 > Q_max)then
                      temp_con3 = Q_max  
                  endif
                  
				if (Nodetype(r,j) == 2)then !outflowing
					fvec(p) = x(p) - yref(k) 
					fvec(p+1) = (x(p+1) - temp_con3)/Aref(k)  !Normalization
					!Negative sign because flow is entering the reservoir
				elseif(Nodetype(r,j) == 1)then !inflowing
					fvec(p) = x(p) - yref(k)						
					fvec(p+1) = (x(p+1) + temp_con3)/Aref(k)  !Normalization
                  else
                      write(98,*),'Nodetype(r,j) .ne. 1,2'
				    call endprog
                  endif
			else
				write(98,*),'cond_mixed1(j) .ne. 1,2'
				write(98,*),'cond_mixed not supported'
				write(98,*),'Subr. Junction general'
				call endprog
              endif              
		else
			write(98,*),'Error in SumIDFIA(j) .ne. 0, 1, and 2'
			write(98,*),'Subr. Junction general'
			call endprog
		endif
100		continue
	enddo	
	
	!fvec(n) represents the storage relation (if pond is not present, 
	!the storage relation reduces to the continuity equation)
	Qsum = 0d0
	do j = 1, NodeNS(r)
		p=2*j-1
		!if (Nodetype(r,j) == 1)then !inflowing
		!	Qsum = Qsum + 0.5*(Qb_oldIA(j)+x(p+1))
		!else !outflowing
		!	Qsum = Qsum - 0.5*(Qb_oldIA(j)+x(p+1))
		!endif
		if (Nodetype(r,j) == 1)then !inflowing
			Qsum = Qsum + x(p+1)			
		elseif(Nodetype(r,j) == 2)then !outflowing
			Qsum = Qsum - x(p+1)
		else
			write(98,*),'Error in Nodetype(r,j) .ne. 1,2'
			write(98,*),'Subr. Junction general'
			call endprog
		endif
	end do
	temp3 = 5d-1*(Qinf_new+Qinf_old)
	temp4 = (x(n)-yres_jun_old(R))
      fvec(n) = (temp3+Qsum)*dt - temp4*Ares
      !Counter_printing =  Counter_printing+1      
      
      !if (r == 6)then
      !    if (T_GLOBAL > 31480 .and. T_GLOBAL < 31486)then 
      !
      !        write(99,*),'_____________________' 
      !        write(99,*),'T_GLOBAL',T_GLOBAL   
      !        write(99,*),'T_GLOBAL+DT',T_GLOBAL+dt   
      !        write(99,*),'ITERATION',Counter_printing
      !        write(99,*),'fvec(n)',fvec(n)
      !        do j = 1, NodeNS(r)
		    !    p=2*j-1        
      !            write(99,*),'Node R,j = ',R,j
      !            write(99,*),'x(p)',x(p) 
      !            write(99,*),'x(p+1)',x(p+1)      
      !            write(99,*),'fvec(p)',fvec(p)
      !            write(99,*),'fvec(p+1)',fvec(p+1)
      !            write(99,*),'SumIDFIA(j) = ',SumIDFIA(j)
      !            write(99,*),'Nodetype(r,j) = ',Nodetype(r,j)
      !            write(99,*),'flowcaseIA(j) = ',flowcaseIA(j)
      !            write(99,*),'flow_regIA(j)=',flow_regIA(j)
      !    if (abs(x(p))>100.or.abs(x(p+1))>100.or.abs(x(n))>100)then 
      !             write(99,*),'attention'   
      !    endif
      !        end do
      !        write(99,*),'_____________________'
      !    endif
      !    endif
120	continue
      end
                    
         !     subroutine junction_volume_balance(n, x, fvec, iflag )
         !     use common_module
         !     implicit none  
         !     integer k,R,iter,j,code_error,flow_type
         !     integer n, iflag
         !     integer conver_result,info
         !     double precision fvec(1), x(1)
         !     double precision A,Y,teta,Vol_iter,dmax,vol,dt
         !     double precision tol_H_area,dmin
         !     parint1000 = 0 
         !     vol = param1 
         !     dt = param13 
         !     r = parint4
         !     
	        !If (ISNAN(vol))then 
         !         write(98,*),'NaN is found in routine junction_volume_balance' 
         !         call endprog
	        !endif           
	        !tol_H_area = 1.d-6
         !     dmax = 0d0
         !     dmin = 1000000000d0
         !     Vol_iter = 0d0
         !     do j = 1, NodeNS(r)	
         !         if(ID_Number_of_zero_drops(R,j) == 1)then
         !             k = NodeID(r,j) 
         !             dmax = max(dmax,d(k))
         !             dmin = min(dmin,0.1*ydry(k))
         !         endif 
         !     enddo
         !      if (x(1) < dmin)then
         !         x(1) = dmin
         !         parint1000 = 1
         !     endif
         !     if (x(1) > 0.999*dmax)then
         !         x(1) = 0.999*dmax
         !         parint1000 = 1
         !     endif 
         !     do j = 1, NodeNS(r)	
         !         if(ID_Number_of_zero_drops(R,j) == 1)then
         !             k = NodeID(r,j)
         !             teta = 2*acos(1d0-2d0*x(1)/d(k))
         !             A = d(k)**2d0/8d0*(teta-SIN(teta))		
         !             Vol_iter = Vol_iter + A*dx(k) 
         !             dmax = max(0d0,d(k))
         !         endif          
         !     enddo
         !     Vol_iter = Ares_junct(R)*x(1)
         !     fvec(1) = Vol_iter - vol  
         !     end     
         !             
         !             