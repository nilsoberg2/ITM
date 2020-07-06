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

	subroutine Reservoirs(Dt,R) 
	!Reservoir boundary condition		
	!Purpose: This routine is used for computing fluxes at a reservoir boundary.  
	!At this stage only one inflow/outflow pipe is considered. 
	!Later may be added "n" inflow/outflow pipes. 
	use common_module
	implicit none	 
      integer j,sum,n,R,Id1,Idb,cond_mixed,CODL,Idf0
	double precision A,Ts,RH,F11,F12,dt,Stora_old
      double precision AA,u11,y11,A11,Q11,c11,T1,P_pho11
	double precision ub,Qb,Yb,Ab,cb,Tb,P_phob
	double precision yL,yR,AL,QL,AR,QR,h0b1,A0b1,Q0b1
	double precision YW0,Aw0,Qw0,FF1,FF2,Wpred
	double precision yres,dr,tol_const_state
	double precision hIA_old,hIA,hIC,ScIA,Ycrit
	double precision ytemp,TEMPIA,k1,Ares_aprox,y
	double precision x(10),fvec(10),tol_local
	double precision Stora_new,inflowtemp,Q_allow
	double precision fun,Dfun,av_y_conv,dely,Ref_level
	double precision temp_con1,temp_con2,temp_con3,temp_con4
      double precision Pw0,Areser_depth      
      double precision delta_h, Q_flux_max, veloc_max, Qvalue
      double precision Q_magnitude, Y_temp_storage,y_water_min 
      double precision Stora_temp
      double precision Hmax,yc,Ac,Qc,Qb_temp
	character*25 temp_id
	external Reser,Reser__noconverg
	integer i,conver_result,info
	integer flowcaseIA1,FLOW_REGIA1,drybed11      

      dr = Drop(R,1)	
	j = NodeID(R,1)	
	Outflow_limited(R) = Reser_outflow(R) !Reservoir outflow. This may be zero for shallow water depths
      If (yres_jun_old(r) <= ydropmin(R))then 
			Outflow_limited(R) = 0d0
      endif
      !write(99,*),'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$' 
      !write(99,*),'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$' 
      !write(99,*),'t, reserv. depth3', T_global, yres_jun_old(R)
      !write(99,*),'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$' 
      !write(99,*),'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$' 
      
      if (yres_jun_old(R) >= reser_maxdepth(R))then
              write(98,*),'Water depth in reservoir exceeds data in 
     &        stage-storage curve'
		    write(98,*),'Subr. Reservoirs'
		    call endprog
      endif  
      
      !To enforce that the minimum water depth at pond is ydropmin(R)
	if (yres_jun_old(R) < ydropmin(R))then
		yres_jun_old(R) = ydropmin(R)	
	endif
      
	!For computing average Area at the reservoir	
	call get_storage(R,yres_jun_old(R),Stora_old)      
      
      if (yres_jun_old(R) <  0.1*yref(j))then
          Y_temp_storage = 0.1*yref(j)
          call get_storage(R,Y_temp_storage,Stora_temp)   
           Areser_depth = Stora_temp/Y_temp_storage      
      elseif (yres_jun_old(R) > 0.1*yref(j))then
	    Areser_depth = Stora_old/yres_jun_old(R)
      else
          write(98,*),'Water depth in reservoir exceeds data in 
     &    stage-storage curve'
          write(98,*),'Subr. Reservoirs'
          call endprog   
      endif 
	            
	
	!Local head loss coefficient	
	if (Nodetype(R,1) == 2)then !outflowing
		K1 = Klocal(j,1)	
	elseIf(Nodetype(R,1) == 1)then
		K1 = Klocal(j,2)
	else
		write(98,*),'Nodetype .ne. 1,2'
		write(98,*),'Subr. Reservoirs'
		call endprog
	endif		
		
 5	if (Nodetype(R,1) == 2)then !outflowing
          call ERP_combined(j,3,DT_GLOBAL,y11,A11,Q11,Id1)
          Ab_oldIA(1) = Abound(j,1); Qb_oldIA(1) = Qbound(j,1)
          yb_oldIA(1) = ybound(j,1)	
          Idb = Idflow(j,2)	    
	Elseif (Nodetype(R,1) == 1)then !inflowing
          call ERP_combined(j,Nx(j)-2,DT_GLOBAL,y11,A11,Q11,Id1)
          Ab_oldIA(1) = Abound(j,2); Qb_oldIA(1) = Qbound(j,2)
          yb_oldIA(1) = ybound(j,2)
          Idb = IdFlow(j,Nx(j)-1)
	Else
		write(98,*),'Nodetype(R,1) .ne. 1,2'
		write(98,*),'Subr. Reservoirs '
		call endprog
	Endif	
      
      call Freesurface_flowregime(R,j,dr,Id1,Q11,y11,A11,
     & FLOW_REGIA1,flowcaseIA1,Nodetype(R,1),cond_mixed,
     & sum)   
      
      !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 
      !When reservoir is near empty
      drybed11 = 0
          if (sum ==0)then
              y_water_min = max(3d0*ydry(j),yref(j)/10d0)
		    if (y11 <= 3d0*ydry(j) .and. 
     &            yres_jun_old(R) <= ydropmin(R)) then     
         			drybed11 = 1 !1 : Flow is dry bed, otherwise it is not       
              elseif (y11 <=  y_water_min .and. 
     &        yres_jun_old(R) <= ydropmin(R))then   
        			drybed11 = -1 !1 : Riemann problem is solved                  
              endif
          endif
      
      if (sum == 0)then
          if (drybed11 == -1)then
              if (Nodetype(R,1) == 1)then !inflowing
			    yL = y11; 
                  QL = Q11 
			    yR = max(ydropmin(R), yres_jun_old(R) - Drop(R,1))
			    QR = 0d0;
              elseif (Nodetype(R,1) == 2)then  !outflowing
			    yL = max(ydropmin(R), yres_jun_old(R) - Drop(R,1))
			    QL = 0d0;	
			    yR = y11; 
                  QR = Q11; 
              else
                  write(98,*),'Pipe is not inflowing or outflowing'
			    write(98,*),'subrout. reservoir'
			    call endprog
              endif
		    i = 1 !i is not important here	
		    call Riemann_open(j,i,YL,YR,QL,QR,F11,F12,Ab,Qb,Pw0)
		    call H_from_Area(j,Ab,yb,118,0)
              yb = max(yb,yres_jun_old(R)-dr,YL,YR);
              if (yb > yref(j)) then
                  yb = yref(j) 
              endif                 
              call Area_from_H(j,yb,Ab,Ts,RH,0)     
          elseif(drybed11 == 1)then
			    yb = ydry(j); Qb = 0d0 
          endif
              
          if (drybed11 == -1 .or. drybed11 == 1)then
              if (Nodetype(R,1) == 2)then !outflowing
			    !sign for Qb is negative for upstream reservoir
			    inflowtemp = (-Qb - Outflow_limited(R))*dt
		    Elseif (Nodetype(R,1) == 1)then !inflowing	
			    inflowtemp = (Qb - Outflow_limited(R))*dt 
		    else
			    write(98,*),'Nodetype(R,1) .ne. 1,2'
			    write(98,*),'Subr. reservoirs4'
			    call endprog
		    Endif
              tol_local = Tol_int_10_10 !This is depth tolerance
	        parint1000 = 0
	        n = 1
              x(1) = yres_jun_old(R)
	        param1 = inflowtemp
	        param2 = Stora_old
              param12 = Areser_depth
	        parint3 = R
              call hybrd1 (Reser__noconverg,n,x,fvec,tol_local,info)
	        call converg (conver_result,info)	
	        If(conver_result == 0 .and. parint1000 == 0)then	
                  yres = x(1)
              elseif(conver_result == 1 .or. parint1000 == 1)then	
		        !Old fluxes are used
		        !call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
		        write(99,*),'No converg.7. Subr.Reservoirs, node',temp_id
                  !yres = yres_jun_old(R) + inflowtemp/Areser_depth
                  yres = yres_jun_old(R); Qb = 0d0
	        else
                  write(98,*),'conver_result .ne. 0,1'
		        write(98,*),'Subr. reservoirs2'
		        call endprog
              endif
              goto 30 
          Endif
      endif
      !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$      
      
      !This is only for dropshafts and reservoirs
      if (abs(dr) <= 0.001*d(j))then
          flowcaseIA1 = 2 !constant state
      endif      
      
	u11=Q11/A11	
	call Pressure_Pho(j,y11,P_pho11,Id1)
	If (Id1==0)then
		call Area_from_H(j,y11,AA,T1,RH,Id1)
		c11 = sqrt(g*A11/T1)
	elseif(Id1 == 1)then
		c11 = pc1(j)
	else
		write(98,*),'Id1 .ne. 0,1'
		write(98,*),'Subr. Reservoirs'
		call endprog
      endif      
      
	!To check if we need to solve the equations at the boundary or not
	if (sum == 0)then
		if (y11 <= (1d0+Tol_int_10_6)*ydry(j) .and. yres_jun_old(R) <= 
     &			 ydropmin(R))then
			!Flux computation
			F11 = 0d0; F12 = fluxdry(j)
			Idb = 0
			if (Nodetype(R,1) == 2)then !outflowing
				Abound(j,1) = Adry(j); Qbound(j,1) = 0d0
				ybound(j,1) = ydry(j)
				Fupst(j,1) = F11; Fupst(j,2) = F12
				Pres_pho_Bound(j,1) = P_pho_dry(j)
			Elseif (Nodetype(R,1) == 1)then !inflowing
				Abound(j,2) = Adry(j); Qbound(j,2) = 0d0
				ybound(j,2) = ydry(j)
				Fdownst(j,1) = F11;	Fdownst(j,2) = F12
				Pres_pho_Bound(j,2) = P_pho_dry(j)
			Endif
			yres = yres_jun_old(R) 
			yres_jun_old(R) = yres
			goto 200
              endif
      endif
	
   	n = 3
	yres =  yres_jun_old(R)
	
	!To start interation use y11 and Q11. These values are very stable 
	!compared to those of yb_oldIA(j) and Qb_oldIA(j)
      
      if (flowcaseIA1 == 2)then 
          x(1) = yres - dr; x(2) = yres; x(3) = Q11 !x(3) = 0d0
      else 
	    x(1:n) = (/y11,yres,Q11/)
      endif
      	
	!Stora_old = volume of storage at y = yres_old (time t)
	!Stora_new = volume of storage at y = yres_new (time t+dt)
	
	param1 = A11; param2 = Q11; param3 = c11; 
	param4 = y11; param5 = Stora_old
	param6 = P_pho11; param7 = dt; param8 = k1
      param12 = Areser_depth
	parint1 = j; parint2 = sum; parint3 = R
	parint4 = Idb; parint5 = flowcaseIA1
	parint6 = FLOW_REGIA1
	parint7 = cond_mixed

	!Choosing appropiate tolerance for solving equations	
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	if (yres_jun_old(R) < 0.3*yref(j))then
          tol_local = Tol_int_10_8   
      else
          tol_local = Tol_int_10_8
	endif 	    
		    
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	parint1000 = 0
      Counter_printing = 0
	call hybrd1 (Reser, n, x, fvec, tol_local, info)
	call converg (conver_result,info)
      IF (conver_result == 0 .and. parint1000 == 0)then
          yb = x(1); yres = x(2); Qb = x(3)     
          !write(99,*),'XXXXXXX RESERVOIR BOUNDARY CONVERGED XXXXXXXXXXX'	
      ElseIf(conver_result == 1 .or. parint1000 == 1)then	          
		!Old fluxes are used
		!write(99,*),'No converg. 2. Subr. reservoirs, node',R
		!goto 150
		!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
		parint1000 = 0          
          if (sum == 1)then
              If(cond_mixed == 2)then
                  write(99,*),'cond_mixed2. No conv. Subr.Reserv.Node',R	
                  yb =  yref(j) !y11
                  !flow in adjacent cell is pressurized, and water level at pond 
                  !is below pipe crown (pressur. flow)						
                  delta_h = yb - (yres_jun_old(R)-Drop(R,1))
                  if (delta_h <= 0d0)then
                      delta_h = 0d0
                  endif
                          
                  veloc_max = 10 !m/s
                  Q_flux_max = veloc_max*Aref(j)
                  Qvalue = Aref(j)*0.6*sqrt(2d0*g*delta_h)
                  Q_magnitude = min(Q_flux_max,Qvalue)
                          
                  if (Nodetype(R,1) == 2)then !outflowing
                      if (delta_h>0)then
                          Qb = -Q_magnitude
                      else
                          Qb = 0d0
                      endif
				    !Negative sign because flow is entering the reservoir
                  else !inflowing				
				    if (delta_h>0)then
                          Qb = Q_magnitude
                      else
                          Qb = 0d0
                      endif
                  endif
			    Idb = 1
          elseif(cond_mixed == 1)then
			    write(99,*),'cond_mixed = 1,  No conv. Subr.Reserv.Node',R
			    yb =  yref(j)
			    !flow in adjacent cell is free surface and 
			    !water level at pond is above pipe crown (pressur. flow)
                    temp_con1 = yres_jun_old(R)-Drop(R,1)-y11/2d0  
			    !endif
			    if (temp_con1 <= 0d0)then
				    temp_con1 = 0d0
			    endif
                      
                    veloc_max = 10 !m/s, to limit flux
                    Q_flux_max = veloc_max*Aref(j)
                    Qvalue = 0.6*Aref(j)*sqrt(2d0*g*temp_con1)
                    Q_magnitude = min(Q_flux_max,Qvalue)
    			
			    if (Nodetype(R,1) == 2)then !outflowing
				    Qb = Q_magnitude
				    !Negative sign because flow is entering the reservoir
			    else !inflowing				
				    Qb = -Q_magnitude
			    endif
			    Idb = 0
              else
                  write(98,*),'cond_mixed n.e. 1,2'
                  write(98,*),'Subr. reservoirs'
                  call endprog    
              endif
          elseIf (sum == 0)then 
              write(99,*),'Open channel,  No conv. Subr.Reserv.Node',R              
              if (Nodetype(R,1) == 1)then !inflowing
				yL = y11; 
                  QL = Q11 
				yR = max(ydropmin(R), yres_jun_old(R) - Drop(R,1))
				QR = 0d0;
              elseif(Nodetype(R,1) == 2)then !outflowing
				yL = max(ydropmin(R), yres_jun_old(R) - Drop(R,1))
				QL = 0d0;	
				yR = y11; 
                  QR = Q11; 
              endif
			i = 1 !i is not important here	
			call Riemann_open(j,i,YL,YR,QL,QR,F11,F12,Ab,Qb,Pw0)                      
			call H_from_Area(j,Ab,yb,118,0)                      
              yb = max(yb,yL,yR);
              call Area_from_H(j,yb,Ab,Ts,RH,0)                      
			Idb = 0
              Hmax = max(YL,YR, yref(j))
              yc = 2d0/3d0*Hmax
              call Area_from_H(j,yc,Ac,Ts,RH,0) 
              Qc = Ac*sqrt(2d0*g*yc)
              if (abs(Qb) > Qc)then
                  Qb_temp = Qc*sign(1d0,Qb)  
                  Qb = Qb_temp
                  yb = yc
              endif  
          elseif (sum == 2)then
              write(99,*),'Pressuriz,  No conv. Subr.Reserv.Node',R            
              yb = yres_jun_old(R) - Drop(R,1); Qb = 0d0
              if (yb <= yref(j))then
                  Idb = 0
              else 
                  Idb = 1
              endif
          else
              write(98,*),'sum n.e. 0,1,2'
              write(98,*),'Subr. Subr. reservoirs'
              call endprog 
		endif      
		
		!Storage equation		
		!If reservoir water depth is very small pumping outflow must be zero
		If (yres_jun_old(r) <= ydropmin(R))then 
			Outflow_limited(R) = 0d0
		endif

		if (Nodetype(R,1) == 2)then !outflowing
			!sign for Qb is negative for upstream reservoir
			inflowtemp = (-Qb - Outflow_limited(R))*dt
		Elseif (Nodetype(R,1) == 1)then !inflowing	
			inflowtemp = (Qb - Outflow_limited(R))*dt 
		else
			write(98,*),'Nodetype(R,1) .ne. 1,2'
			write(98,*),'Subr. reservoirs'
			call endprog
		Endif
				
		!If inflow to the reservoir is very small (near zero, we don't need to solve 
		!the iterative equation at the reservoir.
		!write(99,*),'Qb,Outflow_limited(R)',Qb,Outflow_limited(R)
		!write(99,*),'inflowtemp,Stora_old',inflowtemp,Stora_old		       
          tol_local = Tol_int_10_10 !This is depth tolerance
	    parint1000 = 0
	    n = 1 
	    x(1) = yres_jun_old(R)
	    param1 = inflowtemp
	    param2 = Stora_old
          param12 = Areser_depth
	    parint3 = R
	    call hybrd1 (Reser__noconverg, n, x, fvec, tol_local, info)
	    call converg (conver_result,info)	
	    If(conver_result == 0 .and. parint1000 == 0)then	
              yres = x(1)
          elseif(conver_result == 1 .or. parint1000 == 1)then	
		    !Old fluxes are used
		    call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
		    write(99,*),'No converg. 9. Subr. Reservoirs, node',temp_id
              !yres = yres_jun_old(r) + inflowtemp/Areser_depth
              yres = yres_jun_old(r); Qb = 0d0  
	    else
              write(98,*),'conver_result and parint1000 .ne. 0,1'
		    write(98,*),'Subr. reservoirs2'
		    call endprog
	    endif
25		continue
      else	
	    write(98,*),'conver_result .ne. 0,1 .and. parint1000 .ne. 0,1'
		write(98,*),'Subr. reservoirs1'
		call endprog
	endif	

	!flux computation
30	If (ISNAN(yb) .or. ISNAN(yres) .or. ISNAN(Qb))then 
		    write(98,*),'NaN is found in Subr. reservoir1' 
		    write(98,*),'yb,yres,Qb',yb,yres,Qb
		    call endprog
	endif
 	call Area_from_H(j,yb,Ab,Tb,RH,Idb)
	call Pressure_Pho(j,yb,P_phob,Idb) 
 	F11 = Qb; F12 = Qb**2d0/Ab + P_phob	
	if (Nodetype(R,1) == 2)then !outflowing
		    Abound(j,1) = Ab; Qbound(j,1) = Qb
		    ybound(j,1) = yb
		    Fupst(j,1) = F11; Fupst(j,2) = F12
		    Pres_pho_Bound(j,1) = P_phob
	Elseif (Nodetype(R,1) == 1)then !inflowing
		    Abound(j,2) = Ab; Qbound(j,2) = Qb
		    ybound(j,2) = yb
		    Fdownst(j,1) = F11;	Fdownst(j,2) = F12
		    Pres_pho_Bound(j,2) = P_phob
	else
		    write(98,*),'Nodetype(R,1) .ne. 1,2'
		    write(98,*),'Subr. Reservoirs'
		    call endprog
	Endif 
	yres_jun_old(R) = yres
	goto 200
 150  continue
 	yres = yres_jun_old(r)
 	yres_jun_old(r)	 = yres
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

 200	if(yres_jun_old(R) > 0.99*reser_maxdepth(R))then
		write(98,*),'Storage curve has been exceeded, Reservoir node =', R
		write(98,*),'This also may mean that this reservoir is full'				
		write(98,*),'Elapsed time (s) =',t_global
		If (abs(Error_volume) > 5d0)then		
			write(98,*),'Error in volume (%) = ',Error_volume      			
		endif
		write(98,*),'Subr. Reservoirs'
		call ENDPROG_noerror
      endif
      
      If (yres_jun_old(r) < ydropmin(R))then
          yres_jun_old(r) = ydropmin(R)
      endif      
	return
      end

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	subroutine Reser (n, x, fvec, iflag )
	use common_module
	implicit none	
	integer n,p, iflag, i, j, R, sum,Idb,cond_mixed
	integer flowcaseIA1,FLOW_REGIA1
	double precision fvec(n), x(n)	
	double precision A11, Q11, u11, c11, y11, P_pho11, Stora_old
	double precision Ab, ub, cb, P_phob, Tb, RH,temp8,Areser_depth
	double precision dt,Stora_new,K1,y,A,Ts,Ares_aprox,con1,con2
	double precision temp_con1,temp_con2,temp_con3,temp_con4,res_temp
      integer :: seed
	double precision ran_number

	parint1000 = 0
	A11 = param1; Q11= param2; c11 = param3; 
	y11 = param4; Stora_old = param5; 
	P_pho11 = param6; dt = param7
	k1 = param8; Areser_depth = param12
	
	j = parint1; sum = parint2; R = parint3
	Idb = parint4
	flowcaseIA1 = parint5; FLOW_REGIA1 = parint6
	cond_mixed = parint7
	u11 = Q11/A11
          
      If (ISNAN(x(1)))then 
          !call random_seed(seed)  
          !call random_number(ran_number)
          !if(sum == 0)then
          !    x(1) = ran_number*yref(j)   !y11  
          !elseif(sum == 2)then
          !    x(1) = yref(j)  + ran_number*yref(j)      
          !elseif (sum == 1)then
          !    x(1) = ran_number*yref(j)   !y11  
          !else 
          !    write(98,*),'sum == 2 .ne. 0,2'
          !    write(98,*),'subrout. dropshaft'
          !    call endprog
          !endif	
          x(1) = y11
          parint1000 = 1
      endif      
          
      
      If (ISNAN(x(2)))then 		
		x(2) = yres_jun_old(R)  
          parint1000 = 1
      endif      

	If (ISNAN(x(3)))then 	         
          call random_seed(seed)  
          call random_number(ran_number)              
          x(3) = ran_number*Qcrit_maxIA(j) ! Q11
          parint1000 = 1 
      endif
      
          
      If (x(2) > reser_maxdepth(R))then 		
		x(2) = reser_maxdepth(R)
		parint1000 = 1 
      endif

	!yb = x(1); yres = x(2); Qb = x(3)
	if(sum == 0 .or. sum == 2)then
          If (Idb == 0)then
			If (x(1) >= b2_max(j))then			
				x(1) = b2_max(j)
                  parint1000 = 1
			endif
			If (x(1) < 0.01*b2_min(j))then	
				x(1) = 0.01*b2_min(j)
				x(3) = 0d0
                  parint1000 = 1
			endif
			call Area_from_H(j,x(1),Ab,Tb,RH,0)
			cb = sqrt(g*Ab/Tb)
		elseif (Idb == 1)then
			call Area_from_H(j,x(1),Ab,Tb,RH,1)
		else
			write(98,*),'Idb undefined in routine Dropshaft_general2'
			write(98,*),'node,pipe,Idb',R,j,Idb
			write(98,*),'cond_mixed,flowcaseIA1,FLOW_REGIA1',
     &        cond_mixed,flowcaseIA1,FLOW_REGIA1
			write(98,*),'Idb undefined in routine Dropshaft_general2'
			call endprog
		endif			
		
		!If reservoir water depth is very small pumping outflow must be zero
          If (x(2) < 0.5*ydropmin(R))then 
			x(2) = 0.5*ydropmin(R)
			Outflow_limited(R) = 0d0
              parint1000 = 1
          endif              
		
		ub = x(3)/Ab	

		if (Nodetype(R,1) == 2)then !outflowing		
			if (sum == 2)then		
				fvec(1) = ub - pc1(j)*LOG(Ab) - (u11 - pc1(j)*LOG(A11))
			elseif (sum == 0)then			
				fvec(1) = ub - u11 - (cb + c11)*(Ab - A11)/(Ab + A11)
			else
				write(98,*),'sum .ne. 0,2. Subr. Reservoirs'
				call endprog
			endif
		Elseif (Nodetype(R,1) == 1)then !inflowing
			if (sum == 2)then
				fvec(1) = ub + pc1(j)*LOG(Ab) - (u11 + pc1(j)*LOG(A11))
			elseif (sum == 0)then 
				fvec(1) = ub - u11 + (cb + c11)*(Ab - A11)/(Ab + A11)
			else
				write(98,*),'sum .ne. 0,2. Subr. Reservoirs'
				call endprog
			endif
		else
			write(98,*),'Nodetype(R,1) .ne. 1,2'
			write(98,*),'Routine Reservoirs'
			call endprog
		endif	
	endif

	!Constant state, critical flow, normal flow,pressurized/freefall		
	if (sum == 0)then 
		if (Nodetype(R,1) == 2)then !outflowing			    
			If(flowcaseIA1 == 1)then !free outfall or normal flow 
				If(FLOW_REGIA1 == 10)then				
					!negative sign for critical depth (negative flow)					
					fvec(1) = -ub - cb					
                      !fvec(2) = Drop(R,1) + x(1) + con1 - x(2) !Energy equation neglecting losses
                      fvec(2) = Drop(R,1) + x(1) - x(2) !Energy equation neglecting losses
					!Riemann invariants can not be used for outflowing 
					!pipes and supercritical flows					
					!fvec(1) = x(2)-Drop(R,1)-x(1)
					!positive sign for critical depth (positive flow)					
					fvec(1) = ub - cb
                      fvec(2) = Drop(R,1) + x(1) - x(2) !Energy equation 
				else
					write(98,*),'# of FLOW_REGIA1 is not supported'
					write(98,*),'Routine Dropshaft_general'
					call endprog
				endif
              elseIf (flowcaseIA1 == 2)then !Constant head
                  if (Nodetype(R,1) == 2)then !outflowing
		            if (u11 - c11 >= 0d0)then !the characteristic doesn't connect the boundary 
                          fvec(1) = ub - cb
                          !fvec(1) = x(3) - Q_open
                      endif                   
	            Elseif (Nodetype(R,1) == 1)then !inflowing
		            if (u11 + c11 <= 0d0)then !the characteristic doesn't connect the boundary 
                          fvec(1) = ub + cb
                          !fvec(1) = x(3) - Q_open !we can solve Riemann problem
                      endif  
                  else      
                      write(98,*),'Unknown condition. Subr. dropshaft'
                      call endprog
	            endif 
                  fvec(2) = Drop(R,1) + x(1) - x(2) !Energy equation      
			else
				!If the flow is supercritical in the outflowing pipe
				!we may have critical flow at the boundary (posit.) 
				!direction. We can add this later.
				write(98,*),'flowcaseIA1 is not supported'
				write(98,*),'Routine Dropshaft_general'
				call endprog
			endif
		elseIf(Nodetype(R,1) == 1)then !inflowing 
			If(flowcaseIA1 == 1)then	!free outfall or normal flow 
				!free outfall or normal flow 
				If(FLOW_REGIA1 == 1)then	!critical flow
					fvec(2) = ub - cb
				elseif(FLOW_REGIA1 == 2)then !normal flow
					fvec(2) = ub - 1d0/nm(j)*
     &			RH**(2d0/3d0)* sqrt(S0(j))
				else
					write(98,*),'# of FLOW_REGIA1 is not supported'
					write(98,*),'Routine Dropshaft_general'
					call endprog
				endif
                  elseIf (flowcaseIA1 == 2)then !Constant head
                      fvec(2) = Drop(R,1) + x(1) - x(2) !Energy equation
                      if (Nodetype(R,1) == 2)then !outflowing
		                if (u11 - c11 >= 0d0)then !the characteristic doesn't connect the boundary 
                              fvec(1) = ub - cb
                          endif                   
	                Elseif (Nodetype(R,1) == 1)then !inflowing
		                if (u11 + c11 <= 0d0)then !the characteristic doesn't connect the boundary 
                              fvec(1) = ub + cb
                          endif  
                      else      
                          write(98,*),'Nodetype n.e.1,2. Subr.dropshaft'
                          call endprog
	                endif
			else
				write(98,*),'flowcaseIA1 is not supported'
				write(98,*),'Routine Dropshaft_general'
				call endprog
			endif	
		else
			write(98,*),'Nodetype(R,1) .ne. 1,2'
			write(98,*),'Routine Dropshaft_general'
			call endprog
		endif
	elseif (sum == 2)then
			If(flowcaseIA1 == 2)then
				if (Nodetype(R,1) == 2)then !outflowing	
                      fvec(2) = Drop(R,1) + x(1) - x(2) !Energy equation
				elseIf(Nodetype(R,1) == 1)then !inflowing 							
                      fvec(2) = Drop(R,1) + x(1) - x(2) !Energy equation       
				else
					write(98,*),'unknown condit. Dropsh. routine'
					call endprog
				endif
			else
				write(98,*),'11,sum,flowcaseIA(j)',sum,flowcaseIA1
				write(98,*),'flowcaseIA1 undef. in subr. Dropshaft_general'
				call endprog
			endif
	elseif (sum == 1)then
		If(cond_mixed == 2)then
			temp_con1 = y11 - (x(2)-Drop(R,1)) 
			if(temp_con1 < 0d0)then
				temp_con1 = 0d0
			endif
			temp_con3 = 0.6*sqrt(2d0*g*temp_con1)							
			if (Nodetype(R,1) == 2)then !outflowing
				fvec(1) = x(1) - yref(j) 
				fvec(2) = x(3) + temp_con3*Aref(j) 
				!Negative sign because flow is entering the reservoir
			elseif(Nodetype(R,1) == 1)then !inflowing
				!fvec(1) = ub-u11+(cb+c11)*(Ab-A11)/(Ab+A11)					
				fvec(1) = x(1) - yref(j)
				fvec(2) = x(3) - temp_con3*Aref(j) !Normalization
			else
				write(98,*),'Nodetype(R,1) .ne. 1,2. Subr. Dropshaft_general'
				call endprog
			endif
		elseif(cond_mixed == 1)then
			!flow in adjacent cell is free surface and 
			!water level at pond is above pipe crown (pressur. flow)
			temp_con1 = x(2)-Drop(R,1) - y11/2d0
			if(temp_con1 < 0d0)then
				temp_con1 = 0d0
			endif
			!To make sure that as maximum 50% of the volume from the adjacent 
			!cell would enter the reservoir. 					
			temp_con3 = 0.6*sqrt(2d0*g*temp_con1)              
			if (Nodetype(R,1) == 2)then !outflowing
				!fvec(1) = ub-u11-(cb+c11)*(Ab-A11)/(Ab + A11)
				fvec(1) = x(1) - yref(j) 
				fvec(2) = x(3) - temp_con3*Aref(j) !Normalization
				!Negative sign because flow is entering the reservoir
			elseif(Nodetype(R,1) == 1)then !inflowing
				!fvec(1) = ub-u11+(cb+c11)*(Ab-A11)/(Ab+A11)					
				fvec(1) = x(1) - yref(j)
				fvec(2) = x(3) + temp_con3*Aref(j) !Normalization
			else
				write(98,*),'Nodetype(R,1) .ne. 1,2. Subr. Dropshaft_general'
				call endprog
			endif
		else
			write(98,*),'cond_mixed not supported'
			write(98,*),'cond_mixed .ne. 1,2. Subr. Dropshaft_general'
			call endprog
		endif		
	else
		write(98,*),'sum .ne. 0,1,2. Subr. Dropshaft_general'
		call endprog
	endif
	
	!Storage equation
	call get_storage(R,x(2),Stora_new) 
	if (Nodetype(R,1) == 2)then !outflowing
		!sign for x(3) is negative for upstream reservoir
		res_temp = (-x(3) - Outflow_limited(R))*dt
	Elseif (Nodetype(R,1) == 1)then !inflowing
		res_temp = (x(3) - Outflow_limited(R))*dt
	else
		write(98,*),'Nodetype(R,1) .ne. 1,2'
		write(98,*),'Subr. reservoir'
		call endprog
	Endif
	fvec(3) = res_temp/Areser_depth - (Stora_new-Stora_old)/Areser_depth
      !fvec(3) = res_temp - (Stora_new-Stora_old)
      !write(99,*), 'yres', yres
      
          if (T_GLOBAL > 20000000 .and. T_GLOBAL < 30000000)then 
              Counter_printing =  Counter_printing+1      
              write(99,*),'_____________________' 
              write(99,*),'T_GLOBAL',T_GLOBAL   
              write(99,*),'T_GLOBAL+DT',T_GLOBAL+dt   
              write(99,*),'ITERATION',Counter_printing              
              do j = 1, NodeNS(r)
		        p=2*j-1        
      !            write(99,*),'Node R,j = ',R,1
                  write(99,*),'parint1000',parint1000 
                  write(99,*),'Drop(R,1)',Drop(R,1)
                  write(99,*),'x(1)',x(p) 
                  write(99,*),'x(2)',x(p+1)      
                  write(99,*),'x(3)',x(3)      
                  write(99,*),'fvec(p)',fvec(p)
                  write(99,*),'fvec(p+1)',fvec(p+1)
                  write(99,*),'fvec(3)',fvec(3)
                  write(99,*),'SumIDFIA(j) = ',SumIDFIA(j)
                  write(99,*),'Nodetype(r,j) = ',Nodetype(r,j)
                  write(99,*),'flowcaseIA(j) = ',flowcaseIA(j)
                  write(99,*),'flow_regIA(j)=',flow_regIA(j)
          if (abs(x(p))>100.or.abs(x(p+1))>100.or.abs(x(n))>100)then 
                   write(99,*),'attention'   
          endif
              end do
              write(99,*),'_____________________'
          endif
      
 400	return
	end

	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	subroutine Reser__noconverg(n, x, fvec, iflag )
	use common_module
	implicit none	
	integer n, iflag, R	
	double precision fvec(n), x(n)
	double precision Stora_new,Stora_old,inflowtemp,Areser_depth
	parint1000 = 0	
	inflowtemp = param1
	Stora_old = param2
      Areser_depth = param12
	R = parint3	
      If (x(1) > reser_maxdepth(R))then 		
		x(1) = reser_maxdepth(R)
		parint1000 = 1 
      endif
      If (x(1) < ydropmin(R))then 		
		x(1) = ydropmin(R)
		parint1000 = 1 
      endif
	call get_storage(R,x(1),Stora_new) 			
	fvec(1) = Stora_new/Areser_depth-(inflowtemp+Stora_old)/Areser_depth
      !fvec(1) = Stora_new-(inflowtemp+Stora_old)
	return
	end
