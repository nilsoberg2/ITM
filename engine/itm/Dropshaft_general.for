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

	subroutine Dropshaft_general(tf,Dt,R)
	!This routine is used for computing fluxes at a dropshaft boundary.  
	use common_module 
	implicit none		       
	integer n,r,j,k,p,Idb,Id1,sum,i
	integer conver_result,info
	integer flowcaseIA1,FLOW_REGIA1,cond_mixed
	double precision fvec(3),x(3)
	double precision P_phob,AIA,TsIA,RHIA,Wpred
	double precision Ab,yb,Qb,F11,F12
	double precision u11,Q11,temp1,temp3
	double precision tf,Dt,test,der_P_phoIA,P_pho
	double precision c11,y11,A11,P_pho11,hdropsh	
	double precision AA,Ts,RH,AL,AR,Area,Tb
	double precision y,A,AStar,Qstar,h0b1,A0b1,Q0b1
	double precision Qinflow,Qinf_new,Qinf_old,Ares
	double precision con1,con2,con3
	double precision k1,dr,Ref_level,yres
	double precision temp_con1,temp_con2,temp_con3,temp_con4,temp_con5
	double precision Qsum,Error_y,tol_local
	double precision yw0,Aw0,Qw0,FF1,FF2,yL,YR,QL,QR,Pw0
      double precision delta_h, Q_flux_max, vmax, Qvalue
      double precision Q_max
      double precision Hmax,yc,Ac,Qc,Qb_temp
                      
	character*25 temp_id
	integer CODL, Idf0	
	external dropsh
	k = NodeID(R,1)
	dr = Drop(R,1)				
	call get_inflow(R,tf,Qinf_old)
	call get_inflow(R,tf+Dt,Qinf_new)
		
	!To enforce that the minimum water depth at pond is ydropmin(R)      
      if (yres_jun_old(R) < ydropmin(R))then !ydropmin(R))then
		yres_jun_old(R) = ydropmin(R)
      endif
	
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
	Ares = Ares_junct(R)

	!Local head loss coefficient	
 	if (Nodetype(R,1) == 2)then !outflowing  
		K1 = Klocal(k,1)	
	elseIf(Nodetype(R,1) == 1)then
		K1 = Klocal(k,2)
	else
		write(98,*),'Nodetype .ne. 1,2'
		write(98,*),'Subr. Dropshaft_general'
		call endprog
      endif        
	      
	if (Nodetype(R,1) == 2)then !outflowing
            i = 3
              call ERP_combined(k,i,DT_GLOBAL,y11,A11,Q11,Id1)					
		    Ab_oldIA(1) = Abound(k,1); Qb_oldIA(1) = Qbound(k,1)
		    yb_oldIA(1) = ybound(k,1)		    
		    Idb = Idflow(k,2)
	Elseif (Nodetype(R,1) == 1)then !inflowing
	      i = Nx(k)-2
		    call ERP_combined(k,i,DT_GLOBAL,y11,A11,Q11,Id1)			
		    Ab_oldIA(1) = Abound(k,2); Qb_oldIA(1) = Qbound(k,2)
		    yb_oldIA(1) = ybound(k,2)		    
		    Idb = IdFlow(k,Nx(k)-1)
	Else
		    write(98,*),'Nodetype(R,1) .ne. 1,2'
		    write(98,*),'Subr. Dropshaft_general'
		    call endprog
	Endif	  
      	
      call Freesurface_flowregime(R,k,dr,Id1,Q11,y11,A11,
     & FLOW_REGIA1,flowcaseIA1,Nodetype(R,1),cond_mixed,
     & sum)      
           
	u11=Q11/A11
	call Pressure_Pho(k,y11,P_pho11,Id1)
	If (Id1==0)then
		    call Area_from_H(k,y11,AA,Ts,RH,Id1)
		    c11 = sqrt(g*A11/Ts)
      elseIf (Id1==1)then
		    c11 = pc1(k)
      else
              write(98,*),'Id1 .ne. 0,1'
		    write(98,*),'Subr. Dropshaft_general'
		    call endprog
	endif
		
	!To check if we need to solve the equations at the boundary or not
	if (y11 <= ydropmin(R) + ydry(k) .and. yres_jun_old(R) <= 
     &	    ydropmin(R))then !Note that here yres is open channel
		    !Flux computation
			F11 = 0d0; F12 = fluxdry(k)
			if (Nodetype(R,1) == 2)then !outflowing
			    Abound(k,1) = Adry(k); Qbound(k,1) = 0d0
			    ybound(k,1) = ydry(k)
			    Fupst(k,1) = F11; Fupst(k,2) = F12
			    Pres_pho_Bound(k,1) =  P_pho_dry(k)
			Elseif (Nodetype(R,1) == 1)then !inflowing
			    Abound(k,2) = Adry(k); Qbound(k,2) = 0d0
			    ybound(k,2) = ydry(k)
			    Fdownst(k,1) = F11;	Fdownst(k,2) = F12
			    Pres_pho_Bound(k,2) =  P_pho_dry(k)
		    else
			    write(98,*),'Unknown Nodetype(R,1). Subr. dropshaft'
			    call endprog
		    Endif 
		    Qb = 0d0
		    temp3 = 5d-1*(Qinf_new+Qinf_old)
		    if (Nodetype(R,1) == 2)then !outflowing
			    !sign for Qb is negative for upstream reservoir
			    temp1 = - Qb
		    Elseif (Nodetype(R,1) == 1)then !inflowing
			    temp1 = Qb
		    else
			    write(98,*),'unknown condition'
			    write(98,*),'Subr. Dropshaft'
			    call endprog
		    Endif
		    yres = yres_jun_old(R) + (temp3+temp1)*dt/Ares
		    goto 200
      endif
      
	!solving equations: free surface, pressurized and combined
  	!Pure free surface flow or combined 
16	param1 = A11; param2 = Q11; param3 = c11; param4 = y11
	param7 = dt; param8 = k1
	param9 = Ares; param10 = Qinf_new; param11 = Qinf_old
	param15 = tf

	parint1 = k; parint2 = sum; parint3 = R
	parint4 = Idb; parint5 = flowcaseIA1
	parint6 = FLOW_REGIA1	
	parint7 = cond_mixed	
	n = 3
	yres =  yres_jun_old(R)
      
      if (flowcaseIA1 == 2)then 
          x(1) = yres_jun_old(R)-dr; x(2) = yres_jun_old(R); x(3) = Q11 !x(3)=0d0
      else 
	    x(1:n) = (/y11,yres_jun_old(R),Q11/)
      endif	
          
        !Riemann problem will be always solved at dropshafts when the flow is open channel.
      If(sum == 0)then  
          temp3 = Qinflow*Dt/Ares 
          if (Nodetype(R,1) == 1)then !inflowing
              yL = y11; 
              QL = 0d0 !Q11 
		    yR = max(ydropmin(R), yres_jun_old(R) + temp3 - Drop(R,1))
		    QR = 0d0;
          elseif(Nodetype(R,1) == 2)then !outflowing
		    yL = max(ydropmin(R), yres_jun_old(R) + temp3 - Drop(R,1))
		    QL = 0d0;	
		    yR = y11; 
              QR = 0d0 !Q11; 
		endif
		i = 1 !i is not important here	
		call Riemann_open(k,i,YL,YR,QL,QR,F11,F12,Ab,Qb,Pw0)
		call H_from_Area(k,Ab,yb,118,0)
		param12 = Qb  !Which flow should be used depending on inflow and outflow. Revised thsi 
      endif   

	!Appropiate tolerance for solving equations	
	If(sum == 2)then	
          tol_local = Tol_int_10_8
	elseif(sum == 0)then
          if (yres_jun_old(R) < 0.3*yref(k))then
              tol_local = Tol_int_10_6		        
          else
              tol_local = Tol_int_10_6
          endif             
          !write(99,*),'Open channel.dropsh_rout., Node',R             
	elseif(sum == 1)then
          tol_local = Tol_int_10_7
      else
          write(98,*),'sum .ne. 0,1,2'
          write(98,*),'Subr. Dropshaft_general'
          call endprog
	endif
		
	call hybrd1 (dropsh,n,x,fvec,tol_local,info)
	call converg (conver_result,info)
      
      If(conver_result == 0 .and. parint1000 == 0)then		
		yb = x(1); yres = x(2); Qb = x(3)
		!if (cond_mixed == 1)then ! .or. cond_mixed == 4)then
			! Idb = 0
		!elseif (cond_mixed == 2)then
			! Idb = 1
          !endif
	elseIf(conver_result == 1 .or. parint1000 == 1 )then	
		!Old fluxes are used
          call itm_get_swmm_id(0, R, trim(temp_id)) ! 0 for nodes
		write(99,*),'No converg. Subr. Dropsh_gen node=',trim(temp_id)
		write(99,*),'t,sum_no_conv_dropsh',t_global,sum_no_converg
              
          j = 1; k = NodeID(r,j); p=2*j-1; SumIDFIA(j) = sum
          y1(j) = y11; A1(j) = A11; Q1(j) = Q11; IDf1(j) = Id1
          cond_mixed1(j) = cond_mixed
          dropIA(j) = Drop(R,j)
          
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
     &    CODL,yL,yR,AL,AR,QL,QR,F11)
          Qb = F11
          
          
       !  if (sum == 1)then
       !       vmax = 20.0 !max velocity = 20 m/s
       !       if (cond_mixed == 2)then
       !           temp1 = y11 - (yres_jun_old(r)-Drop(R,1))
       !           if (temp1 < 0d0)then
       !               temp1 = 0d0
       !           endif
       !       elseif(cond_mixed == 1)then
       !           temp1 =yres_jun_old(r)-Drop(R,1)-y11/2d0
       !           if (temp1 < 0d0)then
       !               temp1 = 0d0
       !           endif
       !       else 
       !           write(98,*),'cond_mixed1(j) .ne. 1,2'
			    !write(98,*),'subrout. junction_general'
       !           call endprog
       !       endif                 
       !       vmax = 0.6*sqrt(2d0*g*temp1) !max velocity = 20 m/s
       !       vmax = min(vmax,20d0)
      ! !       Q_max = vmax*Aref(j)
      !            
		    !If(cond_mixed == 2)then
			   ! write(99,*),'cond_mixed=2. No conv. Subr.Dropsh, node',R	
      !            yb =  yref(j) !y11
			   ! !flow in adjacent cell is pressurized, and water level at pond 
      !            !is below pipe crown (pressur. flow)						
			   !                 
      !            delta_h = y11 - (yres_jun_old(r)-Drop(R,1))
			   ! if (Nodetype(R,1) == 2)then !outflowing
      !                if (delta_h>0)then
      !                    Qb = -Q_max 
      !                else
      !                    Qb = 0d0
      !                endif
				  !  !Negative sign because flow is entering the reservoir
      !            else !inflowing				
				  !  if (delta_h>0)then
      !                    Qb = Q_max 
      !                else
      !                    Qb = 0d0
      !                endif
			   ! endif
		    !elseif(cond_mixed == 1)then
			   ! write(99,*),'cond_mixed = 1, Subr. Dropsh_gen., node',R
			   ! yb =  yref(j)
			   ! !flow in adjacent cell is free surface and 
			   ! !water level at pond is above pipe crown (pressur. flow)
      !                                              
      !            delta_h = yres_jun_old(r)-Drop(R,1)-y11/2d0
			   ! if (Nodetype(R,1) == 2)then !outflowing
      !                if (delta_h>0)then
      !                    Qb = Q_max 
      !                else
      !                    Qb = 0d0
      !                endif
				  !  !Negative sign because flow is entering the reservoir
      !            else !inflowing				
				  !  if (delta_h>0)then
      !                      Qb = -Q_max 
      !                else
      !                      Qb = 0d0
      !                endif
			   ! endif
      !        else
      !            write(98,*),'cond_mixed n.e. 1,2'
      !            write(98,*),'Subr. Dropshaft_general'
      !            call endprog    
      !        endif
       !       elseIf (sum == 0)then  
       !           write(99,*),'Open channel.dropsh_rout., Node',R
       !           if (Nodetype(R,1) == 1)then !inflowing
			    !	yL = y11; 
       !               QL = Q11 
			    !	yR = max(ydropmin(R), yres_jun_old(R) - Drop(R,1))
			    !	QR = 0d0;
			    !elseif(Nodetype(R,1) == 2)then !outflowing
			    !	yL = max(ydropmin(R), yres_jun_old(R) - Drop(R,1))
			    !	QL = 0d0;	
			    !	yR = y11; 
       !               QR = Q11; 
			    !endif
			    !i = 1 !i is not important here	
			    !call Riemann_open(j,i,YL,YR,QL,QR,F11,F12,Ab,Qb,Pw0)                      
			    !call H_from_Area(j,Ab,yb,118,0)                      
       !           yb = max(yb,yL,yR);
       !           call Area_from_H(j,yb,Ab,Ts,RH,0)                      
			    !Idb = 0
       !           Hmax = max(YL,YR, yref(j))
       !           yc = 2d0/3d0*Hmax
       !           call Area_from_H(j,yc,Ac,Ts,RH,0) 
       !           Qc = Ac*sqrt(2d0*g*yc)
       !           if (abs(Qb) > Qc)then
       !               Qb_temp = Qc*sign(1d0,Qb)  
       !               Qb = Qb_temp
       !               yb = yc
       !           endif  
      !          elseIf (sum == 2)then
      !              write(99,*),'Pressuriz, No conv. dropsh.Node',R      
      !              yb = yres_jun_old(R) - Drop(R,1)
      !              Qb = 0d0                      
      !              if (yb <= yref(j))then
      !                  Idb = 0
      !              else 
      !                  Idb = 1
      !              endif                      
      !          else
      !              write(98,*),'sum n.e. 0,1,2'
      !              write(98,*),'Subr. Dropshaft_general'
      !              call endprog 
		    !endif

 		if (Nodetype(R,1) == 2)then !outflowing
			!sign for Qb is negative for upstream reservoir
			temp1 = -Qb
		Elseif (Nodetype(R,1) == 1)then !inflowing
			temp1 = Qb
          else 
			write(98,*),'Unknown Nodetype(R,1). Subr. dropshaft'
			call endprog
		Endif 					
		temp3 = 5d-1*(Qinf_new+Qinf_old)
          yres = yres_jun_old(R) + (temp3+temp1)*dt/Ares
          goto 200          
      else
          write(98,*),'conver_result n.e. 0,1'
          write(98,*),'Subr. Dropshaft_general'
          call endprog  
	endif
	
	If (ISNAN(yb) .or. ISNAN(yres) .or. ISNAN(Qb))then 
		write(98,*),'NaN is found in Subr. dropshaft1' 
		write(98,*),'yb, Ab or Qb'
		call endprog
	endif

	!Flux computation
	If (Idb == 0)then
		If(yb <= (1d0 + Tol_int_10_12)*ydry(k))then
			yb = ydry(k); Ab = Adry(k); Qb = 0d0
			F11 = 0d0; F12 = fluxdry(k)	
			P_phob = P_pho_dry(k)	
			goto 40				
		endif
	endif

 	call Area_from_H(k,yb,Ab,Ts,RH,Idb)
	call Pressure_Pho(k,yb,P_phob,Idb) 
	F11 = Qb; F12 = Qb**2d0/Ab + P_phob	
 40	if (Nodetype(R,1) == 2)then !outflowing
		Abound(k,1) = Ab; Qbound(k,1) = Qb
		ybound(k,1) = yb;Idflow_bound(k,1) = Idb
		Fupst(k,1) = F11; Fupst(k,2) = F12
		Pres_pho_Bound(k,1) =  P_phob
	Elseif (Nodetype(R,1) == 1)then !inflowing
		Abound(k,2) = Ab; Qbound(k,2) = Qb
		ybound(k,2) = yb; Idflow_bound(k,2) = Idb
		Fdownst(k,1) = F11;	Fdownst(k,2) = F12
		Pres_pho_Bound(k,2) =  P_phob
      else       
          write(98,*),'Unknown Nodetype(R,1). Subr. dropshaft'
          call endprog
	Endif 
200   yres_jun_old(R) = yres       
      end
	
	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	subroutine dropsh(n,x,fvec,iflag)
	use common_module
	implicit none	
	integer n,iflag,r,k,Idb,Id1,flowcaseIA1,FLOW_REGIA1
	integer sum,cond_mixed
	double precision fvec(n),x(n)
	double precision AIA,TsIA,RHIA,u11,Q11,temp1,temp3,temp4
	double precision Qinf_new,Qinf_old,Ares,y11,A11,P_pho11,cb1,c11
	double precision Qb,Ab,ub,yb,ub1,dt,cb,Tb,RH,yres
	double precision con1,con2,con3,con4,con5,Qs,k1,y,A,Ts,tf
	double precision temp_con1,temp_con2,temp_con3,temp_con4,Q_open
      integer :: seed
	double precision ran_number
	parint1000 = 0
	A11 = param1; Q11= param2; c11 = param3; 
	y11 = param4; dt = param7 
	k1 = param8; Ares = param9; Qinf_new = param10
	Qinf_old = param11; Q_open = param12
	tf = param15	

	k = parint1; sum = parint2; R = parint3
	Idb = parint4
	flowcaseIA1 = parint5; FLOW_REGIA1 = parint6
	cond_mixed = parint7
	u11 = Q11/A11
	
	If (x(1) < 0.1*b2_min(k))then	
          x(1) = 0.1*b2_min(k)
          x(3) = 0d0
          parint1000 = 1
      endif
		
	If (ISNAN(x(1)))then 
          call random_seed(seed)  
          call random_number(ran_number)
          if(sum == 0)then
                  x(1) = ran_number*yref(k)   !y11  
          elseif(sum == 2)then
                  x(1) = yref(k)  + ran_number*yref(k)      
          elseif (sum == 1)then
                  x(1) = ran_number*yref(k)   !y11  
          else 
                  write(98,*),'sum == 2 .ne. 0,2'
		        write(98,*),'subrout. dropshaft'
		        call endprog
          endif		
          parint1000 = 1
      endif      

	If (ISNAN(x(2)))then 		
		x(2) = yres_jun_old(R)  
          parint1000 = 1
      endif      

	If (ISNAN(x(3)))then 	         
          call random_seed(seed)  
          call random_number(ran_number)              
          x(3) = ran_number*Qcrit_maxIA(k) ! Q11
          parint1000 = 1 
      endif

	if(sum == 0 .or. sum == 2)then
		If (Idb == 0)then
			If (x(1) >= b2_max(k))then			
				x(1) = b2_max(k)
                  parint1000 = 1
			endif
			If (x(1) < 0.1*b2_min(k))then	
				x(1) = 0.1*b2_min(k)
				x(3) = 0d0
                  parint1000 = 1
			endif
			call Area_from_H(k,x(1),Ab,Tb,RH,0)
			cb = sqrt(g*Ab/Tb)
		elseif (Idb == 1)then
			call Area_from_H(k,x(1),Ab,Tb,RH,1)
		else
			write(98,*),'Idb undefined in routine Dropshaft_general2'
			write(98,*),'node,pipe,Idb',R,k,Idb
			write(98,*),'cond_mixed,flowcaseIA1,FLOW_REGIA1',
     &			cond_mixed,flowcaseIA1,FLOW_REGIA1
			write(98,*),'Idb undefined in routine Dropshaft_general2'
			call endprog
		endif		
		ub = x(3)/Ab	

		if (Nodetype(R,1) == 2)then !outflowing
			if (sum == 2)then		
				fvec(1) = ub - pc1(k)*LOG(Ab) - (u11 - pc1(k)*LOG(A11))
			elseif (sum == 0)then			
				fvec(1) = ub - u11 - (cb + c11)*(Ab - A11)/(Ab + A11)
			else      
                  write(98,*),'Unknown condition sum. Subr. dropshaft'
                  call endprog
			endif
		Elseif (Nodetype(R,1) == 1)then !inflowing
			if (sum == 2)then
				fvec(1) = ub + pc1(k)*LOG(Ab) - (u11 + pc1(k)*LOG(A11))
              elseif (sum == 0)then 
				fvec(1) = ub - u11 + (cb + c11)*(Ab - A11)/(Ab + A11)
			else      
                  write(98,*),'Unknown condition sum. Subr. dropshaft'
                  call endprog
			endif
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
				elseif(FLOW_REGIA1 == 11)then
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
					fvec(2) = ub - 1d0/nm(k)*
     &			RH**(2d0/3d0)* sqrt(S0(k))
				else
					write(98,*),'# of FLOW_REGIA1 is not supported'
					write(98,*),'Routine Dropshaft_general'
					call endprog
				endif
              elseIf (flowcaseIA1 == 2)then !Constant head
                  con1 = ub**2d0/(2d0*g)
                  con2 = u11**2d0/(2d0*g)                     
                  !fvec(1) = x(1)+con1 - (y11+con2) 
                  !fvec(1) = x(3) - Q_open    
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
				!fvec(2) = x(2)-Drop(R,1)-x(1)
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
				fvec(1) = x(1) - yref(k) 
				fvec(2) = x(3)/Aref(k) + temp_con3  !Normalization
				!Negative sign because flow is entering the reservoir
			elseif(Nodetype(R,1) == 1)then !inflowing
				!fvec(1) = ub-u11+(cb+c11)*(Ab-A11)/(Ab+A11)					
				fvec(1) = x(1) - yref(k)
				fvec(2) = x(3)/Aref(k) - temp_con3 !Normalization
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
				fvec(1) = x(1) - yref(k) 
				fvec(2) = x(3)/Aref(k) - temp_con3 !Normalization
				!Negative sign because flow is entering the reservoir
			elseif(Nodetype(R,1) == 1)then !inflowing
				!fvec(1) = ub-u11+(cb+c11)*(Ab-A11)/(Ab+A11)					
				fvec(1) = x(1) - yref(k)
				fvec(2) = x(3)/Aref(k) + temp_con3 !Normalization
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
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	!fvec(3) represents the momentum/mass storage relation	
!	con1 = -g*Ares*x(3) !gravity
!	con2 = g*Ares*x(1) !pressure force	
!	con4 = con3*x(3)*(x(3)+x(2))/2d0*abs((x(3)+x(2))/2d0) !friction
!	con4 = 0d0 !friction is neglected
!	con5 = ((Qinf_new+Qinf_old)/2d0)**2d0/Ares	
!	con6 = (Qinf_new+Qinf_old-x(3))*(x(3)-yudrop_n(r))/dt 	
!	con7 = x(3)*(x(3)-2d0*yudrop_n(r)+yudrop_n_1(r))/dt**2d0
!	con8 = ((x(3)-yudrop_n(r))/dt)**2d0
!	temp1 = -(con6-2d0*Ares*(con7+con8))

	!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!	con1 = -g*Ares*(x(3)+yudrop_n(r))/2d0 !gravity
!	con2 = g*Ares*(x(1)+x(1))/2d0 !pressure force
!	con3 = param14
!	Qav = (x(2)+x(3))/2d0
!	Hav = (x(3)+yudrop_n(r))/2d0
!	con4 = con3*Hav*Qav*abs(Qav) !friction modified for upstream
!	con4 = 0d0
!	Qs = (Qinf_new+Qinf_old)/2d0
!	con5 = -(x(2)*x(3)- x(3)*yudrop_n(r))/dt !modified for upstream
!	fvec(2) = con1 + con2 + con4 - con5 
	!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	!Storage equation
	!if (Nodetype(R,1) == 2)then !outflowing
	!	!sign for Qb is negative for upstream reservoir
	!	temp1 = 5d-1*(- Qb_oldIA(1) - x(3)) 			
	!Elseif (Nodetype(R,1) == 1)then !inflowing
	!	temp1 = 5d-1*(Qb_oldIA(1) + x(3)) 
	!Endif

	if (Nodetype(R,1) == 2)then !outflowing
		!sign for Qb is negative for upstream reservoir
		temp1 =  -x(3)
	Elseif (Nodetype(R,1) == 1)then !inflowing
		temp1 = x(3)
      else      
          write(98,*),'Unknown condition. Subr. dropshaft'
          call endprog
	endif
	temp3 = 5.d-1*(Qinf_new+Qinf_old)
	temp4 = x(2)-yres_jun_old(R)
	fvec(3) = (temp3+temp1)*dt/Ares - temp4
400	return
	end
