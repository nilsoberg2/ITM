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

      Subroutine Source_term(j,Dt)	
	!This routine is used for incorporating source terms into the solution through time splitting.
      use common_module
	implicit none	    
      integer i,j,w,prod, int_temp,r
      double precision Q,RH,Sf,A, u, ALF
      double precision Dt,Ts, temp1,temp2,temp3,Atemp,Qtemp 
	double precision htemp,KsD,viscos,RE
	double precision c_coef,c_coef1,c_coef2,fq,fq1,fq2,fu,k
	double precision DuDt,DuDx,ftot,tempU
	integer sum,CODL
	double precision AL,AR,QL,QR,Wpred,htemp2,neg_inter,Wlim
	double precision temp4,cons_ratio
	double precision u_temp,area,s_f_temp,G_aver
	double precision Aminus_aver, Qminus_aver, hminus_aver, U_minus
	double precision G_minus, Sf_minus
	double precision Aplus_aver, Qplus_aver, U_plus, hplus_aver
	double precision G_plus, Sf_plus,Apho_source,hL,hR,h_reconst
	double precision P_pho_L, P_pho_R,u2
	double precision ptemp,Dy_rel,htemp_source,minA, area_res
      double precision Area_pressur, Area_depressur,drop_node
      double precision h_junc, TsIA  
                                      
	!temp3 = (1d0 -  Tol_int_10_12)*Aref(j)  !It was 10^8 10^6
	temp3 = Aref(j) 
	temp4 = (1d0+Tol_int_10_10)*temp3
      
      Area_pressur = (1d0+Tol_int_10_6)*Aref(j) 
      Area_depressur = (1d0-Tol_int_10_6)*Aref(j) 
	!neg_inter = 0.84 !Yref(j)/d(j) !0.84 Has to be used. 
	!If 0.84 is not used wrong pressure values are obtained
	!neg_inter = 0.98*Yref(j)/d(j) !0.84 Has to be used. 
	
	neg_inter = yfree_press	
	Wlim = 0.001	!This is same as Riemann Pressurized Open
      !Wlim = 2.0 !0.5!
												
	!Updating the state of the flow at internal cells
	do i = 3,Nx(j)-2
		sum = IdFlow(j,i-1)+IdFlow(j,i)+IdFlow(j,i+1) 		
		    if (i == 3 .or. i == Nx(j)-2)then !At boundaries
	          if(sum == 0)then !fully free surface flow
                      if(Atemp1(i) <= Aref(j))then !temp4
					    IdFlow1(i) = 0						
					    if(Atemp1(i) > temp3)then
						    Atemp1(i) = temp3
					    endif
				    else 
					    IdFlow1(i)  = 1
                          if(Atemp1(i) <= Area_pressur)then !temp4
                              Atemp1(i) = Area_pressur
                          endif
				    endif
			    elseif(sum == 3)then  !Fully pressurized flow 
				    IdFlow1(i)  = 1   
                        !if (i == 3)then
                        !    R = Node1(j) !Upstream node
                        !    drop_node = zb(j,1)-junct_elev(R)
                        !elseif(i == Nx(j)-2)then !At boundaries
                        !    R = Node2(j) !downstream node
                        !    drop_node = zb(j,2)-junct_elev(R)	                                           
                        !endif                      
         !                 if (BCnode(R)==20 .or. BCnode(R)==4 .or.
         !&                            BCnode(R)==7 .or. BCnode(R)==30)then
         !                     if(Atemp1(i) <= Area_depressur)then !temp4                                  
         !                         h_junc =yres_jun_old(R)-drop_node
         !                         call Area_from_H(j,h_junc,
         !&                        area_res,TsIA,RH,0)
         !                         Atemp1(i) = Area_depressur
         ! !                          Atemp1(i)=max(Area_depressur,
         ! !&                        area_res)
         !                     endif
         !             endif                        
                  elseif (sum == 1 .or. sum == 2)then
                      If(IdFlow(j,i)  == 1)then
                          if(Atemp1(i) <= Area_depressur)then !Possibility for depressurization 
                              IdFlow1(i)  = 0       
                          else 
                              IdFlow1(i)  = 1
                              !Atemp1(i) = Aref(j) 
                          endif
                      elseIf(IdFlow(j,i)  == 0)then
                          if(Atemp1(i) > Area_pressur)then !Possibility for depressurization 
                              IdFlow1(i)  = 1
                          else
                              IdFlow1(i)  = 0   
                              !Atemp1(i) = Aref(j)
                          endif
                      else
                          write(98,*),'IdFlow1(i) .ne. 0,1'
				        write(98,*),'Routine Source_term'
                      endif
                      
		      !      if(Atemp1(i) <= Aref(j))then !Possibility for depressurization
					   ! IdFlow1(i) = 0						
					   ! if(Atemp1(i) > temp3)then
						  !  Atemp1(i) = temp3
					   ! endif
				    !else 
					   ! IdFlow1(i)  = 1					    				
				    !endif
			    else
				    write(98,*),'Error in sum'
				    write(98,*),'Routine Source_term'
				    write(99,*),'Error in sum'
				    write(99,*),'Routine Source_term'
				    call endprog    
		        endif
		        goto 10
		    endif
		
		
			if(sum == 0)then !free surface flow
				if(Atemp1(i) <= Aref(j))then !temp4
					IdFlow1(i) = 0						
					if(Atemp1(i) > temp3)then
						Atemp1(i) = temp3
					endif
				else 
					IdFlow1(i)  = 1
					!if(Atemp1(i) <							!modified
     &				!	Area_for_pressur(j))then			!modified
					!	Atemp1(i) = Area_for_pressur(j)		!modified
					!endif							
				endif
			elseif(IdFlow(j,i) == 1 .and. sum == 3)then 
				IdFlow1(i)  = 1
			elseif(IdFlow(j,i) == 0 .and. sum .ne. 0)then 
				if(IdFlow(j,i-1) + IdFlow(j,i) == 1)then
				    CODL = 1 !This was added					
					AL = A0(j,i-1); AR = A0(j,i)
					QL = Q0(j,i-1); QR = Q0(j,i)
					call interf_speed(j,AL,AR,QL,QR,CODL,Wpred)
					if(Wpred > -Wlim)then 
						!positive interface
						if(Atemp1(i) >= Aref(j))then  !temp4
							IdFlow1(i)  = 1
							if(Atemp1(i) <							!modified
     &							Area_for_pressur(j))then			!modified
								Atemp1(i) = Area_for_pressur(j)		!modified
							endif									!modified
						else
							IdFlow1(i) = 0
							if(Atemp1(i) > (1d0-1.d-15)*Aref(j))then !modified
								Atemp1(i) = (1d0-1.d-15)*Aref(j)	 !modified
							endif
							!Although we can not keep this cell in free surface regime forever, 
							!for stability reasons we have to keep its value sligthly below Aref 
							!This is very important						
						endif
					else !negative interface (open channel side)
						if(Atemp1(i) < Aref(j))then  
							IdFlow1(i) = 0
							if(Atemp1(i) > temp3)then
								Atemp1(i) = temp3
							endif
						else
							IdFlow1(i)  = 1
							if(Atemp1(i) <							!modified
     &							Area_for_pressur(j))then			!modified
								Atemp1(i) = Area_for_pressur(j)		!modified
							endif	
						endif
					endif
				elseif(IdFlow(j,i) + IdFlow(j,i+1) == 1)then
				    CODL = 0 !This was added
					AL = A0(j,i); AR = A0(j,i+1)
					QL = Q0(j,i); QR = Q0(j,i+1)
					call interf_speed(j,AL,AR,QL,QR,CODL,Wpred)
					if(Wpred < Wlim)then 
						!positive interface
						if(Atemp1(i) >= Aref(j))then  !temp4
							IdFlow1(i)  = 1
							if(Atemp1(i) <							!modified
     &							Area_for_pressur(j))then			!modified
								Atemp1(i) = Area_for_pressur(j)		!modified
							endif									!modified
						else
							IdFlow1(i) = 0
							if(Atemp1(i) > (1d0-1.d-15)*Aref(j))then !modified
								Atemp1(i) = (1d0-1.d-15)*Aref(j)	 !modified
							endif
							!Although we can not keep this cell in free surface regime forever, 
							!for stability reasons we have to keep its value sligthly below Aref 
							!This is very important	
						endif
					else !negative interface
						if(Atemp1(i) < Aref(j))then !(open channel side)
							IdFlow1(i) = 0
							if(Atemp1(i) > temp3)then
								Atemp1(i) = temp3
							endif
						else
							IdFlow1(i)  = 1
							if(Atemp1(i) <							!modified
     &							Area_for_pressur(j))then			!modified
								Atemp1(i) = Area_for_pressur(j)		!modified
							endif
						endif
					endif
				else 
					write(98,*),'unknown condition'
					write(98,*),'subroutine source term'
					call endprog
				endif							
			elseif(IdFlow(j,i) == 1 .and. sum .ne. 3)then 							
				if(IdFlow(j,i-1) + IdFlow(j,i) == 1)then
				    CODL = 0 !This was added
					AL = A0(j,i-1); AR = A0(j,i)
					QL = Q0(j,i-1); QR = Q0(j,i)
					call interf_speed(j,AL,AR,QL,QR,CODL,Wpred)
					if(Wpred < Wlim)then
						!positive interface
						if(Atemp1(i) >= Aref(j))then
							IdFlow1(i)  = 1							
						else 						
							call H_from_Area(j,Atemp1(i),htemp2,43,1)
							if (htemp2>neg_inter*d(j))then  
								!flow is still pressurized
								IdFlow1(i) = 1
							else
								IdFlow1(i) = 0
								if(Atemp1(i) > temp3)then
									Atemp1(i) = temp3
								endif							
							endif
						endif
					else !negative interface
						if(Atemp1(i) < Aref(j))then  !Aref(j)														
							call H_from_Area(j,Atemp1(i),htemp2,43,1)
							if (htemp2 > neg_inter*d(j))then !check how htemp is defined htemp2>htemp1
								!flow is still pressurized
								IdFlow1(i) = 1
							else
								IdFlow1(i) = 0
								if(Atemp1(i) > temp3)then
									Atemp1(i) = temp3
								endif												
							endif							
						else
							IdFlow1(i)  = 1
						endif 
					endif
				elseif(IdFlow(j,i) + IdFlow(j,i+1) == 1)then
					CODL = 1
					AL = A0(j,i); AR = A0(j,i+1)
					QL = Q0(j,i); QR = Q0(j,i+1)
					call interf_speed(j,AL,AR,QL,QR,CODL,Wpred)
					if(Wpred > -Wlim)then 
						!positive interface
						if(Atemp1(i) >= Aref(j))then
							IdFlow1(i)  = 1
						else					
							call H_from_Area(j,Atemp1(i),htemp2,43,1)
							if (htemp2 > neg_inter*d(j))then !check how htemp is defined
								!flow is still pressurized
								IdFlow1(i) = 1
							else
								IdFlow1(i) = 0
								if(Atemp1(i) > temp3)then
									Atemp1(i) = temp3
								endif	
							endif
						endif
					else !negative interface
						if(Atemp1(i) < Aref(j))then
							call H_from_Area(j,Atemp1(i),htemp2,43,1)
							if (htemp2 > neg_inter*d(j))then !check how htemp is defined
								!flow is still pressurized
								IdFlow1(i) = 1
							else
								IdFlow1(i) = 0
								if(Atemp1(i) > temp3)then
									Atemp1(i) = temp3
								endif
							endif							
						else
							IdFlow1(i)  = 1
						endif
 						continue
					endif
				else 
					write(98,*),'unknown condition2'
					write(98,*),'subroutine source term'
					call endprog
				endif
			else
				write(98,*),'not recognized condition'
				write(98,*),'routine Source_term'
				call endprog
			endif	
10          continue			
	enddo
     

	!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	!goto 50 !when the flow is pure pressurized flow only
	!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		
      !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
      !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
  	!New computation of source term (Well-balanced scheme)
      !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
      !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
  40    do i = 3,Nx(j)-2 
            ALF = abs(ATan(S0(j)))
            If (ISNAN(Atemp1(i)))then 
			    write(98,*),'Atemp1(i)',Atemp1(i),'pipe =',j,'cell =',i
			    write(98,*),'IdFlow',IdFlow1(i)
			    write(98,*),'NaN is found in routine Source_term'
			    call endprog
		    endif
		    
		    If (ISNAN(Qtemp1(i)))then 
			    write(98,*),'Qtemp1(i)',Qtemp1(i),'pipe =',j,'cell =',i
			    write(98,*),'IdFlow',IdFlow1(i)
			    write(98,*),'NaN is found in routine Source_term'
			    call endprog
		    endif
   
            if(IdFlow1(i) == 0)then
			    If (Atemp1(i) <= (1d0+Tol_int_10_6)*Adry(j))then                  
			        htemp1(i) = ydry(j); Atemp1(i) = Adry(j); Qtemp1(i) = 0d0
				    h0_Rec(j,i) = -s0(j)*dx(j)/2d0
				    goto 45
			    endif
		    endif   
	      
	      Dy_rel = (y_interf(i+1)-y_interf(i))/(y_interf(i+1)+y_interf(i))
	      Dy_rel = 2d0*abs(Dy_rel)
	      If (ISNAN(Dy_rel))then 			   
	          write(98,*),'Dy_rel is NaN. Routine Source_term' 
			    call endprog
	      endif
	      	      	      
	      !Atemp1(i) is used for strict mass conservation
	      !Atemp is used ONLY for time splitting and for avoiding numerical 
	      !oscillation near or "lake at rest" conditions
	      
	      if (S0(j) > Min_Slope_for_Sloped_pipe)then	      
	          if(IdFlow1(i) == 0)then	          
	              if (Dy_rel < Tol_int_10_4)then !tol_higher gives problems
	                  Atemp = Atemp1(i) !This is area referred to an elipse
	                  !and hence it doesn't need to be multiplied by Cos alfa
	              else
                        Atemp = (P_pho_interf(i+1)-P_pho_interf(i))/
     &	                (g*(y_interf(i+1)-y_interf(i)))
                        Atemp = Atemp*(Cos(ALF))**2d0
    !                   Atemp = (P_pho_interf(i+1)-P_pho_interf(i))/
    ! &	                (g*S0(j)*dx(j))
	              endif
	          elseif(IdFlow1(i) == 1)then
	              if (Dy_rel < Tol_int_10_4)then
	                  Atemp = Atemp1(i) !This is area referred to an elipse
	                  !and hence it doesn't need to be multiplied by Cos alfa
	              else
                        Atemp = (P_pho_interf(i+1)-P_pho_interf(i))/
     &	                (g*(y_interf(i+1)-y_interf(i)))
                        Atemp = Atemp*(Cos(ALF))**2d0
	              endif
	          else
	              write(98,*),'IdFlow1(i) .ne. 0,1'
	              write(99,*),'IdFlow1(i) .ne. 0,1. Endprog'
	              call endprog
	          endif
	      else
	          Atemp = Atemp1(i)
	      endif	      	      
	      
43	      If(ISNAN(Atemp))then 			    
			    write(98,*),'Atemp is NaN. Routine Source_term'
			    call endprog
		    endif	  
	      
            if(IdFlow1(i) == 0)then
			    If (Atemp <= (1d0+Tol_int_10_6)*Adry(j))then                  
			        htemp1(i) = ydry(j);Atemp1(i) = Adry(j); Qtemp1(i) = 0d0
				    h0_Rec(j,i) = -s0(j)*dx(j)/2d0
				    goto 45
	          endif
		    endif	      	      
	      
	      !Atemp is used for preserving "lake at rest conditions"
	      !Atemp1(i) [or htemp] is used for estimating Sf
	      !Atemp1(i) strictly ensures conservation of mass
	      !Sf (energy slope) Must be using h*(cos*alfa)
	      
	      !Computation of !Sf (energy slope)
	      call H_from_Area(j,Atemp1(i),htemp,216,IdFlow1(i))
	      If (ISNAN(htemp))then 			    
			    write(98,*),'htemp is NaN. Routine Source_term'
			    call endprog
		    endif
	      	      
	      !Computation of hydraulic radius (RH) to be used in Sf
	      if (S0(j) > Min_Slope_for_Sloped_pipe)then
	          htemp_source = htemp*Cos(ALF)
	      else
	          htemp_source = htemp
	      endif	      
	      call Area_from_H(j,htemp_source,area,Ts,RH,IdFlow1(i))
		    	       
		    !To avoid large velocities in shallow depths
		    if (S0(j) > Min_Slope_for_Sloped_pipe)then
		        minA = max(A_cell_dry_sloped(j),2d0*Adry(j))
		        if (Atemp1(i) < minA)then
		            Qtemp1(i) = 0d0
		        endif
		    else
		        minA = max(2d0*Adry(j),0.000005*Aref(j))
		        if (Atemp1(i) < minA)then
		            Qtemp1(i) = 0d0
		        endif
		    endif
		    Q = Qtemp1(i)
		    u = Q/area
		    
		    !To avoid unrealistic velocities
		    if (abs(u) > 40d0)then
		        u = 40d0*SIGN(1.0,u)
		        Q = u*Atemp1(i)
		    endif
		    
		    !Update with source term by Dt  
		    Sf = nm(j)*nm(j)*u*abs(u)/RH**(4d0/3d0)
		    Qtemp1(i) = Q + (sin(ALF) - Sf)*g*Atemp*cos(ALF)*DT_GLOBAL
		    !Atemp is used for preserving "lake at rest" conditions
		    
	      !Update with source term by Dt/2  
		    !Sf = nm(j)*nm(j)*u*abs(u)/RH**(4d0/3d0)
		    !Qtemp = Q + (sin(ALF) - Sf)*g*Atemp*cos(ALF)*5.d-1*DT_GLOBAL		   		    		    
		    !Reupdate with source term by Dt
		    !The velocity is computed with the new Q.
		    !tempU = Qtemp/Atemp 		     		    
		    !Sf = nm(j)*nm(j)*tempU*abs(tempU)/RH**(4d0/3d0)
		    !Qtemp1(i) = Q + (sin(ALF) - Sf)*g*Atemp*cos(ALF)*Dt
		    
		    
		    !To avoid large velocities in shallow depths
		    if (S0(j) > Min_Slope_for_Sloped_pipe)then
		        minA = max(A_cell_dry_sloped(j),2d0*Adry(j))
		        if (Atemp1(i) < minA)then
		            Q = 0d0; Qtemp1(i) = 0d0
		        endif
		    else
		        minA = max(2d0*Adry(j),0.000005*Aref(j))
		        if (Atemp1(i) < minA)then
		            Q = 0d0; Qtemp1(i) = 0d0
		        endif
		    endif
		    
		    u = Qtemp1(i)/Atemp1(i)
		    !To avoid unrealistic velocities
		    if (abs(u) > 40d0)then
		        u = 40d0*SIGN(1.0,u)
		        Qtemp1(i) = u*Atemp1(i)
		    endif
		    		    
		   		   
		    if(IdFlow1(i) == 0)then
			    If (Atemp <= (1d0+Tol_int_10_4)*Adry(j))then
				    htemp1(i) = ydry(j);Atemp1(i) = Adry(j); Qtemp1(i) = 0d0
				    h0_Rec(j,i) = -s0(j)*dx(j)/2d0
				    goto 45
	            endif
		    endif	      	      
	      		    
		    If (ISNAN(Qtemp1(i)))then 	
		        write(99,*),'Q is NaN. Pipe = ',j,'cell = ',i		        
			    write(98,*),'Routine Source_term'
			    call endprog
		    endif		    
		    
		    !htemp1(i) must be computed with Atemp1(i) not with that used for Reconstruction
		    if (S0(j) > Min_Slope_for_Sloped_pipe)then
		        call H_reconst_sloped(j,Atemp1(i),h0_Rec(j,i),
     &		            10001,IdFlow1(i))
                  call H_from_Area(j,Atemp1(i),htemp1(i),218,IdFlow1(i))
                  If (ISNAN(h0_Rec(j,i)))then 			    		       
			        write(98,*),'h0_Rec(j,i) is NaN. Pipe = ',j,'cell = ',i
			        write(98,*),'Routine Source_term4'
			        call endprog
		        endif	
		    else
		        call H_from_Area(j,Atemp1(i),htemp1(i),213,IdFlow1(i))	
		        h0_Rec(j,i) = htemp1(i)
		    endif
		    
		    If (ISNAN(htemp1(i)))then 			    		       
			    write(98,*),'htemp1(i) is NaN. Pipe = ',j,'cell = ',i
			    write(98,*),'Routine Source_term4'
			    call endprog
		    endif	    
  45        continue
      enddo
      goto 120
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	!Previous version of Source term
	!friction term, using splitting using a second order Runge-Kutta
	do i = 3,Nx(j)-2	
	      Dy_rel =  (y_interf(i+1)-y_interf(i))/(y_interf(i+1)+y_interf(i))
	      Dy_rel = 2d0*abs(Dy_rel)
	      if (Dy_rel < Tol_int_10_6)then
	          Atemp = (A_interf(i+1)+A_interf(i))/2d0
	      else
	          Atemp = (P_pho_interf(i+1)-P_pho_interf(i))/
     &	        (g*(y_interf(i+1)-y_interf(i)))
	      endif		
		    u_temp = Q/Atemp 								
		    Sf = nm(j)*nm(j)*u_temp*abs(u_temp)/RH**(4d0/3d0)
		    Qtemp1(i) = Q + (S0(j) - Sf)*g*Atemp*Dt
		    goto 100	    
	    
		!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
		!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
		!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
		!Unsteady Friction		
50		u = Qtemp1(i)/Atemp1(i)
		Q = Qtemp1(i)
		!variables
		KsD = 0.0001
		viscos= 1.d-6						
		RE = abs(u)*d(j)/viscos	
		if (RE < 2000d0)then
			if (RE < 10d0)then
				fq = 6.4
			else
				fq = 64d0/RE
			endif
			c_coef = 0.00476 !(c_coef = shear decay coefficient)
		elseif (RE < 4000d0)then
			fq1 = 64d0/2000d0 
			fq2 = 0.25/(LOG10(KsD/3.7 + 5.74/4000d0**0.9))**2.0	!Swamee-Jain RE=4000	
			fq = (fq2*(RE-2000d0)+fq1*(4000d0-RE))/2000d0
			c_coef1 = 0.00476 
			c_coef2 = 12.86/(4000d0**(LOG10(15.29/4000d0**0.0567)))
			c_coef = (c_coef2*(RE-2000d0)+c_coef1*(4000d0-RE))/2000d0
		else
		!write(99,*),'RE,viscos,u',RE,viscos,u
			c_coef = 12.86/(RE**(LOG10(15.29/RE**0.0567)))						 
			fq = 0.25/(LOG10(KsD/3.7 + 5.74/RE**0.9))**2.0	!Swamee-Jain eq.	
		endif	
		k = 5d-1*sqrt(c_coef)
		DuDt = (u-Q0(j,i)/A0(j,i))/dt

		if (i == 3 .or. i == Nx(j)-2)then
			DuDx = 0d0
		else
			DuDx = (Qtemp1(i+1)/Atemp1(i+1) - Qtemp1(i-1)/
     $			Atemp1(i-1))/Dx(j)
		endif	


		if (abs(u) < 1d-2)then
			fu = 0d0
		else
			fu = k*d(j)/(u*abs(u))*(DuDt+pc1(j)*SIGN(1.0,u)*abs(DuDx))
			if (fu < 0d0)then
			    fu = 0d0
			endif
			fu = 0d0
		endif		
		ftot = fq+fu
		
		!Update with source term by Dt/2  
		Sf = ftot*u*abs(u)/(2d0*g*d(j))
		Qtemp = Q + (S0(j) - Sf)*g*Atemp*5d-1*Dt
		!Reupdate with source term by Dt
		!The velocity is computed with the new Q.
		tempU = Qtemp/Atemp 
		Sf = ftot*tempU*abs(tempU)/(2d0*g*d(j))						
		Qtemp1(i) = Q + (S0(j)-Sf)*g*Atemp*Dt
		!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%				 
100		continue											
	enddo
120   continue			
	end subroutine
