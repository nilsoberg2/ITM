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

      subroutine Riemann_open(j,i,YL,YR,QL,QR,FF1,FF2,Ab,Qb,Pw0)	
	!This routine is used for computing fluxes at a pure free surface flow interface. 
      use common_module
	implicit none
	integer n  
	double precision fvec(10), x(10)
	integer conver_result,info
      double precision AL,AR,AStar,Ab,yb,Qb	      
      double precision QL,QR,QStar,cL,cR,cStar
      double precision YL,YR,hStar,uL,uR,uStar
	double precision small,var1,Area
	double precision teta,FF,DFF,DELY,Ts,iter
	double precision temp1,temp4,y_temp,A_temp,phi_temp,RH
	double precision phiL,phiR,phiStar
	double precision FL1,FL2,FR1,FR2 !fluxes left and right
	double precision FF1,FF2 !fluxes intermediate state 
	double precision SL,SR
	double precision P_phoL,P_phoR,P_phoStar  !To compute average pressure 
	double precision Linearized,depth_posit
	double precision c2,Pw0,temp_const,TOL_local 
	integer j,i
      parameter (small = 1d-14)
      
      TOL_local = 1.d-8
      y_temp = b2_max(j)
      A_temp = A2_max(j)
      phi_temp = Phiref(j)
      temp_const = 1d0+Tol_int_10_6
     
c     Determination of the intermediate state
	If (yL <=temp_const*ydry(j) .and. 
     &	    yR <= temp_const*ydry(j))then
		    Ab = Adry(j); Qb = 0d0; Pw0 = P_pho_dry(j)
		    FF1 = 0d0; FF2 = fluxdry(j)
		    If (Code_star_region == 1)then	
                  P_pho_interf(i) = P_pho_dry(j) !Pressure_Pho at interfaces
	            y_interf(i) = ydry(j) !Water depth
                  A_interf(i) = Adry(j) !Hydraulic area
	        endif
		    goto 20
	endif	

	If (yL <= temp_const*ydry(j))then
		    yL = ydry(j); AL = Adry(j); QL = 0d0 
		    uL = 0d0; cL = Celer_dry(j); P_phoL = P_pho_dry(j)
		    FL1 = QL; FL2 = fluxdry(j)
		    goto 2	
	endif
	call Area_from_H(j,YL,AL,Ts,RH,0)
	uL = QL/AL 
	!computing cL and P_phoL		
	cL = sqrt(g*AL/Ts)						
	!computation of fluxes left FL
	FL1 = QL	
	call Pressure_Pho(j,YL,P_phoL,0)
	FL2 = QL*QL/AL + P_phoL 
2	If (yR <= temp_const*ydry(j))then
            yR = ydry(j); AR = Adry(j); QR = 0d00 
		    uR = 0d00; cR = Celer_dry(j); P_phoR = P_pho_dry(j)
		    FR1 = QR; FR2 = fluxdry(j)
		    goto 3
	endif
	call Area_from_H(j,YR,AR,Ts,RH,0)
	uR = QR/AR
	!computing cR and phiR				
	cR = sqrt(g*AR/Ts)
	!computation of fluxes right FR
	FR1 = QR	
	call Pressure_Pho(j,YR,P_phoR,0)
	FR2 = QR*QR/AR + P_phoR
	
	!dry bed computations
3	if(YL <= temp_const*Ydry(j) .or. 
     &	    YR <= temp_const*Ydry(j))then	
		    if(YL <= temp_const*Ydry(j) .and. 
     &		    YR <= temp_const*Ydry(j))then
			    FF1 = 0d0; FF2 = fluxdry(j)			
			    Ab = Adry(j); Qb = 0d0; Pw0 = P_pho_dry(j)		
			    If (Code_star_region == 1)then	
		            P_pho_interf(i) = P_pho_dry(j) !Pressure_Pho at interfaces
	              y_interf(i) = ydry(j) !Water depth
	              A_interf(i) = Adry(j) !Hydraulic area
	          endif			
			    goto 20
		    elseif(YL <= temp_const*Ydry(j))then			
			    call Phi1(j,YR,phiR)
			    SL = uR - phiR; SR = uR + cR			    
			    phiL = phi_dry(j) !phy at dry conditions
		    elseif (YR <= temp_const*Ydry(j))then
			    call Phi1(j,YL,phiL)		
			    SL = uL - cL; SR = uL + phiL
			    phiR = phi_dry(j) !phy at dry conditions
		    else
			    write(98,*),'Problems defining YL and YR'
			    write(98,*),'subroutine Riemann_open'
			    call endprog
		    endif		    
		    phiStar = (phiL + phiR)/2d0 + (uL-uR)/2d0
		    If (ISNAN(phiStar))then 
                write(98,*),'phiStar is NaN. Routine Riemann_open'
                call endprog
            endif			    
		    If (phiStar <= phi_dry(j))then
		        phiStar = phi_dry(j)
		        hStar = ydry(j); Astar = Adry(j)
		        P_phoStar = P_pho_dry(j)
		        goto 10
		    elseIf (phiStar > phi_temp)then
		        phiStar = phi_temp      
		        hStar = y_temp; Astar = A_temp
		        call Pressure_Pho(j,hStar,P_phoStar,0)		        
		        goto 10		        
		    endif		    
		    call H_from_Phi(j,phiStar,hStar)
		    call Area_from_H(j,hStar,Astar,Ts,RH,0)
		    call Pressure_Pho(j,hStar,P_phoStar,0)	      
		    goto 10	
	else	!No dry-bed conditions 
            If(max(YL,YR) <= 0.20*Yref(j))then
		        !Linearization + depth positivity condition
	    	    call Phi1(j,YL,phiL)
	    	    call Phi1(j,YR,phiR)		
	    	    AStar = depth_posit(AL,AR,phiL,phiR,uL,uR)	    
	      elseif(max(YL,YR) <= 0.80*Yref(j))then    
		        !Two rarefaction-wave approximation		    
		        call Phi1(j,YL,phiL)
		        call Phi1(j,YR,phiR)
		        phiStar = (phiL + phiR)/2d0 + (uL-uR)/2d0
		        If (phiStar < phi_dry(j))then
		            phiStar = phi_dry(j)
		            hStar = ydry(j); Astar = Adry(j)
		            P_phoStar = P_pho_dry(j)
		            goto 7
		        elseIf (phiStar > phi_temp)then
		            phiStar = phi_temp      
		            hStar = y_temp; Astar = A_temp
		            call Pressure_Pho(j,hStar,P_phoStar,0)		        
		            goto 7	        
		        endif	        
		        
		        If (ISNAN(phiStar))then 
                    write(98,*),'phiStar is NaN. Routine Riemann_open'
                    call endprog
                endif	
		        call H_from_Phi(j,phiStar,hStar)
		        If (ISNAN(hStar))then 
                    write(98,*),'hStar is NaN. Routine Riemann_open'
                    call endprog
                endif		    
		        If (hstar > y_temp)then	
		            hstar = y_temp	
		            AStar = A_temp
		            call Pressure_Pho(j,hStar,P_phoStar,0)
	          elseif(hstar <= ydry(j))then		            
		            hStar = ydry(j); Astar = Adry(j)
		            P_phoStar = P_pho_dry(j)
		        else
		            call Area_from_H(j,hStar,Astar,Ts,RH,0)
		            call Pressure_Pho(j,hStar,P_phoStar,0)
	          endif
		        goto 7
            else
			    AStar = Linearized(AL,AR,cL,cR,uL,uR)
	      endif
	endif

      If (ISNAN(AStar))then 
            write(98,*),'AStar is NaN. Routine Riemann_open'
            call endprog
      endif	
	
	If (Astar > A_temp)then		
          AStar = A_temp  !(AL+AR)/2d0
      endif
      
	if(Astar < A2_min(j))then
          AStar = A2_min(j) !Minimum water area 
          hStar = b2_min(j) !Minimum water depth 
          goto 5
      endif	
      
      !Minimum water depth and area (for iteration purposes)
		        
      !if(Astar < Adry(j))then
      !    AStar = Adry(j)
      !    hStar = ydry(j)
      !    goto 5
      !endif
      
	call H_from_Area(j,Astar,hStar,9,0)
5	call Pressure_Pho(j,hStar,P_phoStar,0)

      If (ISNAN(hStar))then 
        write(98,*),'hStar is NaN. Routine Riemann_open'
        call endprog
      endif		
      
c	Computation of shock celerities
7	var1 = (AStar-AL)/(5d-1*(AStar+AL))
	if(abs(var1) < TOL_local .or. AStar <= AL)then
		    SL = uL-cL	!Rarefaction wave
	else 
		    temp1 = (P_phoStar - P_phoL)*Astar/AL/(Astar-AL)	!shock wave	
		    if (temp1 < 0d0)then
			    write(98,*),'negative sqrt. Subr. Riemann_open1'
			    write(99,*),'P_phoStar,P_phoL, Astar, AL'	
			    write(99,18),P_phoStar,P_phoL, Astar, AL	
			    write(99,*),'negative sqrt. Subr. Riemann_open1'	
			    call endprog
		    endif
		    temp1 = sqrt(temp1)
		    SL = uL-temp1
	endif
	var1 = (AStar-AR)/(5d-1*(AStar+AR))
	if(abs(var1) < TOL_local .or. AStar <= AR)then
		    SR = uR+cR	!Rarefaction wave	
	else
		    temp1 = (P_phoStar - P_phoR)*Astar/AR/(Astar-AR)  !shock wave	
		    if (temp1 < 0d0)then		    
			    write(98,*),'negative sqrt. Subr. Riemann_open2'
			    write(99,*),'P_phoStar,P_phoR, Astar, AR'
			    write(99,18),P_phoStar,P_phoR, Astar, AR
			    write(99,*),'negative sqrt. Subr. Riemann_open2'	
			    call endprog
		    endif
		    temp1 = sqrt(temp1) 
		    SR = uR+temp1		
	endif
	!computation of fluxes
10	if(SL > 0d0)then       
c		    Right-going supercritical flow            
		    FF1 = FL1; FF2 = FL2
		    Ab = AL; Qb = QL
		    Pw0 = P_phoL
		    If (Code_star_region == 1)then		    
		        P_pho_interf(i) = P_phoL !Pressure_Pho at interfaces		    
	          y_interf(i) = yL !Water depth
	          A_interf(i) = AL !Hydraulic area
	      endif	  
	elseif(SL <= 0d0.AND.SR >= 0d0)then
c		    Subcritical flow        
		    FF1 = (SR*FL1-SL*FR1+SR*SL*(AR-AL))/(SR-SL)
 		    FF2 = (SR*FL2-SL*FR2+SR*SL*(QR-QL))/(SR-SL)
 	      If (ISNAN(FF1) .or. ISNAN(FF2))then 
		        write(98,*),'FF1 or FF2 is NaN. Routine Riemann_open'
		        call endprog
	      endif
	     
		    Ab = Astar
		    Qb = FF1
		    Pw0 = P_phoStar
		    If (Code_star_region == 1)then
		        If (hStar <= ydry(j))then		        
		            Ab = Adry(j); Qb = 0d0; Pw0 = P_pho_dry(j)
		            yb = ydry(j)
		        else
		            yb = (YL+YR)/2d0
		            call Area_from_H(j,yb,Ab,Ts,RH,0)
		            call Pressure_Pho(j,yb,Pw0,0)
		            !call H_from_Area(j,Ab,yb,45,0)		            
		        endif
		        y_interf(i) =  yb !Water depth
	            A_interf(i) = Ab !Hydraulic area	    
		        P_pho_interf(i) = Pw0		        
	      endif
	elseif (SR < 0d0)THEN	     
c		    Left-going supercritical flow
		    FF1 = FR1; FF2 = FR2
		    Ab = AR; Qb = QR
		    Pw0 = P_phoR
		    If (Code_star_region == 1)then		    
		        P_pho_interf(i) = P_phoR !Pressure_Pho at interfaces		    
	          y_interf(i) = yR !Water depth
	          A_interf(i) = AR !Hydraulic area	      
	      endif   
	else
		write(98,*), 'SL and SR do not satisfy HLL Riemann conditions. 
     &	Error subroutine Riemann_open'
		call endprog
      endif 
18	format(4f20.16)
20	continue      
      return
      end