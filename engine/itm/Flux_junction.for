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

      subroutine Flux_junction(R,sum,j,CODL,yL,yR,AL,AR,QL,QR,F11)
	!This routine computes the fluxes at a junction boundary of two pipes 
	!that have the same diameter with no inflow hydrograph.
	use common_module
	implicit none 
	integer j,p,k,r,IH,IK,sum,CODL,Idf0,i
	double precision F11,F12,Aw0,Qw0,Ab,Qb,yb,Wpred	
	double precision yL,YR,AL,AR,QL,QR,h0b1,A0b1,Q0b1,Dt,Pw0
	double precision dh_junction1,dh_junction2
      double precision Qinf_old,Qinf_new,Qinflow,tim,Ts,RH
      double precision yjunction_temp, Q_temp
      double precision yser_temp, temp_sum
      double precision x(10)
      doubleprecision	temp_con1,temp_con2,temp_con3,temp_con4,temp_con5
      double precision Q_max1,Q_max2,Ares
      double precision delta_h,AA,vmax,Q_max,TsIA,P_phoIA
           
	!values at adjacent cells	
	k = NodeID(r,j)
	Dt = DT_GLOBAL
      tim = T_GLOBAL        
      
	If (sum == 2)then
	    i = 1 !i is not important here	
		call Riemann_pressur(k,i,AL,AR,QL,QR,F11,F12,Ab,Qb,Pw0)
		call H_from_Area(k,Ab,yb,520,1)		
	elseif (sum == 0)then
	    i = 1 !i is not important here	
		call Riemann_open(k,i,yL,yR,QL,QR,F11,F12,Ab,Qb,Pw0)
		call H_from_Area(k,Ab,yb,520,0)		        
      elseif (sum == 1)then		
          !i = 1 !i is not important here	
          !call Riemann_pressur_open(k,i,yL,yR,QL,
          !&   QR,CODL,Aw0,Qw0,Idf0,F11,F12,Wpred,Pw0)			
          !Ab = Aw0; Qb = Qw0
          !call H_from_Area(k,Ab,yb,520,Idf0)           
          p=2*j-1
          !write(99,*),'p=',p
          vmax = 20d0
          Q_max = vmax*Aref(k)
          !write(99,*),'Q_max=',Q_max
          Ares = Ares_junct(R)
          !write(99,*),'Ares=',Ares
          !write(99,*),'cond_mixed1(j)',cond_mixed1(j)
          if (cond_mixed1(j) == 2)then              
              !flow in adjacent cell is pressurized, and water level at pond is below pipe crown (pressur. flow)						
              x(p) = yref(k) !y1(j)  
              !write(99,*),'x(p)',x(p)
              IDfbIA(j) = 0
              !write(99,*),'IDfbIA(j)',IDfbIA(j)
              delta_h = y1(j) - (yres_jun_old(r)-dropIA(j))
              !write(99,*),'delta_h',delta_h
              !write(99,*),'y1(j)',y1(j)
              !write(99,*),'dropIA(j)',dropIA(j)
              
              if (delta_h<0d0)then 
                  delta_h = 0d0  
              endif
              temp_con1 = 0.6*Aref(k)*sqrt(2d0*g*delta_h) 
              Q_max1 = vmax*Aref(k)                  
              Q_max2 = (1d0/Dt)*Aref(k)*dx(k)/2d0 
              Q_max = min(Q_max1,Q_max2)
              !write(99,*),'dx(k)=',dx(k)
                          
              if (temp_con1 > Q_max)then
                  temp_con1 = Q_max  
              endif
              !write(99,*),'Nodetype(r,j)=',Nodetype(r,j)
              if (Nodetype(r,j) == 2)then !outflowing                   
                  x(p+1) = -temp_con1 !Negative sign because flow is entering the reservoir
              else if (Nodetype(r,j) == 1)then !inflowing				        
                  x(p+1) = temp_con1                        
              else
                  write(98,*),'Pipe is not inflow. or outflow.'
	            write(98,*),'subrout. Flux_junction'
	            call endprog  
              endif
              !write(99,*),'x(p+1)=',x(p+1)
          elseIf (cond_mixed1(j) == 1)then
              IDfbIA(j) = 0 !jet flow 
              !flow in adjacent cell is free surface and water level at pond is above pipe crown (pressur. flow)
	        x(p) = yref(k); Ab = Aref(k)  
              !write(99,*),'IDfbIA(j)=',IDfbIA(j)
              !write(99,*),'x(p)=',x(p)
              temp_con1 = yres_jun_old(R)-dropIA(j)-y1(j)/2d0 
              !write(99,*),'temp_con1=',temp_con1
	        if(temp_con1 < 0d0)then
                    temp_con1 = 0d0
              endif
              Q_max1 = vmax*Aref(k)                  
              Q_max2 = (1d0/Dt)*Ares*temp_con1
              Q_max = min(Q_max1,Q_max2)   
              !write(99,*),'Q_max1=',Q_max1
              !write(99,*),'Q_max2=',Q_max2
	        !To make sure that as maximum 50% of the volume from the adjacent 
	        !cell would enter the reservoir. 					
	        temp_con3 = 0.6*Aref(k)*sqrt(2d0*g*temp_con1)	                  
              if (temp_con3 > Q_max)then
                  temp_con3 = Q_max  
              endif
               !write(99,*),'Nodetype(r,j)=',Nodetype(r,j)            
	        !To make sure that as maximum 50% of the volume from the adjacent 
	        !cell would enter the reservoir. 
	        if (Nodetype(r,j) == 2)then !outflowing
	        	x(p+1) = temp_con3 !Negative sign because flow is entering the reservoir
	        elseif (Nodetype(r,j) == 1)then	!inflowing
	        	x(p+1) = -temp_con3
              else   
                  write(98,*),'Pipe is not inflowomg or outflow'
	        	write(98,*),'Check the input data'
	        	write(98,*),'subrout. Flux_junction'
	        	call endprog
              endif  
          else 
              write(98,*),'cond_mixed1(j) .ne. 1,2'
	        write(98,*),'subrout. Flux_junction'
              call endprog
          endif
          
          !Fluxes for mixed flow conditions
          if (Nodetype(r,j) == 1)then !inflowing
			If (IDfbIA(j) == 0)then				
				call Area_from_H(k,x(p),AA,TsIA,RH,0)
				call Pressure_Pho(k,x(p),P_phoIA,0)
				Fdownst(k,1) = x(p+1)
				Fdownst(k,2) = x(p+1)*x(p+1)/AA + P_phoIA
				Abound(k,2) = AA; Qbound(k,2) = x(p+1)
				ybound(k,2) = x(p)					
				Pres_pho_Bound(k,2) =  P_phoIA                   
			elseif(IDfbIA(j) == 1)then
				call Area_from_H(k,x(p),AA,TsIA,RH,1)
				call Pressure_Pho(k,x(p),P_phoIA,1)
				Fdownst(k,1) = x(p+1)
				Fdownst(k,2) = x(p+1)*x(p+1)/AA + P_phoIA
				Abound(k,2) = AA; Qbound(k,2) = x(p+1)
				ybound(k,2) = x(p)				
				Pres_pho_Bound(k,2) =  P_phoIA
			else
				write(98,*),'IDfbIA(j) is not 0 or 1'
				write(98,*),'subrout. Flux_junction'
				call endprog
              endif
		elseif(Nodetype(r,j) == 2)then !outflowing
			If(IDfbIA(j) == 0)then		
				call Area_from_H(k,x(p),AA,TsIA,RH,0)
				call Pressure_Pho(k,x(p),P_phoIA,0)
				Fupst(k,1) = x(p+1)
				Fupst(k,2) = x(p+1)*x(p+1)/AA + P_phoIA	
				Abound(k,1) = AA; Qbound(k,1) = x(p+1)
				ybound(k,1) = x(p)					
				Pres_pho_Bound(k,1) =  P_phoIA
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
				write(98,*),'subrout. Flux_junction'
				call endprog
              endif
          else
			write(98,*),'Pipe is not inflowing or outflowing'		
			write(98,*),'subrout. Flux_junction'
			call endprog
          endif 
          F11 = x(p+1)
          goto 100
	else	
		write(98,*),'Sum .ne. 0,1,2'
		write(98,*),'subrout. Flux_junction'
		call endprog		
      endif	

10    if (Nodetype(r,j) == 1)then !inflowing
		Fdownst(k,1) = F11
		Fdownst(k,2) = F12
		Abound(k,2) = Ab; Qbound(k,2) = Qb
		ybound(k,2) = yb	
		Pres_pho_Bound(k,2) =  Pw0              
		
          !if (Qinflow > 0d0)then
          !    Fdownst(k,1) = Fdownst(k,1) - Qinflow/2d0
          !    Q_temp = Qbound(k,2) - Qinflow/2d0
          !    Qbound(k,2) = Q_temp
          !endif 
	elseif(Nodetype(r,j) == 2)then !outflowing
		Fupst(k,1) = F11
		Fupst(k,2) = F12
		Abound(k,1) = Ab; Qbound(k,1) = Qb
		ybound(k,1) = yb		
		Pres_pho_Bound(k,1) =  Pw0
          
          !if (Qinflow > 0d0)then
          !    Fupst(k,1) = Fupst(k,1) + Qinflow/2d0
          !    Q_temp = Qbound(k,1) + Qinflow/2d0  
          !    Qbound(k,1) = Q_temp
          !endif 
	else
		write(98,*),'Nodetype(r,j) .ne. 1,2'
		write(98,*),'subrout. Flux_junction'
		call endprog		
      endif
 100  continue
      end subroutine
