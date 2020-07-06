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
	
	Subroutine Pressure_Pho(j,y,P_pho,flow_type) 	
	use common_module    
      implicit none	
	integer j,flow_type
	double precision P_pho,y,temp1,temp2,temp3,temp4,A
	double precision const1,b,ALF,y_elip,v,a2	
      if(flow_type == 0)then
            if (t_global > 0.0)then
                    if (y <= b2_min(j))then
			                y = b2_min(j); P_pho = P_pho_min(j)
			                goto 10
		            endif
		            
		            if (y >= b2_max(j))then
			                y = b2_max(j); P_pho = P_pho_max(j)
			                goto 10
		            endif
            endif
      
            if (S0(j) > Min_Slope_for_Sloped_pipe)then
		            ALF = abs(ATan(S0(j)))
                    a2 = d(j)/2d0
	              b = 1d0/Cos(ALF)*(d(j)/2d0)	              
	              
                    y_elip = y-b
                    v = y_elip/b		        
	              const1 = 2d0*g*a2*b
		            temp1 = -(1d0/4d0)*(b-y)*PI
		            temp2 = Sqrt((2d0*b-y)*y/b**2d0)
		            temp3 = temp2*(3d0*b**2d0-2d0*b*y+y**2d0) + 
     &		                3d0*b*(b-y)*ASin(1d0-y/b)
		            temp4 = temp3/(6d0*b)				
                    P_pho = const1*(temp1 + temp4)
            else             
		            temp1 = g/12d0/sqrt(y*(d(j)-y))						
		            temp2 = (d(j)-y)*y*(3d0*d(j)*d(j)-4d0*d(j)*y+4d0*y*y)			
		            temp3 = -3d0*d(j)*d(j)*(d(j)-2d0*y)*sqrt(d(j)-y)*sqrt(y)*		
     &	            ATAN(sqrt(y)/sqrt(d(j)-y))	                    							
		            P_pho = temp1*(temp2+temp3)
            endif
	elseif(flow_type == 1)then		
		    A = Aref(j) + g*Aref(j)*(y-yref(j))/(pc1(j)*pc1(j))	
              temp 1 = pc1(j)*pc1(j)*(A-Aref(j))	
              if (temp 1 < -10.0)then
                      temp 1 = -10.0
              endif
              
		    P_pho = Aref(j)*g*haver_ref(j) + temp 1
	else
		    write(98,*), 'Flow type is not defined',flow_type
		    write(98,*),'Subroutine Pressure_Pho'
		    call endprog
	endif
10    continue					        
      End

