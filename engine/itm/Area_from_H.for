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

      subroutine Area_from_H(j,y,area,TH,RH,flow_type)	
	!This routine computes the hydraulic area from the piezometric head 
	use common_module
      implicit none		
      integer iter,j,code_error,flow_type
	double precision area,y,teta,TH,RH,Atemp	
	double precision const1,b,ALF,temp1,temp2,y_elip,v,a2
      If (ISNAN(y))then 			
			write(98,*),'NaN is found in routine Area_from_H'
			write(98,*),'y',y,'pipe No',j,'flow_type',flow_type	
			write(99,*),'NaN is found in routine Area_from_H'
			write(99,*),'y',y,'pipe No',j,'flow_type',flow_type			
			call endprog
	endif

	if(flow_type == 0)then
	      if (t_global > 0.0)then
	              if (y >= b2_max(j))then 
		                    y = b2_max(j)
		            endif
		            if(y < b2_min(j))then			    
			                y = b2_min(j)
		            endif
		    endif
            if (S0(j) > Min_Slope_for_Sloped_pipe)then
                ALF = abs(ATan(S0(j)))
                a2 = d(j)/2d0
	          b = 1d0/Cos(ALF)*(d(j)/2d0)	          
	          
                y_elip = y-b
                v = y_elip/b		        
	          const1 = a2*b
		        
		        temp1 = v* Sqrt(1d0 - v**2d0) + ASin(v) + PI/2d0
		        area = const1*temp1		        
		        TH = 2d0*a2*Sqrt(1d0 - v**2d0)
		        !!!!!!!!compute TH and RH	
		        teta = 2d0*ACOS(1d0-2d0*y/d(j))
		        RH = d(j)/4d0*(1d0-sin(teta)/teta)  !This should be modified later		       
		    else
		        teta = 2d0*ACOS(1d0-2d0*y/d(j))		
		        area = 1d0/8d0*(teta-SIN(teta))*d(j)*d(j)
		        TH = d(j)*sin(teta/2d0)			
		        RH = d(j)/4d0*(1d0-sin(teta)/teta)
		    endif 
	elseif(flow_type == 1)then
		    area = Aref(j) + g*Aref(j)*(y-yref(j))/(pc1(j)*pc1(j))
		    TH = g*Aref(j)/(pc1(j)*pc1(j)) !T = dA/dy for pressurized flows
		    teta = 2d0*ACOS(1d0-2d0*yref(j)/d(j))
		    RH = d(j)/4d0*(1d0-sin(teta)/teta)
              
              temp 1 = g*Aref(j)*(y-yref(j)) !See Pressure_Pho routine to understand this
              if (temp 1 < -10.0)then
                      temp 1 = -10.0
              endif              
		    area = Aref(j) + temp 1/(pc1(j)*pc1(j))
	else
		write(98,*), 'Flow type is not defined',flow_type
		write(98,*),'Subroutine Area_from_H' 
		call Endprog		
	endif
	return		
      end
