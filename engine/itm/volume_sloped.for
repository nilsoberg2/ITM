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
      subroutine Volume_sloped(j,y_downs,Vol)
	use common_module
      implicit none  
      integer j
	double precision x(1),x2(1)
	double precision A,y_downs,Vol1,Vol2,Vol
	double precision DXTemp,Dx_free,dy
	double precision const1,b,ALF
	double precision temp1,temp2,temp3,temp4,temp44
	double precision temp5,temp6,temp7,temp77,dz
	
	if (y_downs <= 0d00)then
	      y_downs = 0d0
	      Vol = 0d0
	      goto 100
	endif
	
	DXTemp = y_downs/abs(S0(j))
	dz = abs(S0(j))*dx(j)
	ALF = abs(ATan(S0(j)))
	b = 1d0/Cos(ALF)*(d(j)/2d0)
		
	!Four cases are possible
	!Dry bed, open channel, partial open-channel, partial surcharged, 
	!fully pressurized flow
	
	if (y_downs <=  abs(S0(j))*dx(j))then !Dry bed conditionS
	      !S0 instead of dz is used because we are computing Volume
	      X2(1) = y_downs
	      const1 = b*(d(j)/2d0)/abs(S0(j)) 
	      temp1 = PI*x2(1)
	      temp2 = -2d0/3d0*b*((2d0*b-x2(1))*x2(1)/b**2d0)**(3d0/2d0)
	      temp3 = -2d0*x2(1)*ASin(1d0-x2(1)/b)
	      temp4 = x2(1)*(-2d0*b+x2(1))+2d0*b*Sqrt(2d0*b-x2(1))*
     &	    Sqrt(x2(1))*ATan(Sqrt(x2(1))/Sqrt(2d0*b-x2(1)))     
            temp5 = b*Sqrt(((2d0*b-x2(1))*x2(1))/b**2d0)
            temp6 = -2d0*temp4/temp5 
            Vol = const1*(1d0/2d0)*(temp1+temp2+temp3+temp6)	
	elseif (y_downs <= yref(j))then !Fully-open channel
	      X2(1) = y_downs
            dz = abs(S0(j)*dx(j)) 
	      const1 = b*(d(j)/2d0)/abs(S0(j)) !S0 instead of dz is used because we are computing Volume	
	      x(1)= x2(1)-dz                    
	      temp1 = PI*x(1)
	      temp2 = -2d0/3d0*b*((2d0*b-x(1))*x(1)/b**2d0)**(3d0/2d0)
	      temp3 = -2d0*x(1)*ASin(1d0-x(1)/b)
	      temp4 = x(1)*(-2d0*b+x(1))+2d0*b*Sqrt(2d0*b-x(1))*Sqrt(x(1))*
     &	      ATan(Sqrt(x(1))/Sqrt(2d0*b-x(1)))     
            temp5 = b*Sqrt(((2d0*b-x(1))*x(1))/b**2d0)
            temp6 = -2d0*temp4/temp5
            temp7 = const1*(1d0/2d0)*(temp1+temp2+temp3+temp6)
                            
            temp1 = PI*x2(1)
	      temp2 = -2d0/3d0*b*((2d0*b-x2(1))*x2(1)/b**2d0)**(3d0/2d0)
	      temp3 = -2d0*x2(1)*ASin(1d0-x2(1)/b)
	      temp4 = x2(1)*(-2d0*b+x2(1))+2d0*b*Sqrt(2d0*b-x2(1))*
     &	     Sqrt(x2(1))*ATan(Sqrt(x2(1))/Sqrt(2d0*b-x2(1)))     
            temp5 = b*Sqrt(((2d0*b-x2(1))*x2(1))/b**2d0)
            temp6 = -2d0*temp4/temp5 
            temp77 = const1*(1d0/2d0)*(temp1+temp2+temp3+temp6)
	      Vol = (temp77-temp7)		
	elseif (y_downs <= yref(j)+dz)then !partial open channel partial pressurized
	      dy = y_downs-yref(j)
	      DXTemp =  dy/abs(S0(j))
	      Vol1 = Aref(j)*DXTemp	      
	      Dx_free = dx(j)-DXTemp
	      dz = abs(S0(j))*Dx_free	      
	      const1 = b*(d(j)/2d0)/abs(S0(j)) !S0 instead of dz is used because we are computing Volume	
	      x2(1) = yref(j)
	      x(1)= x2(1)-dz                    
	      temp1 = PI*x(1)
	      temp2 = -2d0/3d0*b*((2d0*b-x(1))*x(1)/b**2d0)**(3d0/2d0)
	      temp3 = -2d0*x(1)*ASin(1d0-x(1)/b)
	      temp4 = x(1)*(-2d0*b+x(1))+2d0*b*Sqrt(2d0*b-x(1))*Sqrt(x(1))*
     &	      ATan(Sqrt(x(1))/Sqrt(2d0*b-x(1)))     
            temp5 = b*Sqrt(((2d0*b-x(1))*x(1))/b**2d0)
            temp6 = -2d0*temp4/temp5
            temp7 = const1*(1d0/2d0)*(temp1+temp2+temp3+temp6)
                            
            temp1 = PI*x2(1)
	      temp2 = -2d0/3d0*b*((2d0*b-x2(1))*x2(1)/b**2d0)**(3d0/2d0)
	      temp3 = -2d0*x2(1)*ASin(1d0-x2(1)/b)
	      temp4 = x2(1)*(-2d0*b+x2(1))+2d0*b*Sqrt(2d0*b-x2(1))*
     &	     Sqrt(x2(1))*ATan(Sqrt(x2(1))/Sqrt(2d0*b-x2(1)))     
            temp5 = b*Sqrt(((2d0*b-x2(1))*x2(1))/b**2d0)
            temp6 = -2d0*temp4/temp5 
            temp77 = const1*(1d0/2d0)*(temp1+temp2+temp3+temp6)
	      Vol2 = (temp77-temp7)
	      Vol = Vol1 + Vol2
	elseif(y_downs >= yref(j)+dz)then !fully pressurized flow
	      !Flow is fully pressurized
	      Vol = Aref(j)*dx(j)
      else
            write(98,*),'Condition unknown. Subr. Volume_sloped'
            write(99,*),'Condition unknown. Subr. Volume_sloped'
            call endprog
	endif
100   continue	
	End Subroutine