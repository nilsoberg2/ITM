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

      subroutine H_from_Phi(j,phi_Star,y)
	use common_module
      implicit none
      integer j,i
	double precision y,teta,Phi,phi_Star,deriv_phi
	double precision FF,DFF,DELY,temp1
	
	If (ISNAN(phi_Star))then 
	      write(99,*),'phi_Star is NaN',phi_Star
	      write(99,*),'subroutine H_from_Phi'
	      write(98,*),'phi_Star is NaN',phi_Star
	      write(98,*),'subroutine H_from_Phi'
	      call endprog			
      endif
	
	if(phi_Star <= phi_min(j))then
		   y = b2_min(j)
		    goto 10
	endif
	
	if(phi_Star >= phi_max(j))then
		    y = y_for_phi_max(j)
		    goto 10
	endif
	
	y = ydry(j)
	teta = 2d0*ACOS(1d0-2d0*y/d(j))
	do i = 1,numitera
	      if (teta < 1.d-13)then
			    teta = 1.d-13
		    endif
		    if (teta > (1d0-1.d-12)*2d0*PI)then
			    teta = (1d0-1.d-12)*2d0*PI
	      endif
	
		    call Phi1(j,y,phi)
		    FF = phi-phi_Star
		    call der_Phi(g,y,d(j),deriv_phi)
		    DFF = deriv_phi
		    DELY = FF/DFF								
		    temp1 = teta - DELY/2d0 
		    teta = teta - DELY	
		    y = d(j)/2d0*(1d0-COS(teta/2d0))
		    IF(ABS(DELY)<Tol_int_10_8*temp1)goto 10
 	enddo
	write(98,*),'Error in finding theta that corresponds	
     & to Phi. subroutine H_from_Phi',y,phi_Star			
	CALL endprog											
10	continue	
	return		
      end
