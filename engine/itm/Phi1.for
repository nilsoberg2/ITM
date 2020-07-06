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

      subroutine Phi1(j,h,phi)
	use common_module
      implicit none
	double precision h,phi 	     
	double precision teta,temp1,temp2
	integer j
      if (t_global > 0.0)then      
	      If (h >= y_for_phi_max(j))then 
	          !y_for_phi_max(j) is used instead of b2max because phi 
	          !is computed for the circular cross section only. 
	          h = y_for_phi_max(j)
	          phi = phi_max(j)
	          goto 10
	      Endif
	    
	      If (h <= b2_min(j))then
		        h = b2_min(j)
		        phi = phi_min(j)
	          goto 10
	      Endif
      endif
	
	teta = 2d0*ACOS(1d0-2d0*h/d(j))				
	temp1 = sqrt(g*d(j)/8d0)*sqrt(3d0)
	temp2 = (teta - teta**3d0/80d0 + 19d0/448000d0*
     &	    teta**5d0 + teta**7d0/10035200d0 +
     &	    491d0*teta**9d0/27d0/7064780800d0)	
	phi = temp1*temp2 		
10    return
      end subroutine
