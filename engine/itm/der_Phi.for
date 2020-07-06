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

	subroutine der_Phi(g,h,d,deriv_phi)
c	Derivative of Phi	
      implicit none 
	double precision g,h,d, deriv_phi  	     
	double precision teta, temp2,temp3,temp4
	If (h <= 0.0) then
		write(98,*),'Negative water depth. Subroutine der_Phi'
		call endprog
	Endif
	
	If (h >= d) then
		write(98,*),'water depth exceeds diameter. Subroutine der_Phi'
		call endprog
	Endif
			

	temp2 = sqrt(g*d/8d0)
	teta = 2d0*acos(1d0-2d0*h/d)	
	temp3 = 1d0-cos(teta)
	temp4 = (teta-sin(teta))*sin(teta/2d0)
	If (temp4 <= 0) then
		write(98,*),'temp4 <= 0. Subroutine der_Phi'
		call endprog
	Endif			
	deriv_phi = temp2*temp3/sqrt(temp4)
      return
      end subroutine
