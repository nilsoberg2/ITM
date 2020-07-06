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


subroutine get_curve_area(node_id, time, volume)
	implicit none

	integer node_id
	double precision time 
	double precision volume

	interface
		double precision function itm_get_curve_area(id, time)
			!DEC$ ATTRIBUTES C :: itm_get_curve_area
			double precision time
			integer id
		end function itm_get_curve_area
	end interface

	volume = itm_get_curve_area(node_id, time)	

end subroutine 
