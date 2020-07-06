! This file is part of the ITM model.
!
! Copyright 2010 Boise State University
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


subroutine get_Q_from_rat_curve(node_id, yb, Q)
	implicit none

	!Purpose: To get the flow from a rating curve given a depth
	! node_id is the id of the node at which this rating curve is desired to be used for
	integer node_id
	double precision yb
	double precision Q

	interface
		double precision function itm_get_q_from_rat_curve(id, yb)
			!DEC$ ATTRIBUTES C :: itm_get_q_from_rat_curve
			double precision yb
			integer id
		end function itm_get_q_from_rat_curve
	end interface

	Q = itm_get_q_from_rat_curve(node_id, yb)	

end subroutine 
