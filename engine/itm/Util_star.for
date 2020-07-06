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

    	!########################################
	!Star region approximation
	!Linearized Riemann solver
	function Linearized(AL,AR,cL,cR,uL,uR)
      implicit none
	double precision AL,AR,cL,cR,uL,uR	
      double precision Aaver,caver,Astar
	double precision Linearized
	Aaver = (AL+AR)/2d0
	caver = (cL+cR)/2d0
	Astar = Aaver*(1d0 + (uL-uR)/(2d0*caver))
	Linearized = Astar
	return
	end function

	!########################################
	!Star region approximation
	!Linearized Riemann solver + depth positivity condition
	function depth_posit(AL,AR,phiL,phiR,uL,uR)
      implicit none
	double precision AL,AR,phiL,phiR,uL,uR	
      double precision Aaver,Astar
	double precision depth_posit
	Aaver = (AL+AR)/2d0	
	Astar = Aaver*(1d0+(uL-uR)/(phiL+phiR))
	depth_posit = Astar
	return
	end function
