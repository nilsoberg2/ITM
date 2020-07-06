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

      subroutine converg (conver_result, info)	
	use common_module
	implicit none
	integer conver_result, info
	!This routine is used for determining if convergence has been achieved 
	!when solving non-linear equations. 

	!conver_result = 0 (solution converges) 
	!conver_result = 1 (solution doesn't converge) 
	
	if (info == 1)then	
		conver_result = 0
		!parint1000 = 0	  
	else
		conver_result = 1
		parint1000 = 1
		sum_no_converg = sum_no_converg + 1	  
		!write(99,*),'failure in convergence No', sum_no_converg 
		if (info == 0)then	
			!write(99,*),'0, improper input parameters. Time = ',T_GLOBAL  
		elseif (info == 2)then	
!			write(99,*),'2, # of calls to FCN has reached 
!     &			or exceeded 200*(N+1). Time = ',T_GLOBAL  
		elseif (info == 3)then	
!			write(99,*),'3, TOL is too small.  No further improvement in the
!     &			 approximate solution of X is possible. Time = ',T_GLOBAL 
		elseif (info == 4)then	
!			write(99,*),'4, Iteration is not making progress. Time = ',T_GLOBAL
		else  
			write(98,*),'Unknown reason for stoppping iterations. Time=',T_GLOBAL
			call endprog
		endif
	endif
	end subroutine