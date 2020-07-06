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

      subroutine comments (j) 
      !Purpose: To present changes that need to be done  and important comments to keep in mind

      !Recent comments
      
      !3. complex system is not working> it seems that something is wrong with relax condition == 2 or 1
      
	!single-phase flows
	 !1. Riemann invariants can not be used for outflowing pipes and supercritical flows
     		
	!3. Modify ERP combined subroutine. Free surface flow is accepted until (Aref +Afull)/2. 
	!This may generate problems when free surface flows and pressurized flows are present

	
	 !4. the computation of the interface speed is good in this version. use this for all.
	
	

	!Two-phase flows

	! 7. use this W. W = QL-QR/(AL-AR). This is the best.
	 !8. criteria for pressurized flow was modified in routine source_term. This is very important.
	 
	 !9. Routines for weak interfaces were added in routine open-pressurized

	!10. Source term has been substantially modified. very important  





	end
