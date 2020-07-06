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

!! This function is called by ITM.  It calls the C itm_open_swmm function.
subroutine itm_end
	implicit none

	interface
		integer function itm_end_swmm
			!DEC$ ATTRIBUTES C :: itm_end_swmm
		end function itm_end_swmm
		integer function itm_close_swmm
			!DEC$ ATTRIBUTES C :: itm_close_swmm
		end function itm_close_swmm
	end interface

	character*1024 errorstring
	integer itmresult
	
	itmresult = itm_end_swmm()
    !itmresult = itm_close_swmm()

	if (itmresult > 0) then
		call itm_get_error_message(itmresult, errorstring)
		write(99,*) errorstring
		return
	endif
	
	return
end subroutine


!! This function is called by ITM.  It calls the C itm_report_swmm function.
subroutine itm_finish_report
	implicit none

	interface
		integer function itm_report_swmm
			!DEC$ ATTRIBUTES C :: itm_report_swmm
		end function itm_report_swmm
	end interface

	character*1024 errorstring
	integer itmresult

	itmresult = itm_report_swmm()

	if (itmresult > 0) then
		call itm_get_error_message(itmresult, errorstring)
		write(99,*) errorstring
		return
	endif

	return
end subroutine


!! This function is called by ITM.  It calls the C itm_start_swmm function.
function itm_start
	implicit none

	interface
		integer function itm_start_swmm
			!DEC$ ATTRIBUTES C :: itm_start_swmm
		end function itm_start_swmm
	end interface

	integer itm_start
	character*1024 errorstring
	integer itmresult

	itmresult = itm_start_swmm()

	if (itmresult > 0) then
!		call itm_get_error_message(itmresult, errorstring)
!		write(99,*) errorstring
		itm_start = itmresult
	else
		itm_start = 0
	endif

end


!! This function is called by ITM.  It calls the C itm_step_swmm function.
subroutine itm_step
	implicit none

	interface
		integer function itm_step_swmm
			!DEC$ ATTRIBUTES C :: itm_step_swmm
		end function itm_step_swmm
	end interface

	character*1024 errorstring
	integer itmresult

	itmresult = itm_step_swmm()

	if (itmresult > 0) then
		call itm_get_error_message(itmresult, errorstring)
		write(99,*) errorstring
		return
	endif

	return
end subroutine


!! This function is called by ITM.  It calls the C itm_report_step_swmm function.
subroutine itm_report_step(currentStep, numRows, linkFlows, linkDepths, linkVelocities, linkFroudes, nodeDepths, sysVolErrPct, sysVolErrDiff)
	implicit none

	interface
		integer function itm_report_step_swmm(currentStep, numRows, linkDepths, linkFlows, linkVelocities, linkFroudes, nodeDepths, sysVolErrPct, sysVolErrDiff)
			!DEC$ ATTRIBUTES C :: itm_report_step_swmm
			integer numRows
			double precision linkDepths(numRows)
			double precision linkFlows(numRows)
			double precision nodeDepths(numRows)
			double precision linkVelocities(numRows)
			double precision linkFroudes(numRows)
			double precision currentStep
	        double precision sysVolErrPct
	        double precision sysVolErrDiff
		end function itm_report_step_swmm
	end interface

	integer numRows
	double precision linkDepths(numRows)
	double precision linkFlows(numRows)
	double precision nodeDepths(numRows)
	double precision linkVelocities(numRows)
	double precision linkFroudes(numRows)
	double precision currentStep
	double precision sysVolErrPct
	double precision sysVolErrDiff

	character*1024 errorstring
	integer itmresult

	! Open and parse the input file using SWMM
	itmresult = itm_report_step_swmm(currentStep, numRows, linkDepths, linkFlows, linkVelocities, linkFroudes, nodeDepths, sysVolErrPct, sysVolErrDiff)

	if (itmresult > 0) then
		call itm_get_error_message(itmresult, errorstring)
		write(99,*) errorstring
		return
	endif

	return
end subroutine


!! This function is called by ITM.  It calls the C itm_open_start_swmm function.
subroutine itm_open_start(f1, f2, f3, itmresult)
	implicit none

	interface
		integer function itm_open_start_swmm(f1, f2, f3)
			!DEC$ ATTRIBUTES C :: itm_open_start_swmm
			character*(*) f1, f2, f3
			!DEC$ ATTRIBUTES REFERENCE :: f1, f2, f3
		end function itm_open_start_swmm
	end interface

	character*(*) f1, f2, f3
	character*1024 errorstring
	integer itmresult


	! Open and parse the input file using SWMM
	itmresult = itm_open_start_swmm(f1, f2, f3)

	if (itmresult > 0) then
		call itm_get_error_message(itmresult, errorstring)
		write(99,*) errorstring
		return
	endif


	return
end subroutine


!! This function is called by ITM.  It calls the C itm_close_swmm function.
subroutine itm_close(itmresult)
	implicit none

	interface
		integer function itm_close_swmm ()
			!DEC$ ATTRIBUTES C :: itm_close_swmm
		end function itm_close_swmm
	end interface

	integer itmresult


	itmresult = itm_close_swmm()

	return
end subroutine
