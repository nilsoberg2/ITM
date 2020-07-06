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

subroutine readswmm(max_num_pipes, node1, node2, length, d, nm, zb, init_flow, init_depth_type, init_depth, entrance_loss, exit_loss, max_num_nodes, const_depth_flow, drop_count, Adrop, hdrops_overf, res_count, flowdepth_res, reser_outflow, reser_maxdepth, bc_count, junc_elev, node_bc, open_bc, itmresult)
	implicit none

	!!!!!!!!!!!! IMPORTANT STUFF
	! Put these !DEC$ things in the function prototype interface

	!!DEC$ ATTRIBUTES C :: itm_run								! FOR ACCESSING C FUNCTION
	!!DEC$ ATTRIBUTES DLLIMPORT :: itm_run						! FOR ACCESSING C/C++ FUNCTION IN DLL
	!!DEC$ ATTRIBUTES ALIAS:'®itm_run@@YAXHPAD0@Z' :: itm_run	! FOR ACCESSING C++ FUNCTION
	!!DEC$ ATTRIBUTES REFERENCE :: s1, s2... ! Specify that the CHARACTER arguments are by reference with no hidden length 

	!!!!!!!!!!!! INTERFACE TO C FUNCTIONS
	interface

!		integer function itm_run (f1, f2, f3)
!			!DEC$ ATTRIBUTES C :: itm_run
!			character*(*) f1, f2, f3
!			!DEC$ ATTRIBUTES REFERENCE :: f1, f2, f3
!		end function itm_run
!
!		integer function itm_open(f1, f2, f3)
!			!DEC$ ATTRIBUTES C :: itm_open
!			character*(*) f1, f2, f3
!			!DEC$ ATTRIBUTES REFERENCE :: f1, f2, f3
!		end function itm_open
!
!		integer function itm_close ()
!			!DEC$ ATTRIBUTES C :: itm_close
!		end function itm_close

		subroutine itm_get_error_msg (errorcode, errorstring, errorstringlen)
			!DEC$ ATTRIBUTES C :: itm_get_error_msg
			integer errorcode, errorstringlen
			character*256 errorstring
			!DEC$ ATTRIBUTES REFERENCE :: errorstring
		end subroutine itm_get_error_msg

		integer function itm_copy (max_num_pipes, node1, node2, length, d, nm, zb, init_flow, init_depth_type, init_depth, entrance_loss, exit_loss, max_num_nodes, const_depth_flow, drop_count, Adrop, hdrops_overf, res_count, flowdepth_res, reser_outflow, reser_maxdepth, bc_count, junc_elev, node_bc, open_bc)
			!DEC$ ATTRIBUTES C :: itm_copy
			integer				max_num_pipes
			integer				max_num_nodes
			integer				drop_count
			integer				res_count
			integer				bc_count

			integer				node1(max_num_pipes)
			integer				node2(max_num_pipes)
			double precision	zb(max_num_pipes, 2)
			double precision	length(max_num_pipes)
			double precision	d(max_num_pipes)
			double precision	nm(max_num_pipes)
			double precision	init_flow(max_num_pipes)
			integer				init_depth_type(max_num_pipes)
			double precision	init_depth(max_num_pipes)
			double precision	entrance_loss(max_num_pipes)
			double precision	exit_loss(max_num_pipes)
			double precision	Adrop(drop_count)
			double precision	hdrops_overf(drop_count)
			double precision	flowdepth_res(res_count)
			double precision	reser_outflow(res_count)
			integer				node_bc(bc_count)
			integer				open_bc(bc_count)
			double precision	junc_elev(bc_count)
			double precision	const_depth_flow(max_num_nodes)
			double precision	reser_maxdepth(res_count)
		end function itm_copy

	end interface
	!!!!!!!!!!!! END INTERFACE TO C FUNCTIONS

	integer				max_num_pipes
	integer				max_num_nodes
	integer				drop_count
	integer				res_count
	integer				bc_count

	integer				node1(max_num_pipes)
	integer				node2(max_num_pipes)
	double precision	zb(max_num_pipes, 2)
	double precision	length(max_num_pipes)
	double precision	d(max_num_pipes)
	double precision	nm(max_num_pipes)
	double precision	init_flow(max_num_pipes)
	integer				init_depth_type(max_num_pipes)
	double precision	init_depth(max_num_pipes)
	double precision	entrance_loss(max_num_pipes)
	double precision	exit_loss(max_num_pipes)
	double precision	Adrop(drop_count)
	double precision	hdrops_overf(drop_count)
	double precision	flowdepth_res(res_count)
	double precision	reser_outflow(res_count)
	integer				node_bc(bc_count)
	integer				open_bc(bc_count)
	double precision	junc_elev(bc_count)
	double precision	const_depth_flow(max_num_nodes)
	double precision	reser_maxdepth(res_count)

	character*1024 errorstring
	integer itmresult


	! itm_open must have already been called by this point.
	!itmresult = itm_open("test.inp"C, "test.report.txt"C, "test.output.dat"C)
	!if (itmresult > 0) then
	!	call itm_get_error_msg(itmresult, errorstring, 256)
	!	write(99,*) errorstring
	!	readswmm = itmresult
	!	return
	!endif


	! Copy the values that have been parsed by SWMM and placed in memory to the ITM arrays
	itmresult = itm_copy(max_num_pipes, node1, node2, length, d, nm, zb, init_flow, init_depth_type, init_depth, entrance_loss, exit_loss, max_num_nodes, const_depth_flow, drop_count, Adrop, hdrops_overf, res_count, flowdepth_res, reser_outflow, reser_maxdepth, bc_count, junc_elev, node_bc, open_bc)

	if (itmresult > 0) then
		call itm_get_error_message(itmresult, errorstring)
		write(99,*) errorstring
		return
	endif


	return
end


subroutine itm_save_link_data(currentStep, id, numCells, plotStation, plotFlows, plotDepths, plotEnergy)

	use common_module

	integer id, numCells, j
	double precision plotStation(MaxNumPlotCells)
	double precision plotFlows(MaxNumPlotCells)
	double precision plotDepths(MaxNumPlotCells)
	double precision plotEnergy(MaxNumPlotCells)
	double precision currentStep
	integer blank1, blank2

	! blanks to pad the record if there are fewer than MaxNumPlotCells written.
	blank1 = 22
	blank2 = 97
	! 2297 =  78 105 108 115 32 76 111 118 101 115 32 69 109 105 108 121 32 70 111 114 101 118 101 114 33

	write(95) currentStep, id, numCells
	write(95) (plotStation(j),	j=1, numCells)
	write(95) (plotFlows(j),	j=1, numCells)
	write(95) (plotDepths(j),	j=1, numCells)
	write(95) (plotEnergy(j),	j=1, numCells)
	write(95) (blank1, blank2,	j=4*numCells+1, 4*MaxNumPlotCells)
	
end subroutine


function itm_open_output_file(itm_file)
	use common_module

	character(*) itm_file
	integer itm_open_output_file

	itm_open_output_file = 0
	open(95, file=itm_file, status='REPLACE', form='BINARY', iostat=itm_open_output_file, err=756)

	! Save the maximum number of plot cells to the ITM output file.
	! We pad the record with zeros if we save less than MaxNumPlotCells
	! to the ITM output file.
	write(95) MaxNumPlotCells

756	return

end function


function itm_close_output_file

	integer itm_close_output_file

	itm_close_output_file = 0
	close(95, iostat=itm_close_output_file)

end function


!subroutine itm_save_vol_err(current_time, vol_error_pct, vol_error_diff)
!
!	use common_module
!
!	interface
!		subroutine itm_save_vol_err_swmm(current_t, vol_err_pct, vol_err_diff)
!			!DEC$ ATTRIBUTES C :: itm_save_vol_err_swmm
!	        double precision vol_err_pct
!	        double precision vol_err_diff
!	        double precision current_t
!		end subroutine itm_save_vol_err_swmm
!	end interface
!	
!	double precision vol_error_pct
!	double precision vol_error_diff
!	double precision current_time
!
!	call itm_save_vol_err_swmm(current_time, vol_error_pct, vol_error_diff)
!	
!end subroutine


!function itm_open_vol_err_file(vol_err_file)
!	use common_module
!
!	character(*) vol_err_file
!	integer itm_open_vol_err_file
!
!	itm_open_output_file = 0
!	open(94, file=vol_err_file, status='REPLACE', form='BINARY', iostat=itm_open_vol_err_file, err=757)
!
!757	return
!
!end function


!function itm_close_vol_err_file
!
!	integer itm_close_vol_err_file
!
!	itm_close_vol_err_file = 0
!	write(94) NumVolErrPoints
!	close(94, iostat=itm_close_vol_err_file)
!
!end function
