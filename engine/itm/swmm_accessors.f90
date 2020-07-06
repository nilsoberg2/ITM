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

subroutine itm_get_max_curve_val_x(node_id, val)
	implicit none

	integer node_id
	double precision val

	interface
		double precision function itm_get_max_curve_val_swmm(xory, id)
			!DEC$ ATTRIBUTES C :: itm_get_max_curve_val_swmm
			integer xory
			integer id
		end function itm_get_max_curve_val_swmm
	end interface

	val = itm_get_max_curve_val_swmm(1, node_id)

end subroutine 


subroutine itm_get_max_curve_val_y(node_id, val)
	implicit none

	integer node_id
	double precision val

	interface
		double precision function itm_get_max_curve_val_swmm(xory, id)
			!DEC$ ATTRIBUTES C :: itm_get_max_curve_val_swmm
			integer xory
			integer id
		end function itm_get_max_curve_val_swmm
	end interface

	val = itm_get_max_curve_val_swmm(2, node_id)

end subroutine 


subroutine itm_get_gate_opening(node_id, time, gateOpening)
	implicit none

	!Purpose: To get the inflow of hydrographs
	! node_id is the id of the node at which this inflow enters the network 
	! this will call SWMM routine eventually but for now we return a constant
	integer node_id
	double precision time 
	double precision gateOpening

	interface
		double precision function itm_get_gate_opening_swmm(id, time)
			!DEC$ ATTRIBUTES C :: itm_get_gate_opening_swmm
			double precision time
			integer id
		end function itm_get_gate_opening_swmm
	end interface

	gateOpening = itm_get_gate_opening_swmm(node_id, time)

end subroutine 


subroutine itm_get_gate_loss_coeff(node_id, gateOpening, coeff)
	implicit none

	!Purpose: To get the inflow of hydrographs
	! node_id is the id of the node at which this inflow enters the network 
	! this will call SWMM routine eventually but for now we return a constant
	integer node_id
	double precision gateOpening
	double precision coeff

	interface
		double precision function itm_get_gate_coeff_swmm(id, gateOpening)
			!DEC$ ATTRIBUTES C :: itm_get_gate_coeff_swmm
			double precision gateOpening
			integer id
		end function itm_get_gate_coeff_swmm
	end interface

	coeff = itm_get_gate_coeff_swmm(node_id, gateOpening)

end subroutine 


subroutine itm_get_max_rating_head(node_id, maxHead)
	implicit none

	integer node_id
	double precision maxHead

	interface
		double precision function itm_get_max_rating_head_swmm(id)
			!DEC$ ATTRIBUTES C :: itm_get_max_rating_head_swmm
			integer id
		end function itm_get_max_rating_head_swmm
	end interface

	maxHead = itm_get_max_rating_head_swmm(node_id)

end subroutine 


subroutine itm_get_swmm_id(want_link, idx, swmm_id)
	implicit none

	interface
		subroutine itm_get_swmm_id_swmm(want_link, idx, swmm_id, swmm_id_len)
			!DEC$ ATTRIBUTES C :: itm_get_swmm_id_swmm
			character*25 swmm_id
			integer idx, want_link, swmm_id_len
			!DEC$ ATTRIBUTES REFERENCE :: swmm_id
		end subroutine itm_get_swmm_id_swmm
	end interface

	integer idx, want_link, swmm_id_len
	character*25 swmm_id

	call itm_get_swmm_id_swmm(want_link, idx, swmm_id, 25)

end subroutine


!! This function is called by ITM.  It calls the C itm_open_swmm function.
function itm_get_step_count
	implicit none

	interface
		integer function itm_get_step_count_swmm
			!DEC$ ATTRIBUTES C :: itm_get_step_count_swmm
		end function itm_get_step_count_swmm
	end interface

	integer itm_get_step_count

	itm_get_step_count = itm_get_step_count_swmm()

end function


!! This function is called by ITM.  It calls the C itm_get_num_links_swmm function.
subroutine itm_get_num_links(numlinks)
	implicit none

	interface
		integer function itm_get_num_links_swmm()
			!DEC$ ATTRIBUTES C :: itm_get_num_links_swmm
		end function itm_get_num_links_swmm
	end interface

	integer numlinks


	numlinks = itm_get_num_links_swmm()

	return
end subroutine


!! This function is called by ITM.  It calls the C itm_get_num_links_swmm function.
subroutine itm_get_num_nodes(numnodes)
	implicit none

	interface
		integer function itm_get_num_nodes_swmm()
			!DEC$ ATTRIBUTES C :: itm_get_num_nodes_swmm
		end function itm_get_num_nodes_swmm
	end interface

	integer numnodes


	numnodes = itm_get_num_nodes_swmm()

	return
end subroutine


!! This function is called by ITM.  It calls the C itm_get_num_links_swmm function.
function itm_has_inflow(nodeID)
	implicit none

	interface
		integer function itm_has_inflow_swmm(nodeID)
			!DEC$ ATTRIBUTES C :: itm_has_inflow_swmm
			integer nodeID
		end function itm_has_inflow_swmm
	end interface

	integer nodeID
	integer itm_has_inflow

	itm_has_inflow = itm_has_inflow_swmm(nodeID)
end function


!! This function is called by ITM.  It calls the C itm_get_num_links_swmm function.
subroutine itm_get_starting_nodes(Nstart, start, start_count)
	implicit none

	interface
		integer function itm_get_starting_nodes_swmm(start, start_count)
			!DEC$ ATTRIBUTES C :: itm_get_starting_nodes_swmm
			integer start_count
			integer start(start_count)
		end function itm_get_starting_nodes_swmm
	end interface

	integer Nstart
	integer start_count
	integer start(start_count)


	Nstart = itm_get_starting_nodes_swmm(start, start_count)

	return
end subroutine


subroutine itm_get_error_message(errorcode, errorstring)
	implicit none

	interface
		subroutine itm_get_error_msg_swmm (errorcode, errorstring, errorstringlen)
			!DEC$ ATTRIBUTES C :: itm_get_error_msg_swmm
			integer errorcode, errorstringlen
			character*1024 errorstring
			!DEC$ ATTRIBUTES REFERENCE :: errorstring
		end subroutine itm_get_error_msg_swmm
	end interface

	integer errorcode
	character*1024 errorstring

	call itm_get_error_msg_swmm(errorcode, errorstring, 1024)

	return
end subroutine


subroutine itm_report_error
	use common_module
	implicit none
	
	interface
		subroutine itm_report_error_swmm(errorstring, errorstringlen)
			!DEC$ ATTRIBUTES C :: itm_report_error_swmm
			integer errorstringlen
			character(len=*) errorstring
			!DEC$ ATTRIBUTES REFERENCE :: errorstring
		end subroutine itm_report_error_swmm
	end interface

	call itm_report_error_swmm(error_message, error_message_len)
end subroutine


function itm_check_hotstart_status
	use common_module
	implicit none

	interface
		function itm_hotstart_status()
			!DEC$ ATTRIBUTES C :: itm_hotstart_status
			integer itm_hotstart_status
		end function itm_hotstart_status
	end interface

	integer itm_check_hotstart_status

	itm_check_hotstart_status = itm_hotstart_status()
end function


subroutine itm_set_swmm_error(error_code, error_time)
	use common_module
	implicit none

	interface
		subroutine itm_set_swmm_error_swmm(err_code, err_time)
			!DEC$ ATTRIBUTES C :: itm_set_swmm_error_swmm
			integer err_code
			double precision err_time
		end subroutine itm_set_swmm_error_swmm
	end interface
	
	integer error_code
	double precision error_time

	call itm_set_swmm_error_swmm(error_code, error_time)
	
end subroutine


subroutine itm_get_globals(nxmax_in, numitera_in, min_num_grids_in, pc_in, pcm_in, yfree_press_in, Tmax_in, Dtmax1_in, Tstor_in, T_NEXT_REPORT_in, MaxNumPlotCells_in, tol_in, tol_lower_in, tol_very_low_in, tol_higher_in, tol_crit_in, water_init_elevation_in, flow_type_in)!, hotstart_step_in)

	interface

		integer function itm_get_min_num_grids()
			!DEC$ ATTRIBUTES C :: itm_get_min_num_grids
		end function itm_get_min_num_grids

		integer function itm_get_max_num_iterations()
			!DEC$ ATTRIBUTES C :: itm_get_max_num_iterations
		end function itm_get_max_num_iterations

		integer function itm_get_max_num_cells()
			!DEC$ ATTRIBUTES C :: itm_get_max_num_cells
		end function itm_get_max_num_cells


		double precision function itm_get_polytrophic_exponent()
			!DEC$ ATTRIBUTES C :: itm_get_polytrophic_exponent
		end function itm_get_polytrophic_exponent

		double precision function itm_get_pressurized_wave_celerity()
			!DEC$ ATTRIBUTES C :: itm_get_pressurized_wave_celerity
		end function itm_get_pressurized_wave_celerity

		double precision function itm_get_mixed_flow_wave_celerity()
			!DEC$ ATTRIBUTES C :: itm_get_mixed_flow_wave_celerity
		end function itm_get_mixed_flow_wave_celerity

		double precision function itm_get_reference_depth_fraction()
			!DEC$ ATTRIBUTES C :: itm_get_reference_depth_fraction
		end function itm_get_reference_depth_fraction


		double precision function itm_get_max_simulation_time()
			!DEC$ ATTRIBUTES C :: itm_get_max_simulation_time
		end function itm_get_max_simulation_time

		double precision function itm_get_max_timestep()
			!DEC$ ATTRIBUTES C :: itm_get_max_timestep
		end function itm_get_max_timestep

		double precision function itm_get_report_interval()
			!DEC$ ATTRIBUTES C :: itm_get_report_interval
		end function itm_get_report_interval

		double precision function itm_get_report_start_time()
			!DEC$ ATTRIBUTES C :: itm_get_report_start_time
		end function itm_get_report_start_time

		integer function itm_get_max_num_plot_cells()
			!DEC$ ATTRIBUTES C :: itm_get_max_num_plot_cells
		end function itm_get_max_num_plot_cells

		double precision function itm_get_tol_normal()
			!DEC$ ATTRIBUTES C :: itm_get_tol_normal
		end function itm_get_tol_normal

		double precision function itm_get_tol_low()
			!DEC$ ATTRIBUTES C :: itm_get_tol_low
		end function itm_get_tol_low

		double precision function itm_get_tol_very_low()
			!DEC$ ATTRIBUTES C :: itm_get_tol_very_low
		end function itm_get_tol_very_low

		double precision function itm_get_tol_high()
			!DEC$ ATTRIBUTES C :: itm_get_tol_high
		end function itm_get_tol_high

		double precision function itm_get_tol_transition()
			!DEC$ ATTRIBUTES C :: itm_get_tol_transition
		end function itm_get_tol_transition

		!integer function itm_get_ini_cond()
		!	!DEC$ ATTRIBUTES C :: itm_get_ini_cond
		!end function itm_get_ini_cond

		integer function itm_get_flow_type()
			!DEC$ ATTRIBUTES C :: itm_get_flow_type
		end function itm_get_flow_type

		double precision function itm_get_water_init_elevation()
			!DEC$ ATTRIBUTES C :: itm_get_water_init_elevation
		end function itm_get_water_init_elevation

	end interface

	integer nxmax_in, numitera_in, min_num_grids_in, MaxNumPlotCells_in, ini_cond_in, flow_type_in
	double precision pc_in, pcm_in
	double precision yfree_press_in, Tmax_in, Dtmax1_in, Tstor_in
	double precision tol_in, tol_lower_in, tol_very_low_in
	double precision tol_higher_in, tol_crit_in
	double precision water_init_elevation_in
	double precision T_NEXT_REPORT_in

!	double precision hotstart_step_in

	nxmax_in			= itm_get_max_num_cells()
	numitera_in			= itm_get_max_num_iterations()
	min_num_grids_in	= itm_get_min_num_grids()
	pc_in				= itm_get_pressurized_wave_celerity()
	pcm_in				= itm_get_mixed_flow_wave_celerity()
	yfree_press_in		= itm_get_reference_depth_fraction()
	Tmax_in				= itm_get_max_simulation_time()
	Dtmax1_in			= itm_get_max_timestep()
	Tstor_in			= itm_get_report_interval()
	T_NEXT_REPORT_in	= itm_get_report_start_time()
	MaxNumPlotCells_in  = itm_get_max_num_plot_cells()

	tol_in				= itm_get_tol_normal()
	tol_lower_in		= itm_get_tol_low()
	tol_very_low_in		= itm_get_tol_very_low()
	tol_higher_in		= itm_get_tol_high()
	tol_crit_in			= itm_get_tol_transition()

	!ini_cond_in			= itm_get_ini_cond()
	water_init_elevation_in = itm_get_water_init_elevation()

	flow_type_in		= itm_get_flow_type()

!	hotstart_step_in    = itm_get_hotstart_step()

end subroutine
