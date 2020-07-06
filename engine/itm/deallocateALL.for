	subroutine deallocateALL
	use common_module
	implicit none
	
	!if (376 .ne. CURRENT_REVISION) then
	!	! Error
	!	write(99,*) 'CURRENT_REVISION does not match the deallocator ',
	!   &			'version of 376'
	!	write(99,*) 'Check versions and recompile if necessary.'
	!	close(99)
	!	stop
	!endif
	
!! This file is part of the ITM model.
!!
!! Copyright 2009 University of Illinois at Urbana-Champaign
!! Copyright 2011 Oregon State University, Corvallis
!!
!! Authors: Arturo S. Leon (Hydraulics), Nils Oberg (User interface)
!!
!! ITM is a free software; you can redistribute it and/or modify it
!! under the terms of the GNU General Public License as published
!! by the Free Software Foundation; either version 2.0 of the
!! License, or (at your option) any later version.
!! 
!! This program is distributed in the hope that it will be useful,
!! but WITHOUT ANY WARRANTY; without even the implied warranty of
!! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!! GNU General Public License for more details.
!! 
!! You should have received a copy of the GNU General Public License
!! along with this program; if not, write to the Free Software
!! Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
!! 02110-1301, USA.
!
!c	General parameters						
	if (ALLOCATED(S0)) DEALLOCATE(S0)
	if (ALLOCATED(z0)) DEALLOCATE(z0)
	if (ALLOCATED(zb)) DEALLOCATE(zb)
	if (ALLOCATED(IdFlow)) DEALLOCATE(IdFlow)
	if (ALLOCATED(d)) DEALLOCATE(d)
	if (ALLOCATED(Length)) DEALLOCATE(Length)
	if (ALLOCATED(Dx)) DEALLOCATE(Dx)
!
!	!Init
	if (ALLOCATED(inf)) DEALLOCATE(inf)
	if (ALLOCATED(oufl)) DEALLOCATE(oufl)
	if (ALLOCATED(Ninf)) DEALLOCATE(Ninf)
	if (ALLOCATED(Noufl)) DEALLOCATE(Noufl)
	if (ALLOCATED(Qcrit_maxIA)) DEALLOCATE(Qcrit_maxIA)
	if (ALLOCATED(Qnor_maxIA)) DEALLOCATE(Qnor_maxIA)
	if (ALLOCATED(ycrit_min)) DEALLOCATE(ycrit_min)
	if (ALLOCATED(d1_min)) DEALLOCATE(d1_min)
	if (ALLOCATED(d2_min)) DEALLOCATE(d2_min)
!
!c     Boundaries		
	if (ALLOCATED(Nnod)) DEALLOCATE(Nnod)
	if (ALLOCATED(line_elem)) DEALLOCATE(line_elem)
	if (ALLOCATED(const_depth_flow)) DEALLOCATE(const_depth_flow)
	if (ALLOCATED(min_water_pipeID)) DEALLOCATE(min_water_pipeID)
	if (ALLOCATED(Del_Y_res)) DEALLOCATE(Del_Y_res)
	if (ALLOCATED(V_head_reser)) DEALLOCATE(V_head_reser)
!
!	!for hydrographs and reserv. 	
	if (ALLOCATED(Abound)) DEALLOCATE(Abound)
	if (ALLOCATED(Qbound)) DEALLOCATE(Qbound)
	if (ALLOCATED(ybound)) DEALLOCATE(ybound)
	if (ALLOCATED(Idflow_bound)) DEALLOCATE(Idflow_bound)
	if (ALLOCATED(Idflow_bound_reser)) DEALLOCATE(Idflow_bound_reser)
!	
!	!variables from the boundaries	
	if (ALLOCATED(BCnode)) DEALLOCATE(BCnode)
	if (ALLOCATED(sum_dry_bed_node)) DEALLOCATE(sum_dry_bed_node)
	if (ALLOCATED(open_closed_bound)) DEALLOCATE(open_closed_bound)
!	
!c	Roughness
	if (ALLOCATED(nm)) DEALLOCATE(nm)
	if (ALLOCATED(fd)) DEALLOCATE(fd)
	if (ALLOCATED(Node1)) DEALLOCATE(Node1)
	if (ALLOCATED(Node2)) DEALLOCATE(Node2)
!
!
!c	Parameters Star region 
!      !CODE_STAR_REGION = 1. Variabels at star region are computed. Otherwise, no. 
!
!c	Parameters open channel/pressurized flow
	if (ALLOCATED(h0)) DEALLOCATE(h0)
	if (ALLOCATED(h0_Rec)) DEALLOCATE(h0_Rec)
	if (ALLOCATED(A0)) DEALLOCATE(A0)
	if (ALLOCATED(Q0)) DEALLOCATE(Q0)
	if (ALLOCATED(AREA_FULL)) DEALLOCATE(AREA_FULL)
	if (ALLOCATED(Area_for_pressur)) DEALLOCATE(Area_for_pressur)
	if (ALLOCATED(y_for_pressur)) DEALLOCATE(y_for_pressur)
!
!
	if (ALLOCATED(Aref)) DEALLOCATE(Aref)
	if (ALLOCATED(Phiref)) DEALLOCATE(Phiref)
	if (ALLOCATED(Yref)) DEALLOCATE(Yref)
	if (ALLOCATED(haver_ref)) DEALLOCATE(haver_ref)
	if (ALLOCATED(celer_ref)) DEALLOCATE(celer_ref)
	if (ALLOCATED(P_pho_ref)) DEALLOCATE(P_pho_ref)
	if (ALLOCATED(P_pho_dry)) DEALLOCATE(P_pho_dry)
!	
!
	if (ALLOCATED(pc1)) DEALLOCATE(pc1)
	if (ALLOCATED(fully_pressuri)) DEALLOCATE(fully_pressuri)
!c	pref	=	Reference pressure
!c	RoRef	=	Reference density
!c	pc = pressurized flow wave speed
!
!	
!	!values at adjacent cells and at boundaries at old time step	
!	!NodeNS(r) = Number of pipes connected to the node (1, 2, 3 ....)
!	!NodeID(r,j) = pipe ID(1, 2, 3, ...) 
!	!Nodetype = inflowing or outflowing
!	!maximum 10 pipes can be connected to each node
	if (ALLOCATED(NodeNS)) DEALLOCATE(NodeNS)
	if (ALLOCATED(Nodetype)) DEALLOCATE(Nodetype)
!	
!c	Parameters air pocket
!	!air pressure head expressed in equivalent water head
	if (ALLOCATED(ha)) DEALLOCATE(ha)
!	!X minimum and maximum cell IDS for air pockets
	if (ALLOCATED(XapMIN)) DEALLOCATE(XapMIN)
	if (ALLOCATED(XapMAX)) DEALLOCATE(XapMAX)
	if (ALLOCATED(XapMINold)) DEALLOCATE(XapMINold)
	if (ALLOCATED(XapMAXold)) DEALLOCATE(XapMAXold)
!	!Number of air pocket
	if (ALLOCATED(Nap)) DEALLOCATE(Nap)
	if (ALLOCATED(Napold)) DEALLOCATE(Napold)
!	!volume of air pocket
	if (ALLOCATED(VOLap)) DEALLOCATE(VOLap)
	if (ALLOCATED(VOLap_old)) DEALLOCATE(VOLap_old)
!	!air pressure in water column 
	if (ALLOCATED(hap)) DEALLOCATE(hap)
	if (ALLOCATED(hapold)) DEALLOCATE(hapold)
!	!politropic exponent for air 
!	!ka = 1.0, 1.4 or somewhere between
!	!Cd = air discharge coefficient 
!
!	!air density
	if (ALLOCATED(dens_old)) DEALLOCATE(dens_old)
	if (ALLOCATED(dens)) DEALLOCATE(dens)
!
!c	Various	
	if (ALLOCATED(Atemp1)) DEALLOCATE(Atemp1)
	if (ALLOCATED(htemp1)) DEALLOCATE(htemp1)
	if (ALLOCATED(Qtemp1)) DEALLOCATE(Qtemp1)
	if (ALLOCATED(IdFlow1)) DEALLOCATE(IdFlow1)
	if (ALLOCATED(IdFlow_REC_L)) DEALLOCATE(IdFlow_REC_L)
	if (ALLOCATED(IdFlow_REC_R)) DEALLOCATE(IdFlow_REC_R)
	if (ALLOCATED(A0L)) DEALLOCATE(A0L)
	if (ALLOCATED(A0R)) DEALLOCATE(A0R)
	if (ALLOCATED(Q0L)) DEALLOCATE(Q0L)
	if (ALLOCATED(Q0R)) DEALLOCATE(Q0R)
!
!	!## Upstream IDs
!	
!
!	!## Junction
	if (ALLOCATED(Junct)) DEALLOCATE(Junct)
	if (ALLOCATED(max_crown_pipe)) DEALLOCATE(max_crown_pipe)
	if (ALLOCATED(Drop)) DEALLOCATE(Drop)
	if (ALLOCATED(Ares_junct)) DEALLOCATE(Ares_junct)
	if (ALLOCATED(yres_jun_old)) DEALLOCATE(yres_jun_old)
	if (ALLOCATED(height_jun)) DEALLOCATE(height_jun)
	if (ALLOCATED(max_elev_crown)) DEALLOCATE(max_elev_crown)
!	
!		
!	!## Reservoir 	
!	
	if (ALLOCATED(flowdepth_res)) DEALLOCATE(flowdepth_res)
	if (ALLOCATED(yres_up)) DEALLOCATE(yres_up)
	if (ALLOCATED(Reser_outflow)) DEALLOCATE(Reser_outflow)
	if (ALLOCATED(reser_maxdepth)) DEALLOCATE(reser_maxdepth)
!
!	!## Dropshaft
	if (ALLOCATED(Adrop)) DEALLOCATE(Adrop)
	if (ALLOCATED(hdrops_overf)) DEALLOCATE(hdrops_overf)
	if (ALLOCATED(junct_elev)) DEALLOCATE(junct_elev)
!
	if (ALLOCATED(V_over)) DEALLOCATE(V_over)
	if (ALLOCATED(nodeID)) DEALLOCATE(nodeID)
!
!	!(len=1024)
!
!		
	if (ALLOCATED(yudrop_n)) DEALLOCATE(yudrop_n)
	if (ALLOCATED(yudrop_n_1)) DEALLOCATE(yudrop_n_1)
	if (ALLOCATED(ydropmin)) DEALLOCATE(ydropmin)
	if (ALLOCATED(dropmin)) DEALLOCATE(dropmin)
	if (ALLOCATED(Areser_min)) DEALLOCATE(Areser_min)
!			
!	!## Variables at interfaces (i+1/2)	
	if (ALLOCATED(Fupst)) DEALLOCATE(Fupst)
	if (ALLOCATED(Fdownst)) DEALLOCATE(Fdownst)
	if (ALLOCATED(Pres_pho_Bound)) DEALLOCATE(Pres_pho_Bound)
	if (ALLOCATED(F)) DEALLOCATE(F)
	if (ALLOCATED(P_pho_interf)) DEALLOCATE(P_pho_interf)
	if (ALLOCATED(y_interf)) DEALLOCATE(y_interf)
	if (ALLOCATED(A_interf)) DEALLOCATE(A_interf)
!	
!	!constants	
!	!Hb = Atmospheric pressure in m of water
!	!RoRef_air =  air density
	if (ALLOCATED(NX)) DEALLOCATE(NX)
!	!result of discretization
!
!	!Dry bed 
	if (ALLOCATED(ydry)) DEALLOCATE(ydry)
	if (ALLOCATED(Adry)) DEALLOCATE(Adry)
	if (ALLOCATED(phi_dry)) DEALLOCATE(phi_dry)
	if (ALLOCATED(Celer_dry)) DEALLOCATE(Celer_dry)
	if (ALLOCATED(ydry_CFL)) DEALLOCATE(ydry_CFL)
	if (ALLOCATED(Adry_CFL)) DEALLOCATE(Adry_CFL)
	if (ALLOCATED(fluxdry)) DEALLOCATE(fluxdry)
!	
!	!sloped pipe
!	
	if (ALLOCATED(A_cell_dry_sloped)) DEALLOCATE(A_cell_dry_sloped)
	if (ALLOCATED(A_open_sloped_pipe)) DEALLOCATE(A_open_sloped_pipe)
	if (ALLOCATED(b2_max)) DEALLOCATE(b2_max)
	if (ALLOCATED(A2_max)) DEALLOCATE(A2_max)
!      !b2_min and A2_min are for iteration purposes
	if (ALLOCATED(b2_min)) DEALLOCATE(b2_min)
	if (ALLOCATED(A2_min)) DEALLOCATE(A2_min)
	if (ALLOCATED(P_pho_min)) DEALLOCATE(P_pho_min)
	if (ALLOCATED(P_pho_max)) DEALLOCATE(P_pho_max)
	if (ALLOCATED(phi_max)) DEALLOCATE(phi_max)
	if (ALLOCATED(phi_min)) DEALLOCATE(phi_min)
	if (ALLOCATED(y_for_phi_max)) DEALLOCATE(y_for_phi_max)
!	
!	!Dtmax
!	
!      !Internal Tolerances
!
!	!stored volume
!	
!	!Lost volume
!
!	!SWMM
!	
!	!various
	if (ALLOCATED(Klocal)) DEALLOCATE(Klocal)
!	
!	!Rating curve
	if (ALLOCATED(Max_flow_rating_curve)) DEALLOCATE(Max_flow_rating_curve)
!	!Maximum flow specified at the rating curve. This is used to check if the water level in the rating curve is exceeded. 
	if (ALLOCATED(Max_Head_rating_curve)) DEALLOCATE(Max_Head_rating_curve)
!	!Maximum flow specified at the rating curve. This is used to check if the water level in the rating curve is exceeded. 
!	
!	!Gates
	if (ALLOCATED(Cd_gate)) DEALLOCATE(Cd_gate)
	if (ALLOCATED(Hgate_open)) DEALLOCATE(Hgate_open)
	if (ALLOCATED(h_gate_m)) DEALLOCATE(h_gate_m)
!		
!	!Volume check
!
!	
!	
!
	if (ALLOCATED(EntranceLoss)) DEALLOCATE(EntranceLoss)
	if (ALLOCATED(ExitLoss)) DEALLOCATE(ExitLoss)
!
!
!
!
!
!
!
!
!	
!	!Parameters used for solving non-linear equations
!		
!
	if (ALLOCATED(Qmin)) DEALLOCATE(Qmin)
!
!	!Pumping rates at selected nodes. 
	if (ALLOCATED(Qpump)) DEALLOCATE(Qpump)
	if (ALLOCATED(t_begin_pump)) DEALLOCATE(t_begin_pump)
	if (ALLOCATED(IDpump)) DEALLOCATE(IDpump)
!
!	
!	!!!!!! Parameters thtat don't need to get saved to hotstart file go here.
	if (ALLOCATED(h0L)) DEALLOCATE(h0L)
	if (ALLOCATED(h0R)) DEALLOCATE(h0R)
!	
!
!	!Pressurization and Depressurization of the system
!
!	!To relax time steps
!
	end subroutine
