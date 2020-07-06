function WRITE_HOTSTART_VARS
	use common_module
	implicit none
	integer M,N,temp
	integer WRITE_HOTSTART_VARS

	WRITE_HOTSTART_VARS = 0
	
	!if (376 .ne. CURRENT_REVISION) then
	!	WRITE_HOTSTART_VARS = 1
	!	write(99,*) 'CURRENT_REVISION does not match the WRITE_HOTSTART_VARS version of 376'
	!	write(99,*) 'Check versions and recompile if necessary.'
	!	call endprog
	!endif
	
	write(77) 376
	
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
	if (ALLOCATED(S0)) then
		write(77) 1
		temp = UBOUND(S0,1)
		write(77) 1,1,temp
		write(77) (S0(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(z0)) then
		write(77) 1
		temp = UBOUND(z0,1)
		write(77) 1,2,temp,UBOUND(z0,2)
		write(77) ((z0(N,M),N=1,temp),M=1,UBOUND(z0,2))
	else
		write(77) 0
	endif
	if (ALLOCATED(zb)) then
		write(77) 1
		temp = UBOUND(zb,1)
		write(77) 1,2,temp,UBOUND(zb,2)
		write(77) ((zb(N,M),N=1,temp),M=1,UBOUND(zb,2))
	else
		write(77) 0
	endif
	if (ALLOCATED(IdFlow)) then
		write(77) 1
		temp = UBOUND(IdFlow,1)
		write(77) 1,2,temp,UBOUND(IdFlow,2)
		write(77) ((IdFlow(N,M),N=1,temp),M=1,UBOUND(IdFlow,2))
	else
		write(77) 0
	endif
	if (ALLOCATED(d)) then
		write(77) 1
		temp = UBOUND(d,1)
		write(77) 1,1,temp
		write(77) (d(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(Length)) then
		write(77) 1
		temp = UBOUND(Length,1)
		write(77) 1,1,temp
		write(77) (Length(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(Dx)) then
		write(77) 1
		temp = UBOUND(Dx,1)
		write(77) 1,1,temp
		write(77) (Dx(M),M=1,temp)
	else
		write(77) 0
	endif
!
!	!Init
	if (ALLOCATED(inf)) then
		write(77) 1
		temp = UBOUND(inf,1)
		write(77) 1,2,temp,UBOUND(inf,2)
		write(77) ((inf(N,M),N=1,temp),M=1,UBOUND(inf,2))
	else
		write(77) 0
	endif
	if (ALLOCATED(oufl)) then
		write(77) 1
		temp = UBOUND(oufl,1)
		write(77) 1,2,temp,UBOUND(oufl,2)
		write(77) ((oufl(N,M),N=1,temp),M=1,UBOUND(oufl,2))
	else
		write(77) 0
	endif
	if (ALLOCATED(Ninf)) then
		write(77) 1
		temp = UBOUND(Ninf,1)
		write(77) 1,1,temp
		write(77) (Ninf(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(Noufl)) then
		write(77) 1
		temp = UBOUND(Noufl,1)
		write(77) 1,1,temp
		write(77) (Noufl(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(Qcrit_maxIA)) then
		write(77) 1
		temp = UBOUND(Qcrit_maxIA,1)
		write(77) 1,1,temp
		write(77) (Qcrit_maxIA(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(Qnor_maxIA)) then
		write(77) 1
		temp = UBOUND(Qnor_maxIA,1)
		write(77) 1,1,temp
		write(77) (Qnor_maxIA(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(ycrit_min)) then
		write(77) 1
		temp = UBOUND(ycrit_min,1)
		write(77) 1,1,temp
		write(77) (ycrit_min(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(d1_min)) then
		write(77) 1
		temp = UBOUND(d1_min,1)
		write(77) 1,1,temp
		write(77) (d1_min(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(d2_min)) then
		write(77) 1
		temp = UBOUND(d2_min,1)
		write(77) 1,1,temp
		write(77) (d2_min(M),M=1,temp)
	else
		write(77) 0
	endif
!
!c     Boundaries		
	if (ALLOCATED(Nnod)) then
		write(77) 1
		temp = UBOUND(Nnod,1)
		write(77) 1,1,temp
		write(77) (Nnod(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(line_elem)) then
		write(77) 1
		temp = UBOUND(line_elem,1)
		write(77) 1,2,temp,UBOUND(line_elem,2)
		write(77) ((line_elem(N,M),N=1,temp),M=1,UBOUND(line_elem,2))
	else
		write(77) 0
	endif
	if (ALLOCATED(const_depth_flow)) then
		write(77) 1
		temp = UBOUND(const_depth_flow,1)
		write(77) 1,1,temp
		write(77) (const_depth_flow(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(min_water_pipeID)) then
		write(77) 1
		temp = UBOUND(min_water_pipeID,1)
		write(77) 1,1,temp
		write(77) (min_water_pipeID(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(Del_Y_res)) then
		write(77) 1
		temp = UBOUND(Del_Y_res,1)
		write(77) 1,1,temp
		write(77) (Del_Y_res(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(V_head_reser)) then
		write(77) 1
		temp = UBOUND(V_head_reser,1)
		write(77) 1,1,temp
		write(77) (V_head_reser(M),M=1,temp)
	else
		write(77) 0
	endif
!
!	!for hydrographs and reserv. 	
	if (ALLOCATED(Abound)) then
		write(77) 1
		temp = UBOUND(Abound,1)
		write(77) 1,2,temp,UBOUND(Abound,2)
		write(77) ((Abound(N,M),N=1,temp),M=1,UBOUND(Abound,2))
	else
		write(77) 0
	endif
	if (ALLOCATED(Qbound)) then
		write(77) 1
		temp = UBOUND(Qbound,1)
		write(77) 1,2,temp,UBOUND(Qbound,2)
		write(77) ((Qbound(N,M),N=1,temp),M=1,UBOUND(Qbound,2))
	else
		write(77) 0
	endif
	if (ALLOCATED(ybound)) then
		write(77) 1
		temp = UBOUND(ybound,1)
		write(77) 1,2,temp,UBOUND(ybound,2)
		write(77) ((ybound(N,M),N=1,temp),M=1,UBOUND(ybound,2))
	else
		write(77) 0
	endif
	if (ALLOCATED(Idflow_bound)) then
		write(77) 1
		temp = UBOUND(Idflow_bound,1)
		write(77) 1,2,temp,UBOUND(Idflow_bound,2)
		write(77) ((Idflow_bound(N,M),N=1,temp),M=1,UBOUND(Idflow_bound,2))
	else
		write(77) 0
	endif
	if (ALLOCATED(Idflow_bound_reser)) then
		write(77) 1
		temp = UBOUND(Idflow_bound_reser,1)
		write(77) 1,2,temp,UBOUND(Idflow_bound_reser,2)
		write(77) ((Idflow_bound_reser(N,M),N=1,temp),M=1,UBOUND(Idflow_bound_reser,2))
	else
		write(77) 0
	endif
!	
!	!variables from the boundaries	
	if (ALLOCATED(BCnode)) then
		write(77) 1
		temp = UBOUND(BCnode,1)
		write(77) 1,1,temp
		write(77) (BCnode(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(sum_dry_bed_node)) then
		write(77) 1
		temp = UBOUND(sum_dry_bed_node,1)
		write(77) 1,1,temp
		write(77) (sum_dry_bed_node(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(open_closed_bound)) then
		write(77) 1
		temp = UBOUND(open_closed_bound,1)
		write(77) 1,1,temp
		write(77) (open_closed_bound(M),M=1,temp)
	else
		write(77) 0
	endif
!	
!c	Roughness
	if (ALLOCATED(nm)) then
		write(77) 1
		temp = UBOUND(nm,1)
		write(77) 1,1,temp
		write(77) (nm(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(fd)) then
		write(77) 1
		temp = UBOUND(fd,1)
		write(77) 1,1,temp
		write(77) (fd(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(Node1)) then
		write(77) 1
		temp = UBOUND(Node1,1)
		write(77) 1,1,temp
		write(77) (Node1(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(Node2)) then
		write(77) 1
		temp = UBOUND(Node2,1)
		write(77) 1,1,temp
		write(77) (Node2(M),M=1,temp)
	else
		write(77) 0
	endif
!
!
!c	Parameters Star region 
	write(77) 2,CODE_STAR_REGION
!      !CODE_STAR_REGION = 1. Variabels at star region are computed. Otherwise, no. 
!
!c	Parameters open channel/pressurized flow
	if (ALLOCATED(h0)) then
		write(77) 1
		temp = UBOUND(h0,1)
		write(77) 1,2,temp,UBOUND(h0,2)
		write(77) ((h0(N,M),N=1,temp),M=1,UBOUND(h0,2))
	else
		write(77) 0
	endif
	if (ALLOCATED(h0_Rec)) then
		write(77) 1
		temp = UBOUND(h0_Rec,1)
		write(77) 1,2,temp,UBOUND(h0_Rec,2)
		write(77) ((h0_Rec(N,M),N=1,temp),M=1,UBOUND(h0_Rec,2))
	else
		write(77) 0
	endif
	if (ALLOCATED(A0)) then
		write(77) 1
		temp = UBOUND(A0,1)
		write(77) 1,2,temp,UBOUND(A0,2)
		write(77) ((A0(N,M),N=1,temp),M=1,UBOUND(A0,2))
	else
		write(77) 0
	endif
	if (ALLOCATED(Q0)) then
		write(77) 1
		temp = UBOUND(Q0,1)
		write(77) 1,2,temp,UBOUND(Q0,2)
		write(77) ((Q0(N,M),N=1,temp),M=1,UBOUND(Q0,2))
	else
		write(77) 0
	endif
	if (ALLOCATED(AREA_FULL)) then
		write(77) 1
		temp = UBOUND(AREA_FULL,1)
		write(77) 1,1,temp
		write(77) (AREA_FULL(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(Area_for_pressur)) then
		write(77) 1
		temp = UBOUND(Area_for_pressur,1)
		write(77) 1,1,temp
		write(77) (Area_for_pressur(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(y_for_pressur)) then
		write(77) 1
		temp = UBOUND(y_for_pressur,1)
		write(77) 1,1,temp
		write(77) (y_for_pressur(M),M=1,temp)
	else
		write(77) 0
	endif
!
!
	if (ALLOCATED(Aref)) then
		write(77) 1
		temp = UBOUND(Aref,1)
		write(77) 1,1,temp
		write(77) (Aref(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(Phiref)) then
		write(77) 1
		temp = UBOUND(Phiref,1)
		write(77) 1,1,temp
		write(77) (Phiref(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(Yref)) then
		write(77) 1
		temp = UBOUND(Yref,1)
		write(77) 1,1,temp
		write(77) (Yref(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(haver_ref)) then
		write(77) 1
		temp = UBOUND(haver_ref,1)
		write(77) 1,1,temp
		write(77) (haver_ref(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(celer_ref)) then
		write(77) 1
		temp = UBOUND(celer_ref,1)
		write(77) 1,1,temp
		write(77) (celer_ref(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(P_pho_ref)) then
		write(77) 1
		temp = UBOUND(P_pho_ref,1)
		write(77) 1,1,temp
		write(77) (P_pho_ref(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(P_pho_dry)) then
		write(77) 1
		temp = UBOUND(P_pho_dry,1)
		write(77) 1,1,temp
		write(77) (P_pho_dry(M),M=1,temp)
	else
		write(77) 0
	endif
!	
!
	if (ALLOCATED(pc1)) then
		write(77) 1
		temp = UBOUND(pc1,1)
		write(77) 1,1,temp
		write(77) (pc1(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(fully_pressuri)) then
		write(77) 1
		temp = UBOUND(fully_pressuri,1)
		write(77) 1,1,temp
		write(77) (fully_pressuri(M),M=1,temp)
	else
		write(77) 0
	endif
!c	pref	=	Reference pressure
!c	RoRef	=	Reference density
!c	pc = pressurized flow wave speed
	write(77) 2,RoRef
	write(77) 2,PI
	write(77) 2,pc_air
	write(77) 2,pc_press
	write(77) 2,pc_mixed
!
!	
!	!values at adjacent cells and at boundaries at old time step	
!	!NodeNS(r) = Number of pipes connected to the node (1, 2, 3 ....)
!	!NodeID(r,j) = pipe ID(1, 2, 3, ...) 
!	!Nodetype = inflowing or outflowing
!	!maximum 10 pipes can be connected to each node
	if (ALLOCATED(NodeNS)) then
		write(77) 1
		temp = UBOUND(NodeNS,1)
		write(77) 1,1,temp
		write(77) (NodeNS(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(Nodetype)) then
		write(77) 1
		temp = UBOUND(Nodetype,1)
		write(77) 1,2,temp,UBOUND(Nodetype,2)
		write(77) ((Nodetype(N,M),N=1,temp),M=1,UBOUND(Nodetype,2))
	else
		write(77) 0
	endif
!	
!c	Parameters air pocket
!	!air pressure head expressed in equivalent water head
	if (ALLOCATED(ha)) then
		write(77) 1
		temp = UBOUND(ha,1)
		write(77) 1,2,temp,UBOUND(ha,2)
		write(77) ((ha(N,M),N=1,temp),M=1,UBOUND(ha,2))
	else
		write(77) 0
	endif
!	!X minimum and maximum cell IDS for air pockets
	if (ALLOCATED(XapMIN)) then
		write(77) 1
		temp = UBOUND(XapMIN,1)
		write(77) 1,2,temp,UBOUND(XapMIN,2)
		write(77) ((XapMIN(N,M),N=1,temp),M=1,UBOUND(XapMIN,2))
	else
		write(77) 0
	endif
	if (ALLOCATED(XapMAX)) then
		write(77) 1
		temp = UBOUND(XapMAX,1)
		write(77) 1,2,temp,UBOUND(XapMAX,2)
		write(77) ((XapMAX(N,M),N=1,temp),M=1,UBOUND(XapMAX,2))
	else
		write(77) 0
	endif
	if (ALLOCATED(XapMINold)) then
		write(77) 1
		temp = UBOUND(XapMINold,1)
		write(77) 1,2,temp,UBOUND(XapMINold,2)
		write(77) ((XapMINold(N,M),N=1,temp),M=1,UBOUND(XapMINold,2))
	else
		write(77) 0
	endif
	if (ALLOCATED(XapMAXold)) then
		write(77) 1
		temp = UBOUND(XapMAXold,1)
		write(77) 1,2,temp,UBOUND(XapMAXold,2)
		write(77) ((XapMAXold(N,M),N=1,temp),M=1,UBOUND(XapMAXold,2))
	else
		write(77) 0
	endif
!	!Number of air pocket
	if (ALLOCATED(Nap)) then
		write(77) 1
		temp = UBOUND(Nap,1)
		write(77) 1,1,temp
		write(77) (Nap(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(Napold)) then
		write(77) 1
		temp = UBOUND(Napold,1)
		write(77) 1,1,temp
		write(77) (Napold(M),M=1,temp)
	else
		write(77) 0
	endif
!	!volume of air pocket
	if (ALLOCATED(VOLap)) then
		write(77) 1
		temp = UBOUND(VOLap,1)
		write(77) 1,2,temp,UBOUND(VOLap,2)
		write(77) ((VOLap(N,M),N=1,temp),M=1,UBOUND(VOLap,2))
	else
		write(77) 0
	endif
	if (ALLOCATED(VOLap_old)) then
		write(77) 1
		temp = UBOUND(VOLap_old,1)
		write(77) 1,2,temp,UBOUND(VOLap_old,2)
		write(77) ((VOLap_old(N,M),N=1,temp),M=1,UBOUND(VOLap_old,2))
	else
		write(77) 0
	endif
!	!air pressure in water column 
	if (ALLOCATED(hap)) then
		write(77) 1
		temp = UBOUND(hap,1)
		write(77) 1,2,temp,UBOUND(hap,2)
		write(77) ((hap(N,M),N=1,temp),M=1,UBOUND(hap,2))
	else
		write(77) 0
	endif
	if (ALLOCATED(hapold)) then
		write(77) 1
		temp = UBOUND(hapold,1)
		write(77) 1,2,temp,UBOUND(hapold,2)
		write(77) ((hapold(N,M),N=1,temp),M=1,UBOUND(hapold,2))
	else
		write(77) 0
	endif
!	!politropic exponent for air 
!	!ka = 1.0, 1.4 or somewhere between
!	!Cd = air discharge coefficient 
	write(77) 2,ka
	write(77) 2,Cd
!
!	!air density
	if (ALLOCATED(dens_old)) then
		write(77) 1
		temp = UBOUND(dens_old,1)
		write(77) 1,2,temp,UBOUND(dens_old,2)
		write(77) ((dens_old(N,M),N=1,temp),M=1,UBOUND(dens_old,2))
	else
		write(77) 0
	endif
	if (ALLOCATED(dens)) then
		write(77) 1
		temp = UBOUND(dens,1)
		write(77) 1,2,temp,UBOUND(dens,2)
		write(77) ((dens(N,M),N=1,temp),M=1,UBOUND(dens,2))
	else
		write(77) 0
	endif
!
!c	Various	
	if (ALLOCATED(Atemp1)) then
		write(77) 1
		temp = UBOUND(Atemp1,1)
		write(77) 1,1,temp
		write(77) (Atemp1(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(htemp1)) then
		write(77) 1
		temp = UBOUND(htemp1,1)
		write(77) 1,1,temp
		write(77) (htemp1(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(Qtemp1)) then
		write(77) 1
		temp = UBOUND(Qtemp1,1)
		write(77) 1,1,temp
		write(77) (Qtemp1(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(IdFlow1)) then
		write(77) 1
		temp = UBOUND(IdFlow1,1)
		write(77) 1,1,temp
		write(77) (IdFlow1(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(IdFlow_REC_L)) then
		write(77) 1
		temp = UBOUND(IdFlow_REC_L,1)
		write(77) 1,1,temp
		write(77) (IdFlow_REC_L(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(IdFlow_REC_R)) then
		write(77) 1
		temp = UBOUND(IdFlow_REC_R,1)
		write(77) 1,1,temp
		write(77) (IdFlow_REC_R(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(A0L)) then
		write(77) 1
		temp = UBOUND(A0L,1)
		write(77) 1,1,temp
		write(77) (A0L(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(A0R)) then
		write(77) 1
		temp = UBOUND(A0R,1)
		write(77) 1,1,temp
		write(77) (A0R(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(Q0L)) then
		write(77) 1
		temp = UBOUND(Q0L,1)
		write(77) 1,1,temp
		write(77) (Q0L(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(Q0R)) then
		write(77) 1
		temp = UBOUND(Q0R,1)
		write(77) 1,1,temp
		write(77) (Q0R(M),M=1,temp)
	else
		write(77) 0
	endif
	write(77) 2,pressurized
!
!	!## Upstream IDs
!	
!
!	!## Junction
	if (ALLOCATED(Junct)) then
		write(77) 1
		temp = UBOUND(Junct,1)
		write(77) 1,2,temp,UBOUND(Junct,2)
		write(77) ((Junct(N,M),N=1,temp),M=1,UBOUND(Junct,2))
	else
		write(77) 0
	endif
	if (ALLOCATED(max_crown_pipe)) then
		write(77) 1
		temp = UBOUND(max_crown_pipe,1)
		write(77) 1,1,temp
		write(77) (max_crown_pipe(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(Drop)) then
		write(77) 1
		temp = UBOUND(Drop,1)
		write(77) 1,2,temp,UBOUND(Drop,2)
		write(77) ((Drop(N,M),N=1,temp),M=1,UBOUND(Drop,2))
	else
		write(77) 0
	endif
	if (ALLOCATED(Ares_junct)) then
		write(77) 1
		temp = UBOUND(Ares_junct,1)
		write(77) 1,1,temp
		write(77) (Ares_junct(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(yres_jun_old)) then
		write(77) 1
		temp = UBOUND(yres_jun_old,1)
		write(77) 1,1,temp
		write(77) (yres_jun_old(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(height_jun)) then
		write(77) 1
		temp = UBOUND(height_jun,1)
		write(77) 1,1,temp
		write(77) (height_jun(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(max_elev_crown)) then
		write(77) 1
		temp = UBOUND(max_elev_crown,1)
		write(77) 1,1,temp
		write(77) (max_elev_crown(M),M=1,temp)
	else
		write(77) 0
	endif
!	
!		
!	!## Reservoir 	
!	
	if (ALLOCATED(flowdepth_res)) then
		write(77) 1
		temp = UBOUND(flowdepth_res,1)
		write(77) 1,1,temp
		write(77) (flowdepth_res(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(yres_up)) then
		write(77) 1
		temp = UBOUND(yres_up,1)
		write(77) 1,1,temp
		write(77) (yres_up(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(Reser_outflow)) then
		write(77) 1
		temp = UBOUND(Reser_outflow,1)
		write(77) 1,1,temp
		write(77) (Reser_outflow(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(reser_maxdepth)) then
		write(77) 1
		temp = UBOUND(reser_maxdepth,1)
		write(77) 1,1,temp
		write(77) (reser_maxdepth(M),M=1,temp)
	else
		write(77) 0
	endif
!
!	!## Dropshaft
	if (ALLOCATED(Adrop)) then
		write(77) 1
		temp = UBOUND(Adrop,1)
		write(77) 1,1,temp
		write(77) (Adrop(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(hdrops_overf)) then
		write(77) 1
		temp = UBOUND(hdrops_overf,1)
		write(77) 1,1,temp
		write(77) (hdrops_overf(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(junct_elev)) then
		write(77) 1
		temp = UBOUND(junct_elev,1)
		write(77) 1,1,temp
		write(77) (junct_elev(M),M=1,temp)
	else
		write(77) 0
	endif
!
	if (ALLOCATED(V_over)) then
		write(77) 1
		temp = UBOUND(V_over,1)
		write(77) 1,1,temp
		write(77) (V_over(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(nodeID)) then
		write(77) 1
		temp = UBOUND(nodeID,1)
		write(77) 1,2,temp,UBOUND(nodeID,2)
		write(77) ((nodeID(N,M),N=1,temp),M=1,UBOUND(nodeID,2))
	else
		write(77) 0
	endif
!
!	!(len=1024)
!
!		
	if (ALLOCATED(yudrop_n)) then
		write(77) 1
		temp = UBOUND(yudrop_n,1)
		write(77) 1,1,temp
		write(77) (yudrop_n(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(yudrop_n_1)) then
		write(77) 1
		temp = UBOUND(yudrop_n_1,1)
		write(77) 1,1,temp
		write(77) (yudrop_n_1(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(ydropmin)) then
		write(77) 1
		temp = UBOUND(ydropmin,1)
		write(77) 1,1,temp
		write(77) (ydropmin(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(dropmin)) then
		write(77) 1
		temp = UBOUND(dropmin,1)
		write(77) 1,1,temp
		write(77) (dropmin(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(Areser_min)) then
		write(77) 1
		temp = UBOUND(Areser_min,1)
		write(77) 1,1,temp
		write(77) (Areser_min(M),M=1,temp)
	else
		write(77) 0
	endif
!			
!	!## Variables at interfaces (i+1/2)	
	if (ALLOCATED(Fupst)) then
		write(77) 1
		temp = UBOUND(Fupst,1)
		write(77) 1,2,temp,UBOUND(Fupst,2)
		write(77) ((Fupst(N,M),N=1,temp),M=1,UBOUND(Fupst,2))
	else
		write(77) 0
	endif
	if (ALLOCATED(Fdownst)) then
		write(77) 1
		temp = UBOUND(Fdownst,1)
		write(77) 1,2,temp,UBOUND(Fdownst,2)
		write(77) ((Fdownst(N,M),N=1,temp),M=1,UBOUND(Fdownst,2))
	else
		write(77) 0
	endif
	if (ALLOCATED(Pres_pho_Bound)) then
		write(77) 1
		temp = UBOUND(Pres_pho_Bound,1)
		write(77) 1,2,temp,UBOUND(Pres_pho_Bound,2)
		write(77) ((Pres_pho_Bound(N,M),N=1,temp),M=1,UBOUND(Pres_pho_Bound,2))
	else
		write(77) 0
	endif
	if (ALLOCATED(F)) then
		write(77) 1
		temp = UBOUND(F,1)
		write(77) 1,2,temp,UBOUND(F,2)
		write(77) ((F(N,M),N=1,temp),M=1,UBOUND(F,2))
	else
		write(77) 0
	endif
	if (ALLOCATED(P_pho_interf)) then
		write(77) 1
		temp = UBOUND(P_pho_interf,1)
		write(77) 1,1,temp
		write(77) (P_pho_interf(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(y_interf)) then
		write(77) 1
		temp = UBOUND(y_interf,1)
		write(77) 1,1,temp
		write(77) (y_interf(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(A_interf)) then
		write(77) 1
		temp = UBOUND(A_interf,1)
		write(77) 1,1,temp
		write(77) (A_interf(M),M=1,temp)
	else
		write(77) 0
	endif
!	
!	!constants	
!	!Hb = Atmospheric pressure in m of water
!	!RoRef_air =  air density
	write(77) 2,dt2
	write(77) 2,Hb
	write(77) 2,RoRef_air
	write(77) 2,number_steps
	write(77) 2,NR
	if (ALLOCATED(NX)) then
		write(77) 1
		temp = UBOUND(NX,1)
		write(77) 1,1,temp
		write(77) (NX(M),M=1,temp)
	else
		write(77) 0
	endif
	write(77) 2,Num_max_cells
!	!result of discretization
!
!	!Dry bed 
	if (ALLOCATED(ydry)) then
		write(77) 1
		temp = UBOUND(ydry,1)
		write(77) 1,1,temp
		write(77) (ydry(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(Adry)) then
		write(77) 1
		temp = UBOUND(Adry,1)
		write(77) 1,1,temp
		write(77) (Adry(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(phi_dry)) then
		write(77) 1
		temp = UBOUND(phi_dry,1)
		write(77) 1,1,temp
		write(77) (phi_dry(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(Celer_dry)) then
		write(77) 1
		temp = UBOUND(Celer_dry,1)
		write(77) 1,1,temp
		write(77) (Celer_dry(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(ydry_CFL)) then
		write(77) 1
		temp = UBOUND(ydry_CFL,1)
		write(77) 1,1,temp
		write(77) (ydry_CFL(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(Adry_CFL)) then
		write(77) 1
		temp = UBOUND(Adry_CFL,1)
		write(77) 1,1,temp
		write(77) (Adry_CFL(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(fluxdry)) then
		write(77) 1
		temp = UBOUND(fluxdry,1)
		write(77) 1,1,temp
		write(77) (fluxdry(M),M=1,temp)
	else
		write(77) 0
	endif
!	
!	!sloped pipe
	write(77) 2,Min_Slope_for_Sloped_pipe
!	
	if (ALLOCATED(A_cell_dry_sloped)) then
		write(77) 1
		temp = UBOUND(A_cell_dry_sloped,1)
		write(77) 1,1,temp
		write(77) (A_cell_dry_sloped(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(A_open_sloped_pipe)) then
		write(77) 1
		temp = UBOUND(A_open_sloped_pipe,1)
		write(77) 1,1,temp
		write(77) (A_open_sloped_pipe(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(b2_max)) then
		write(77) 1
		temp = UBOUND(b2_max,1)
		write(77) 1,1,temp
		write(77) (b2_max(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(A2_max)) then
		write(77) 1
		temp = UBOUND(A2_max,1)
		write(77) 1,1,temp
		write(77) (A2_max(M),M=1,temp)
	else
		write(77) 0
	endif
!      !b2_min and A2_min are for iteration purposes
	if (ALLOCATED(b2_min)) then
		write(77) 1
		temp = UBOUND(b2_min,1)
		write(77) 1,1,temp
		write(77) (b2_min(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(A2_min)) then
		write(77) 1
		temp = UBOUND(A2_min,1)
		write(77) 1,1,temp
		write(77) (A2_min(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(P_pho_min)) then
		write(77) 1
		temp = UBOUND(P_pho_min,1)
		write(77) 1,1,temp
		write(77) (P_pho_min(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(P_pho_max)) then
		write(77) 1
		temp = UBOUND(P_pho_max,1)
		write(77) 1,1,temp
		write(77) (P_pho_max(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(phi_max)) then
		write(77) 1
		temp = UBOUND(phi_max,1)
		write(77) 1,1,temp
		write(77) (phi_max(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(phi_min)) then
		write(77) 1
		temp = UBOUND(phi_min,1)
		write(77) 1,1,temp
		write(77) (phi_min(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(y_for_phi_max)) then
		write(77) 1
		temp = UBOUND(y_for_phi_max,1)
		write(77) 1,1,temp
		write(77) (y_for_phi_max(M),M=1,temp)
	else
		write(77) 0
	endif
!	
!	!Dtmax
	write(77) 2,DtMax
!	
!      !Internal Tolerances
	write(77) 2,Tol_int_10_2
	write(77) 2,Tol_int_10_3
	write(77) 2,Tol_int_10_4
	write(77) 2,Tol_int_10_5
	write(77) 2,Tol_int_10_6
	write(77) 2,Tol_int_10_7
	write(77) 2,Tol_int_10_8
	write(77) 2,Tol_int_10_9
	write(77) 2,Tol_int_10_10
	write(77) 2,Tol_int_10_12
	write(77) 2,Tol_int_10_14
!
!	!stored volume
	write(77) 2,Vol_entered_system
!	
!	!Lost volume
	write(77) 2,Vol_lost_system
!
!	!SWMM
	write(77) 2,Nnodes
!	
!	!various
	write(77) 2,SUM_VAR
	write(77) 2,sum_no_converg
	if (ALLOCATED(Klocal)) then
		write(77) 1
		temp = UBOUND(Klocal,1)
		write(77) 1,2,temp,UBOUND(Klocal,2)
		write(77) ((Klocal(N,M),N=1,temp),M=1,UBOUND(Klocal,2))
	else
		write(77) 0
	endif
!	
!	!Rating curve
	if (ALLOCATED(Max_flow_rating_curve)) then
		write(77) 1
		temp = UBOUND(Max_flow_rating_curve,1)
		write(77) 1,1,temp
		write(77) (Max_flow_rating_curve(M),M=1,temp)
	else
		write(77) 0
	endif
!	!Maximum flow specified at the rating curve. This is used to check if the water level in the rating curve is exceeded. 
	if (ALLOCATED(Max_Head_rating_curve)) then
		write(77) 1
		temp = UBOUND(Max_Head_rating_curve,1)
		write(77) 1,1,temp
		write(77) (Max_Head_rating_curve(M),M=1,temp)
	else
		write(77) 0
	endif
!	!Maximum flow specified at the rating curve. This is used to check if the water level in the rating curve is exceeded. 
!	
!	!Gates
	if (ALLOCATED(Cd_gate)) then
		write(77) 1
		temp = UBOUND(Cd_gate,1)
		write(77) 1,1,temp
		write(77) (Cd_gate(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(Hgate_open)) then
		write(77) 1
		temp = UBOUND(Hgate_open,1)
		write(77) 1,1,temp
		write(77) (Hgate_open(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(h_gate_m)) then
		write(77) 1
		temp = UBOUND(h_gate_m,1)
		write(77) 1,1,temp
		write(77) (h_gate_m(M),M=1,temp)
	else
		write(77) 0
	endif
!		
!	!Volume check
	write(77) 2,Volume_stored_current_step
	write(77) 2,Error_volume
	write(77) 2,balance_volume
	write(77) 3,1,10,(Q1(M),M=1,10)
	write(77) 3,1,10,(A1(M),M=1,10)
	write(77) 3,1,10,(u1(M),M=1,10)
	write(77) 3,1,10,(y1(M),M=1,10)
	write(77) 3,1,10,(c1(M),M=1,10)
	write(77) 3,1,10,(P_pho1(M),M=1,10)
	write(77) 3,1,10,(dropIA(M),M=1,10)
	write(77) 3,1,10,(Ab_oldIA(M),M=1,10)
	write(77) 3,1,10,(Qb_oldIA(M),M=1,10)
	write(77) 3,1,10,(yb_oldIA(M),M=1,10)
	write(77) 3,1,10,(phi11(M),M=1,10)
	write(77) 3,1,10,(phi1IA(M),M=1,10)
!
!	
	write(77) 3,1,10,(yw01(M),M=1,10)
	write(77) 3,1,10,(Aw01(M),M=1,10)
	write(77) 3,1,10,(Qw01(M),M=1,10)
	write(77) 3,1,10,(y_drybed(M),M=1,10)
	write(77) 3,1,10,(Q_drybed(M),M=1,10)
!	
	write(77) 3,1,10,(IDf1(M),M=1,10)
	write(77) 3,1,10,(IDfbIA(M),M=1,10)
	write(77) 3,1,10,(IDfbIA_old(M),M=1,10)
	write(77) 3,1,10,(drybed(M),M=1,10)
	write(77) 3,1,10,(flow_regIA(M),M=1,10)
	write(77) 3,1,10,(flowcaseIA(M),M=1,10)
	write(77) 3,1,10,(SumIDFIA(M),M=1,10)
	write(77) 2,init_volume_counter
	write(77) 3,1,10,(cond_mixed1(M),M=1,10)
	write(77) 3,1,10,(Idf01(M),M=1,10)
!
	if (ALLOCATED(EntranceLoss)) then
		write(77) 1
		temp = UBOUND(EntranceLoss,1)
		write(77) 1,1,temp
		write(77) (EntranceLoss(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(ExitLoss)) then
		write(77) 1
		temp = UBOUND(ExitLoss,1)
		write(77) 1,1,temp
		write(77) (ExitLoss(M),M=1,temp)
	else
		write(77) 0
	endif
!
	write(77) 2,temp9
!
	write(77) 2,Istor
	write(77) 2,T_GLOBAL
	write(77) 2,DT_GLOBAL
	write(77) 2,T_NEXT_REPORT
	write(77) 2,TIME_BEGIN
	write(77) 2,TIME_END
!
	write(77) 2,GLOBAL_STATUS_FLAG
	write(77) 2,MaxNumPlotCells
	write(77) 2,tol
	write(77) 2,tol_lower
	write(77) 2,tol_very_low
	write(77) 2,tol_higher
	write(77) 2,tol_crit
	write(77) 2,convergen
!
	write(77) 2,NxMax
	write(77) 2,numitera
	write(77) 2,min_num_grids
	write(77) 2,pc
	write(77) 2,pcm
	write(77) 2,yfree_press
	write(77) 2,Tmax
	write(77) 2,Dtmax1
	write(77) 2,Tstor
!
	write(77) 2,Initial_volume_stored
!
!
!
!	
!	!Parameters used for solving non-linear equations
	write(77) 2,paramOP1
	write(77) 2,paramOP2
	write(77) 2,paramOP3
	write(77) 2,paramOP4
	write(77) 2,paramOP5
	write(77) 2,paramOP6
	write(77) 2,paramOP7
	write(77) 2,paramOP8
	write(77) 2,paramOP9
	write(77) 2,param1
	write(77) 2,param2
	write(77) 2,param3
	write(77) 2,param4
	write(77) 2,param5
	write(77) 2,param6
	write(77) 2,param7
	write(77) 2,param8
	write(77) 2,param9
	write(77) 2,param10
	write(77) 2,param11
	write(77) 2,param12
	write(77) 2,param13
	write(77) 2,param14
	write(77) 2,param15
	write(77) 2,param16
	write(77) 2,param17
	write(77) 2,param18
	write(77) 2,param19
	write(77) 2,param20
	write(77) 2,param21
	write(77) 2,param22
	write(77) 2,param23
	write(77) 2,param24
	write(77) 2,param25
	write(77) 2,param26
	write(77) 2,param27
	write(77) 2,param28
	write(77) 2,param29
	write(77) 2,param30
	write(77) 2,param31
	write(77) 2,param32
	write(77) 2,param33
	write(77) 2,param34
	write(77) 2,param35
	write(77) 2,param36
	write(77) 2,param_ener
!		
	write(77) 2,parintOP1
	write(77) 2,parintOP2
	write(77) 2,parint1
	write(77) 2,parint2
	write(77) 2,parint3
	write(77) 2,parint4
	write(77) 2,parint5
	write(77) 2,parint6
	write(77) 2,parint7
	write(77) 2,parint8
	write(77) 2,parint9
	write(77) 2,parint10
	write(77) 2,parint11
	write(77) 2,parint12
	write(77) 2,parint13
	write(77) 2,parint14
	write(77) 2,parint15
	write(77) 2,parint16
	write(77) 2,parint17
	write(77) 2,parint18
	write(77) 2,parint1000
!
	write(77) 2,sumIA
	write(77) 2,sumIB
	write(77) 2,sumIC
	if (ALLOCATED(Qmin)) then
		write(77) 1
		temp = UBOUND(Qmin,1)
		write(77) 1,1,temp
		write(77) (Qmin(M),M=1,temp)
	else
		write(77) 0
	endif
	write(77) 2,code_vol_bal
	write(77) 2,Vol_bal
	write(77) 2,water_init_elevation
	write(77) 2,ini_cond
	write(77) 2,sum_temp
	write(77) 2,temp_outflow
!
!	!Pumping rates at selected nodes. 
	if (ALLOCATED(Qpump)) then
		write(77) 1
		temp = UBOUND(Qpump,1)
		write(77) 1,1,temp
		write(77) (Qpump(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(t_begin_pump)) then
		write(77) 1
		temp = UBOUND(t_begin_pump,1)
		write(77) 1,1,temp
		write(77) (t_begin_pump(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(IDpump)) then
		write(77) 1
		temp = UBOUND(IDpump,1)
		write(77) 1,1,temp
		write(77) (IDpump(M),M=1,temp)
	else
		write(77) 0
	endif
!
!	
!	!!!!!! Parameters thtat don't need to get saved to hotstart file go here.
	if (ALLOCATED(h0L)) then
		write(77) 1
		temp = UBOUND(h0L,1)
		write(77) 1,1,temp
		write(77) (h0L(M),M=1,temp)
	else
		write(77) 0
	endif
	if (ALLOCATED(h0R)) then
		write(77) 1
		temp = UBOUND(h0R,1)
		write(77) 1,1,temp
		write(77) (h0R(M),M=1,temp)
	else
		write(77) 0
	endif
!	
!
!	!Pressurization and Depressurization of the system
	write(77) 2,system_pressur
	write(77) 2,vol_factor
	write(77) 2,type_of_flow
!
!	!To relax time steps
	write(77) 2,Relax_condit
!
end function
