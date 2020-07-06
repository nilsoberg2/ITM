function READ_HOTSTART_VARS
	use common_module
	implicit none
	integer M,N,Vtype,Vdims,Dsize1,Dsize2,ver,En
	integer READ_HOTSTART_VARS
	
	! Should be 376
	read(77) ver
	
	! No error
	READ_HOTSTART_VARS = 0

	!if (ver .ne. CURRENT_REVISION) then
	!	! Error
	!	READ_HOTSTART_VARS = 1
	!	return
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
	!S0
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (S0(Dsize1))
				read(77) (S0(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!z0
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (z0(Dsize1,Dsize2))
				read(77) ((z0(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!zb
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (zb(Dsize1,Dsize2))
				read(77) ((zb(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!IdFlow
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (IdFlow(Dsize1,Dsize2))
				read(77) ((IdFlow(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!d
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (d(Dsize1))
				read(77) (d(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Length
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Length(Dsize1))
				read(77) (Length(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Dx
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Dx(Dsize1))
				read(77) (Dx(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!
!	!Init
	!inf
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (inf(Dsize1,Dsize2))
				read(77) ((inf(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!oufl
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (oufl(Dsize1,Dsize2))
				read(77) ((oufl(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Ninf
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Ninf(Dsize1))
				read(77) (Ninf(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Noufl
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Noufl(Dsize1))
				read(77) (Noufl(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Qcrit_maxIA
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Qcrit_maxIA(Dsize1))
				read(77) (Qcrit_maxIA(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Qnor_maxIA
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Qnor_maxIA(Dsize1))
				read(77) (Qnor_maxIA(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!ycrit_min
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (ycrit_min(Dsize1))
				read(77) (ycrit_min(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!d1_min
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (d1_min(Dsize1))
				read(77) (d1_min(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!d2_min
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (d2_min(Dsize1))
				read(77) (d2_min(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!
!c     Boundaries		
	!Nnod
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Nnod(Dsize1))
				read(77) (Nnod(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!line_elem
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (line_elem(Dsize1,Dsize2))
				read(77) ((line_elem(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!const_depth_flow
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (const_depth_flow(Dsize1))
				read(77) (const_depth_flow(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!min_water_pipeID
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (min_water_pipeID(Dsize1))
				read(77) (min_water_pipeID(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Del_Y_res
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Del_Y_res(Dsize1))
				read(77) (Del_Y_res(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!V_head_reser
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (V_head_reser(Dsize1))
				read(77) (V_head_reser(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!
!	!for hydrographs and reserv. 	
	!Abound
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (Abound(Dsize1,Dsize2))
				read(77) ((Abound(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Qbound
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (Qbound(Dsize1,Dsize2))
				read(77) ((Qbound(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!ybound
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (ybound(Dsize1,Dsize2))
				read(77) ((ybound(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Idflow_bound
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (Idflow_bound(Dsize1,Dsize2))
				read(77) ((Idflow_bound(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Idflow_bound_reser
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (Idflow_bound_reser(Dsize1,Dsize2))
				read(77) ((Idflow_bound_reser(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!	
!	!variables from the boundaries	
	!BCnode
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (BCnode(Dsize1))
				read(77) (BCnode(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!sum_dry_bed_node
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (sum_dry_bed_node(Dsize1))
				read(77) (sum_dry_bed_node(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!open_closed_bound
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (open_closed_bound(Dsize1))
				read(77) (open_closed_bound(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!	
!c	Roughness
	!nm
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (nm(Dsize1))
				read(77) (nm(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!fd
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (fd(Dsize1))
				read(77) (fd(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Node1
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Node1(Dsize1))
				read(77) (Node1(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Node2
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Node2(Dsize1))
				read(77) (Node2(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!
!
!c	Parameters Star region 
	!CODE_STAR_REGION
	read(77) Vtype
	if (Vtype == 2) then
		read(77) CODE_STAR_REGION
	else
		READ_HOTSTART_VARS = 1
		return
	endif
!      !CODE_STAR_REGION = 1. Variabels at star region are computed. Otherwise, no. 
!
!c	Parameters open channel/pressurized flow
	!h0
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (h0(Dsize1,Dsize2))
				read(77) ((h0(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!h0_Rec
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (h0_Rec(Dsize1,Dsize2))
				read(77) ((h0_Rec(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!A0
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (A0(Dsize1,Dsize2))
				read(77) ((A0(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Q0
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (Q0(Dsize1,Dsize2))
				read(77) ((Q0(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!AREA_FULL
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (AREA_FULL(Dsize1))
				read(77) (AREA_FULL(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Area_for_pressur
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Area_for_pressur(Dsize1))
				read(77) (Area_for_pressur(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!y_for_pressur
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (y_for_pressur(Dsize1))
				read(77) (y_for_pressur(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!
!
	!Aref
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Aref(Dsize1))
				read(77) (Aref(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Phiref
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Phiref(Dsize1))
				read(77) (Phiref(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Yref
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Yref(Dsize1))
				read(77) (Yref(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!haver_ref
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (haver_ref(Dsize1))
				read(77) (haver_ref(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!celer_ref
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (celer_ref(Dsize1))
				read(77) (celer_ref(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!P_pho_ref
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (P_pho_ref(Dsize1))
				read(77) (P_pho_ref(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!P_pho_dry
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (P_pho_dry(Dsize1))
				read(77) (P_pho_dry(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!	
!
	!pc1
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (pc1(Dsize1))
				read(77) (pc1(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!fully_pressuri
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (fully_pressuri(Dsize1))
				read(77) (fully_pressuri(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!c	pref	=	Reference pressure
!c	RoRef	=	Reference density
!c	pc = pressurized flow wave speed
	!RoRef
	read(77) Vtype
	if (Vtype == 2) then
		read(77) RoRef
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!PI
	read(77) Vtype
	if (Vtype == 2) then
		read(77) PI
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!pc_air
	read(77) Vtype
	if (Vtype == 2) then
		read(77) pc_air
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!pc_press
	read(77) Vtype
	if (Vtype == 2) then
		read(77) pc_press
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!pc_mixed
	read(77) Vtype
	if (Vtype == 2) then
		read(77) pc_mixed
	else
		READ_HOTSTART_VARS = 1
		return
	endif
!
!	
!	!values at adjacent cells and at boundaries at old time step	
!	!NodeNS(r) = Number of pipes connected to the node (1, 2, 3 ....)
!	!NodeID(r,j) = pipe ID(1, 2, 3, ...) 
!	!Nodetype = inflowing or outflowing
!	!maximum 10 pipes can be connected to each node
	!NodeNS
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (NodeNS(Dsize1))
				read(77) (NodeNS(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Nodetype
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (Nodetype(Dsize1,Dsize2))
				read(77) ((Nodetype(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!	
!c	Parameters air pocket
!	!air pressure head expressed in equivalent water head
	!ha
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (ha(Dsize1,Dsize2))
				read(77) ((ha(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!	!X minimum and maximum cell IDS for air pockets
	!XapMIN
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (XapMIN(Dsize1,Dsize2))
				read(77) ((XapMIN(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!XapMAX
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (XapMAX(Dsize1,Dsize2))
				read(77) ((XapMAX(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!XapMINold
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (XapMINold(Dsize1,Dsize2))
				read(77) ((XapMINold(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!XapMAXold
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (XapMAXold(Dsize1,Dsize2))
				read(77) ((XapMAXold(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!	!Number of air pocket
	!Nap
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Nap(Dsize1))
				read(77) (Nap(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Napold
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Napold(Dsize1))
				read(77) (Napold(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!	!volume of air pocket
	!VOLap
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (VOLap(Dsize1,Dsize2))
				read(77) ((VOLap(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!VOLap_old
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (VOLap_old(Dsize1,Dsize2))
				read(77) ((VOLap_old(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!	!air pressure in water column 
	!hap
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (hap(Dsize1,Dsize2))
				read(77) ((hap(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!hapold
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (hapold(Dsize1,Dsize2))
				read(77) ((hapold(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!	!politropic exponent for air 
!	!ka = 1.0, 1.4 or somewhere between
!	!Cd = air discharge coefficient 
	!ka
	read(77) Vtype
	if (Vtype == 2) then
		read(77) ka
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!Cd
	read(77) Vtype
	if (Vtype == 2) then
		read(77) Cd
	else
		READ_HOTSTART_VARS = 1
		return
	endif
!
!	!air density
	!dens_old
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (dens_old(Dsize1,Dsize2))
				read(77) ((dens_old(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!dens
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (dens(Dsize1,Dsize2))
				read(77) ((dens(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!
!c	Various	
	!Atemp1
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Atemp1(Dsize1))
				read(77) (Atemp1(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!htemp1
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (htemp1(Dsize1))
				read(77) (htemp1(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Qtemp1
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Qtemp1(Dsize1))
				read(77) (Qtemp1(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!IdFlow1
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (IdFlow1(Dsize1))
				read(77) (IdFlow1(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!IdFlow_REC_L
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (IdFlow_REC_L(Dsize1))
				read(77) (IdFlow_REC_L(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!IdFlow_REC_R
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (IdFlow_REC_R(Dsize1))
				read(77) (IdFlow_REC_R(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!A0L
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (A0L(Dsize1))
				read(77) (A0L(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!A0R
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (A0R(Dsize1))
				read(77) (A0R(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Q0L
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Q0L(Dsize1))
				read(77) (Q0L(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Q0R
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Q0R(Dsize1))
				read(77) (Q0R(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!pressurized
	read(77) Vtype
	if (Vtype == 2) then
		read(77) pressurized
	else
		READ_HOTSTART_VARS = 1
		return
	endif
!
!	!## Upstream IDs
!	
!
!	!## Junction
	!Junct
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (Junct(Dsize1,Dsize2))
				read(77) ((Junct(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!max_crown_pipe
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (max_crown_pipe(Dsize1))
				read(77) (max_crown_pipe(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Drop
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (Drop(Dsize1,Dsize2))
				read(77) ((Drop(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Ares_junct
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Ares_junct(Dsize1))
				read(77) (Ares_junct(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!yres_jun_old
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (yres_jun_old(Dsize1))
				read(77) (yres_jun_old(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!height_jun
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (height_jun(Dsize1))
				read(77) (height_jun(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!max_elev_crown
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (max_elev_crown(Dsize1))
				read(77) (max_elev_crown(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!	
!		
!	!## Reservoir 	
!	
	!flowdepth_res
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (flowdepth_res(Dsize1))
				read(77) (flowdepth_res(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!yres_up
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (yres_up(Dsize1))
				read(77) (yres_up(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Reser_outflow
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Reser_outflow(Dsize1))
				read(77) (Reser_outflow(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!reser_maxdepth
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (reser_maxdepth(Dsize1))
				read(77) (reser_maxdepth(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!
!	!## Dropshaft
	!Adrop
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Adrop(Dsize1))
				read(77) (Adrop(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!hdrops_overf
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (hdrops_overf(Dsize1))
				read(77) (hdrops_overf(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!junct_elev
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (junct_elev(Dsize1))
				read(77) (junct_elev(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!
	!V_over
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (V_over(Dsize1))
				read(77) (V_over(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!nodeID
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (nodeID(Dsize1,Dsize2))
				read(77) ((nodeID(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!
!	!(len=1024)
!
!		
	!yudrop_n
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (yudrop_n(Dsize1))
				read(77) (yudrop_n(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!yudrop_n_1
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (yudrop_n_1(Dsize1))
				read(77) (yudrop_n_1(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!ydropmin
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (ydropmin(Dsize1))
				read(77) (ydropmin(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!dropmin
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (dropmin(Dsize1))
				read(77) (dropmin(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Areser_min
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Areser_min(Dsize1))
				read(77) (Areser_min(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!			
!	!## Variables at interfaces (i+1/2)	
	!Fupst
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (Fupst(Dsize1,Dsize2))
				read(77) ((Fupst(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Fdownst
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (Fdownst(Dsize1,Dsize2))
				read(77) ((Fdownst(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Pres_pho_Bound
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (Pres_pho_Bound(Dsize1,Dsize2))
				read(77) ((Pres_pho_Bound(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!F
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (F(Dsize1,Dsize2))
				read(77) ((F(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!P_pho_interf
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (P_pho_interf(Dsize1))
				read(77) (P_pho_interf(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!y_interf
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (y_interf(Dsize1))
				read(77) (y_interf(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!A_interf
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (A_interf(Dsize1))
				read(77) (A_interf(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!	
!	!constants	
!	!Hb = Atmospheric pressure in m of water
!	!RoRef_air =  air density
	!dt2
	read(77) Vtype
	if (Vtype == 2) then
		read(77) dt2
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!Hb
	read(77) Vtype
	if (Vtype == 2) then
		read(77) Hb
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!RoRef_air
	read(77) Vtype
	if (Vtype == 2) then
		read(77) RoRef_air
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!number_steps
	read(77) Vtype
	if (Vtype == 2) then
		read(77) number_steps
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!NR
	read(77) Vtype
	if (Vtype == 2) then
		read(77) NR
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!NX
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (NX(Dsize1))
				read(77) (NX(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Num_max_cells
	read(77) Vtype
	if (Vtype == 2) then
		read(77) Num_max_cells
	else
		READ_HOTSTART_VARS = 1
		return
	endif
!	!result of discretization
!
!	!Dry bed 
	!ydry
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (ydry(Dsize1))
				read(77) (ydry(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Adry
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Adry(Dsize1))
				read(77) (Adry(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!phi_dry
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (phi_dry(Dsize1))
				read(77) (phi_dry(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Celer_dry
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Celer_dry(Dsize1))
				read(77) (Celer_dry(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!ydry_CFL
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (ydry_CFL(Dsize1))
				read(77) (ydry_CFL(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Adry_CFL
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Adry_CFL(Dsize1))
				read(77) (Adry_CFL(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!fluxdry
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (fluxdry(Dsize1))
				read(77) (fluxdry(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!	
!	!sloped pipe
	!Min_Slope_for_Sloped_pipe
	read(77) Vtype
	if (Vtype == 2) then
		read(77) Min_Slope_for_Sloped_pipe
	else
		READ_HOTSTART_VARS = 1
		return
	endif
!	
	!A_cell_dry_sloped
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (A_cell_dry_sloped(Dsize1))
				read(77) (A_cell_dry_sloped(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!A_open_sloped_pipe
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (A_open_sloped_pipe(Dsize1))
				read(77) (A_open_sloped_pipe(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!b2_max
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (b2_max(Dsize1))
				read(77) (b2_max(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!A2_max
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (A2_max(Dsize1))
				read(77) (A2_max(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!      !b2_min and A2_min are for iteration purposes
	!b2_min
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (b2_min(Dsize1))
				read(77) (b2_min(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!A2_min
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (A2_min(Dsize1))
				read(77) (A2_min(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!P_pho_min
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (P_pho_min(Dsize1))
				read(77) (P_pho_min(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!P_pho_max
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (P_pho_max(Dsize1))
				read(77) (P_pho_max(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!phi_max
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (phi_max(Dsize1))
				read(77) (phi_max(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!phi_min
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (phi_min(Dsize1))
				read(77) (phi_min(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!y_for_phi_max
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (y_for_phi_max(Dsize1))
				read(77) (y_for_phi_max(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!	
!	!Dtmax
	!DtMax
	read(77) Vtype
	if (Vtype == 2) then
		read(77) DtMax
	else
		READ_HOTSTART_VARS = 1
		return
	endif
!	
!      !Internal Tolerances
	!Tol_int_10_2
	read(77) Vtype
	if (Vtype == 2) then
		read(77) Tol_int_10_2
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!Tol_int_10_3
	read(77) Vtype
	if (Vtype == 2) then
		read(77) Tol_int_10_3
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!Tol_int_10_4
	read(77) Vtype
	if (Vtype == 2) then
		read(77) Tol_int_10_4
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!Tol_int_10_5
	read(77) Vtype
	if (Vtype == 2) then
		read(77) Tol_int_10_5
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!Tol_int_10_6
	read(77) Vtype
	if (Vtype == 2) then
		read(77) Tol_int_10_6
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!Tol_int_10_7
	read(77) Vtype
	if (Vtype == 2) then
		read(77) Tol_int_10_7
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!Tol_int_10_8
	read(77) Vtype
	if (Vtype == 2) then
		read(77) Tol_int_10_8
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!Tol_int_10_9
	read(77) Vtype
	if (Vtype == 2) then
		read(77) Tol_int_10_9
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!Tol_int_10_10
	read(77) Vtype
	if (Vtype == 2) then
		read(77) Tol_int_10_10
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!Tol_int_10_12
	read(77) Vtype
	if (Vtype == 2) then
		read(77) Tol_int_10_12
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!Tol_int_10_14
	read(77) Vtype
	if (Vtype == 2) then
		read(77) Tol_int_10_14
	else
		READ_HOTSTART_VARS = 1
		return
	endif
!
!	!stored volume
	!Vol_entered_system
	read(77) Vtype
	if (Vtype == 2) then
		read(77) Vol_entered_system
	else
		READ_HOTSTART_VARS = 1
		return
	endif
!	
!	!Lost volume
	!Vol_lost_system
	read(77) Vtype
	if (Vtype == 2) then
		read(77) Vol_lost_system
	else
		READ_HOTSTART_VARS = 1
		return
	endif
!
!	!SWMM
	!Nnodes
	read(77) Vtype
	if (Vtype == 2) then
		read(77) Nnodes
	else
		READ_HOTSTART_VARS = 1
		return
	endif
!	
!	!various
	!SUM_VAR
	read(77) Vtype
	if (Vtype == 2) then
		read(77) SUM_VAR
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!sum_no_converg
	read(77) Vtype
	if (Vtype == 2) then
		read(77) sum_no_converg
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!Klocal
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 2) then
				read(77) Dsize2
				allocate (Klocal(Dsize1,Dsize2))
				read(77) ((Klocal(N,M),N=1,Dsize1),M=1,Dsize2)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!	
!	!Rating curve
	!Max_flow_rating_curve
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Max_flow_rating_curve(Dsize1))
				read(77) (Max_flow_rating_curve(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!	!Maximum flow specified at the rating curve. This is used to check if the water level in the rating curve is exceeded. 
	!Max_Head_rating_curve
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Max_Head_rating_curve(Dsize1))
				read(77) (Max_Head_rating_curve(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!	!Maximum flow specified at the rating curve. This is used to check if the water level in the rating curve is exceeded. 
!	
!	!Gates
	!Cd_gate
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Cd_gate(Dsize1))
				read(77) (Cd_gate(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!Hgate_open
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Hgate_open(Dsize1))
				read(77) (Hgate_open(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!h_gate_m
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (h_gate_m(Dsize1))
				read(77) (h_gate_m(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!		
!	!Volume check
	!Volume_stored_current_step
	read(77) Vtype
	if (Vtype == 2) then
		read(77) Volume_stored_current_step
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!Error_volume
	read(77) Vtype
	if (Vtype == 2) then
		read(77) Error_volume
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!balance_volume
	read(77) Vtype
	if (Vtype == 2) then
		read(77) balance_volume
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!Q1
	read(77) Vtype
	if (Vtype == 3) then
		read(77) Vdims,Dsize1
		if (Vdims == 1) then
			!static array, no need to allocate
			read(77) (Q1(M),M=1,Dsize1)
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!A1
	read(77) Vtype
	if (Vtype == 3) then
		read(77) Vdims,Dsize1
		if (Vdims == 1) then
			!static array, no need to allocate
			read(77) (A1(M),M=1,Dsize1)
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!u1
	read(77) Vtype
	if (Vtype == 3) then
		read(77) Vdims,Dsize1
		if (Vdims == 1) then
			!static array, no need to allocate
			read(77) (u1(M),M=1,Dsize1)
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!y1
	read(77) Vtype
	if (Vtype == 3) then
		read(77) Vdims,Dsize1
		if (Vdims == 1) then
			!static array, no need to allocate
			read(77) (y1(M),M=1,Dsize1)
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!c1
	read(77) Vtype
	if (Vtype == 3) then
		read(77) Vdims,Dsize1
		if (Vdims == 1) then
			!static array, no need to allocate
			read(77) (c1(M),M=1,Dsize1)
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!P_pho1
	read(77) Vtype
	if (Vtype == 3) then
		read(77) Vdims,Dsize1
		if (Vdims == 1) then
			!static array, no need to allocate
			read(77) (P_pho1(M),M=1,Dsize1)
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!dropIA
	read(77) Vtype
	if (Vtype == 3) then
		read(77) Vdims,Dsize1
		if (Vdims == 1) then
			!static array, no need to allocate
			read(77) (dropIA(M),M=1,Dsize1)
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!Ab_oldIA
	read(77) Vtype
	if (Vtype == 3) then
		read(77) Vdims,Dsize1
		if (Vdims == 1) then
			!static array, no need to allocate
			read(77) (Ab_oldIA(M),M=1,Dsize1)
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!Qb_oldIA
	read(77) Vtype
	if (Vtype == 3) then
		read(77) Vdims,Dsize1
		if (Vdims == 1) then
			!static array, no need to allocate
			read(77) (Qb_oldIA(M),M=1,Dsize1)
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!yb_oldIA
	read(77) Vtype
	if (Vtype == 3) then
		read(77) Vdims,Dsize1
		if (Vdims == 1) then
			!static array, no need to allocate
			read(77) (yb_oldIA(M),M=1,Dsize1)
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!phi11
	read(77) Vtype
	if (Vtype == 3) then
		read(77) Vdims,Dsize1
		if (Vdims == 1) then
			!static array, no need to allocate
			read(77) (phi11(M),M=1,Dsize1)
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!phi1IA
	read(77) Vtype
	if (Vtype == 3) then
		read(77) Vdims,Dsize1
		if (Vdims == 1) then
			!static array, no need to allocate
			read(77) (phi1IA(M),M=1,Dsize1)
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	else
		READ_HOTSTART_VARS = 1
		return
	endif
!
!	
	!yw01
	read(77) Vtype
	if (Vtype == 3) then
		read(77) Vdims,Dsize1
		if (Vdims == 1) then
			!static array, no need to allocate
			read(77) (yw01(M),M=1,Dsize1)
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!Aw01
	read(77) Vtype
	if (Vtype == 3) then
		read(77) Vdims,Dsize1
		if (Vdims == 1) then
			!static array, no need to allocate
			read(77) (Aw01(M),M=1,Dsize1)
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!Qw01
	read(77) Vtype
	if (Vtype == 3) then
		read(77) Vdims,Dsize1
		if (Vdims == 1) then
			!static array, no need to allocate
			read(77) (Qw01(M),M=1,Dsize1)
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!y_drybed
	read(77) Vtype
	if (Vtype == 3) then
		read(77) Vdims,Dsize1
		if (Vdims == 1) then
			!static array, no need to allocate
			read(77) (y_drybed(M),M=1,Dsize1)
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!Q_drybed
	read(77) Vtype
	if (Vtype == 3) then
		read(77) Vdims,Dsize1
		if (Vdims == 1) then
			!static array, no need to allocate
			read(77) (Q_drybed(M),M=1,Dsize1)
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	else
		READ_HOTSTART_VARS = 1
		return
	endif
!	
	!IDf1
	read(77) Vtype
	if (Vtype == 3) then
		read(77) Vdims,Dsize1
		if (Vdims == 1) then
			!static array, no need to allocate
			read(77) (IDf1(M),M=1,Dsize1)
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!IDfbIA
	read(77) Vtype
	if (Vtype == 3) then
		read(77) Vdims,Dsize1
		if (Vdims == 1) then
			!static array, no need to allocate
			read(77) (IDfbIA(M),M=1,Dsize1)
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!IDfbIA_old
	read(77) Vtype
	if (Vtype == 3) then
		read(77) Vdims,Dsize1
		if (Vdims == 1) then
			!static array, no need to allocate
			read(77) (IDfbIA_old(M),M=1,Dsize1)
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!drybed
	read(77) Vtype
	if (Vtype == 3) then
		read(77) Vdims,Dsize1
		if (Vdims == 1) then
			!static array, no need to allocate
			read(77) (drybed(M),M=1,Dsize1)
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!flow_regIA
	read(77) Vtype
	if (Vtype == 3) then
		read(77) Vdims,Dsize1
		if (Vdims == 1) then
			!static array, no need to allocate
			read(77) (flow_regIA(M),M=1,Dsize1)
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!flowcaseIA
	read(77) Vtype
	if (Vtype == 3) then
		read(77) Vdims,Dsize1
		if (Vdims == 1) then
			!static array, no need to allocate
			read(77) (flowcaseIA(M),M=1,Dsize1)
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!SumIDFIA
	read(77) Vtype
	if (Vtype == 3) then
		read(77) Vdims,Dsize1
		if (Vdims == 1) then
			!static array, no need to allocate
			read(77) (SumIDFIA(M),M=1,Dsize1)
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!init_volume_counter
	read(77) Vtype
	if (Vtype == 2) then
		read(77) init_volume_counter
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!cond_mixed1
	read(77) Vtype
	if (Vtype == 3) then
		read(77) Vdims,Dsize1
		if (Vdims == 1) then
			!static array, no need to allocate
			read(77) (cond_mixed1(M),M=1,Dsize1)
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!Idf01
	read(77) Vtype
	if (Vtype == 3) then
		read(77) Vdims,Dsize1
		if (Vdims == 1) then
			!static array, no need to allocate
			read(77) (Idf01(M),M=1,Dsize1)
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	else
		READ_HOTSTART_VARS = 1
		return
	endif
!
	!EntranceLoss
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (EntranceLoss(Dsize1))
				read(77) (EntranceLoss(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!ExitLoss
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (ExitLoss(Dsize1))
				read(77) (ExitLoss(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!
	!temp9
	read(77) Vtype
	if (Vtype == 2) then
		read(77) temp9
	else
		READ_HOTSTART_VARS = 1
		return
	endif
!
	!Istor
	read(77) Vtype
	if (Vtype == 2) then
		read(77) Istor
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!T_GLOBAL
	read(77) Vtype
	if (Vtype == 2) then
		read(77) T_GLOBAL
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!DT_GLOBAL
	read(77) Vtype
	if (Vtype == 2) then
		read(77) DT_GLOBAL
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!T_NEXT_REPORT
	read(77) Vtype
	if (Vtype == 2) then
		read(77) T_NEXT_REPORT
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!TIME_BEGIN
	read(77) Vtype
	if (Vtype == 2) then
		read(77) TIME_BEGIN
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!TIME_END
	read(77) Vtype
	if (Vtype == 2) then
		read(77) TIME_END
	else
		READ_HOTSTART_VARS = 1
		return
	endif
!
	!GLOBAL_STATUS_FLAG
	read(77) Vtype
	if (Vtype == 2) then
		read(77) GLOBAL_STATUS_FLAG
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!MaxNumPlotCells
	read(77) Vtype
	if (Vtype == 2) then
		read(77) MaxNumPlotCells
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!tol
	read(77) Vtype
	if (Vtype == 2) then
		read(77) tol
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!tol_lower
	read(77) Vtype
	if (Vtype == 2) then
		read(77) tol_lower
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!tol_very_low
	read(77) Vtype
	if (Vtype == 2) then
		read(77) tol_very_low
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!tol_higher
	read(77) Vtype
	if (Vtype == 2) then
		read(77) tol_higher
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!tol_crit
	read(77) Vtype
	if (Vtype == 2) then
		read(77) tol_crit
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!convergen
	read(77) Vtype
	if (Vtype == 2) then
		read(77) convergen
	else
		READ_HOTSTART_VARS = 1
		return
	endif
!
	!NxMax
	read(77) Vtype
	if (Vtype == 2) then
		read(77) NxMax
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!numitera
	read(77) Vtype
	if (Vtype == 2) then
		read(77) numitera
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!min_num_grids
	read(77) Vtype
	if (Vtype == 2) then
		read(77) min_num_grids
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!pc
	read(77) Vtype
	if (Vtype == 2) then
		read(77) pc
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!pcm
	read(77) Vtype
	if (Vtype == 2) then
		read(77) pcm
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!yfree_press
	read(77) Vtype
	if (Vtype == 2) then
		read(77) yfree_press
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!Tmax
	read(77) Vtype
	if (Vtype == 2) then
		read(77) Tmax
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!Dtmax1
	read(77) Vtype
	if (Vtype == 2) then
		read(77) Dtmax1
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!Tstor
	read(77) Vtype
	if (Vtype == 2) then
		read(77) Tstor
	else
		READ_HOTSTART_VARS = 1
		return
	endif
!
	!Initial_volume_stored
	read(77) Vtype
	if (Vtype == 2) then
		read(77) Initial_volume_stored
	else
		READ_HOTSTART_VARS = 1
		return
	endif
!
!
!
!	
!	!Parameters used for solving non-linear equations
	!paramOP1
	read(77) Vtype
	if (Vtype == 2) then
		read(77) paramOP1
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!paramOP2
	read(77) Vtype
	if (Vtype == 2) then
		read(77) paramOP2
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!paramOP3
	read(77) Vtype
	if (Vtype == 2) then
		read(77) paramOP3
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!paramOP4
	read(77) Vtype
	if (Vtype == 2) then
		read(77) paramOP4
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!paramOP5
	read(77) Vtype
	if (Vtype == 2) then
		read(77) paramOP5
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!paramOP6
	read(77) Vtype
	if (Vtype == 2) then
		read(77) paramOP6
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!paramOP7
	read(77) Vtype
	if (Vtype == 2) then
		read(77) paramOP7
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!paramOP8
	read(77) Vtype
	if (Vtype == 2) then
		read(77) paramOP8
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!paramOP9
	read(77) Vtype
	if (Vtype == 2) then
		read(77) paramOP9
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param1
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param1
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param2
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param2
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param3
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param3
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param4
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param4
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param5
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param5
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param6
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param6
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param7
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param7
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param8
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param8
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param9
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param9
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param10
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param10
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param11
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param11
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param12
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param12
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param13
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param13
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param14
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param14
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param15
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param15
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param16
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param16
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param17
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param17
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param18
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param18
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param19
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param19
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param20
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param20
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param21
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param21
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param22
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param22
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param23
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param23
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param24
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param24
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param25
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param25
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param26
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param26
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param27
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param27
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param28
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param28
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param29
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param29
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param30
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param30
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param31
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param31
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param32
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param32
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param33
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param33
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param34
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param34
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param35
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param35
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param36
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param36
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!param_ener
	read(77) Vtype
	if (Vtype == 2) then
		read(77) param_ener
	else
		READ_HOTSTART_VARS = 1
		return
	endif
!		
	!parintOP1
	read(77) Vtype
	if (Vtype == 2) then
		read(77) parintOP1
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!parintOP2
	read(77) Vtype
	if (Vtype == 2) then
		read(77) parintOP2
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!parint1
	read(77) Vtype
	if (Vtype == 2) then
		read(77) parint1
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!parint2
	read(77) Vtype
	if (Vtype == 2) then
		read(77) parint2
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!parint3
	read(77) Vtype
	if (Vtype == 2) then
		read(77) parint3
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!parint4
	read(77) Vtype
	if (Vtype == 2) then
		read(77) parint4
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!parint5
	read(77) Vtype
	if (Vtype == 2) then
		read(77) parint5
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!parint6
	read(77) Vtype
	if (Vtype == 2) then
		read(77) parint6
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!parint7
	read(77) Vtype
	if (Vtype == 2) then
		read(77) parint7
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!parint8
	read(77) Vtype
	if (Vtype == 2) then
		read(77) parint8
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!parint9
	read(77) Vtype
	if (Vtype == 2) then
		read(77) parint9
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!parint10
	read(77) Vtype
	if (Vtype == 2) then
		read(77) parint10
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!parint11
	read(77) Vtype
	if (Vtype == 2) then
		read(77) parint11
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!parint12
	read(77) Vtype
	if (Vtype == 2) then
		read(77) parint12
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!parint13
	read(77) Vtype
	if (Vtype == 2) then
		read(77) parint13
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!parint14
	read(77) Vtype
	if (Vtype == 2) then
		read(77) parint14
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!parint15
	read(77) Vtype
	if (Vtype == 2) then
		read(77) parint15
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!parint16
	read(77) Vtype
	if (Vtype == 2) then
		read(77) parint16
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!parint17
	read(77) Vtype
	if (Vtype == 2) then
		read(77) parint17
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!parint18
	read(77) Vtype
	if (Vtype == 2) then
		read(77) parint18
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!parint1000
	read(77) Vtype
	if (Vtype == 2) then
		read(77) parint1000
	else
		READ_HOTSTART_VARS = 1
		return
	endif
!
	!sumIA
	read(77) Vtype
	if (Vtype == 2) then
		read(77) sumIA
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!sumIB
	read(77) Vtype
	if (Vtype == 2) then
		read(77) sumIB
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!sumIC
	read(77) Vtype
	if (Vtype == 2) then
		read(77) sumIC
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!Qmin
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Qmin(Dsize1))
				read(77) (Qmin(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!code_vol_bal
	read(77) Vtype
	if (Vtype == 2) then
		read(77) code_vol_bal
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!Vol_bal
	read(77) Vtype
	if (Vtype == 2) then
		read(77) Vol_bal
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!water_init_elevation
	read(77) Vtype
	if (Vtype == 2) then
		read(77) water_init_elevation
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!ini_cond
	read(77) Vtype
	if (Vtype == 2) then
		read(77) ini_cond
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!sum_temp
	read(77) Vtype
	if (Vtype == 2) then
		read(77) sum_temp
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!temp_outflow
	read(77) Vtype
	if (Vtype == 2) then
		read(77) temp_outflow
	else
		READ_HOTSTART_VARS = 1
		return
	endif
!
!	!Pumping rates at selected nodes. 
	!Qpump
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (Qpump(Dsize1))
				read(77) (Qpump(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!t_begin_pump
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (t_begin_pump(Dsize1))
				read(77) (t_begin_pump(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!IDpump
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (IDpump(Dsize1))
				read(77) (IDpump(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!
!	
!	!!!!!! Parameters thtat don't need to get saved to hotstart file go here.
	!h0L
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (h0L(Dsize1))
				read(77) (h0L(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
	!h0R
	read(77) En
	if (En == 1) then ! check if enabled
		read(77) Vtype
		if (Vtype == 1) then
			read(77) Vdims,Dsize1
			if (Vdims == 1) then
				allocate (h0R(Dsize1))
				read(77) (h0R(M),M=1,Dsize1)
			else
				READ_HOTSTART_VARS = 1
				return
			endif
		else
			READ_HOTSTART_VARS = 1
			return
		endif
	endif
!	
!
!	!Pressurization and Depressurization of the system
	!system_pressur
	read(77) Vtype
	if (Vtype == 2) then
		read(77) system_pressur
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!vol_factor
	read(77) Vtype
	if (Vtype == 2) then
		read(77) vol_factor
	else
		READ_HOTSTART_VARS = 1
		return
	endif
	!type_of_flow
	read(77) Vtype
	if (Vtype == 2) then
		read(77) type_of_flow
	else
		READ_HOTSTART_VARS = 1
		return
	endif
!
!	!To relax time steps
	!Relax_condit
	read(77) Vtype
	if (Vtype == 2) then
		read(77) Relax_condit
	else
		READ_HOTSTART_VARS = 1
		return
	endif
!
end function
