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

      Subroutine Riemann_pressur(j,i,AL,AR,QL,QR,FF1,FF2,AStar,Qstar,
     &	P_phoStar)
	!This routine is used for computing fluxes at a pure pressurized flow interface.
	use common_module
	implicit none		
      integer i,j
	double precision FF1,FF2      
      double precision AL,AR,QL,QR,uL,uR,Astar,Qstar
	double precision P_phoStar,Linearized,caver,uaver
	double precision delA,delU
	character*25 temp_id	
	
	uL = QL/AL; uR = QR/AR
	delA = abs((AL-AR)/(5d-1*(AL+AR)))
	!delU = abs((UL-UR)/(5d-1*(UL+UR)))
	if (delA < 1.d-14)then
		AStar = (AL+AR)/2d0
	else
		AStar = Linearized(AL,AR,pc1(j),pc1(j),uL,uR)
	endif
	
	If (ISNAN(AStar))then 
	      temp_id = ''
		    call itm_get_swmm_id(1, j, temp_id) ! 1 for pipes			
          write(98,*),'NaN is found in Riemann_pressur. Error in pipe',
     &          temp_id
			call endprog
	endif	
	caver = pc1(j)
	uaver = (uL+uR)/2d0
	Qstar = QL + (uaver-caver)*(AStar-AL)
	P_phoStar = Aref(j)*g*haver_ref(j) + pc1(j)*pc1(j)*(AStar-Aref(j))
	!computation of fluxes (star region)	
	FF1 = Qstar
	FF2 = Qstar*Qstar/Astar + P_phoStar
	If (Code_star_region == 1)then
            P_pho_interf(i) = P_phoStar !Pressure_Pho at interfaces
            A_interf(i) = AStar !Hydraulic area
            call H_from_Area(j,A_interf(i),y_interf(i),411,1)!Water depth
      endif
	End Subroutine
