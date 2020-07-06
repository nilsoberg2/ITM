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

      subroutine interf_speed(j,AL,AR,QL,QR,CODL,Wpred)	      
	!This routine computes the interface speed of a mixed flow interface. 
	use common_module
	implicit none
	integer j,CODL
	double precision AL,AR,QL,QR,Wpred,wtemp3,wtemp4,u_max
	double precision P_phoL,P_phoR,uL,uR,hL,hR,TL,TR,RH
	!Sign and magnitude of an open channel-press.flow interface	
	
	!If(CODL == 0)then
	!      call H_from_Area(j,AL,hL,610,0)!Water depth    		    
      !	     call Pressure_Pho(j,hL,P_phoL,0)
		    
	!	    P_phoR = Aref(j)*g*haver_ref(j) + pc1(j)*pc1(j)*(AR-Aref(j))
	!elseif(CODL == 1)then	 		    
	!	    P_phoL = Aref(j)*g*haver_ref(j) + pc1(j)*pc1(j)*(AL-Aref(j))		
		    
	!	    call H_from_Area(j,AR,hR,640,0)!Water depth    		    
	!	    call Pressure_Pho(j,hR,P_phoR,0)
      !else
      !     write(99,*),'CODL .ne. 0,1. Error in subr. interf_speed'
      !	    write(98,*),'CODL .ne. 0,1. Error in subr. interf_speed'
	!	    call endprog
	!endif
	!uL = QL/AL; uR = QR/AR	
      !u_max = 20d0*max(abs(uL),abs(uR))
      !u_max = min(u_max, 0.30*pc_mixed)
      u_max = 5.d-1*pc_mixed
	
	wtemp3 = (QR-QL)/(AR-AL)
	!if (abs(AR-AL) < 1.d-14)then
	!      Wpred = wtemp3
	!      goto 1
	!endif	
	
	!wtemp4 = (uR**2d0+P_phoR - (uL**2d0+P_phoL))/(AR-AL)
	!if (wtemp4 <= 0.0)then
	!      Wpred = wtemp3
	!      goto 1
	!endif	
	!wtemp4 = sqrt(wtemp4)*SIGN(1d0,wtemp3)
	!Wpred = wtemp4
	Wpred = wtemp3
1	If(abs(Wpred) > u_max)then
            Wpred = u_max*SIGN(1d0,wtemp3)
      endif
2     continue
      end subroutine
