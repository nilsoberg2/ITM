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

      Subroutine ERP_Combined(j,i,Dt,h0b1,A0b1,Q0b1,IDfBound_cell)
	!This routine is used for performing reconstruction of the flow variables 
	!to achieve second-order accuracy. 
	use common_module
	implicit none 
	double precision Dt,h0b1,A0b1,Q0b1
	double precision AL,AR,YL,YR,hL,hR,etaL,etaR
	double precision P_phoL,P_phoR
	double precision QL,QR,DELFLUX
	double precision small,DELT_new
	double precision A,B,htemp5,Atemp5,Ts,RH
	integer i,j,IDfBound_cell,sum
	parameter (small = 1.d-14)
		
	if (i < 3 .or. i > Nx(j)-2)then
	      write(98,*),'i < 3 .or. i > Nx(j)-2'
	      write(99,*),'i < 3 .or. i > Nx(j)-2'
	      call endprog
	endif	
		
	!Setting (as initial guess) the flow type at the cell edges to be the same 
	!as that of the center of the cell 
	IdFlow_REC_L(i) = IdFlow(j,i); IdFlow_REC_R(i) = IdFlow(j,i)
	hb = S0(j)*Dx(j)
	sum = IdFlow(j,i-1)+IdFlow(j,i)+IdFlow(j,i+1) 
	!Profile reconstruction in the cells      
	
	!TVD version of the MUSCL-HANCOCK scheme (MINMOD) 	
	!For variable A	
	!Water stages	
10	If (i == 3 .or. i == Nx(j)-2)then
            If (i == 3)then
                B = (h0_Rec(j,i+1)+z0(j,i+1)-h0_Rec(j,i)- z0(j,i))/Dx(j)
                A = B
            elseif (i == Nx(j)-2)then
                A = (h0_Rec(j,i)+z0(j,i)-h0_Rec(j,i-1)-z0(j,i-1))/Dx(j)
                B = A
            else                
                write(98,*),'i .ne. 3, Nx(j)-2'
	          write(98,*),'Pipe, cell',j,i
	          call endprog
            endif
      else
            A = (h0_Rec(j,i)+z0(j,i) - h0_Rec(j,i-1)-z0(j,i-1))/Dx(j)
            B = (h0_Rec(j,i+1)+z0(j,i+1) - h0_Rec(j,i)-z0(j,i))/Dx(j)
      endif      
				
	!Small changes are reset, preserving their sign
	IF(ABS(A).LE.small) A = small*SIGN(1d0,A)
	IF(ABS(B).LE.small) B = small*SIGN(1d0,B)	
	CALL MINMOD(A,B,DELT_new)
	
	!Boundary extrapolated values etaL and etaR (Water elevation)
	etaL = h0_Rec(j,i) + z0(j,i) - 5d-1*Dx(j)*DELT_new
	etaR = h0_Rec(j,i) + z0(j,i) + 5d-1*Dx(j)*DELT_new
	hL = etaL - (z0(j,i-1)+z0(j,i))/2d0 
	hR = etaR - (z0(j,i)+z0(j,i+1))/2d0	
		
	If(IdFlow(j,i) == 0)then
	      if(hL <= ydry(j))then
	          hL = ydry(j); AL = Adry(j); QL = 0d0
	      endif
	      if(hR <= ydry(j))then
	          hR = ydry(j); AR = Adry(j); QR = 0d0
	      endif
	endif	
	
	!Treatment of dry-bed conditions
	If (IdFlow(j,i) == 0)then
                if(hR <= abs(S0(j)*dx(j)))then
                        hL = ydry(j); AL = Adry(j); QL = 0d0
                        hR = h0_Rec(j,i) + 1d0/2d0*abs(S0(j)*dx(j))
                        if (hR <= ydry(j))then
                            hR = ydry(j);AR = Adry(j);QR = 0d0
                            goto 5
                        endif
                        call Area_from_H(j,hR,AR,Ts,RH,IdFlow(j,i))
5                       QR = 0d0
                        A0L(i)=AL; A0R(i)= AR; h0L(i)=hL; h0R(i)=hR
	                  !Near dry bed conditions must have zero flow. Only the interface should be able to propagate
	                  Q0L(i) = QL; Q0R(i) = QR 
	                  IdFlow_REC_L(i) = IdFlow(j,i)
	                  IdFlow_REC_R(i) = IdFlow(j,i)
	                  !call endprog
	                  goto 15
                endif
       endif
		
	!To verify if any cell that was open channel in the previous time step changes the
	!type of flow (e.g., pressurized flow) in the next time step. This is only done 
	!for open-channel flows but not for pressurized flows since pressrurized flows
	!can sustain negatibve pressrures 
	If(IdFlow(j,i) == 0)then
	      if (hL > yref(j))then
	          IdFlow_REC_L(i) = 1	          
	      endif
	      if (hR > yref(j))then
	          IdFlow_REC_R(i) = 1
	      endif
	endif 		
		 
	!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
	!Fluxes for Q.
	!No reconstruction for Q is performed because this generates numerical storms,
	!especially during still water conditions. 
	!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
		
	!Fluxes for Q
	!Boundary extrapolated values AXL and AXR are computed
	!Please note that we are using the flow type of the reconstruction
	call Area_from_H(j,hL,AL,Ts,RH,IdFlow_REC_L(i))
	call Area_from_H(j,hR,AR,Ts,RH,IdFlow_REC_R(i))  

      !For variable Q
      !If (i == 3 .or. i == Nx(j)-2)then
      !      If (i == 3)then
      !          B = (Q0(j,i+1)-Q0(j,i))/Dx(j)
      !          A = B
      !      elseif (i == Nx(j)-2)then
      !          A = (Q0(j,i)-Q0(j,i-1))/Dx(j)
      !          B = A
      !      else                
      !          write(98,*),'i .ne. 3, Nx(j)-2. Routine ERP_Combined'
	!          write(98,*),'Pipe, cell',j,i
	!          call endprog
      !      endif
      !else
      !      A = (Q0(j,i)-Q0(j,i-1))/Dx(j)
	!      B = (Q0(j,i+1)-Q0(j,i))/Dx(j)
      !endif 

	!Small changes are reset, preserving their sign
	!IF(ABS(A).LE.small) A = small*SIGN(1d0,A)	
	!IF(ABS(B).LE.small) B = small*SIGN(1d0,B)	
	!CALL MINMOD(A,B,DELT_new)
	!Boundary extrapolated values UXL and UXR are computed		
	!QL = Q0(j,i) - 5d-1*DELT_new*Dx(j)
	!QR = Q0(j,i) + 5d-1*DELT_new*Dx(j)
	
	
	!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
	!Fluxes for A	
	!A0L(i) and A0R(i) are not corrected by QL and QR to avoid numerical storms. 
	!especially during still water conditions. 
	!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	!Without DELFLUX to avoid numerical storms
	A0L(i) = AL; A0R(i) = AR 
	h0L(i) = hL; h0R(i) = hR	
	
      !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
	!Fluxes for Q	
	!No correction for Q is performed to avoid numerical storms. 
	!especially during still water conditions. 
	!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		    
    
	!Fluxes for Q	
	!QL and  QR are evolved by a half time step and stored
	!into QL(i) and QR(i), where i denotes i+1/2		
	!If(IdFlow(j,i) == 0)then
	!      If (AL <= Adry(j))then
	!		    AL = Adry(j); YL = ydry(j); QL = 0d0
	!		    P_phoL = fluxdry(j)
	!	    elseif (AL > Atemp5)then
	!	        AL = Atemp5; YL = htemp5
	!		    call Pressure_Pho(j,YL,P_phoL,IdFlow(j,i))
	!	    else	
	!	        call H_from_Area(j,AL,YL,25,IdFlow(j,i))
	!	        call Pressure_Pho(j,YL,P_phoL,IdFlow(j,i))
	!	    endif
	!
	!     If (AR <= Adry(j))then
	!		    AR = Adry(j); YR = ydry(j); QR = 0d0
	!		    P_phoR = fluxdry(j)
	!		elseif (AR > Atemp5 )then
	!		    AR = Atemp5; YR = htemp5
	!		    call Pressure_Pho(j,YR,P_phoR,IdFlow(j,i))
	!	    else		        
	!	        call H_from_Area(j,AR,YR,25,IdFlow(j,i))		        
	!	        call Pressure_Pho(j,YR,P_phoR,IdFlow(j,i))		        
	!	    endif 		    
	!elseIf(IdFlow(j,i) == 1)then
	!	    P_phoL = Aref(j)*g*haver_ref(j) + pc1(j)*pc1(j)*(AL-Aref(j))
	!	    P_phoR = Aref(j)*g*haver_ref(j) + pc1(j)*pc1(j)*(AR-Aref(j))
	!else
	!	    write(98,*),'IdFlow(j,i) .ne. 0, 1'
	!	    write(98,*),'Subr. ERP_Combined4'
	!	    call endprog
	!endif
	!DELFLUX = (QL**2d0/AL + P_phoL) - (QR**2d0/AR + P_phoR)		
	!DELFLUX = 5d-1*(Dt/Dx(j))*DELFLUX	
 	!Q0L(i) = QL + DELFLUX
	!Q0R(i) = QR + DELFLUX
	
	Q0L(i) = Q0(j,i) !This was added to avoid numerical storms 
	Q0R(i) = Q0(j,i) !This was added to avoid numerical storms 
		
	!Variables at boundaries (This is needed ONLY at boundaries)
  15  if (i == 3 .or. i == Nx(j)-2)then
			if (i == 3)then
				h0b1 = h0L(i); A0b1 = A0L(i); Q0b1 = Q0L(i)
				if (sum == 3)then
				    IDfBound_cell = 1
				else
				    IDfBound_cell = IdFlow_REC_L(i)
				endif				
			elseif (i == Nx(j)-2)then 
				h0b1 = h0R(i); A0b1 = A0R(i); Q0b1 = Q0R(i)
				if (sum == 3)then
				    IDfBound_cell = 1
				else
				    IDfBound_cell = IdFlow_REC_R(i) 
				endif
			else
				write(98,*),'unknown condition'
				write(98,*),'Subroutine ERP_Combined1'
				write(99,*),'unknown condition. Subroutine ERP_Combined1'
				call endprog
			endif
      endif
20	continue	
      return
      end