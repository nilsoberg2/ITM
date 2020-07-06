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

      subroutine Riemann_pressur_open(j,i,yL,yR,QL,QR,CODL,Aw0,Qw0,Idf0,
     &  FF1,FF2,Wpred,Pw0)	      
	!Purpose: This routine is used for computing fluxes at an open channel-pressurized flow interface.
      use common_module
	implicit none		
	double precision hL,hR,yL,yR,AL,AR,uL,uR,QL,QR,cL,cR
	double precision P_phoL,P_phoR,P_phoS
      double precision R,AS,TS,TL,TR,hS,area,RH
	double precision FL1,FL2,FR1,FR2,FF1,FF2 
	double precision fun,Dfun1L,Dfun1R,Dfun2L,Dfun2R,Dfun
	double precision delh,delA,var_temp	
	double precision FL,FR,SL,SR,Wlim,tol_local
	integer j,i,iter,CODL,n,Idf0
	double precision Aminus,Qminus,uminus,Aplus,Qplus
	double precision hminus,Tminus,cminus,P_phominus
	double precision hplus,uplus,Tplus,cplus,P_phoplus
	double precision AaverL,AaverR,QaverL,QaverR
	double precision uaverL,uaverR,caverL,caverR,Wpred
	double precision Aw0, Qw0, hw0, P_phohw0,Ab,Qb,Pw0
	double precision wtemp3,wtemp4, temp100
	double precision fvec(10), x(10)
	external posint1,posint2,negint1,negint2,posint1A,posint2A
	integer conver_result,info
	character*25 temp_id

	!conventions: (-) left, (+) right.
	!Posint1 = left is pressurized and right is free surface flow: positive interface propagating downstream
	!Posint2 = left is free surface flow and right is pressurized flow: positive interface propagating upstream
	!negint1 = left is free surface flow and right is pressurized flow: negative interface is propagating downstream
	!negint2 = left is pressurized flow and right is free surface flow: negative interface is propagating upstream
	Wlim = 0.001	!(This has to be positive and very small)	
	hL = yL; hR = yR
	!fluxes L and R	
	If(CODL == 0)then
		    !write(99,*),'129xx1--j,AL,AR,QL,QR,CODL',j,AL,AR,QL,QR,CODL			
		    call Area_from_H(j,hL,AL,TL,RH,0)	    
		    call Pressure_Pho(j,hL,P_phoL,0)
		    cL = sqrt(g*AL/TL)
		    call Area_from_H(j,hR,AR,TR,RH,1)		
		    P_phoR = Aref(j)*g*haver_ref(j) + pc1(j)*pc1(j)*(AR-Aref(j))
		    cR = pc1(j)
	else 
		    !write(99,*),'129xx2--j,AL,AR,QL,QR,CODL',j,AL,AR,QL,QR,CODL
		    call Area_from_H(j,hL,AL,TL,RH,1)
		    P_phoL = Aref(j)*g*haver_ref(j) + pc1(j)*pc1(j)*(AL-Aref(j))		
		    call Area_from_H(j,hR,AR,TR,RH,0)		
		    call Pressure_Pho(j,hR,P_phoR,0)
		    cL = pc1(j)	
		    cR = sqrt(g*AR/TR)		
	endif

	uR = QR/AR; uL = QL/AL
	FL1 = QL; FR1 = QR
	If(CODL == 0)then
		    FL2 = QL*QL/AL + P_phoL 
		    FR2 = QR*QR/AR + P_phoR 
	else
		    FL2 = QL*QL/AL + P_phoL 
		    FR2 = QR*QR/AR + P_phoR 
	endif
	
	!sign and magnitude of open channel-press.flow interface
	call interf_speed(j,AL,AR,QL,QR,CODL,Wpred)	
	
	!Interface speed to avoid unnecessary iterations
	
	
	!wtemp3 = (QR-QL)/(AR-AL)
	!if (abs(AR-AL) < 1.d-14)then
	!      Wpred = wtemp3 
	!      goto 1
	!endif	
	!wtemp4 = (uR**2d0+P_phoR - (uL**2d0+P_phoL))/(AR-AL)
	!if (wtemp4 <= 0d0)then
	!      Wpred = wtemp3
	!      goto 1
	!endif	
	!wtemp4 = dsqrt(wtemp4)*SIGN(1d0,wtemp3)
	!Wpred = wtemp4
	!Wpred = wtemp3
!1	If(abs(Wpred) > 0.50*pc_mixed)Wpred = 0.50*pc_mixed*
!     &	    SIGN(1d0,wtemp3)
!2     continue

	
	!Initial values
      parint1000 = 0
	Aminus = AL; Qminus = QL; hminus = hL
	Aplus = AR; Qplus = QR; hplus = hR
	paramOP1 = AL; paramOP2 = AR; paramOP3 = QL
	paramOP4 = QR; paramOP5 = FR2; paramOP6 = FL2
	paramOP7 = Wpred; paramOP8 = cL;paramOP9= cR
	parintOP1 = j

	If(CODL == 0)then		
		    If(Wpred >= Wlim)then			
			    !negative interface moving downstream
			    parint1000 = 0			
			    n = 4
			    x(1:n) = (/ hminus, hplus, Qminus, Qplus/)
			    !Choosing appropiate tolerance for solving equations	
			    tol_local = Tol_int_10_4
			    call hybrd1 (negint1,n,x,fvec,tol_local,info)			
			    call converg (conver_result, info)
                  If(conver_result == 1 .or. parint1000 == 1)then
			        temp_id = ''
		            call itm_get_swmm_id(1, j, temp_id) ! 1 for pipes
			        write(99,*),'No conv. Subr. Riemann_pressur_open/conv.'
				   !write(99,80),'Pipe,i,AL,AR,QL,QR,CODL',temp_id,i,AL,AR,QL,QR,CODL	
				   !write(99,*),'Nx(j)-2',Nx(j)-2
				    write(99,*),'routine: negint1'
				    !x(1:n) = (/ hminus, hplus, Qminus, Qplus/)
				    temp100 = 0d0
                      QL = 0d0
				    x(1:n) = (/ hminus, hplus, temp100, temp100/) !Whenever there is no convergence
			        !it is a good practice to set the flow equal to zero. In this way 
			        !we don't produce mass error
				    parint1000 = 0
			    endif
			    hL = hL; hR = x(1) 
			    QL = QL; QR = x(3)	
                  call Riemann_open(j,i,hL,hR,QL,QR,FF1,FF2,Ab,Qb,Pw0)
			    Idf0 = 0  !this was added
			    Aw0 = Ab; Qw0 = Qb
			    !This Riemann solver handles subcritical and supercritical flow
		    else			
			    !This routine computes fluxes at a positive mixed flow interface moving upstream. 

			    !If(uL-cL < Wpred)then
			    !	goto 5
			    !	parint1000 = 0				
			    !	n = 4
			    !	x(1:n) = (/Aminus, Aplus, Qminus, Qplus/)	
			    !	call hybrd1 (posint2A, n, x, fvec, tol_local, info )
			    !	call converg (conver_result, info)	
			    !	If(conver_result == 1 .or. parint1000 == 1)then					
			    !		write(99,*),'No conv. Sub. Riemann_pressur_open/conv.'
			    !		write(99,*),'routine: posint2A'
			    !		x(1:n) = (/Aminus, Aplus, Qminus, Qplus/)
			    !		parint1000 = 0
			    !	endif
			    !	Aw0 = x(2); Qw0 = x(4)
			    !	Idf0 = 1
				    !Fluxes at interface
			    !	FF1 = x(4)
			    !	P_phominus = Aref(j)*g*haver_ref(j) + 
         !&		!		pc1(j)*pc1(j)*(x(2)-Aref(j))
 			    !	FF2 = x(4)*x(4)/x(2) + P_phominus
			    !else
5				n = 2
				x(1:n) = (/ Aplus, Qplus/)
				!Choosing appropiate tolerance for solving equations	
				tol_local = Tol_int_10_4
				call hybrd1 ( posint2, n, x, fvec, tol_local, info )
				call converg (conver_result, info)	
				If(conver_result == 1 .or. parint1000 == 1)then
				    temp_id = ''
		            call itm_get_swmm_id(1, j, temp_id) ! 1 for pipes
					write(99,*),'No conv. Subr. Riemann_pressur_open/conv.'
					!write(99,80),'Pipe,i,AL,AR,QL,QR,CODL',temp_id,i,AL,AR,QL,QR,CODL	
                      !write(99,*),'Nx(j)-2',Nx(j)-2
					write(99,*),'routine: posint2'
					!x(1:n) = (/ Aplus, Qplus/)
					x(1:n) = (/ Aplus, 0d0/) !Whenever there is no convergence
			        !it is a good practice to set the flow equal to zero. In this way 
			        !we don't produce mass error
				endif
				Aw0 = x(1); Qw0 = x(2)
				Idf0 = 1
				!Fluxes at interface
				FF1 = x(2)
				P_phoplus = Aref(j)*g*haver_ref(j) + 
     &				pc1(j)*pc1(j)*(x(1)-Aref(j))
				Pw0 = P_phoplus
 				FF2 = x(2)*x(2)/x(1) + P_phoplus
 				If (Code_star_region == 1)then
                    P_pho_interf(i) = P_phoplus !Pressure_Pho at interfaces
                    A_interf(i) = Aw0 !Hydraulic area
                    call H_from_Area(j,A_interf(i),y_interf(i),513,1)!Water depth
                endif			
		    endif	
	else
		If(Wpred > -wlim)then
			!If(uR+cR > Wpred)then
			!	goto 10
			!	parint1000 = 0
			!	!positive interface propagating downstream
			!	!one free surface flow wave is in pressurized flow regime
			!	n = 4
			!	x(1:n) = (/Aminus, Aplus, Qminus, Qplus/)	
			!	call hybrd1 ( posint1A, n, x, fvec, tol_local, info )
			!	call converg (conver_result, info)	
			!	If(conver_result == 1 .or. parint1000 == 1)then					
			!		write(99,*),'No conv. Sub. Riemann_pressur_open/conv.'
			!		write(99,*),'routine: posint1A'
			!		x(1:n) = (/Aminus, Aplus, Qminus, Qplus/)
			!		parint1000 = 0
			!	endif
			!	Aw0 = x(1); Qw0 = x(3)
			!	Idf0 = 1
			!	!Fluxes at interface
			!	FF1 = x(3)
			!	P_phominus = Aref(j)*g*haver_ref(j) + 
     &		!		pc1(j)*pc1(j)*(x(1)-Aref(j))
 			!	FF2 = x(3)*x(3)/x(1) + P_phominus
			!else				
				!This routine computes fluxes at a positive mixed flow interface moving downstream. 
				!the two free surface flow waves are in pressurized flow regime
10				n = 2
				x(1:n) = (/ Aminus, Qminus/)
				!Choosing appropiate tolerance for solving equations	
				tol_local = Tol_int_10_4
				call hybrd1 ( posint1, n, x, fvec, tol_local, info )
				call converg (conver_result, info)	
				If(conver_result == 1 .or. parint1000 == 1)then
				    temp_id = ''
		            call itm_get_swmm_id(1, j, temp_id) ! 1 for pipes
					write(99,*),'No conv. Sub. Riemann_pressur_open/conv.'
					!write(99,80),'Pipe,i,AL,AR,QL,QR,CODL',temp_id,i,AL,AR,QL,QR,CODL	
					write(99,*),'routine: posint1'
					!x(1:n) = (/ Aminus, Qminus/)
					x(1:n) = (/ Aminus, 0d0/) !Whenever there is no convergence
			        !it is a good practice to set the flow equal to zero. In this way 
			        !we don't produce mass error
				endif
				Aw0 = x(1); Qw0 = x(2)
				Idf0 = 1
				!Fluxes at interface
				FF1 = x(2)
				P_phominus = Aref(j)*g*haver_ref(j) +
     &				 pc1(j)*pc1(j)*(x(1)-Aref(j))
				Pw0 = P_phominus
 				FF2 = x(2)*x(2)/x(1) + P_phominus 				
 				If (Code_star_region == 1)then
                    P_pho_interf(i) = P_phominus !Pressure_Pho at interfaces
                    A_interf(i) = Aw0 !Hydraulic area
                    call H_from_Area(j,A_interf(i),y_interf(i),511,1)!Water depth
                endif 				
			!endif			
		else
			parint1000 = 0
			!negative interface. Interface moving upstream
			n = 4
			x(1:n) = (/hminus, hplus, Qminus, Qplus/)
			!Choosing appropiate tolerance for solving equations	
			tol_local = Tol_int_10_4
				
			call hybrd1 ( negint2, n, x, fvec, tol_local, info )
			call converg (conver_result, info)			
			If(conver_result == 1 .or. parint1000 == 1)then
			    temp_id = ''
		        call itm_get_swmm_id(1, j, temp_id) ! 1 for pipes
			    write(99,*),'No conv. Subr. Riemann_pressur_open/conv.'
				!write(99,80),'Pipe,i,AL,AR,QL,QR,CODL',temp_id,i,AL,AR,QL,QR,CODL
				!write(99,*),'Nx(j)-2',Nx(j)-2	
				write(99,*),'routine: negint2'
				!x(1:n) = (/ hminus, hplus, Qminus, Qplus/) 
				temp100 = 0d0
				x(1:n) = (/ hminus, hplus, temp100, temp100/) !Whenever there is no convergence
                  QR = 0d0
			    !it is a good practice to set the flow equal to zero. In this way 
			    !we don't produce mass error				
				parint1000 = 0
			endif			
			hL = x(2); hR = hR
			QL = x(4); QR = QR			
			call Riemann_open(j,i,hL,hR,QL,QR,FF1,FF2,Ab,Qb,Pw0)
			Idf0 = 0  !this was added
			Aw0 = Ab; Qw0 = Qb
			!This Riemann solver handles subcritical and supercritical flow
		endif		
	endif	
80	format(A40,A30,I2,4F14.6,I10) 
      return
	end subroutine

	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	subroutine posint1 ( n, x, fvec, iflag )
	use common_module
	implicit none
	integer n, iflag, j
	double precision fvec(n), x(n)
	double precision AL, AR, QL, QR, FR2
	double precision AaverL, uaverL, QaverL
	double precision P_phominus, F2minus
	
	AL = paramOP1; AR = paramOP2; QL = paramOP3; 
	QR = paramOP4; FR2 = paramOP5
	j = parintOP1
	AaverL = (AL+x(1))/2d0; QaverL = (QL+x(2))/2d0
	uaverL = QaverL/AaverL
	P_phominus = Aref(j)*g*haver_ref(j) + pc1(j)*pc1(j)*(x(1)-Aref(j))	
	F2minus = x(2)*x(2)/x(1) + P_phominus
	fvec(1) = FR2 - F2minus - (QR-x(2))*(QR-x(2))/(AR-x(1))
	fvec(2) = (x(2)-QL-(uaverL-pc1(j))*(x(1)-AL))*uaverL !Normalization
	return
	end
	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	subroutine posint2 ( n, x, fvec, iflag )
	use common_module
	implicit none
	integer n, iflag, j
	double precision fvec(n), x(n)
	double precision AL, AR, QL, QR, FL2
	double precision AaverR, uaverR, QaverR
	double precision P_phoplus, F2plus
	
	AL = paramOP1; AR = paramOP2; QL = paramOP3; 
	QR = paramOP4; FL2 = paramOP6
	j = parintOP1

	AaverR = (AR+x(1))/2d0; QaverR = (QR+x(2))/2d0	
	uaverR = QaverR/AaverR
	P_phoplus = Aref(j)*g*haver_ref(j) + pc1(j)*pc1(j)*(x(1)-Aref(j))	
	F2plus = x(2)*x(2)/x(1) + P_phoplus
	fvec(1) = F2plus - FL2 - (x(2)-QL)*(x(2)-QL)/(x(1)-AL)
	fvec(2) = (QR - x(2) - (uaverR+pc1(j))*(AR-x(1)))*uaverR !Normalization
	return
	end
	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	subroutine negint1 ( n, x, fvec, iflag )		
	!This routine computes fluxes at a negative mixed flow interface moving downstream. 
	use common_module
	implicit none
	integer n,iflag,j
	double precision fvec(n), x(n)
	double precision AL,AR,QL,QR,uL,uR,FR2
	double precision P_phominus, P_phoplus, F2minus
	double precision area, RH, Wpred, var_temp
	double precision Aminus,Qminus,Aplus,Qplus
	double precision hminus, Tminus, cminus
	double precision hplus, Tplus, cplus, F2plus	
	double precision uaverR,QaverR,AaverR
	double precision uaverL,QaverL,AaverL,cL,caverL

	parint1000 = 0 
	AL = paramOP1; AR = paramOP2; QL = paramOP3; 
	QR = paramOP4; Wpred = paramOP7 
	cL = paramOP8
	uL = QL/AL; uR = QR/AR
	j = parintOP1	

	hminus = x(1)
	hplus = x(2)
	
	If (hminus >= b2_max(j))then
		hminus = b2_max(j)  !Free surface flow
		x(1) = hminus
          parint1000 = 1
	endif
	
	If (hminus < b2_min(j))then
		    hminus = b2_min(j)
		    x(1) = hminus  !Free surface flow	
              parint1000 = 1
	endif	
	
	call Area_from_H(j,hminus,Aminus,Tminus,RH,0)	    
	call Area_from_H(j,hplus,Aplus,Tplus,RH,1)	    

	AaverR = (AR+Aplus)/2d0; QaverR = (QR+x(4))/2d0	
	uaverR = QaverR/AaverR	

	AaverL = (AL+Aminus)/2d0; QaverL = (QL+x(3))/2d0	   
	uaverL = QaverL/AaverL
	call Pressure_Pho(j,hminus,P_phominus,0)
	cminus = sqrt(g*Aminus/Tminus)
	F2minus = x(3)*x(3)/Aminus + P_phominus  
	hplus = yref(j) + pc1(j)*pc1(j)/(g*Aref(j))*(Aplus-Aref(j))
	P_phoplus = Aref(j)*g*haver_ref(j) + pc1(j)*pc1(j)*(Aplus-Aref(j))	
	F2plus = x(4)*x(4)/Aplus + P_phoplus
	!fvec(1) = Wpred*(Aplus-Aminus) - (x(4)-x(3))
      fvec(1) = (Wpred*(Aplus-Aminus) - (x(4)-x(3)))*Wpred !Normalization
	fvec(2) = (x(4)-x(3))*Wpred - (F2plus-F2minus)
	!fvec(3) = (QR-x(4)) - (uaverR+pc1(j))*(AR-Aplus)
      fvec(3) = ((QR-x(4)) - (uaverR+pc1(j))*(AR-Aplus))*Wpred !Normalization
	!fvec(3) = uR - pc1(j)*LOG(AR) -(x(4)/Aplus - pc1(j)*LOG(Aplus))

	caverL = (cL + cminus)/2d0
	!If (uaverL+caverL > = Wpred)then
	!	fvec(4) = (QL-x(3)) - (uaverL-caverL)*(AL-Aminus)
	!else
		var_temp = 5.d-1*(x(4)/Aplus)**2d0 - 5.d-1*(x(3)/Aminus)**2d0
		var_temp = var_temp + g*(hplus - hminus)
		!fvec(4) = var_temp - Wpred*(x(4)/Aplus-x(3)/Aminus)
          fvec(4) = (var_temp - Wpred*(x(4)/Aplus-x(3)/Aminus))*Aref(j) !Normalization
	!endif
	return
	end
	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	subroutine negint2 ( n, x, fvec, iflag )		
	!This routine computes fluxes at a negative mixed flow interface moving upstream. 
	use common_module
	implicit none
	integer n,iflag,j
	double precision fvec(n), x(n)
	double precision uL,AL,AR,QL,QR,FR2
	double precision P_phominus,P_phoplus,F2minus
	double precision area,RH,Wpred,var_temp
	double precision Aminus,Qminus,Aplus,Qplus
	double precision hminus,Tminus
	double precision hplus,Tplus,F2plus
	double precision uaverL,QaverL,AaverL
	double precision uaverR,QaverR,AaverR,caverR,cR,cplus
	parint1000 = 0
	AL = paramOP1; AR = paramOP2; QL = paramOP3
	uL = QL/AL
	QR = paramOP4; Wpred = paramOP7; cR = paramOP9
	j = parintOP1
	
	hminus = x(1)
	hplus = x(2)
	
	If (hplus >= b2_max(j))then
		hplus = b2_max(j)
		x(2) = hplus
          parint1000 = 1
	endif

	If (hplus < b2_min(j))then
		hplus = b2_min(j)
		x(2) = hplus
          parint1000 = 1
	endif	
	call Area_from_H(j,hminus,Aminus,Tminus,RH,1)	    
	call Area_from_H(j,hplus,Aplus,Tplus,RH,0)
	AaverR = (AR+Aplus)/2d0; QaverR = (QR+x(4))/2d0	
	uaverR = QaverR/AaverR
	AaverL = (AL+Aminus)/2d0; QaverL = (QL+x(3))/2d0
	uaverL = QaverL/AaverL

	hminus = yref(j) + pc1(j)*pc1(j)/(g*Aref(j))*(Aminus-Aref(j))
	P_phominus = Aref(j)*g*haver_ref(j) + pc1(j)*pc1(j)*
     &	(Aminus-Aref(j))		
	call Pressure_Pho(j,hplus,P_phoplus,0)
	cplus = sqrt(g*Aplus/Tplus)
	F2minus = x(3)*x(3)/Aminus + P_phominus 
	F2plus = x(4)*x(4)/Aplus + P_phoplus 
	!fvec(1) = Wpred*(Aplus-Aminus) - (x(4)-x(3)) !Normalization
      fvec(1) = (Wpred*(Aplus-Aminus) - (x(4)-x(3)))*Wpred !Normalization
	fvec(2) = (x(4)-x(3))*Wpred - (F2plus-F2minus)
	fvec(3) = ((x(3)-QL) - (uaverL - pc1(j))*(Aminus-AL))*Wpred !Normalization
	!fvec(3) = uL + pc1(j)*LOG(AL) -(x(3)/Aminus + pc1(j)*LOG(Aminus))
	caverR = (cR + cplus)/2d0
	!If (uaverR-caverR <= Wpred)then
	!	fvec(4) = (QR-x(4)) - (uaverR+caverR)*(AR-Aplus)
	!else
		var_temp = 5.d-1*(x(4)/Aplus)**2d0 - 5.d-1*(x(3)/Aminus)**2d0
		var_temp = var_temp + g*(hplus - hminus)
		!fvec(4) = var_temp - Wpred*(x(4)/Aplus-x(3)/Aminus)	!Normalization
          fvec(4) = (var_temp - Wpred*(x(4)/Aplus-x(3)/Aminus))*Aref(j) !Normalization
	!endif
	return
	end

	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	subroutine posint1A ( n, x, fvec, iflag )	
	!positive interface propagating downstream
	!one free surface flow wave is in pressurized flow regime
	use common_module
	implicit none
	integer n,iflag,j
	double precision fvec(n), x(n)
	double precision AL,AR,QL,QR,uL,uR,FR2
	double precision P_phominus, P_phoplus, F2minus
	double precision area, RH, Wpred, var_temp
	double precision Aminus,Qminus,Aplus,Qplus
	double precision hminus, Tminus, cminus
	double precision hplus, Tplus, cplus, F2plus	
	double precision uaverR,QaverR,AaverR
	double precision uaverL,QaverL,AaverL,cR,caverR
	parint1000 = 0
	AL = paramOP1; AR = paramOP2; QL = paramOP3
	uL = QL/AL
	QR = paramOP4; Wpred = paramOP7; cR = paramOP9
	j = parintOP1
	
	Aminus = x(1)
	Aplus = x(2)
	
	If (Aplus >= A2_max(j))then
		Aplus = A2_max(j)
		x(2) = Aplus
          parint1000 = 1
	endif

	If (Aplus < A2_min(j))then
		Aplus = A2_min(j)
		x(2) = Aplus
          parint1000 = 1
	endif

	AaverR = (AR+x(2))/2d0; QaverR = (QR+x(4))/2d0	
	uaverR = QaverR/AaverR
	AaverL = (AL+Aminus)/2d0; QaverL = (QL+x(3))/2d0
	uaverL = QaverL/AaverL
	P_phominus = Aref(j)*g*haver_ref(j) + pc1(j)*pc1(j)*(x(1)-Aref(j))
	call H_from_Area(j,Aplus,hplus,1293,0)
	call Area_from_H(j,hplus,area,Tplus,RH,0)	    
	call Pressure_Pho(j,hplus,P_phoplus,0)
	cplus = sqrt(g*Aplus/Tplus)
	F2minus = x(3)*x(3)/x(1) + P_phominus 
	F2plus = x(4)*x(4)/x(2) + P_phoplus 
	fvec(1) = (Wpred*(x(2)-x(1)) - (x(4)-x(3)))*Wpred !Normalization
	fvec(2) = (x(4)-x(3))*Wpred - (F2plus-F2minus)
	fvec(3) = ((QL-x(3)) - (uaverL-pc1(j))*(AL-x(1)))*Wpred !Normalization
	caverR = (cR + cplus)/2d0	
	fvec(4) = (QR-x(4) - (uaverR+caverR)*(AR-x(2)))*Wpred !Normalization
	return
	end
	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	subroutine posint2A ( n, x, fvec, iflag )	
	!positive interface propagating upstream
	!one free surface flow wave is in pressurized flow regime (ghost wave)
	use common_module
	implicit none
		
	integer n,iflag,j
	double precision fvec(n), x(n)
	double precision AL,AR,QL,QR,uL,uR,FR2
	double precision P_phominus, P_phoplus, F2minus
	double precision area, RH, Wpred, var_temp
	double precision Aminus,Qminus,Aplus,Qplus
	double precision hminus, Tminus, cminus
	double precision Tplus, cplus, F2plus	
	double precision uaverR,QaverR,AaverR
	double precision uaverL,QaverL,AaverL,cL,caverL
	parint1000 = 0 
	AL = paramOP1; AR = paramOP2; QL = paramOP3; 
	QR = paramOP4; Wpred = paramOP7 
	cL = paramOP8
	uL = QL/AL; uR = QR/AR
	j = parintOP1
	
	Aminus = x(1)
	Aplus = x(2)
	
	If (Aminus >= A2_max(j))then
		Aminus = A2_max(j)
		x(1) = Aminus
          parint1000 = 1
	endif
	If (Aminus < A2_min(j))then
		Aminus = A2_min(j)
		x(1) = Aminus
          parint1000 = 1
	endif

	AaverR = (AR+x(2))/2d0; QaverR = (QR+x(4))/2d0	
	uaverR = QaverR/AaverR
	AaverL = (AL+Aminus)/2d0; QaverL = (QL+x(3))/2d0	   
	uaverL = QaverL/AaverL

	call H_from_Area(j,Aminus,hminus,1292,0)
	call Area_from_H(j,hminus,area,Tminus,RH,0)	    
	call Pressure_Pho(j,hminus,P_phominus,0)
	cminus = sqrt(g*Aminus/Tminus)
	F2minus = x(3)*x(3)/x(1) + P_phominus  	
	P_phoplus = Aref(j)*g*haver_ref(j) + pc1(j)*pc1(j)*(x(2)-Aref(j))	
	F2plus = x(4)*x(4)/x(2) + P_phoplus
	caverL = (cL + cminus)/2d0
	fvec(1) = (Wpred*(x(2)-x(1)) - (x(4)-x(3)))*Wpred !Normalization
	fvec(2) = (x(4)-x(3))*Wpred - (F2plus-F2minus)
	fvec(3) = ((QL-x(3)) - (uaverL-caverL)*(AL-x(1)))*Wpred !Normalization
	fvec(4) = ((QR-x(4)) - (uaverR+pc1(j))*(AR-x(2)))*Wpred !Normalization	
	return
	end
	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

