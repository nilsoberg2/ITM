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

	function Ycrit(j,Qc)
	use common_module
	implicit none      
	!This routine is used for computing the critical depth.
	double precision Qc,yc,Ycrit,Ac,Tc
	double precision f1,df1,delf,temp1
	double precision dt_dy,R,tol_local
	double precision fvec(1), x(1)
	integer conver_result,info
	integer i,j,n
	External Crit_iter	
	
	if (Qc <= 0d0 .or. Qc > Qcrit_maxIA(j))then 
		write(98,*),'Qc is < 0 or extremely large'
		write(98,*),'Subroutine Ycrit'
		call endprog
	endif
	tol_local = 1.d-8
	if (Qc > 0.5*Qcrit_maxIA(j))then
		yc = Yref(j)
	else
		yc = (Qc/d(j))**(2d0/3d0)/g**(1d0/3d0) 	
	endif

	paramOP1 = 	Qc; parintOP1 = j
	n = 1; x(1) = yc
	parint1000 = 0
	!write(99,*),'Qc',Qc,'yc',yc,'diam',d(j),'j',j
	!write(99,*),'Ycrit1', x(1)
	call hybrd1 (Crit_iter, n, x, fvec, tol_local, info )
	Ycrit = x(1)
	!write(99,*),'Ycrit2', x(1)
	call converg (conver_result, info)	
	If(conver_result == 1)then	
		write(98,*),'No conv. critical flow'
		write(98,*),'Subr. Util'
		write(98,*),'Ycrit,Qc',Ycrit,Qc
		write(98,*),'Qcrit_maxIA',Qcrit_maxIA(j)
		call endprog
	endif
	end	
	
	!##############################################################	
	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	subroutine Crit_iter ( n, x, fvec, iflag )
	!routine for iterating the solution of a critical flow
	use common_module
	implicit none
	integer n, iflag, j
	double precision fvec(n), x(n), Qc	
	double precision Ac, Tc, RH
	parint1000 = 0
	Qc = paramOP1
	j = parintOP1
	if (x(1) >= b2_max(j)) then		
		    x(1)=b2_max(j)
	endif
	if (x(1) <= 0.01*b2_min(j))then
		x(1) = 0.01*b2_min(j)
	endif

	call Area_from_H(j,x(1),Ac,Tc,RH,0)				
	fvec(1) = 1d0-Qc**2d0*Tc/(g*Ac**3d0)	
	return
	end
	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

	!########################################
	function Ynormal(j,Q)
	use common_module
	implicit none
	!This routine is used for computing the normal depth.
	double precision Q,y,Ynormal,A,T,R
	double precision f1,df1,delf,temp1,temp2
	double precision tol_local
	integer i,j,n
	double precision fvec(1), x(1)
	integer conver_result,info
	External Norm_iter

	if (Q <= 0d0 .or. Q >1.d16)then 
		write(98,*),'Q is <= 0 or extremely large'
		write(98,*),'Q',Q,'dia',d(j),'pipe',j 	
		write(98,*),'slo =',S0(J),'n = ',nM(J)
		write(98,*),'Subroutine Ynormal'
		call endprog
	endif
	y = Q*nm(j)/(d(j)*sqrt(S0(j))) !to start iteration
	y = y**(6.d-1)
	parint1000 = 0
	paramOP1 = 	Q; parintOP1 = j
	n = 1; x(1) = y
	!write(99,*),'Ynorm1', x(1)

	tol_local = 1.d-8
	call hybrd1 (Norm_iter, n, x, fvec, tol_local, info )
	Ynormal = x(1)
	
	call converg (conver_result, info)	
	If(conver_result == 1)then	
		write(98,*),'Q =',Q
          write(98,*),'Slope', S0(j)
          write(98,*),'n roughness', nM(j)
          write(98,*),'Diameter', d(j)
		write(98,*),'Ynorm2', x(1)
		write(98,*),'Subr. Util'
		call endprog
	endif
		
	return
	end
	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	subroutine Norm_iter ( n, x, fvec, iflag )
	!routine for iterating the solution of a critical flow
	use common_module
	implicit none
	integer n, iflag, j
	double precision fvec(n), x(n), Q	
	double precision A, T, RH
	parint1000 = 0
	Q = paramOP1
	j = parintOP1
	if (x(1)>=b2_max(j)) then		
		    x(1)=b2_max(j)
	endif
	
	if (x(1) <= 0.01*b2_min(j))then
	  x(1) = 0.01*b2_min(j)
	endif
	
	call Area_from_H(j,x(1),A,T,RH,0)
	fvec(1) = Q - 1d0/nm(j)*A*RH**(2d0/3d0)*sqrt(S0(j))
	return
	end
	!###########################################################
	!###########################################################
	function Yconjugate(j,d1,Q)
	use common_module
	implicit none
	!Purpose: To compute the conjugate depth (supercritical flows)
	!Theory is in Program Annel written by Arturo Leon
	double precision d1,d2,Q,Yconjugate
	double precision te,A,T,RH,v1,v2,ff1,ymax
	double precision temp1,delf,tol_local
	integer i,j,n
	double precision fvec(1), x(1)
	integer conver_result,info
	External Conjug_iter

	if (Q <= 0.000001*Qmin(j) .or. Q >1.d20)then 
		write(98,*),'Q is < 0 or extremely large'
		write(98,*),'Subroutine Yconjugate'
		write(99,*),'Q is < 0 or extremely large'
		write(99,*),'Subroutine Yconjugate'
		call endprog
	endif	
	parint1000 = 0
	paramOP1 = Q; paramOP2 = d1	
	parintOP1 = j
	call Area_from_H(j,d1,A,T,RH,0)
      v1 = (Q/A)
	paramOP3 = Q*v1+g*((d(j)-d1)*d1*(3d0*d(j)*d(j)-4d0*d(j)*d1+
     &	 4d0*d1*d1)-3d0*d(j)*d(j)*(d(j)-2d0*d1)*
     &	sqrt(d(j)-d1)*sqrt(d1)*atan(sqrt(d1/(d(j)-d1))))
     &                   /(12d0*sqrt((d(j)-d1)*(d1)))
	n = 1; x(1) = 0.5*d(j)
	tol_local = 1.d-8
	      
	!write(99,*),'j,d1,Q,dia', j,d1,d2,Q,d(j)
	call hybrd1 (Conjug_iter, n, x, fvec, tol_local, info)
	call converg (conver_result, info)	
	If(conver_result == 1)then
	      !ymax = 5.d-1*(Yref(j)+b2_max(j))		
            ymax = Yref(j)
            if(x(1) < b2_min(j))then
                x(1) = b2_min(j)
                 write(99,*),'No conv1. Util Yconjugate1'
	      elseif(x(1) > ymax)then
	          
	          !write(98,*),'j,guessY,d2,Q,dia', j,d2,x(1),Q,d(j)
	          !write(98,*),'No conv2. Util Yconjugate2'
		        write(99,*),'No conv2. Util Yconjugate2'
		        x(1) = ymax	
		    else
		        write(99,*),'No conv3. Util Yconjugate3'
		        x(1) = ymax
	      endif
	endif
	Yconjugate = x(1)
	return
	end
	
      !&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	subroutine Conjug_iter ( n, x, fvec, iflag )
	!routine for iterating the solution of a critical flow
	use common_module
	implicit none
	integer n, iflag, j
	double precision fvec(n), x(n), Q	
	double precision A, T, RH,d1,v1,v2,ff1,ymax
	parint1000 = 0	
	
	Q = paramOP1; d1 = paramOP2; ff1 = paramOP3
	j = parintOP1	
	
	!ymax = 5.d-1*(Yref(j)+b2_max(j))
      ymax = max(Yref(j),0.8*d(j))
      
      if (x(1)>= ymax) then		
		    x(1)= ymax
	endif
	
	if (x(1) <= 0.01*b2_min(j))then
	      x(1) = 0.01*b2_min(j)
      endif
      
      If (ISNAN(x(1)))then !To make sure x(1) is not a NAN
          write(98,*),'NaN is found in Subr. Util. Conjugate depth' 
          call endprog
	endif
      
	
	!write(99,*),'t,j,x(1)',t_global,j,x(1)
	
	call Area_from_H(j,x(1),A,T,RH,0)
	v2 = (Q/A)

	fvec(1) = Q*v2+g*((d(j)-x(1))*x(1)*(3d0*d(j)*d(j)-4d0*d(j)*
     &		x(1)+4d0*x(1)*x(1))-3d0*d(j)*d(j)*(d(j)-2d0*x(1))*
     &		sqrt(d(j)-x(1))*sqrt(x(1))*atan(sqrt(x(1)/(d(j)-x(1)))))
     &          /(12d0*sqrt((d(j)-x(1))*(x(1))))-ff1	
	return
	end
	!###########################################################

