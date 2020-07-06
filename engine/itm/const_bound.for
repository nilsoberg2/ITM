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

      subroutine const_bound(R)
      use common_module    
      implicit none
      integer Typ,iter,j,R,sum,n,i
	double precision t,AA,RH,R1,F11,F12,Val,Wpred
      double precision u11,y11,A11,Q11,c11,T1,P_pho11
	double precision ub,Qb,Yb,Ab,cb,Tb,P_phob,Ycrit
	double precision k1,x(10),fvec(10),TOL_CONST_STATE
	double precision hL,hR,AL,AR,QL,QR,AW0,QW0,cb1
	double precision Pw0,tol_local
	external const_disch,const_depth
	integer conver_result,info,Id1,Idb,Idb_old,IDF0,CODL
	
	!This routine computes the flux when using a constant boundary (flow discharge or piezometric head)
	j = NodeID(R,1)	
	tol_const_state = Tol_int_10_4
	typ = BCnode(R)
	Val = const_depth_flow(R)
	!Local head loss coefficient	
	if (Nodetype(R,1) == 2)then !outflowing  
		    K1 = Klocal(j,1)	
	elseIf(Nodetype(R,1) == 1)then
		    K1 = Klocal(j,2)
	else
		    write(98,*),'Nodetype .ne. 1,2'
		    write(98,*),'Subr. const_bound'
		    call endprog
	endif

	if (Nodetype(R,1) == 2)then !outflowing
	      i = 3
		    call ERP_combined(j,i,DT_GLOBAL,y11,A11,Q11,Id1)		
		    Ab_oldIA(1) = A11; Qb_oldIA(1) = Q11; yb_oldIA(1) = y11
		    Idb_old = Idflow_bound(j,1)		
	elseif (Nodetype(R,1) == 1)then !inflowing
	      i = Nx(j)-2
		    call ERP_combined(j,i,DT_GLOBAL,y11,A11,Q11,Id1)
		    Ab_oldIA(1) = A11; Qb_oldIA(1) = Q11; yb_oldIA(1) = y11
		    Idb_old = Idflow_bound(j,2)
	else
		    write(98,*),'Nodetype(R,1) .ne. 1,2'
		    write(98,*),'Subr. const_bound'
		    call endprog
	endif

	!Determining type of flow at the boundary
	if (Typ.eq.11)then !Prescribed water level
		    If (val >= yref(j))then 
			    Idb = 1
		    else
			    Idb = 0
		    endif
	else if (Typ.eq.10) then !Prescribed discharge
		    If (Id1 == 1)then 
			    Idb = 1
		    else
			    if (Idb_old == 1 .and. yb_oldIA(1)>=Yref(j))then
				    Idb = 1
			    else
				    Idb = 0
			    endif
		    endif
	endif
      
      If(Id1 == 0)then
          if (y11 <= ydry(j))then
                y11 = ydry(j); A11 = Adry(j); Q11 = 0d0; u11 = 0d0
                P_pho11 = P_pho_dry(j)
                goto 15
          endif
      endif
	
	u11=Q11/A11	
	call Pressure_Pho(j,y11,P_pho11,Id1)
15	If (Id1==0)then
		    call Area_from_H(j,y11,AA,Tb,RH,Id1)
		    c11 = sqrt(g*A11/Tb)
	else
		    c11 = pc1(j)
	endif
	
	!Sum of IDFlows (boundary and adjacent cell)
	sum = Id1+Idb
	
	if (Typ.eq.11)then !Prescribed water level
		    !Riemann invariants Method
		    yb = Val
		    If (Idb==0)then
		       call Area_from_H(j,yb,Ab,Tb,RH,Idb)
		        cb1 = sqrt(g*Ab/Tb)	
	      else
		        cb1 = pc1(j)
	      endif    		
		    If (sum == 1)goto 20
		    param1 = A11;  param2 = c11; param3 = Q11
		    param4 = Ab; param5 = cb1
	      parint1 = j;parint4 = sum;parint5 = R
		    n = 1
		    x(1:n) = (/Q11/)
              tol_local = Tol_int_10_8
		    call hybrd1 (const_depth,n,x,fvec,tol_local,info)
		    call converg (conver_result, info)
		    If (parint1000 == 1 .or. conver_result == 1)goto 20	
		    Qb = x(1)		    
		    goto 30
	elseif (Typ.eq.10) then !Prescribed discharge
		    If (Idb==1)then
			    Qb = Val*Ab_oldIA(1)/Aref(j)
		    elseif (Idb==0)then
			    Qb = Val
		    else
			    write(98,*),'Idb .ne. 0,1'
			    write(98,*),'Subr. const_bound'
			    call endprog		
		    endif
		    		       		
		    parint1000 = 0
		    If (sum == 1)then
			    yb = yb_oldIA(1)
			    goto 20
		    endif
		    param1 = A11;  param2 = c11; param3 = Q11
		    param4 = Qb; param5 = yb_oldIA(1)
	      parint1 = j;parint2 = Id1;parint3 = Idb
		    parint4 = sum; parint5 = R
		    n = 1
		    x(1:n) = (/y11/)
              tol_local = Tol_int_10_8
		    call hybrd1 (const_disch,n,x,fvec,tol_local,info)
		    call converg (conver_result, info)		 
		    
		    !If (parint1000 == 1 .or. conver_result == 1)goto 20 (This line doesn't work fine)	   
		    If(conver_result == 1 .or. parint1000 == 1)then	
			    parint1000 = 0
			    write(99,*),'No converg. Subr. const_bound. const_disch'
			    write(99,*),'cell,',i			    
			    write(99,*),'sum_no_converg',sum_no_converg		
			    yb = y11
			    goto 30	
		    else
			    yb = x(1)
			    goto 30
		    endif		
  	else
		    write(98,*),'Boundary condition type not 
     &		supported (downstream)  pipe = ',j
		    write(98,*),'const_bound'
		    call endprog
	endif

20	if (Nodetype(R,1) == 2)then !outflowing
		hL = ybound(j,1); hR = y11
		AL = Abound(j,1); AR = A11
		QL = Qbound(j,1); QR = Q11
	else !inflowing
		hL = y11; hR = ybound(j,2)
		AL = A11; AR = Abound(j,2)
		QL = Q11; QR = Qbound(j,2)
	endif
	parint1000 = 0
	If (sum == 0)then
	      i = 1 !i is not important here	
		    call Riemann_open(j,i,hL,hR,
     &		QL,QR,F11,F12,Aw0,Qw0,Pw0) 			
	elseIf (sum == 2)then
	      i = 1 !i is not important here	
		    call Riemann_pressur(j,i,AL,AR,
     &		QL,QR,F11,F12,Aw0,Qw0,Pw0)
	elseIf (sum == 1)then
		    if (Nodetype(R,1) == 2)then !outflowing
			    If(ID1 == 0)then
				    CODL = 1
			    else
				    CODL = 0
			    endif
			    i = 1 !i is not important here	
			    call Riemann_pressur_open(j,i,hL,hR,QL,
     &		    QR,CODL,Aw0,Qw0,Idf0,F11,F12,Wpred,Pw0)
		    else !inflowing
			    If(ID1 == 0)then
				    CODL = 0
			    else
				    CODL = 1
			    endif
			    i = 1 !i is not important here
			    call Riemann_pressur_open(j,i,hL,hR,QL,
     &		    QR,CODL,Aw0,Qw0,Idf0,F11,F12,Wpred,Pw0)	
		    endif
	else
		    write(98,*),'sum n.e. 0,1,2'
		    write(98,*),'Error in Subr. const_bound'
		    call endprog
	endif		
	Ab = Aw0
	call H_from_Area(j,Ab,yb,9,Idb)
	goto 35
	
	!Flux computation
 30	call Area_from_H(j,yb,Ab,Tb,RH,Idb)
 35	call Pressure_Pho(j,yb,P_phob,Idb) 
	F11 = Qb; F12 = Qb**2d0/Ab + P_phob
	if (Nodetype(R,1) == 2)then !outflowing
		    Abound(j,1) = Ab; Qbound(j,1) = Qb
		    ybound(j,1) = yb;Idflow_bound(j,1) = Idb
		    Fupst(j,1) = F11; Fupst(j,2) = F12
		    yres_jun_old(R) = h0(j,3) 
		    Pres_pho_Bound(j,1) = P_phob
	Elseif (Nodetype(R,1) == 1)then !inflowing
		    Abound(j,2) = Ab; Qbound(j,2) = Qb
		    ybound(j,2) = yb; Idflow_bound(j,2) = Idb
		    Fdownst(j,1) = F11;	Fdownst(j,2) = F12
		    yres_jun_old(R) = h0(j,Nx(j)-2)
		    Pres_pho_Bound(j,2) = P_phob
	Endif		
	return	
      end

	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	subroutine const_disch(n, x, fvec, iflag )
	use common_module
	implicit none	
	integer n,iflag,IA,IDfbIA1,sum,R
	double precision fvec(n),x(n),Qb,ub1,ybIA,temp8
	double precision AIA,TsIA,RHIA,u11,Q11,y11,A11,cb1,c11
	parint1000 = 0
	A11 = param1; c11 = param2; Q11 = param3; Qb = param4
	ybIA = param5 
	IA = parint1; IDfbIA1 = parint3
	sum = parint4; R = parint5
	u11 = Q11/A11
	If (IDfbIA1 == 0)then
		    temp8 = (yref(IA)+d(IA))/2d0
		    If (x(1) >= temp8)then			
			    x(1) = temp8
			    parint1000 = 1 
		    endif
		    If (x(1) < b2_min(IA))then	
			    x(1) = b2_min(IA)
			    parint1000 = 1 
		    endif
	endif

	call Area_from_H(IA,x(1),AIA,TsIA,RHIA,IDfbIA1)
	ub1 = Qb/AIA; cb1 = sqrt(g*AIA/TsIA)
	if (Nodetype(R,1) == 2)then !outflowing
		    If (sum == 0) then
			    fvec(1) = ub1 - u11 - (cb1 + c11)*(AIA - A11)/(AIA + A11)				    
		    elseif (sum == 2) then			
			    fvec(1) = ub1 - pc1(IA)*LOG (AIA)-(u11 - pc1(IA)*LOG(A11))
		    else
			    write(98,*),'sum n.e. 0,2'
			    write(98,*),'Error in Subr. const_disch'
			    call endprog		
		    endif
	elseif (Nodetype(R,1) == 1)then !inflowing
		    If (sum == 0) then
			    fvec(1) = ub1 - u11 + (cb1 + c11)*(AIA - A11)/(AIA + A11)	    
		    elseif (sum == 2) then
			    fvec(1) = ub1 + pc1(IA)*LOG (AIA) - (u11 + pc1(IA)*LOG(A11))
		    else
			    write(98,*),'sum n.e. 0,2'
			    write(98,*),'Error in Subr. const_disch'
			    call endprog		
		    endif
	endif
	return
	end	
	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	subroutine const_depth( n, x, fvec, iflag )
	use common_module
	implicit none	
	integer n,iflag,IA,sum,R
	double precision fvec(n),x(n),Qb,ub1
	double precision AIA,u11,Q11,A11,cb1,c11
	parint1000 = 0
	A11 = param1; c11 = param2;Q11 = param3;AIA = param4
	cb1 = param5
	IA = parint1;sum = parint4;R = parint5
	u11 = Q11/A11	
	Qb = x(1)
	ub1 = Qb/AIA

	if (Nodetype(R,1) == 2)then !outflowing
		    If (sum == 0) then
			    fvec(1) = ub1 - u11 - (cb1 + c11)*(AIA - A11)/(AIA + A11)				    
		    elseif (sum == 2) then			
			    fvec(1) = ub1 - pc1(IA)*LOG(AIA) - (u11 - pc1(IA)*LOG(A11))
		    else
			    write(98,*),'sum n.e. 0,2'
			    write(98,*),'Error in Subr. const_depth'
			    call endprog		
		    endif
	elseif (Nodetype(R,1) == 1)then !inflowing
		    If (sum == 0) then
			    fvec(1) = ub1 - u11 + (cb1 + c11)*(AIA - A11)/(AIA + A11)	    
		    elseif (sum == 2) then
			    fvec(1) = ub1 + pc1(IA)*LOG (AIA) - (u11 + pc1(IA)*LOG(A11))
		    else
			    write(98,*),'sum n.e. 0,2'
			    write(98,*),'Error in Subr. const_depth'
			    call endprog		
		    endif
	endif
	return
	end	
	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
