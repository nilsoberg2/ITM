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

      subroutine Rating_curve(R)
      use common_module    
      implicit none
      integer iter,j,i,R,sum,n
	double precision t,AA,RH,R1,F11,F12,Wpred
      double precision u11,y11,A11,Q11,c11,T1,P_pho11
	double precision ub,Qb,Yb,Ab,cb,Tb,P_phob,Ycrit
	double precision k1,x(10),fvec(10),TOL_CONST_STATE
	double precision hL,hR,AL,AR,QL,QR,AW0,QW0
	double precision Pw0,p1
	external rat_curve
	integer conver_result,info,Id1,Idb,IDF0,CODL
	character*25 temp_id	
	
	!This routine computes the flux for a rating curve boundary 	
	j = NodeID(R,1)	
	!Distance from pipe invert to weir (or spillway) crest	
	if (Nodetype(R,1) == 2)then !outflowing
          i = 3
          call ERP_combined(j,i,DT_GLOBAL,y11,A11,Q11,Id1)
      Elseif (Nodetype(R,1) == 1)then !inflowing
          i = Nx(j)-2
          call ERP_combined(j,i,DT_GLOBAL,y11,A11,Q11,Id1)
      Else
          write(98,*),'Nodetype(R,1) .ne. 1,2'
          write(98,*),'Subr. Rating_curve'
          call endprog
	Endif
        
      
	!Type of flow at the boundary
	If (Id1 == 1)then !Pressurized flow
		Idb = 1
	elseIf (Id1 == 0)then !Open channel flow	
		Idb = 0
	else
	  write(98,*),'Id1 n.e. 0,1'
		write(98,*),'Error in Subr. Rating_curve'
		call endprog	
	endif

	sum = Id1+Idb
	u11=Q11/A11
      
      
      !Dry bed conditions
      if (Id1 == 0)then
          if (y11 < (1d0+Tol_int_10_4)*ydry(j))then
              yb = ydry(j); Qb = 0d0; Idb = 0
              goto 10
          endif   
      endif
	
	!Wave celerity
	call Pressure_Pho(j,y11,P_pho11,Id1)
	If (Id1==0)then
		    call Area_from_H(j,y11,AA,Tb,RH,Id1)
		    c11 = sqrt(g*A11/Tb)
	elseIf (Id1 == 1)then 
		    c11 = pc1(j)
	else
	        write(98,*),'Id1 n.e. 0,1'
              write(98,*),'Error in Subr. Rating_curve'
		    call endprog
	endif
			
	parint1000 = 0	
	param1 = A11;  param2 = c11; param3 = Q11	
	param4 = y11
	parint1 = j; parint2 = Id1; parint3 = Idb
	parint4 = sum; parint5 = R
	n = 1
	x(1:n) = (/y11/)
      yb = x(1)
      yb = y11
      goto 3
	
	call hybrd1 (rat_curve,n,x,fvec,Tol_int_10_6,info)
	call converg (conver_result, info)	
	If(conver_result == 0 .and. parint1000 == 0)then
          yb = x(1) 
          If (Id1==0)then  
              If (yb >= yref(j))then
                  yb = yref(j)                  
              elseif (yb < ydry(j))then
                  yb = ydry(j) 
              endif
          else if (Id1==1)then   
              If (yb >= Max_Head_rating_curve(R)-Tol_int_10_4)then
                  !yb = yref(j)
                  yb = Max_Head_rating_curve(R)-Tol_int_10_4
              elseif (yb < ydry(j))then
                  yb = ydry(j) 
              endif    
          else
              write(98,*),'Id1 n.e. 0,1'
		    write(98,*),'Error in Subr. Rating_curve'
		    call endprog      
          endif
      ElseIf(conver_result == 1)then
          temp_id = ''
          call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
          write(99,*),'No converg. Subr. Rating_curve. Node =',temp_id
          yb = y11
          If (Id1==0)then  
              If (yb >= yref(j))then
                  yb = yref(j)                  
              elseif (yb < ydry(j))then
                  yb = ydry(j) 
              endif
          else if (Id1==1)then   
              If (yb >= Max_Head_rating_curve(R)-Tol_int_10_4)then
                  !yb = yref(j)
                  yb = Max_Head_rating_curve(R)-Tol_int_10_4
              elseif (yb < ydry(j))then
                  yb = ydry(j) 
              endif    
          else
              write(98,*),'Id1 n.e. 0,1'
		    write(98,*),'Error in Subr. Rating_curve'
		    call endprog      
          endif
      else
          write(98,*),'conver_result n.e. 0,1'
		write(98,*),'Error in Subr. Rating_curve'
		call endprog
      endif      
      
	If (yb > Max_Head_rating_curve(R))then
	      temp_id = ''
          call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes	
          write(98,*),'NO enough data in rating curve. node',temp_id
          write(98,*),'Please extend rating curve for larger heads'
          call endprog	
      endif      
      
      !3   If (yb >= yref(j))then
      !        yb = yref(j)
      !        !yb = Max_Head_rating_curve(R)-Tol_int_10_4
      !    elseif (yb < ydry(j))then
      !        yb = ydry(j) 
      !    endif      
      
 3     call get_Q_from_rat_curve(R,yb,Qb)	
      
       p1 = abs(Drop(R,1)) !weir height 
      !To limit the withdrawal flow	
      if (A11 > area_weir(R))then
           if (dabs(Qb) > (1d0/DT_GLOBAL)*dx(j)*(A11-area_weir(R)))then
              Qb = (1d0/DT_GLOBAL)*dx(j)*(A11-area_weir(R))
              yb = p1
           endif
      endif
      
	if (Nodetype(R,1) == 2)then !outflowing
		    Qb = -Qb	
	elseif (Nodetype(R,1) == 1)then !inflowing
		    Qb = Qb	
	else
	      temp_id = ''
		    call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
		    write(98,*),'Nodetype(R,1) n.e. 1,2'
		    write(98,*),'Error in Subr. rat_curve. Node (SWMM) =',temp_id
		    call endprog
      endif	      
     

	!Flux computation
10	call Area_from_H(j,yb,Ab,Tb,RH,Idb)
 	call Pressure_Pho(j,yb,P_phob,Idb) 
	F11 = Qb; F12 = Qb**2d0/Ab + P_phob
	if (Nodetype(R,1) == 2)then !outflowing
		Abound(j,1) = Ab; Qbound(j,1) = Qb
		ybound(j,1) = yb; Idflow_bound(j,1) = Idb
		Fupst(j,1) = F11; Fupst(j,2) = F12		
	elseif (Nodetype(R,1) == 1)then !inflowing
		Abound(j,2) = Ab; Qbound(j,2) = Qb
		ybound(j,2) = yb; Idflow_bound(j,2) = Idb
		Fdownst(j,1) = F11;	Fdownst(j,2) = F12		
	else
	  temp_id = ''
		call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
		write(98,*),'Nodetype(R,1) n.e. 1,2'
		write(98,*),'Error in Subr. rat_curve. Node (SWMM) =',temp_id
		call endprog
	Endif 
	
	!Water level over the weir or spillway crest 
	!if (yb > Drop(R,1))then !outflowing
	!  yres_jun_old(R) = yb - Drop(R,1)
	!else
          !yres_jun_old(R) = yb
          yres_jun_old(R) = y11 !yb
	!endif		 			
	return	
      end

	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	subroutine rat_curve(n, x, fvec, iflag )
	use common_module
	implicit none	
	integer j,n,iflag,IA,IDfbIA1,sum,R
	double precision fvec(n),x(n),Qb,ub1,temp8
	double precision AIA,TsIA,RHIA,u11,Q11,y11,A11,cb1,c11
      double precision E11
	character*25 temp_id
      integer :: seed
	double precision ran_number
	
	parint1000 = 0
	A11 = param1; c11 = param2; Q11 = param3; y11 = param4  
	IA = parint1; IDfbIA1 = parint3
	sum = parint4; R = parint5
	u11 = Q11/A11
      E11 = y11 + (1.0/2.0*g)*u11**2.0
      j = NodeID(R,1)
	If (x(1) < b2_min(IA))then	
		x(1) = b2_min(IA)
		parint1000 = 1 
	endif 
	
	If (x(1) >= Max_Head_rating_curve(R))then
          x(1) = Max_Head_rating_curve(R)
          parint1000 = 1
	endif
	
	 If (ISNAN(x(1)))then 	          
          call random_seed(seed)  
          call random_number(ran_number)
          if(sum == 0)then
                  x(1) = ran_number*yref(j)   !y11  
          elseif(sum == 2)then
                  x(1) = yref(j)  + ran_number*yref(j)      
          elseif (sum == 1)then
                  x(1) = ran_number*yref(j)   !y11  
          else 
                  write(98,*),'sum == 2 .ne. 0,2'
		        write(98,*),'subrout. dropshaft'
		        call endprog
          endif		
          parint1000 = 1
	endif	
	
	call Area_from_H(IA,x(1),AIA,TsIA,RHIA,IDfbIA1)
	cb1 = sqrt(g*AIA/TsIA)	
	
	!Compute flow discharge from rating curve
	call get_Q_from_rat_curve(R,x(1),Qb)
	if (Nodetype(R,1) == 2)then !outflowing
		Qb = -Qb	
	elseif (Nodetype(R,1) == 1)then !inflowing
		Qb = Qb	
	else
	  temp_id = ''
		call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
		write(98,*),'Nodetype(R,1) n.e. 1,2'
		write(98,*),'Error in Subr. rat_curve. Node (SWMM) =',temp_id
		call endprog
	endif
	
	ub1 = Qb/AIA
	if (Nodetype(R,1) == 2)then !outflowing
		If (sum == 0) then
			fvec(1) = ub1 - u11 - (cb1 + c11)*(AIA - A11)/(AIA + A11)
              !USE Energy equation instead
              !Work here
              !fvec(1) = x(1) + (1.0/2.0*g)*ub1**2.0 - E11 !Energy equation    
              
          elseif (sum == 2) then			
			fvec(1) = ub1 - pc1(IA)*LOG (AIA)-(u11 - pc1(IA)*LOG(A11))
              !fvec(1) = x(1) + (1.0/2.0*g)*ub1**2.0 - E11 !Energy equation                   
          elseif (sum == 1) then			
              fvec(1) = x(1) - y11 
          else    
			temp_id = ''
		    call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
			write(98,*),'sum n.e. 0,1,2'
			write(98,*),'Error in Subr. rat_curve. Node (SWMM) =',temp_id
			call endprog		
		endif		
	elseif (Nodetype(R,1) == 1)then !inflowing
		If (sum == 0) then
			fvec(1) = ub1 - u11 + (cb1 + c11)*(AIA - A11)/(AIA + A11)	 
              !fvec(1) = x(1) + (1.0/2.0*g)*ub1**2.0 - E11 !Energy equation  
          elseif (sum == 2) then
			fvec(1) = ub1 + pc1(IA)*LOG (AIA) - (u11 + pc1(IA)*LOG(A11))
              !fvec(1) = x(1) + (1.0/2.0*g)*ub1**2.0 - E11 !Energy equation     
          elseif (sum == 1) then			
              fvec(1) = x(1) - y11
		else
		    temp_id = ''
		    call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
			write(98,*),'sum n.e. 0,1,2'
			write(98,*),'Error in Subr. rat_curve. Node (SWMM) =',temp_id
			call endprog		
		endif
	else
	  write(98,*),'Nodetype(R,1) n.e. 1,2'
		write(98,*),'Error in Subr. rat_curve'
		call endprog				
	endif
	return
	end	