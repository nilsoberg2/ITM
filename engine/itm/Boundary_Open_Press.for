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

	subroutine Boundary_Open_Press(R)
	!This routine is used for determining if a junction/reservoir/dropshaft boundary is surcharged or not. 
	use common_module 
	implicit none	
	integer i,j,R,k,S,IDfb_new,IDt1,sum,IDf11
	double precision dr,tol_const_state	
	double precision Ref_level
	
	If(BCnode(R)==7.or.BCnode(R)==20.or.BCnode(R)==4)then !Junction/Reserv/Dropsh
	
	      !For junctions, we have to check if surrounding cells to the junction are depressurized
	      !Also we have to check if the jucntion is ventilated. If it is ventilated, we could use 
	      !Yser_jun_old(R) as a waw of checking depressurization. Otherwise this can not be used.
	      
	      if (BCnode(R) == 20)then
	          Idt1 = 0
	          goto 5
	      endif  
	      
	      if (open_closed_bound(R) == 0)then
	          IDt1 = 0 
	          goto 5
	      endif    
	      
	      sum = 0
	      IDt1 = 1 !To initialize
	      do i = 1, NodeNS(R) 
	          j = NodeID(R,i)
	          if (Nodetype(R,i) == 2)then !outflowing
	              sum = sum + IDFlow(j,3)
                    IDf1(i) = IDFlow(j,3)
	          Elseif (Nodetype(R,i) == 1)then !inflowing
	              sum = sum + IdFlow(j,Nx(j)-2)
                    IDf1(i) = IdFlow(j,Nx(j)-2)
                else
	              write(98,*),'Nodetype UNKNOWN'
	              write(98,*),'Boundary_Open_Press'
	              write(99,*),'Nodetype UNKNOWN'
	              write(99,*),'Boundary_Open_Press'
		            call endprog
	          Endif
            enddo
              
          if (sum < NodeNS(R))then
              IDt1 = 0
          endif            
          
5         do i = 1, NodeNS(R)      
              dr = Drop(R,i) 
              j = NodeID(R,i)
              tol_const_state = Tol_int_10_8*yref(j) !0d0 !Tol_int_10_12	 
              Ref_level = yres_jun_old(R) - (dr + yref(j))
                
              If(Ref_level > tol_const_state) then
		            IDfb_new = 1
              else
		            IDfb_new = 0	!To check if this actually is depressurization
		            if (IDt1 == 1)then
		                IDfb_new = 1  
		            endif
              endif	                    
    	      
              if (Nodetype(R,i) == 2)then !outflowing
	              IDFlow(j,2) = IDfb_new
              Elseif (Nodetype(R,i) == 1)then !inflowing
	              IdFlow(j,Nx(j)-1) = IDfb_new
              else
	              write(98,*),'Nodetype UNKNOWN. Subr. Boundary_Open_Press'
		            call endprog
              Endif
          enddo   
      elseIf(BCnode(R) == 10 .or. BCnode(R) == 11)then !constant boundary
		    do i = 1, NodeNS(R) 
 	          j = NodeID(R,i)
	          if (Nodetype(R,i) == 2)then !outflowing
	             IDFlow(j,2) = IDFlow(j,3)
	          Elseif (Nodetype(R,i) == 1)then !inflowing
	             IdFlow(j,Nx(j)-1) = IdFlow(j,Nx(j)-2)
                else
	              write(98,*),'Nodetype UNKNOWN. Subr. Boundary_Open_Press'
		            call endprog
	          Endif
            enddo
        elseIf(BCnode(R) == 24)then !Junction with two pipes of same diameter
		    do i = 1, NodeNS(R) 
 	          j = NodeID(R,i) 	          
 	          if (i == 1)then
 	              k = 2 	              
 	          else
 	              k = 1    
 	          endif
 	          S = NodeID(R,k) 	          
	          if (Nodetype(R,i) == 2)then !outflowing
	              if (Nodetype(R,k) == 2)then !outflowing
	                  IDFlow(j,2) = IDFlow(S,3)
	              Else !inflowing
	                  IDFlow(j,2) = IdFlow(S,Nx(S)-2)                    
	              Endif	          
	          Elseif (Nodetype(R,i) == 1)then !inflowing
	              if (Nodetype(R,k) == 2)then !outflowing
	                  IdFlow(j,Nx(j)-1) = IDFlow(S,3)
	              Else !inflowing
	                  IdFlow(j,Nx(j)-1) = IdFlow(S,Nx(S)-2) 
	              Endif
                else
	              write(98,*),'Nodetype UNKNOWN. Subr. Boundary_Open_Press'
		            call endprog
	          Endif
            enddo
      elseIf(BCnode(R) == 40)then !Gate boundary (No ventilation)
            sum = 0
	      IDt1 = 1 !To initialize
	      do i = 1, NodeNS(R) 
	          j = NodeID(R,i)
	          if (Nodetype(R,i) == 2)then !outflowing
	              sum = sum + IDFlow(j,3)
	          Elseif (Nodetype(R,i) == 1)then !inflowing
	              sum = sum + IdFlow(j,Nx(j)-2)
                else
	              write(98,*),'Nodetype UNKNOWN'
	              write(98,*),'Boundary_Open_Press'
	              write(99,*),'Nodetype UNKNOWN'
	              write(99,*),'Boundary_Open_Press'
		            call endprog
	          Endif
	          if (sum < i)then
	              IDt1 = 0
	              goto 18
	          endif
	      enddo
      
   18         do i = 1, NodeNS(R) 
 	          j = NodeID(R,i) 	          
	          if (Nodetype(R,i) == 2)then !outflowing
	              IdFlow(j,2) = IDt1
	          Elseif (Nodetype(R,i) == 1)then !inflowing
	              IdFlow(j,Nx(j)-1) = IDt1
                else
	              write(98,*),'Nodetype UNKNOWN. Subr. Boundary_Open_Press'
		            call endprog
	          Endif
            enddo
      
      
		    !do i = 1, NodeNS(R) 
 	      !    j = NodeID(R,i) 	          
 	      !    if (i == 1)then
 	      !        k = 2 	              
 	      !    else
 	      !        k = 1    
 	      !    endif
 	      !    S = NodeID(R,k)
 	          
	      !    if (Nodetype(R,i) == 2)then !outflowing
	      !        if (Nodetype(R,k) == 2)then !outflowing
	      !            IDFlow(j,2) = IDFlow(S,3)
	      !        Else !inflowing
	      !            IDFlow(j,2) = IdFlow(S,Nx(S)-2)                    
	      !        Endif	          
	      !    Elseif (Nodetype(R,i) == 1)then !inflowing
	      !        if (Nodetype(R,k) == 2)then !outflowing
	      !            IdFlow(j,Nx(j)-1) = IDFlow(S,3)
	      !        Else !inflowing
	      !            IdFlow(j,Nx(j)-1) = IdFlow(S,Nx(S)-2)                    
	      !        Endif
            !    else
	      !        write(98,*),'Nodetype UNKNOWN. Subr. Boundary_Open_Press'
		    !        call endprog
	      !    Endif
            !enddo
      elseIf(BCnode(R) == 30 .or. BCnode(R) == 41)then 
      !Rating curve boundary (always ventilated)
      !Gate with one single pipe (always ventilated)
	      do i = 1, NodeNS(R) 
 	          j = NodeID(R,i)
	          if (Nodetype(R,i) == 2)then !outflowing
	              IDFlow(j,2) = 0
	          Elseif (Nodetype(R,i) == 1)then !inflowing
	              IdFlow(j,Nx(j)-1) = 0
                else
	              write(98,*),'Nodetype UNKNOWN. Subr. Boundary_Open_Press'
		            call endprog
	          Endif
            enddo
      else
	      write(98,*),'Boundary condition type not supported'
		    write(98,*),'Subr. boundaries'
		    call endprog
      endif
      end subroutine