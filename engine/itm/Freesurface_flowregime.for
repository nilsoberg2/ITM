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

	subroutine Freesurface_flowregime(R,j,dr,IDf11,Q11,y11,A11,
     &flow_regIA1,flowcaseIA1,nodetype1,cond_mixed,sum)
      !&flow_regIA1,flowcaseIA1,nodetype1,cond_mixed,sum,yw0,Aw0,Qw0,Idf0)
     
	!This routine is used for determining if a junction/reservoir/dropshaft boundary is surcharged or not. 
	use common_module 
	implicit none	
	integer j,i,R,sum,flow_regIA1,flowcaseIA1,nodetype1
	integer IDf11,IDfb_old,IDfb_new,cond_mixed,node_pressur,IDfb_reser
	double precision dr,tol_const_state,tempIA
	double precision ScIA,yb,A,Q,Ab,dh,Ts,RH,Ycrit,Q_allow
	double precision Ref_level,tol_level
	double precision hIA,hIC,vIA,Q11,y11,A11,hIA_old,ytemp,Slo
	double precision F1,d1,A1_temp,Ynormal,Yconjugate,area,y
	double precision Ener_super,E1,Wpred,Pw0
	double precision FF1,FF2,yL,YR,QL,QR
      double precision Eres, yc_res,yc_11,yc_11_hager,Ac_temp,Qc_temp 
      double precision E11_infl,E11_outf,con1,temp1
	integer CODL,IDfb_reser_old
            
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!flowcase = 1 --> free outfall or normal flow
	!flowcase = 2 --> constant state	
      !cond_mixed  = 1 --> pipe is in open channel conditions and junction is pressurized. Orifice formula is used. 
      !cond_mixed  = 2      	
	
	IDfb_reser = 1	
      cond_mixed  = -100 !to initialize	
	flowcaseIA1 = -100  !to initialize
	flow_regIA1 = -100  !to initialize
	IDfb_new = -100 !to initialize	
	sum = -100  !to initialize	
	!#######################################################
	!#######################################################
	!If at old time step, flow is fully pressurized at a junction, and
	!pipes surrounding the node are pressurized (new time) and the water 
	!depth in the junction pond is below of any of the crowns of the pipes,
	!the water depths at this pond is set to the maximum crown depth and 
	!the flow at the junction remains pressurized
		           
      if (nodetype1 == 1)then !outflowing 
          IDfb_reser = IdFlow(j,Nx(j)-1)          
      elseif (nodetype1 == 2)then
          IDfb_reser = IdFlow(j,2) !This should be the case
      else 
          write(98,*),'Error defining nodetype1 in subr. 
     &    Freesurface_flowregime'
          call endprog   
      endif       
           
      !tol_const_state = Tol_int_10_8*d(j)	!This is compied from Boundary_Open_Press
      !Ref_level = yres_jun_old(R) - (dr + yref(j))
      !
      !If(Ref_level >= tol_const_state) then
      !        IDfb_reser = 1
      !else
      !        IDfb_reser = 0	
      !endif	
      
         !     if (yres_jun_old(R)-dr < Yref(j))then 
         !         IDfb_reser = 0
	        !else 
         !         IDfb_reser = 1
         !     Endif
	!#######################################################
 5	sum = IDf11+IDfb_reser	
 10	if (sum == 2)then		
		    IDfb_reser = 1; IDfb_new = 1 
		    flowcaseIA1 = 2
		    goto 20	
      elseIf (sum == 0)then
              IDfb_reser = 0; IDfb_new = 0 
              !if (BCnode(R) .ne. 20 .and. BCnode(R) .ne. 4)then  !Reservoirs and dropshafts don't need to have this condition
              if (BCnode(R) == 4 .or. BCnode(R) == 7)then  !Reservoirs don't need to have this condition
                  if (abs(dr) <= 0.02*d(j))then
                      flowcaseIA1 = 2 !constant state
                      goto 20   
                  endif
              endif 
              !WORK HERE Yc = 2/3*(Y11+v^2/(2g)). Use this as a metric.	  
              
              
		    IDfb_new = 0
              con1 = Q11*Q11/(2d0*g*A11*A11)
              E11_infl = y11 + 0.5*abs(s0(j)*dx(j))  + con1 !+ 0.5*abs(s0(j)*dx(j))
              E11_outf = y11 - 0.5*abs(s0(j)*dx(j)) + con1 !- 0.5*abs(s0(j)*dx(j))
              Eres = yres_jun_old(R) - dr
		    !Determining if the inflows are subcrit. or supercrit or pressurized	
		    if (nodetype1 == 1)then !inflowing 
			    !Determining critical slope to determine if the flow at 
			    !the incoming pipe is free outfall [normal flow or 
			    !critical flow (Free Surface)],  or Constant state 
			    ![free and pressurized flow)                            
                  
                  if (E11_infl > Eres)then !Flow is from pipe to pond      
                      yc_11 = 2d0/3d0*E11_infl
                      
                      if (Q11 > 0d0)then
                          yc_11_hager = (Q11/sqrt(g*d(j)))**0.5  !Hagger's formula Page 150
                      else
                          yc_11_hager = 0d0
                      endif
                      yc_11 = max(yc_11, yc_11_hager)
                      
                      if (yc_11 > (ycrit_max(j)+d(j))/2d0)then
                          yc_11 = (ycrit_max(j)+d(j))/2d0
                      endif
                      
                      if (E11_infl > Ecrit_max(j))then
                          yc_11 =     ycrit_max(j)
                          E11_infl =  Ecrit_max(j)                          
                      endif
                      call Area_from_H(j,yc_11,Ac_temp,Ts,RH,0)                  
                      Qc_temp = Ac_temp*sqrt(g*Ac_temp/Ts)                  
			        Q_allow = Qc_temp
                      if (Q_allow <= Qmin(j))then
				        Q_allow = Qmin(j)
			        elseif (Q_allow > Qcrit_maxIA(j))then
				        Q_allow = Qcrit_maxIA(j)
                      endif
                      ScIA = ((Q_allow*nm(j))/
     &                    (Ac_temp*RH**(2d0/3d0)))**2d0			
			        if (S0(j) <= ScIA)then !flow is subcritical	
				        flow_regIA1 = 1
                          ytemp = y11
			        elseif (S0(j) > ScIA)then !flow is supercritical
				        flow_regIA1 = 2
                      else
                          write(98,*),'S0(j) not>=<than ScIA. Subr. 
     &                    Freesurface_flowreg'
			            call endprog 
                      endif                                           
                      
                      if(flow_regIA1 == 2)then !flow is supercritical
			            !This is done only for supercritical flows because 
			            !for subcritical flow already was made before
				        if (Q_allow <= Qmin(j))then	
					        ytemp = d2_min(j)		
				        else 					        
					        if (Q_allow > Qnor_maxIA(j))then		
						        ytemp = (ycrit_max(j)+d(j))/2d0
					        else
						        d1 = Ynormal(j,Q_allow)
                                  call Area_from_H(j,d1,A1_temp,Ts,RH,0)                  
                                  !according to Hager (Wastewater Hydraulics, page 182, d2/d1 = 1.16*F1^0.85, where F1 = Froude Number) 
                                  F1 = Q_allow/A1_temp
                                  F1 = F1/sqrt(g*A1_temp/Ts)
                                  
                                   If (ISNAN(F1))then 		
                              write(98,*),'F1 is NaN'    
                              write(98,*),'Subr. Freesurface_flowreg'
			                call endprog  
                                  endif  
                                  
                                  if (F1 <= 1.0)then
                              write(98,*),'Flow is suposed to be' 
                              write(98,*),'supercritical but F1 < 1.0' 
                              write(98,*),'Subr. Freesurface_flowreg'
			                call endprog                                           
                                  endif
                                  
                                  ytemp = d1*(1.16*F1**0.85)  !Conjugate depth according to Hager's formula
                                  
                                  if (ytemp>(ycrit_max(j)+d(j))/2d0)then
                                      ytemp = (ycrit_max(j)+d(j))/2d0 
                                  endif                                  
                                  
						        !ytemp = Yconjugate(j,d1,Q_allow)
                                  !ytemp = d1
					        endif					
				        endif
                      endif
                      hIA = ytemp
                      hIC = yres_jun_old(r)								
			        !Including the slope of the inflowing pipe
			        !tempIA = hIA +dr + 0.5*S0(j)*dx(j)+vIA*vIA/(2.0*g) -hIC 
			        tempIA = hIA +dr -hIC 
			        !The slope of the incoming pipe is considered
			        if (tempIA > Tol_int_10_4*d(j))then	  	    
				        flowcaseIA1 = 1 
                          goto 20
			        else
				        flowcaseIA1 = 2
				        goto 20
                      endif    
                  elseif (E11_infl <= Eres)then !Flow is from pond to pipe
                      flowcaseIA1 = 2
                      goto 20
                  else
			        write(98,*),'E11 not>=<than Eres. Subr. Freesurface_flowreg'
			        call endprog    
                  endif   
              elseif (nodetype1 == 2)then !outflowing                           
			    if (E11_outf < Eres)then !flow is from pond to pipe     
                      yc_res = 2d0/3d0*Eres 
                      if (Eres > Ecrit_max(j))then
                          yc_res = ycrit_max(j)
                          Eres =  Ecrit_max(j)                          
                      endif                      
                      if (yc_res < 0d0)then 
                          yc_res = 0d0
                      endif
                      call Area_from_H(j,yc_res,Ac_temp,Ts,RH,0)                  
                      Qc_temp = Ac_temp*sqrt(g*Ac_temp/Ts)  
                      
			        Q_allow = Qc_temp
                      if (Q_allow <= Qmin(j))then
				        Q_allow = Qmin(j)
			        elseif (Q_allow > Qcrit_maxIA(j))then
				        Q_allow = Qcrit_maxIA(j)
                      endif
                      ScIA = ((Q_allow*nm(j))/
     &                    (Ac_temp*RH**(2d0/3d0)))**2d0			
			        if (S0(j) <= ScIA)then !flow is subcritical	
				        flow_regIA1 = 1
			        else !flow is supercritical
				        flow_regIA1 = 2
                      endif		
                      
                      if (flow_regIA1 ==1)then
                          flowcaseIA1 = 2
                          goto 20
                      endif
                      if (flow_regIA1 ==2)then                          
                          if (Eres > E11_outf)then
                              flow_regIA1 = 11							   !added
					        flowcaseIA1 = 1
                          else
                              flowcaseIA1 = 2
                              goto 20         
                          endif
                      else 
                          flowcaseIA1 = 2
                          goto 20
                      endif
                  elseif (E11_outf >= Eres)then !Flow is from pipe to pond
                      yc_11 = 2d0/3d0*E11_outf
                      if (E11_outf > Ecrit_max(j))then
                          yc_11 = ycrit_max(j)
                          E11_outf =  Ecrit_max(j)                          
                      endif                                            
                      
                      if (yc_11 > Eres)then !Flow is from pipe to pond
                          flowcaseIA1 = 1
					    !flow is critical at the node (negative flow)
					    flow_regIA1 = 10 
                      elseif (yc_11 <  Eres)then
                          flowcaseIA1 = 2
                          goto 20
                      else
                          write(98,*),'yc_11 not>=<than Eres. Subr.Free'
			            call endprog    
                      endif
                  else
			        write(98,*),'E11 not>=<than Eres. Subr. Freesurface_flowreg'
			        call endprog    
                  endif   
		    else
			    write(98,*),'Error defining nodetype1 in subr. 
     &			Freesurface_flowregime'
			    call endprog
		    endif
	elseIf (sum == 1)then
              if (IDf11 == 0 .and. IDfb_reser ==1)then
			    IDfb_new = 0 !this needs to be zero because flow is inetering to the pipe in free surface
			    cond_mixed = 1 !Shooting flow from reservoir to pipe
              elseif (IDf11 == 1 .and. IDfb_reser ==0)then
                  IDfb_new = 1 !modified
	            cond_mixed = 2 !Shooting flow from pipe to reservoir
              else
			    write(98,*),'IDf11, IDfb_reser .ne. 0,1'
			    write(98,*),'subr. Freesurface_flowregime'
			    call endprog
		    endif
      else
		    write(98,*),'sum .ne. 0,1,2'
		    write(98,*),'subr. Freesurface_flowregime'
		    call endprog
	endif

20    continue
	end subroutine