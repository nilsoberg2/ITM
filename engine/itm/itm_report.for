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

	Subroutine report_all(currentStep)
	!Purpose: To store average data of water depths and flow discharges
	use common_module
	implicit none
	double precision linkDepths(1000)
	double precision linkFlows(1000)
	double precision nodeDepths(1000)
	double precision linkVelocities(1000)
	double precision linkFroudes(1000)
	double precision currentStep
	double precision sum_temp1,sum_temp2,sum_temp3,v
	double precision sysVolErrPct, sysVolErrDiff
	integer i,j,k,R
	integer numCells
	integer cellNum
	double precision AA, Ts, RH,VOL_OVERFLOW

	! don't want to pass MaxNumPlotCells directly to report_pipe because
	! report_pipe modifies that parameter, which since Fortran is pass-by
	! reference, modifies MaxNumPlotCells.  So we make a copy here.
	numCells = MaxNumPlotCells	

	do j = 1,NR	
		cellNum = 1 + int(Nx(j)/2)	
		linkFlows(j) = Q0(j,cellNum)
		linkDepths(j) = h0(j,cellNum)
		linkVelocities(j) = Q0(j,cellNum)/A0(j,cellNum)
		call Area_from_H(j,h0(j,cellNum),AA,Ts,RH,IDflow(j,cellNum))
		linkFroudes(j) = abs(linkVelocities(j)) /
     &		sqrt(g * A0(j,cellNum)/Ts)
		call report_pipe(j, numCells, currentStep)
	enddo

	!water depths at nodes
	do R=1,Nnodes
        nodeDepths(R) = yres_jun_old(R)			
		if (BCnode(R) == 30)then !Rating curve boundary
		    j = NodeID(R,1) !Pipe connected to rating curve boundary
		    if(Nodetype(R,1) == 1)then !inflowing
			    nodeDepths(R) = h0(j,Nx(j)-2)+Drop(R,1) + 5d-1*S0(j)*dx(j)
			    !Drop height must be always positive except' 
			    !for a rating curve boundary'
		    elseif(Nodetype(R,1) == 2)then !outflowing					    
		        !if (h0(j,3) < S0(j)*dx(j))then
			        nodeDepths(R) = h0(j,3)+Drop(R,1)
			    !else
			        !nodeDepths(R) = h0(j,3)+Drop(R,1) - 5d-1*S0(j)*dx(j)
			    !endif
		    else 
		        write(98,*),'Nodetype .ne. 1,2' 
				write(98,*),'Subroutine itm_report'
				call endprog
				GLOBAL_STATUS_FLAG = 1
				return 
	      endif		    			
		endif					
	enddo

	!For determining series times of volume overflows at each dropshaft.
	!vol_overflow = 0d0
	!do R=1,Nnodes
	!	if (BCnode(R) == 7 .or. BCnode(R) == 4)then
	!		if (yres_jun_old(R) > height_jun(R))then
	!			vol_overflow = (yres_jun_old(r)-height_jun(R))*
      !&				Ares_junct(R)
	!		else
	!			vol_overflow = 0d0
	!		endif
	!	else
	!		vol_overflow = 0d0
	!	endif
	!	V_over(R) = V_over(R) + vol_overflow
	!end do	
	!write(101,996),T_GLOBAL,(V_over(R), R = 1,Nnodes)
 996	format(f20.3,1000f20.1)
 
      sysVolErrPct = Error_Volume !Error volume in percentage   
      !Error volume in m3   
      sysVolErrDiff = Initial_volume_stored+Vol_inflows-Vol_lost_system
     & - Volume_stored_current_step
      

	!average flow in the node doesn't make much sense.
	! Save the node values and upstream/downstream pipe values into the 
	! SWMM data file.  Data for the ITM file gets stored elewhere
	call itm_report_step(currentStep, 1000, linkFlows, linkDepths,
     &					 linkVelocities, linkFroudes, nodeDepths,
     &					 sysVolErrPct, sysVolErrDiff)
	end subroutine
	
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	! Report the ITM values for a given pipe.
	Subroutine report_pipe (j, numCells, currentStep)  !j is pipe ID
	!Purpose: To store average data of water depths and flow discharges
	use common_module
	implicit none
	integer numCells							! number of cells saved (less than or equal to MaxNumPlotCells)
	double precision plotLinkStation(numCells)	! x-station
	double precision plotLinkFlows(numCells)	! flow at x-station for given index
	double precision plotLinkDepths(numCells)	! depth at x-station for given index
	double precision plotLinkEnergy(numCells)
	double precision currentStep
	double precision sum_temp1,sum_temp2,sum_temp3,v
	double precision x1p,x2p,dxp,con1,con2
	integer i,j,k,p,R,lim1,lim2
			
	If (numCells > MaxNumPlotCells)then
		!write(98,*),'# of cells to plot can not exceed MaxNumPlotCells'
		!write(98,*),'Increase MaxNumPlotCells or decrease numCells to plot'
		!call endprog
		numCells = MaxNumPlotCells
	endif

	If (numCells >= Nx(j)-4)then
	      numCells = Nx(j)-4
		    do i = 3,Nx(j)-2
			        plotLinkStation(i-2) = (i-2.5)*dx(j)
			        plotLinkDepths(i-2) = h0(j,i) !note that elevation was not added
			        plotLinkFlows(i-2) = Q0(j,i)
			        plotLinkEnergy(i-2) = h0(j,i)
			        !v = abs(Q0(j,i)/A0(j,i))
			        !plotLinkEnergy(i-2) = h0(j,i)+v*v/(2d0*g)
		    enddo
	else
		    dxp = Length(j)/numCells
		    do k = 1,numCells
			    x1p = (k-1)*dxp
			    x2p = k*dxp			
			    con1 = x1p/dx(j)
			    con2 = x2p/dx(j)
			    if (int(con1) < 0)then
				    lim1 = 1
			    else
				    lim1 = 1+int(con1)
			    endif
    			
			    if (int(con2) >= Nx(j)-4)then
				    lim2 = Nx(j)-4
			    else
				    lim2 = 1+int(con2)
			    endif
			    sum_temp1 = 0d0; sum_temp2 = 0d0
			    sum_temp3 = 0d0

			    !For the first and last points of plotting of each pipe, the first 
			    !and last (respectively) values of the actual cells are used
			    !This is done to improve the quality of the plots near boundaries
			    if(lim1 == 1 .or. lim2 == Nx(j)-4)then
				    if(lim1 == 1)then
					    plotLinkStation(k) = 0.5*dx(j)
                          If (BCnode(Node1(j)) == 24)then
                              plotLinkDepths(k) = h0(j,5)
					        plotLinkFlows(k) = Q0(j,5)
					        plotLinkEnergy(k) = h0(j,5)  
                          else
                              plotLinkDepths(k) = h0(j,3)
					        plotLinkFlows(k) = Q0(j,3)
					        plotLinkEnergy(k) = h0(j,3)  
                          endif
       !                   plotLinkDepths(k) = h0(j,4)
					  !  plotLinkFlows(k) = Q0(j,4)
					  !  plotLinkEnergy(k) = h0(j,4)
					  !  !v = abs(Q0(j,3)/A0(j,3))		
					  !  !write(99,*), 'velocity1',v		
					  !  !plotLinkEnergy(k) = h0(j,3)+ v*v/(2d0*g)
				    else
					    plotLinkStation(k) = (Nx(j)-4 - 0.5)*dx(j)
					    !plotLinkDepths(k) = h0(j,Nx(j)-2)
					    !plotLinkFlows(k) = Q0(j,Nx(j)-2)
					    !plotLinkEnergy(k) = h0(j,Nx(j)-2)
                          
                          If (BCnode(Node2(j)) == 24)then
                              plotLinkDepths(k) = h0(j,Nx(j)-4)
					        plotLinkFlows(k) = Q0(j,Nx(j)-4)
					        plotLinkEnergy(k) = h0(j,Nx(j)-4)
                          else
                              plotLinkDepths(k) = h0(j,Nx(j)-2)
					        plotLinkFlows(k) = Q0(j,Nx(j)-2)
					        plotLinkEnergy(k) = h0(j,Nx(j)-2)
                          endif
       !                   
					  !  !v = abs(Q0(j,Nx(j)-2)/A0(j,Nx(j)-2))
					  !  !write(99,*), 'velocity2',v		
					  !  !plotLinkEnergy(k) = h0(j,Nx(j)-2)+ v*v/(2d0*g)
				    endif
				    goto 20
			    endif

			    do i = lim1,lim2
				    p = i+2
				    sum_temp1 = sum_temp1 + Q0(j,p)		
				    sum_temp2 = sum_temp2 + h0(j,p)				
				    sum_temp3 = sum_temp3 + h0(j,p)
    !                sum_temp3 = sum_temp3 + h0(j,p)+(Q0(j,p)/
    ! &				A0(j,p))**2d0/(2d0*g)
			    enddo
10			    plotLinkStation(k) = (x1p+x2p)/2d0
			    plotLinkDepths(k) = sum_temp2/(1 + lim2-lim1)
			    plotLinkFlows(k) = sum_temp1/(1 + lim2-lim1)
			    plotLinkEnergy(k) = sum_temp3/(1 + lim2-lim1)
20			    continue
		    enddo
	endif		
	do i = 1,numCells
		if (plotLinkDepths(i) < 0d0 .or. fully_pressuri(j) == 2) then
			plotLinkDepths(i) = yref(j)
		endif
	enddo
	call itm_save_link_data(currentStep, j, numCells, plotLinkStation,
     &				plotLinkFlows, plotLinkDepths, plotLinkEnergy)
	end subroutine
