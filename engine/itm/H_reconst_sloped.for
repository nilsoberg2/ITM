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
      subroutine H_reconst_sloped(j,A,Y,code_error,flow_type)      
	use common_module
      implicit none  
      integer iter,j,code_error,flow_type,Num_itera,temp_id
	integer n, iflag
	integer conver_result,info
	double precision fvec(1), x(1),x2(1)
	double precision A,Atemp,Atemp100,Y,teta
	double precision const1,b,ALF,FF,DFF,DELY,yrel,tol_H_area
	double precision temp1,temp2,temp3,temp4,temp44
	double precision temp5,temp6,temp7,temp77,x1_old
	double precision dz,teta1,teta2,Vol,y_downs,dy,dxtemp,vol1,dx_free
	integer :: seed
	double precision ran_number
	
      
      Num_itera = 200 !Number of iterations
	tol_H_area = Tol_int_10_10
	If (ISNAN(A))then 
		    write(98,*),'NaN is found in routine H_reconst_sloped' 
		    write(98,*),'Pipe =',j,'code_error = ',code_error		
		    write(98,*),'flow_type',flow_type		
		    call endprog
	endif 
      
      teta = (48d0*A/d(j)**2d0)**(1d0/3d0) !Guess obtained using series expansions
      x(1) = d(j)/2d0*(1d0-COS(teta/2d0))  !Choose a better guess
 	ALF = abs(ATan(S0(j)))
	b = 1d0/Cos(ALF)*(d(j)/2d0)
      dz = abs(S0(j)*dx(j))      
           
	if(flow_type == 0)then
	        if(A < 0.05*Aref(j))then
			        tol_H_area = Tol_int_10_8   !This was Tol_int_10_10
		    else if(A < 0.2*Aref(j))then
			        tol_H_area = Tol_int_10_8			        
		    else if(A < 0.7*Aref(j))then
			        tol_H_area = Tol_int_10_9
	        else
			        tol_H_area = Tol_int_10_10
              endif
					    
              if (A <= A_cell_dry_sloped(j))then !Dry bed conditions
                  x2(1) = x(1) !To start iteration
                  const1 = b*(d(j)/2d0)/abs(S0(j))
                  do iter = 1,Num_itera	
                      dz = x2(1)  
		            if (x2(1) < 0d0)then		            
		                call random_seed(seed)  
                        call random_number(ran_number)
		                x2(1) = ran_number*b2_min(j)
		                !x2(1) = 0.1*b2_min(j)
	                endif
	                if (x2(1) >= abs(S0(j)*dx(j)))then  
		                x2(1) = abs(S0(j)*dx(j))	            
	                endif
                      Call Volume_sloped(j,x2(1),Vol)
	                FF = Vol - A*dx(j)	          
    	          
	                temp1 = (-b+x2(1))*Sqrt((2d0*b-x2(1))*x2(1)/b**2d0) 
	                temp2 = b*ACos(1d0-x2(1)/b)
	                temp3 = b
	                temp44 = const1*(temp1+temp2)/temp3	          
	                DFF = temp44
    	          			
				    DELY = FF/DFF				
				    temp1 = x2(1) - DELY/2d0
				    x2(1) = x2(1) - DELY
				   										
				    IF(abs(DELY)<tol_H_area*temp1)goto 10			
				    if(iter == Num_itera)then			
					        write(98,*),'Error in finding x2(1)[A_cell_dry_sloped(j)] 	
     &				        Subroutine H_from_Area_sloped_103'	
					        write(98,*),'flow_type,d(j),A,Aref(j),code_error',
     &					    flow_type,d(j),A,Aref(j),code_error	
					        CALL endprog						
				    endif													
 		        enddo
 10		        Y = x2(1) - 1d0/2d0*abs(S0(j)*dx(j))		    
		    elseif (A <= A_open_sloped_pipe(j))then
                  dz = abs(S0(j)*dx(j))
                  const1 = b*(d(j)/2d0)/abs(S0(j))      
                  do iter = 1,Num_itera
                      !This doesn't work here. The technique below works when the function change signs. 
                      if (x(1) < 0d0 )then   
                          call random_seed(seed)  
                          call random_number(ran_number)
                          x(1) = ran_number*b2_min(j)
                      endif
	                !if (x(1) < 0d0 )then
		            !    x(1) = 0d0
	                !endif
	                         	            
	                if (x(1)+dz >= yref(j))then  
		                x(1) = yref(j) - dz		            
	                endif  
	                x2(1) = x(1)+dz 
        	                   
	                Call Volume_sloped(j,x2(1),Vol)
	                FF = Vol - A*dx(j)
	                                                          
                        temp1 = (-b+x(1))*Sqrt((2d0*b-x(1))*x(1)/b**2d0) 
	                temp2 = b*ACos(1d0-x(1)/b)
	                temp3 = b
	                temp4 = const1*(temp1+temp2)/temp3
	                temp1 = (-b+x2(1))*Sqrt((2d0*b-x2(1))*x2(1)/b**2d0) 
	                temp2 = b*ACos(1d0-x2(1)/b)
	                temp3 = b
	                temp44 = const1*(temp1+temp2)/temp3	          
	                DFF = temp44-temp4
        	          			
				    DELY = FF/DFF				
				    temp1 = x(1) - DELY/2d0 
				    x1_old = x(1)
				    x(1) = x(1) - DELY
				        
				    if (iter > 100 .and. x(1) < 0d0) then !This works beutifully when the function changes signs 
				        x(1) = (0d0 + x1_old)/2d0
				        goto 20		        
				        !write(99,*),'i,x(1)',iter,x(1)	
				    endif				         
        														
                      IF(abs(DELY)<tol_H_area*temp1)goto 20			
				        if(iter == Num_itera)then
                              call itm_get_swmm_id(1, j, temp_id) ! 1 for pipes
		            write(99,*),'pipe ID = ',temp_id
                      write(98,*),'pipe ID = ',temp_id
                  
					        write(98,*),'Error in finding x(1) [A <=A_open_sloped_pipe]
     &				        Subroutine H_reconst_sloped_104'	
				            write(98,*),'flow_type,d(j),A,Aref(j),code_error',
     &					    flow_type,d(j),A,Aref(j),code_error
                      write(99,*),'flow_type,d(j),A,Aref(j),code_error',
     &					    flow_type,d(j),A,Aref(j),code_error
                      write(99,*),'A_cell_dry_sloped,A_open_sloped_pipe'
                      write(99,*),A_cell_dry_sloped
                      write(99,*),A_open_sloped_pipe(j)
					        CALL endprog						
				        endif													
 		            enddo
20		            Y = x(1) + dz/2d0
		    elseif (A < Aref(j))then		        
	              x(1) = yref(j) - abs(S0(j)*dx(j))/2d0 !Iteration guess for x(1)
	              const1 = b*(d(j)/2d0)/abs(S0(j))
	              	              
		            do iter = 1,Num_itera		                
		                if (x(1) <= yref(j) - abs(S0(j)*dx(j)))then
		                    x(1) = yref(j) - abs(S0(j)*dx(j))
	                  endif	
	                  
	                  if (x(1) >= yref(j))then  
		                    x(1) = yref(j)
	                  endif
	                  
	                  x2(1) = yref(j)   !Note that X2 is constant
	                  y_downs = x(1) + abs(S0(j)*dx(j))	                  
	                  dy = y_downs - yref(j)
	                  DXTemp =  dy/abs(S0(j))	                   
	                  Dx_free = dx(j)-DXTemp		            
		                dz = abs(S0(j)*Dx_free)		                	                
	                  Call Volume_sloped(j,y_downs,Vol)
	                  FF = Vol - A*dx(j)
                                        
	                  temp1 = (-b+x(1))*Sqrt((2d0*b-x(1))*x(1)/b**2d0) 
	                  temp2 = b*ACos(1d0-x(1)/b)
	                  temp3 = b
	                  temp4 = const1*(temp1+temp2)/temp3
	                  
	                  !Note that x2(1) is constant (yref(j)) and hence its derivative is zero. 
	                  !DFF = temp44-temp4 !Derivative of the fully-open channel region 
	                  DFF = -temp4 !Derivative of the fully-open channel region 
	                  DFF = DFF + Aref(j)*1d0/abs(S0(j)) !(Derivative of the fully pressurized region)
				        DELY = FF/DFF				
				        temp1 = x(1) - DELY/2d0 
				        x(1) = x(1) - DELY		
        														
				        IF(abs(DELY)<tol_H_area*temp1)goto 30			
				        if(iter == Num_itera)then			
					        write(98,*),'Error in finding x(1) [A < Aref(j)]	
     &				            Subr.H_from_Area_sloped_105'	
					        write(98,*),'flow_type,d(j),A,Aref(j),code_error',
     &					        flow_type,d(j),A,Aref(j),code_error	
					        CALL endprog						
				        endif													
 		            enddo
30		            Y = x(1) + abs(S0(j)*Dx(j))/2d0
		    elseif(A >= Aref(j))then
			        Y = Yref(j)
			else
			        write(98,*),'Unknown condition. Routine H_reconst_sloped'
			        write(99,*),'Unknown condition. Routine H_reconst_sloped'
			        call endprog
		    endif
	elseif(flow_type == 1)then
		    Y = yref(j) + pc1(j)*pc1(j)/(g*Aref(j))*(A - Aref(j)) 	
	else
		    write(98,*), 'Flow type is not defined',flow_type
		    write(98,*),'Subroutine H_reconst_sloped' 
		    write(98,*),code_error
		    CALL endprog	
	endif	
	return		
      end