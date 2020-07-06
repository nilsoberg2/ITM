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

	SUBROUTINE ENDPROG_noerror
	USE COMMON_MODULE
	integer itm_close_output_file
	integer rescode
	
	rescode = itm_close_output_file()
      
      GLOBAL_STATUS_FLAG = 911

	WRITE(99,*) 'END OF RUN'
	WRITE(98,*) ' '

      END	SUBROUTINE ENDPROG_noerror
      
      
      
      

	SUBROUTINE ENDPROG
	USE COMMON_MODULE
	integer itm_close_output_file
	integer rescode
	
	rescode = itm_close_output_file()

cDEC$ IF DEFINED (ITM_USE_DLL)

	WRITE(99,*) 'END OF RUN'
	WRITE(98,*) 'Error at t(s)=', T_GLOBAL+DT_GLOBAL
	WRITE(98,*) ' '
	if (GLOBAL_STATUS_FLAG .NE. 1) then
      	GLOBAL_STATUS_FLAG = 1
	  call itm_set_swmm_error(901, T_GLOBAL+DT_GLOBAL)
	endif
	!close(99)
	!close(98)

cDEC$ ELSE
      WRITE(98,*) 'What is the error associated to this?'
      WRITE(98,*) 'Subroutine ENDPROG'
	STOP 'END OF RUN'	  
	close(99)
	close(98)

cDEC$ ENDIF

	END	SUBROUTINE ENDPROG 
