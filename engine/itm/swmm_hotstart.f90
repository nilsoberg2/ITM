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

function ITM_OPEN_HOTSTART2(hsfile)
	use common_module
	implicit none
	
	character(*), intent(in) :: hsfile
	integer ITM_OPEN_HOTSTART2
	
	ITM_OPEN_HOTSTART2 = 0

    open(77, file=hsfile, status='REPLACE', form='BINARY', iostat=ITM_OPEN_HOTSTART2)

end function 


function ITM_CLOSE_HOTSTART2()
	use common_module
	implicit none
	
	integer ITM_CLOSE_HOTSTART2
	
	close(77, iostat=ITM_CLOSE_HOTSTART2)

end function 


function ITM_SAVE_HOTSTART(hsfile)
	use common_module
	implicit none
	
	character(*), intent(in) :: hsfile
	integer WRITE_HOTSTART_VARS
	integer ITM_SAVE_HOTSTART

    open(77, file=hsfile, status='REPLACE', form='BINARY')

	ITM_SAVE_HOTSTART = WRITE_HOTSTART_VARS()

	close(77)

end function


function  ITM_LOAD_HOTSTART(hsfile)
	use common_module
	implicit none
	
	character(*), intent(in) :: hsfile
	integer ITM_LOAD_HOTSTART
	integer READ_HOTSTART_VARS
	
	open(77, file=hsfile, status='OLD', form='BINARY', access='SEQUENTIAL', iostat=ITM_LOAD_HOTSTART, err=731)
	
	ITM_LOAD_HOTSTART = READ_HOTSTART_VARS()
	
	close(77)
	
731	if (ITM_LOAD_HOTSTART .ne. 0) then ! non-zero is error
		call deallocateAll
	endif

end function 


integer function strlen(st)
	integer		i
	character		st*(*)
	i = len(st)
	do while (st(i:i) .eq. ' ' .or. st(i:i) .eq. '\0')
		i = i - 1
	enddo
	strlen = i
	return
end
