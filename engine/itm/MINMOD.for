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

      SUBROUTINE MINMOD(A,B,DELT_new)
c     Purpose: to compute a MINMOD type slope limiter 
      IMPLICIT NONE
      double precision A,B,DELT_new 
      IF (A*B .LE. 0d0) THEN
	      DELT_new=0d0
      ELSE IF (ABS(A) .LE. ABS(B)) THEN
	      DELT_new=A
      ELSE
	      DELT_new=B
      END IF
      return
      end
