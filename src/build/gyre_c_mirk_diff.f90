!fpx3_header(0.13_3a)
!
!dependencies
!   dir: ~/gyre_rot/src/build 
!   sources: -
!   includes: ../diff/gyre_mirk_diff.inc ../extern/core/core.inc
!   uses: gyre_eqns ISO_FORTRAN_ENV gyre_ext gyre_linalg gyre_point gyre_state core_linalg core_kinds gyre_diff
!   provides: gyre_c_mirk_diff
!end dependencies
!
!end fpx3_header
! Module   : gyre_c_mirk_diff
! Purpose  : difference equations (mono-implicit Runge-Kutta, complex)
!
! Copyright 2017 Rich Townsend
!
! This file is part of GYRE. GYRE is free software: you can
! redistribute it and/or modify it under the terms of the GNU General
! Public License as published by the Free Software Foundation, version 3.
!
! GYRE is distributed in the hope that it will be useful, but WITHOUT
! ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
! or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
! License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.

! Incfile  : gyre_mirk_diff
! Purpose  : difference equations (mono-implicit Runge-Kutta, template)
!
! Copyright 2017 Rich Townsend
!
! This file is part of GYRE. GYRE is free software: you can
! redistribute it and/or modify it under the terms of the GNU General
! Public License as published by the Free Software Foundation, version 3.
!
! GYRE is distributed in the hope that it will be useful, but WITHOUT
! ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
! or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
! License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.

! Incfile  : core
! Purpose  : fpx3 macros

!****

!****

!****

!****

!****

!****

!****

!****

!****

module gyre_c_mirk_diff

  ! Uses

  use core_kinds
  use core_linalg

  use gyre_diff
  use gyre_eqns
  use gyre_ext
  use gyre_linalg
  use gyre_point
  use gyre_state

  use ISO_FORTRAN_ENV

  ! No implicit typing

  implicit none

  ! Parameter definitions

  real(WP), parameter :: C_I(3) = [0._WP,0.5_WP,1._WP]

  ! Derived-type definitions

  type, extends (c_diff_t) :: c_mirk_diff_t
     private
     class(c_eqns_t), allocatable :: eq
     real(WP)                        :: dx
     logical                         :: origin
   contains
     private
     procedure, public :: build
  end type c_mirk_diff_t

  ! Interfaces

  interface c_mirk_diff_t
     module procedure c_mirk_diff_t_
  end interface c_mirk_diff_t

  ! Access specifiers

  private

  public :: c_mirk_diff_t

contains

  function c_mirk_diff_t_ (eq, pt_a, pt_b) result (df)

    class(c_eqns_t), intent(in) :: eq
    type(point_t), intent(in)      :: pt_a
    type(point_t), intent(in)      :: pt_b
    type(c_mirk_diff_t)         :: df

    type(point_t), allocatable :: pt(:)

    if(.NOT. (pt_a%s == pt_b%s)) then
      write(UNIT=ERROR_UNIT, FMT=*) 'ASSERT ''pt_a%s == pt_b%s'' failed at line 20 <gyre_c_mirk_diff:c_mirk_diff_t_>:'
      write(UNIT=ERROR_UNIT, FMT=*) 'Segment mismatch'
      stop
    endif

    ! Construct the mirk_diff_t

    df%dx = pt_b%x - pt_a%x

    allocate(pt(SIZE(C_I)))

    pt%s = pt_a%s
    pt%x = pt_a%x + C_I*df%dx

    allocate(df%eq, SOURCE=eq)

    call df%eq%stencil(pt)

    df%origin = pt_a%x == 0._WP

    df%n_e = eq%n_e

    ! Finish

    return

  end function c_mirk_diff_t_

  !****

  subroutine build (this, st, E_l, E_r, scl)

    class(c_mirk_diff_t), intent(in) :: this
    class(c_state_t), intent(in)     :: st
    complex(WP), intent(out)              :: E_l(:,:)
    complex(WP), intent(out)              :: E_r(:,:)
    type(c_ext_t), intent(out)       :: scl

    complex(WP) :: A(this%n_e,this%n_e,3)

  if(SIZE(E_l, 1)/= this%n_e) then
    write(UNIT=ERROR_UNIT, FMT=*) 'SIZE(E_l, 1) :', SIZE(E_l, 1)
    write(UNIT=ERROR_UNIT, FMT=*) 'this%n_e :', this%n_e
    write(UNIT=ERROR_UNIT, FMT=*) 'CHECK_BOUNDS SIZE(E_l, 1)==this%n_e failed at line 20 <gyre_c_mirk_diff:build>'
    stop
  endif

  if(SIZE(E_l, 2)/= this%n_e) then
    write(UNIT=ERROR_UNIT, FMT=*) 'SIZE(E_l, 2) :', SIZE(E_l, 2)
    write(UNIT=ERROR_UNIT, FMT=*) 'this%n_e :', this%n_e
    write(UNIT=ERROR_UNIT, FMT=*) 'CHECK_BOUNDS SIZE(E_l, 2)==this%n_e failed at line 20 <gyre_c_mirk_diff:build>'
    stop
  endif

  if(SIZE(E_r, 1)/= this%n_e) then
    write(UNIT=ERROR_UNIT, FMT=*) 'SIZE(E_r, 1) :', SIZE(E_r, 1)
    write(UNIT=ERROR_UNIT, FMT=*) 'this%n_e :', this%n_e
    write(UNIT=ERROR_UNIT, FMT=*) 'CHECK_BOUNDS SIZE(E_r, 1)==this%n_e failed at line 20 <gyre_c_mirk_diff:build>'
    stop
  endif

  if(SIZE(E_r, 2)/= this%n_e) then
    write(UNIT=ERROR_UNIT, FMT=*) 'SIZE(E_r, 2) :', SIZE(E_r, 2)
    write(UNIT=ERROR_UNIT, FMT=*) 'this%n_e :', this%n_e
    write(UNIT=ERROR_UNIT, FMT=*) 'CHECK_BOUNDS SIZE(E_r, 2)==this%n_e failed at line 20 <gyre_c_mirk_diff:build>'
    stop
  endif

    ! Build the difference equations

    if (this%origin) then

       ! If we're at the origin, fudge things to avoid evaluating A there

       E_l = -identity_matrix(this%n_e)
       E_r =  identity_matrix(this%n_e)

       scl = c_ext_t(1._WP)

    else

       ! Evaluate the RHS matrices

       A(:,:,1) = this%eq%A(1, st)
       A(:,:,2) = this%eq%A(2, st)
       A(:,:,3) = this%eq%A(3, st)

       ! Build the difference equations

       E_l = -identity_matrix(this%n_e) - this%dx*A(:,:,1)/6._WP - &
            this%dx*A(:,:,2)/3._WP - this%dx**2*MATMUL(A(:,:,2), A(:,:,1))/12._WP
       E_r =  identity_matrix(this%n_e) - this%dx*A(:,:,3)/6._WP - &
            this%dx*A(:,:,2)/3._WP + this%dx**2*MATMUL(A(:,:,2), A(:,:,3))/12._WP

       scl = c_ext_t(1._WP)

    endif

    ! Finish

  end subroutine build

end module gyre_c_mirk_diff

