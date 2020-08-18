!fpx3_header(0.13_3a)
!
!dependencies
!   dir: ~/gyre_rot/src/build 
!   sources: -
!   includes: ../extern/core/core.inc
!   uses: gyre_nad_diff gyre_mode_par gyre_wave ISO_FORTRAN_ENV gyre_context gyre_nad_bound gyre_grid_factory gyre_grid gyre_bvp gyre_num_par gyre_ext core_kinds gyre_state gyre_osc_par gyre_point gyre_model gyre_nad_trans
!   provides: gyre_nad_bvp
!end dependencies
!
!end fpx3_header
! Module   : gyre_nad_bvp
! Purpose  : nonadiabatic boundary value problem solver
!
! Copyright 2013-2018 Rich Townsend
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

module gyre_nad_bvp

  ! Uses

  use core_kinds

  use gyre_bvp
  use gyre_context
  use gyre_ext
  use gyre_grid
  use gyre_grid_factory
  use gyre_model
  use gyre_mode_par
  use gyre_nad_bound
  use gyre_nad_diff
  use gyre_nad_trans
  use gyre_num_par
  use gyre_osc_par
  use gyre_point
  use gyre_state
  use gyre_wave

  use ISO_FORTRAN_ENV

  ! No implicit typing

  implicit none

  ! Derived-type definitions

  type, extends (c_bvp_t) :: nad_bvp_t
     private
     type(context_t), pointer :: cx => null()
     type(grid_t)             :: gr
     type(nad_trans_t)        :: tr
     type(mode_par_t)         :: md_p
     type(osc_par_t)          :: os_p
  end type nad_bvp_t

  ! Interfaces

  interface nad_bvp_t
     module procedure nad_bvp_t_
  end interface nad_bvp_t

  interface wave_t
     module procedure wave_t_hom_
     module procedure wave_t_inhom_
  end interface wave_t

  ! Access specifiers

  private

  public :: nad_bvp_t
  public :: wave_t

  ! Procedures

contains

  function nad_bvp_t_ (cx, gr, md_p, nm_p, os_p) result (bp)

    class(context_t), pointer, intent(in) :: cx
    type(grid_t), intent(in)              :: gr
    type(mode_par_t), intent(in)          :: md_p
    type(num_par_t), intent(in)           :: nm_p
    type(osc_par_t), intent(in)           :: os_p
    type(nad_bvp_t)                       :: bp

    type(nad_bound_t)             :: bd
    integer                       :: k
    type(nad_diff_t), allocatable :: df(:)

    ! Construct the nad_bvp_t

    ! Initialize the boundary conditions

    bd = nad_bound_t(cx, md_p, os_p)

    ! Initialize the difference equations

    allocate(df(gr%n_k-1))

    !$OMP PARALLEL DO
    do k = 1, gr%n_k-1
       df(k) = nad_diff_t(cx, gr%pt(k), gr%pt(k+1), md_p, nm_p, os_p)
    end do

    ! Initialize the bvp_t

    bp%c_bvp_t = c_bvp_t_(bd, df, nm_p) 

    ! Other initializations

    bp%cx => cx
    bp%gr = gr

    bp%tr = nad_trans_t(cx, md_p, os_p)
    call bp%tr%stencil(gr%pt)

    bp%md_p = md_p
    bp%os_p = os_p

    ! Finish

    return

  end function nad_bvp_t_

  !****

  function wave_t_hom_ (bp, st) result (wv)

    class(nad_bvp_t), intent(inout)   :: bp
    type(c_state_t), intent(in)       :: st
    type(wave_t)                      :: wv

    complex(WP) :: y(6,bp%n_k)
    integer     :: k

    ! Calculate the homogeneous solution vector

    call bp%build(st)
    call bp%factor()

    y = bp%soln_vec_hom()

    !$OMP PARALLEL DO
    do k = 1, bp%n_k
       call bp%tr%trans_vars(y(:,k), k, st)
    end do

    ! Construct the wave_t

    wv = wave_t_y_(bp, st, y)

    ! Finish

    return

  end function wave_t_hom_

  !****

  function wave_t_inhom_ (bp, st, w_i, w_o) result (wv)

    class(nad_bvp_t), intent(inout) :: bp
    type(c_state_t), intent(in)     :: st
    complex(WP), intent(in)         :: w_i(:)
    complex(WP), intent(in)         :: w_o(:)
    type(wave_t)                    :: wv

    complex(WP) :: y(6,bp%n_k)
    integer     :: k

  if(SIZE(w_i)/= bp%n_i) then
    write(UNIT=ERROR_UNIT, FMT=*) 'SIZE(w_i) :', SIZE(w_i)
    write(UNIT=ERROR_UNIT, FMT=*) 'bp%n_i :', bp%n_i
    write(UNIT=ERROR_UNIT, FMT=*) 'CHECK_BOUNDS SIZE(w_i)==bp%n_i failed at line 176 <gyre_nad_bvp:wave_t_inhom_>'
    stop
  endif

  if(SIZE(w_o)/= bp%n_o) then
    write(UNIT=ERROR_UNIT, FMT=*) 'SIZE(w_o) :', SIZE(w_o)
    write(UNIT=ERROR_UNIT, FMT=*) 'bp%n_o :', bp%n_o
    write(UNIT=ERROR_UNIT, FMT=*) 'CHECK_BOUNDS SIZE(w_o)==bp%n_o failed at line 177 <gyre_nad_bvp:wave_t_inhom_>'
    stop
  endif

    ! Calculate the inhomogeneous solution vector

    call bp%build(st)
    call bp%factor()

    y = bp%soln_vec_inhom(w_i, w_o)

    !$OMP PARALLEL DO
    do k = 1, bp%n_k
       call bp%tr%trans_vars(y(:,k), k, st)
    end do

    ! Construct the wave_t

    wv = wave_t_y_(bp, st, y)

    ! Finish

    return

  end function wave_t_inhom_

  !****

  function wave_t_y_ (bp, st, y) result (wv)

    class(nad_bvp_t), intent(inout) :: bp
    type(c_state_t), intent(in)     :: st
    complex(WP), intent(in)         :: y(:,:)
    type(wave_t)                    :: wv

    type(c_ext_t) :: discrim

  if(SIZE(y, 1)/= bp%n_e) then
    write(UNIT=ERROR_UNIT, FMT=*) 'SIZE(y, 1) :', SIZE(y, 1)
    write(UNIT=ERROR_UNIT, FMT=*) 'bp%n_e :', bp%n_e
    write(UNIT=ERROR_UNIT, FMT=*) 'CHECK_BOUNDS SIZE(y, 1)==bp%n_e failed at line 212 <gyre_nad_bvp:wave_t_y_>'
    stop
  endif

  if(SIZE(y, 2)/= bp%n_k) then
    write(UNIT=ERROR_UNIT, FMT=*) 'SIZE(y, 2) :', SIZE(y, 2)
    write(UNIT=ERROR_UNIT, FMT=*) 'bp%n_k :', bp%n_k
    write(UNIT=ERROR_UNIT, FMT=*) 'CHECK_BOUNDS SIZE(y, 2)==bp%n_k failed at line 213 <gyre_nad_bvp:wave_t_y_>'
    stop
  endif

    ! Construct the wave_t

    discrim = bp%det()

    wv = wave_t(st, y, discrim, bp%cx, bp%gr, bp%md_p, bp%os_p)

    ! Finish

    return

  end function wave_t_y_

end module gyre_nad_bvp
