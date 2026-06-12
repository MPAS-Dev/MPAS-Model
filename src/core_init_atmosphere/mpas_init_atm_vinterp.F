! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the
! LICENSE file
! distributed with this code, or at
! http://mpas-dev.github.com/license.html
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! MODULE INTERP_MODULE
!
! This module provides routines for vertical interpolation.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module init_atm_vinterp
   use mpas_kind_types

   contains

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: interp_array_from_string
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   real (kind=RKIND) function vertical_interp(target_z, nz, zf, order, extrap, surface_val, sealev_val, ierr)

      use mpas_log, only : mpas_log_write
      use mpas_derived_types, only : MPAS_LOG_ERR

      implicit none

      real (kind=RKIND), intent(in) :: target_z
      integer, intent(in) :: nz
      real (kind=RKIND), dimension(2,nz), intent(in) :: zf      ! zf(1,:) is column of vertical coordinate values, zf(2,:) is column of field values
      integer, intent(in), optional :: order
      integer, intent(in), optional :: extrap                   ! can take values 0 = constant, 1 = linear (default), 2 = lapse-rate
      real (kind=RKIND), intent(in), optional :: surface_val
      real (kind=RKIND), intent(in), optional :: sealev_val
      integer, intent(out), optional :: ierr
      
      integer :: k, lm, lp, n_lapse
      real (kind=RKIND) :: wm, wp
      real (kind=RKIND) :: slope
      logical :: found

      integer :: interp_order, extrap_type
      real (kind=RKIND) :: surface, sealevel

      if (present(ierr)) ierr = 0
      
      if (present(order)) then
         interp_order = order
      else
         interp_order = 2
      end if

      if (present(extrap)) then
         extrap_type = extrap
      else
         extrap_type = 1
      end if

      if (present(surface_val)) then
         surface = surface_val
      else
         surface = 200100.0
      end if

      if (present(sealev_val)) then
         sealevel = sealev_val
      else
         sealevel = 201300.0
      end if

      !
      ! Extrapolation required
      !
      if (target_z < zf(1,1)) then
         if (extrap_type == 0) then
            vertical_interp = zf(2,1)
         else if (extrap_type == 1) then
            slope = (zf(2,2) - zf(2,1)) / (zf(1,2) - zf(1,1))
            vertical_interp = zf(2,1) + slope * (target_z - zf(1,1))
         else if (extrap_type == 2) then
            n_lapse = min(nz, 3)
            slope = ls_lapse_slope(zf, nz, 1, n_lapse)
            vertical_interp = zf(2,1) + slope * (target_z - zf(1,1))
         end if
         return
      end if
      if (target_z >= zf(1,nz)) then
         if (extrap_type == 0) then
            vertical_interp = zf(2,nz)
         else if (extrap_type == 1) then
            slope = (zf(2,nz) - zf(2,nz-1)) / (zf(1,nz) - zf(1,nz-1))
            vertical_interp = zf(2,nz) + slope * (target_z - zf(1,nz))
         else if (extrap_type == 2) then
            n_lapse = min(nz, 3)
            slope = ls_lapse_slope(zf, nz, nz - n_lapse + 1, nz)
            vertical_interp = zf(2,nz) + slope * (target_z - zf(1,nz))
         end if
         return
      end if


      !
      ! No extrapolation required
      !

      found = .false.

      do k=1,nz-1
         if (target_z >= zf(1,k) .and. target_z < zf(1,k+1)) then
            lm = k
            lp = k+1
            wm = (zf(1,k+1) - target_z) / (zf(1,k+1) - zf(1,k))
            wp = (target_z - zf(1,k)) / (zf(1,k+1) - zf(1,k))
            found = .true.
            exit
         end if
      end do

      if (.not. found) then
         ! Degenerate sorted data (duplicate adjacent heights); return nearest level.
         if (present(ierr)) ierr = 1
         lm = 1
         do k = 2, nz
            if (abs(zf(1,k) - target_z) < abs(zf(1,lm) - target_z)) lm = k
         end do
         vertical_interp = zf(2,lm)
         return
      end if

      vertical_interp = wm*zf(2,lm) + wp*zf(2,lp)

      return

   end function vertical_interp

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Routine: ls_lapse_slope
   !
   ! Purpose: Compute an ordinary least-squares slope over levels k_start..k_end
   !          of zf. x is shifted by the first selected level before accumulating
   !          to prevent catastrophic cancellation in n*sum_x2 - sum_x**2 when z
   !          values are large relative to their spacing (e.g. z~50 km, dz~1 km
   !          loses ~3 digits in single precision without the shift).  After
   !          centering, the denominator is a sum of squares and therefore always
   !          non-negative; a relative epsilon guard catches degenerate z-spacing
   !          before a divide-by-zero or Inf/NaN can propagate.  Returns 0 in all
   !          degenerate cases (constant extrapolation fallback).
   !
   ! Author:  Rubaiat Islam, NCAR/MMM, 2026-04-17
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   real (kind=RKIND) function ls_lapse_slope(zf, nz, k_start, k_end)

      implicit none

      integer, intent(in) :: nz, k_start, k_end
      real (kind=RKIND), dimension(2,nz), intent(in) :: zf

      integer :: k, n_pts
      real (kind=RKIND) :: x_ref, dx
      real (kind=RKIND) :: sum_x, sum_y, sum_x2, sum_xy, n_real, denom

      n_pts = k_end - k_start + 1

      if (n_pts <= 1) then
         ls_lapse_slope = 0.0_RKIND
         return
      end if

      x_ref  = zf(1, k_start)          ! shift origin: dx values are O(dz), not O(z)
      
      sum_x  = 0.0_RKIND;  sum_y  = 0.0_RKIND
      sum_x2 = 0.0_RKIND;  sum_xy = 0.0_RKIND

      do k = k_start, k_end
         dx     = zf(1,k) - x_ref
         sum_x  = sum_x  + dx
         sum_y  = sum_y  + zf(2,k)
         sum_x2 = sum_x2 + dx**2
         sum_xy = sum_xy + dx * zf(2,k)
      end do

      n_real = real(n_pts, RKIND)
      denom  = n_real * sum_x2 - sum_x * sum_x

      ! denom >= 0 after centering; it is ~0 only when the selected levels
      ! share the same (or nearly the same) vertical coordinate.
      if (denom > epsilon(1.0_RKIND) * n_real * sum_x2) then
         ls_lapse_slope = (n_real*sum_xy - sum_x*sum_y) / denom
      else
         ls_lapse_slope = 0.0_RKIND
      end if

   end function ls_lapse_slope

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Routine: vertical_interp_ecmwf
   !
   ! Purpose: ECMWF model-level aware vertical interpolation.
   !
   !   Interior targets:  identical linear interpolation to vertical_interp.
   !
   !   Below/above-data extrapolation: OLS slope fitted over ALL levels whose
   !   height lies within a fixed physical depth from the boundary:
   !
   !     Below data: levels k where  zf(1,k) - zf(1,1)   <= DEPTH_BOT (3000 m)
   !     Above data: levels k where  zf(1,nz) - zf(1,k)  <= DEPTH_TOP (5000 m)
   !
   !   With ERA5/IFS 137-level data this selects ~20-25 levels near the surface
   !   (spanning the well-mixed boundary layer and lower free troposphere) and
   !   ~5-10 levels near the model top, compared with only 3 levels for the
   !   GFS-oriented vertical_interp.  The result is a slope that represents the
   !   actual lower-tropospheric lapse rate rather than the shallow skin layer
   !   captured by the first three fine-resolution levels (~100 m depth).
   !
   !   N is clamped to [N_MIN=3, N_MAX=30] for numerical stability.
   !   The existing ls_lapse_slope helper (x-centred OLS, epsilon-guarded
   !   denominator) is reused unchanged.
   !
   ! Author:  Rubaiat Islam, NCAR/MMM, 2026-04-17
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   real (kind=RKIND) function vertical_interp_ecmwf(target_z, nz, zf, ierr)

      implicit none

      real (kind=RKIND), intent(in) :: target_z
      integer, intent(in) :: nz
      real (kind=RKIND), dimension(2,nz), intent(in) :: zf
      integer, intent(out), optional :: ierr

      real (kind=RKIND), parameter :: DEPTH_BOT = 3000.0_RKIND   ! m: below-boundary window
      real (kind=RKIND), parameter :: DEPTH_TOP = 5000.0_RKIND   ! m: above-boundary window
      integer, parameter :: N_MIN = 3    ! minimum OLS levels (stability)
      integer, parameter :: N_MAX = 30   ! maximum OLS levels (cap distant levels)

      integer :: k, lm, lp, n_depth
      real (kind=RKIND) :: wm, wp, slope
      logical :: found

      if (present(ierr)) ierr = 0

      !
      ! Below lowest level: physical-depth window from the bottom
      !

      if (target_z < zf(1,1)) then
         n_depth = 1
         do k = 2, nz
            if (zf(1,k) - zf(1,1) <= DEPTH_BOT) then
               n_depth = k
            else
               exit   ! zf is sorted ascending; levels beyond are out of window
            end if
         end do
         n_depth = min(max(n_depth, N_MIN), min(N_MAX, nz))
         slope = ls_lapse_slope(zf, nz, 1, n_depth)
         vertical_interp_ecmwf = zf(2,1) + slope * (target_z - zf(1,1))
         return
      end if

      !
      ! Above highest level: physical-depth window from the top
      !

      if (target_z >= zf(1,nz)) then
         n_depth = 1
         do k = nz-1, 1, -1
            if (zf(1,nz) - zf(1,k) <= DEPTH_TOP) then
               n_depth = nz - k + 1
            else
               exit   ! zf is sorted ascending; levels beyond are out of window
            end if
         end do
         n_depth = min(max(n_depth, N_MIN), min(N_MAX, nz))
         slope = ls_lapse_slope(zf, nz, nz - n_depth + 1, nz)
         vertical_interp_ecmwf = zf(2,nz) + slope * (target_z - zf(1,nz))
         return
      end if

      !
      ! Interior: linear interpolation
      !

      found = .false.

      do k = 1, nz-1
         if (target_z >= zf(1,k) .and. target_z < zf(1,k+1)) then
            lm = k
            lp = k+1
            wm = (zf(1,k+1) - target_z) / (zf(1,k+1) - zf(1,k))
            wp = (target_z   - zf(1,k)) / (zf(1,k+1) - zf(1,k))
            found = .true.
            exit
         end if
      end do

      if (.not. found) then
         ! Degenerate sorted data (duplicate adjacent heights); return nearest level.
         if (present(ierr)) ierr = 1
         lm = 1
         do k = 2, nz
            if (abs(zf(1,k) - target_z) < abs(zf(1,lm) - target_z)) lm = k
         end do
         vertical_interp_ecmwf = zf(2,lm)
         return
      end if

      vertical_interp_ecmwf = wm * zf(2,lm) + wp * zf(2,lp)

      return

   end function vertical_interp_ecmwf

end module init_atm_vinterp
