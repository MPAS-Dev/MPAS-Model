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
      
      integer :: k, lm, lp
      real (kind=RKIND) :: wm, wp
      real (kind=RKIND) :: slope

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
            vertical_interp = zf(2,1) - (target_z - zf(1,1))*0.0065
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
             call mpas_log_write('extrap_type == 2 not implemented for target_z >= zf(1,nz)', messageType=MPAS_LOG_ERR)
             if (present(ierr)) ierr = 1
             return
         end if
         return
      end if


      !
      ! No extrapolation required
      !
      do k=1,nz-1
         if (target_z >= zf(1,k) .and. target_z < zf(1,k+1)) then
            lm = k
            lp = k+1
            wm = (zf(1,k+1) - target_z) / (zf(1,k+1) - zf(1,k))
            wp = (target_z - zf(1,k)) / (zf(1,k+1) - zf(1,k))
            exit
         end if
      end do

      vertical_interp = wm*zf(2,lm) + wp*zf(2,lp)

      return

   end function vertical_interp

end module init_atm_vinterp
