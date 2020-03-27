!  SVN:$Id: ice_age_modified.F90 1012 2015-06-26 12:34:09Z eclare $
!=======================================================================
!
! authors Elizabeth Hunke

      module ice_age_modified

      use ice_kinds_mod_modified

      implicit none

      private
      public :: increment_age

!=======================================================================

      contains

!=======================================================================

!  Increase ice age tracer by timestep length.

      subroutine increment_age (dt, iage)

      real (kind=dbl_kind), intent(in) :: &
         dt                    ! time step

      real (kind=dbl_kind), &
         intent(inout) :: &
         iage

      iage = iage + dt 

      end subroutine increment_age

!=======================================================================

      end module ice_age_modified

!=======================================================================
