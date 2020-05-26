! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  ocn_vel_forcing_surface_stress
!
!> \brief MPAS ocean surface stress
!> \author Doug Jacobsen, Mark Petersen, Todd Ringler, Phil Jones
!> \date   September 2011, updated May 2020
!> \details
!>  This module contains the routine for computing
!>  tendencies from surface stress.
!
!-------------------------------------------------------------------------------

module ocn_vel_forcing_surface_stress

   use mpas_timer

   use ocn_constants
   use ocn_config
   use ocn_mesh

   implicit none
   private
   save

   !----------------------------------------------------------------------------
   ! Public parameters
   !----------------------------------------------------------------------------

   !----------------------------------------------------------------------------
   ! Public member functions
   !----------------------------------------------------------------------------

   public :: ocn_vel_forcing_surface_stress_tend, &
             ocn_vel_forcing_surface_stress_init

   !----------------------------------------------------------------------------
   ! Private module variables
   !----------------------------------------------------------------------------

   logical :: surfaceStressOn ! on/off switch for surface stress

!*******************************************************************************

contains

!*******************************************************************************
!
!  routine ocn_vel_forcing_surface_stress_tend
!
!> \brief   Computes tendency term from surface stress
!> \author  Doug Jacobsen, Mark Petersen, Todd Ringler, Phil Jones
!> \date    15 September 2011, updated May 2020
!> \details
!>  This routine computes the surface stress tendency for momentum
!>  based on current state and adds it to the total velocity tendency
!
!-------------------------------------------------------------------------------

   subroutine ocn_vel_forcing_surface_stress_tend( &
                                    surfaceFluxAttenuationCoefficient, &
                                    surfaceStress, layerThicknessEdge, &
                                    tend, err)

      !-------------------------------------------------------------------------
      ! input variables
      !-------------------------------------------------------------------------

      real (kind=RKIND), dimension(:), intent(in) :: &
         surfaceStress, & !< [in] Wind stress at surface
         surfaceFluxAttenuationCoefficient !< [in] attenuation coef for sfc flux

      real (kind=RKIND), dimension(:,:), intent(in) :: &
         layerThicknessEdge     !< [in] thickness at edge

      !-------------------------------------------------------------------------
      ! input/output variables
      !-------------------------------------------------------------------------

      real (kind=RKIND), dimension(:,:), intent(inout) :: &
         tend            !< [inout] velocity tendency

      !-------------------------------------------------------------------------
      ! output variables
      !-------------------------------------------------------------------------

      integer, intent(out) :: err !< [out] error flag

      !{{{
      !-------------------------------------------------------------------------
      ! local variables
      !-------------------------------------------------------------------------

      integer ::      &
         iEdge, k,    &! edge and vertical loop iterators
         kMax,        &! index of bottom most edge
         cell1, cell2  ! neighbor cell indices across edge

      real (kind=RKIND) :: &
         transmissionCoeffTop, &! frac of flux transmitted thru top
         transmissionCoeffBot, &! frac of flux transmitted thru bottom
         zTop, zBot,           &! depth at top, bot of layer
         remainingStress,      &! fraction of stress not absorbed
         attenuationCoeff       ! local attenuation coeff

      real (kind=RKIND), dimension(:,:), allocatable :: &
         absorbFrac      ! local fraction of flux absorbed in the layer

      ! End preamble
      !-------------
      ! Begin code

      ! Exit if not turned on. Otherwise, start timer
      err = 0

      if ( .not. surfaceStressOn ) return
      call mpas_timer_start('vel surface stress')

      ! allocate space and transfer data
      allocate(absorbFrac (nVertLevels,nEdgesOwned))
      !$acc enter data create(absorbFrac)

      !NOTE: For performance reasons, the ocn_forcing_transmission
      !      function has been manually in-lined here. This 
      !      exponential transmission function should be identical
      !      to that used by other forcing so any modification should
      !      be made to other forcing fields as well.

      ! Compute the absorption at each level

#ifdef MPAS_OPENACC
      !$acc parallel loop &
      !$acc    present(cellsOnEdge, maxLevelEdgeTop, absorbFrac, &
      !$acc            surfaceFluxAttenuationCoefficient,        &
      !$acc            layerThicknessEdge)                       &
      !$acc    private(k, kMax, cell1, cell2, zTop, zBot, &
      !$acc            remainingStress, attenuationCoeff, &
      !$acc            transmissionCoeffTop, transmissionCoeffBot)
#else
      !$omp parallel
      !$omp do schedule(runtime) &
      !$omp    private(k, kMax, cell1, cell2, zTop, zBot, &
      !$omp            remainingStress, attenuationCoeff, &
      !$omp            transmissionCoeffTop, transmissionCoeffBot)
#endif
      do iEdge = 1, nEdgesOwned
         cell1 = cellsOnEdge(1,iEdge)
         cell2 = cellsOnEdge(2,iEdge)
         kMax  = maxLevelEdgeTop(iEdge)

         zTop = 0.0_RKIND
         remainingStress = 1.0_RKIND
         transmissionCoeffTop = 1.0_RKIND
         attenuationCoeff = 0.5_RKIND * &
                           (surfaceFluxAttenuationCoefficient(cell1) + &
                            surfaceFluxAttenuationCoefficient(cell2))

         do k = 1, kMax

            zBot = zTop - layerThicknessEdge(k, iEdge)
            transmissionCoeffBot = &
                      exp( max(zBot/attenuationCoeff, -100.0_RKIND) )

            absorbFrac(k,iEdge) = transmissionCoeffTop - &
                                  transmissionCoeffBot
            remainingStress = remainingStress - absorbFrac(k,iEdge)

            zTop = zBot
            transmissionCoeffTop = transmissionCoeffBot
         enddo
         ! If fluxes not absorbed before hitting the bottom
         ! absorb the remaining in the bottom layer
         if (kMax > 0) &
            absorbFrac(kMax,iEdge) = absorbFrac(kMax,iEdge) + &
                                     max(remainingStress,0.0_RKIND) 
      enddo
#ifndef MPAS_OPENACC
      !$omp end do
#endif

      ! Distribute surface flux tendency throughout according
      ! to the absorbed flux at each level

#ifdef MPAS_OPENACC
      !$acc parallel loop &
      !$acc    present(tend, surfaceStress, absorbFrac, &
      !$acc            layerThicknessEdge, maxLevelEdgeTop) &
      !$acc    private(k)
#else
      !$omp do schedule(runtime) &
      !$acc    private(k)
#endif
      do iEdge = 1, nEdgesOwned
      do k = 1, maxLevelEdgeTop(iEdge)
         tend(k,iEdge) =  tend(k,iEdge) + surfaceStress(iEdge)* &
            absorbFrac(k,iEdge)/rho_sw/layerThicknessEdge(k,iEdge)
      enddo
      enddo
#ifndef MPAS_OPENACC
      !$omp end do
      !$omp end parallel
#endif

      ! deallocate space and remove data from device
      !$acc exit data delete(absorbFrac)
      deallocate(absorbFrac)

      call mpas_timer_stop('vel surface stress')

   !----------------------------------------------------------------------------

   end subroutine ocn_vel_forcing_surface_stress_tend!}}}

!*******************************************************************************
!
!  routine ocn_vel_forcing_surface_stress_init
!
!> \brief   Initializes ocean surface stress forcing
!> \author  Doug Jacobsen, Mark Petersen, Todd Ringler, Phil Jones
!> \date    September 2011, updated May 2020
!> \details
!>  This routine initializes quantities related to surface stress
!>  in the ocean.
!
!-------------------------------------------------------------------------------

   subroutine ocn_vel_forcing_surface_stress_init(err)

      !-------------------------------------------------------------------------
      ! output variables
      !-------------------------------------------------------------------------

      integer, intent(out) :: err !< [out] error flag

      !{{{
      ! End of preamble
      !----------------
      ! Begin code

      ! Set default values

      err = 0
      surfaceStressOn = .true.

      ! Change default based on user input in ocn_config

      if (config_disable_vel_surface_stress) surfaceStressOn = .false.

   !----------------------------------------------------------------------------

   end subroutine ocn_vel_forcing_surface_stress_init!}}}

!*******************************************************************************

end module ocn_vel_forcing_surface_stress

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! vim: foldmethod=marker
