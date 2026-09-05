#include <define.h>

#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)

MODULE MOD_Vars_1DPFTFluxes
!-----------------------------------------------------------------------
! !DESCRIPTION:
!  Define PFT flux variables
!
!  Created by Hua Yuan, 08/2019
!-----------------------------------------------------------------------

   USE MOD_Precision
#ifdef BGC
   USE MOD_BGC_Vars_1DPFTFluxes
#endif
   IMPLICIT NONE
   SAVE

!-----------------------------------------------------------------------
!  Fluxes
!-----------------------------------------------------------------------
   real(r8), allocatable :: taux_p   (:)    !wind stress: E-W [kg/m/s2]
   real(r8), allocatable :: tauy_p   (:)    !wind stress: N-S [kg/m/s2]
   real(r8), allocatable :: fsenl_p  (:)    !sensible heat from leaves [W/m2]
   real(r8), allocatable :: fevpl_p  (:)    !evaporation+transpiration from leaves [mm/s]
   real(r8), allocatable :: etr_p    (:)    !transpiration rate [mm/s]
   real(r8), allocatable :: fseng_p  (:)    !sensible heat flux from ground [W/m2]
   real(r8), allocatable :: fevpg_p  (:)    !evaporation heat flux from ground [mm/s]
   real(r8), allocatable :: parsun_p (:)    !solar absorbed by sunlit vegetation [W/m2]
   real(r8), allocatable :: parsha_p (:)    !solar absorbed by shaded vegetation [W/m2]
   real(r8), allocatable :: sabvsun_p(:)    !solar absorbed by sunlit vegetation [W/m2]
   real(r8), allocatable :: sabvsha_p(:)    !solar absorbed by shaded vegetation [W/m2]
   real(r8), allocatable :: qintr_p  (:)    !interception (mm h2o/s)
   real(r8), allocatable :: qintr_rain_p(:) !rainfall interception (mm h2o/s)
   real(r8), allocatable :: qintr_snow_p(:) !snowfall interception (mm h2o/s)
   real(r8), allocatable :: assim_p  (:)    !canopy assimilation rate (mol m-2 s-1)
   real(r8), allocatable :: respc_p  (:)    !canopy respiration (mol m-2 s-1)

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: allocate_1D_PFTFluxes
   PUBLIC :: deallocate_1D_PFTFluxes
   PUBLIC :: set_1D_PFTFluxes

! PRIVATE MEMBER FUNCTIONS:

!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------

   SUBROUTINE allocate_1D_PFTFluxes
   ! -------------------------------------------------------------------
   ! Allocates memory for CoLM PFT 1d [numpft] variables
   ! -------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_MPAS_MPI
   USE MOD_LandPFT
   IMPLICIT NONE

      IF (.true.) THEN
         IF (numpft > 0) THEN

            allocate (taux_p       (numpft)) ; taux_p       (:) = spval !wind stress: E-W [kg/m/s2]
            allocate (tauy_p       (numpft)) ; tauy_p       (:) = spval !wind stress: N-S [kg/m/s2]
            allocate (fsenl_p      (numpft)) ; fsenl_p      (:) = spval !sensible heat from leaves [W/m2]
            allocate (fevpl_p      (numpft)) ; fevpl_p      (:) = spval !evaporation+transpiration from leaves [mm/s]
            allocate (etr_p        (numpft)) ; etr_p        (:) = spval !transpiration rate [mm/s]
            allocate (fseng_p      (numpft)) ; fseng_p      (:) = spval !sensible heat flux from ground [W/m2]
            allocate (fevpg_p      (numpft)) ; fevpg_p      (:) = spval !evaporation heat flux from ground [mm/s]
            allocate (parsun_p     (numpft)) ; parsun_p     (:) = spval !solar absorbed by sunlit vegetation [W/m2]
            allocate (parsha_p     (numpft)) ; parsha_p     (:) = spval !solar absorbed by shaded vegetation [W/m2]
            allocate (sabvsun_p    (numpft)) ; sabvsun_p    (:) = spval !solar absorbed by sunlit vegetation [W/m2]
            allocate (sabvsha_p    (numpft)) ; sabvsha_p    (:) = spval !solar absorbed by shaded vegetation [W/m2]
            allocate (qintr_p      (numpft)) ; qintr_p      (:) = spval !interception (mm h2o/s)
            allocate (qintr_rain_p (numpft)) ; qintr_rain_p (:) = spval !rainfall interception (mm h2o/s)
            allocate (qintr_snow_p (numpft)) ; qintr_snow_p (:) = spval !snowfall interception (mm h2o/s)
            allocate (assim_p      (numpft)) ; assim_p      (:) = spval !canopy assimilation rate (mol m-2 s-1)
            allocate (respc_p      (numpft)) ; respc_p      (:) = spval !canopy respiration (mol m-2 s-1)

         ENDIF
      ENDIF

#ifdef BGC
      CALL allocate_1D_BGCPFTFluxes
#endif

   END SUBROUTINE allocate_1D_PFTFluxes

   SUBROUTINE deallocate_1D_PFTFluxes
   ! -------------------------------------------------------------------
   ! deallocates memory for CoLM PFT 1d [numpft] variables
   ! -------------------------------------------------------------------
   USE MOD_MPAS_MPI
   USE MOD_LandPFT

      IF (.true.) THEN
         IF (numpft > 0) THEN

            deallocate (taux_p       )
            deallocate (tauy_p       )
            deallocate (fsenl_p      )
            deallocate (fevpl_p      )
            deallocate (etr_p        )
            deallocate (fseng_p      )
            deallocate (fevpg_p      )
            deallocate (parsun_p     )
            deallocate (parsha_p     )
            deallocate (sabvsun_p    )
            deallocate (sabvsha_p    )
            deallocate (qintr_p      )
            deallocate (qintr_rain_p )
            deallocate (qintr_snow_p )
            deallocate (assim_p      )
            deallocate (respc_p      )

         ENDIF
      ENDIF

#ifdef BGC
      CALL deallocate_1D_BGCPFTFluxes
#endif

   END SUBROUTINE deallocate_1D_PFTFluxes

   SUBROUTINE set_1D_PFTFluxes(Values, Nan)
   ! -------------------------------------------------------------------
   ! Allocates memory for CoLM PFT 1d [numpft] variables
   ! -------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_MPAS_MPI
   USE MOD_LandPFT
   IMPLICIT NONE

   real(r8),intent(in) :: Values
   real(r8),intent(in) :: Nan

      IF (.true.) THEN
         IF (numpft > 0) THEN

            taux_p      (:) = Values  !wind stress: E-W [kg/m/s2]
            tauy_p      (:) = Values  !wind stress: N-S [kg/m/s2]
            fsenl_p     (:) = Values  !sensible heat from leaves [W/m2]
            fevpl_p     (:) = Values  !evaporation+transpiration from leaves [mm/s]
            etr_p       (:) = Values  !transpiration rate [mm/s]
            fseng_p     (:) = Values  !sensible heat flux from ground [W/m2]
            fevpg_p     (:) = Values  !evaporation heat flux from ground [mm/s]
            parsun_p    (:) = Values  !solar absorbed by sunlit vegetation [W/m2]
            parsha_p    (:) = Values  !solar absorbed by shaded vegetation [W/m2]
            sabvsun_p   (:) = Values  !solar absorbed by sunlit vegetation [W/m2]
            sabvsha_p   (:) = Values  !solar absorbed by shaded vegetation [W/m2]
            qintr_p     (:) = Values  !interception (mm h2o/s)
            qintr_rain_p(:) = Values  !rainfall interception (mm h2o/s)
            qintr_snow_p(:) = Values  !snowfall interception (mm h2o/s)
            assim_p     (:) = Values  !canopy assimilation rate (mol m-2 s-1)
            respc_p     (:) = Values  !canopy respiration (mol m-2 s-1)

         ENDIF
      ENDIF

#ifdef BGC
      CALL set_1D_BGCPFTFluxes (Values, Nan)
#endif

   END SUBROUTINE set_1D_PFTFluxes

END MODULE MOD_Vars_1DPFTFluxes

#endif
! ---------- EOP ------------
