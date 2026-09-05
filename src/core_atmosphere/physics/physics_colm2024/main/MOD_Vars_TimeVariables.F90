#include <define.h>

!-----------------------------------------------------------------------
! Created by Yongjiu Dai, 03/2014
!-----------------------------------------------------------------------

#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
MODULE MOD_Vars_PFTimeVariables
!-----------------------------------------------------------------------
! !DESCRIPTION:
!  Define PFT time variables
!
!  Added by Hua Yuan, 08/2019
!-----------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_TimeManager
#ifdef BGC
   USE MOD_BGC_Vars_PFTimeVariables
#endif

   IMPLICIT NONE
   SAVE
!-----------------------------------------------------------------------
! Time-varying state variables which required by restart run

   ! for LULC_IGBP_PFT or LULC_IGBP_PC
   real(r8), allocatable :: tleaf_p      (:) !shaded leaf temperature [K]
   real(r8), allocatable :: ldew_p       (:) !depth of water on foliage [mm]
   real(r8), allocatable :: ldew_rain_p  (:) !depth of rain on foliage [mm]
   real(r8), allocatable :: ldew_snow_p  (:) !depth of snow on foliage [mm]
   real(r8), allocatable :: fwet_snow_p  (:) !vegetation snow fractional cover [-]
   real(r8), allocatable :: sigf_p       (:) !fraction of veg cover, excluding snow-covered veg [-]
   real(r8), allocatable :: tlai_p       (:) !leaf area index
   real(r8), allocatable :: lai_p        (:) !leaf area index
   real(r8), allocatable :: laisun_p     (:) !sunlit leaf area index
   real(r8), allocatable :: laisha_p     (:) !shaded leaf area index
   real(r8), allocatable :: tsai_p       (:) !stem area index
   real(r8), allocatable :: sai_p        (:) !stem area index
   real(r8), allocatable :: ssun_p   (:,:,:) !sunlit canopy absorption for solar radiation (0-1)
   real(r8), allocatable :: ssha_p   (:,:,:) !shaded canopy absorption for solar radiation (0-1)

#ifdef HYPERSPECTRAL
   real(r8), allocatable :: ssun_hires_p (:,:,:) !sunlit canopy absorption for solar radiation (0-1)
   real(r8), allocatable :: ssha_hires_p (:,:,:) !shaded canopy absorption for solar radiation (0-1)
#endif

   real(r8), allocatable :: thermk_p     (:) !canopy gap fraction for tir radiation
   real(r8), allocatable :: fshade_p     (:) !canopy shade fraction for tir radiation
   real(r8), allocatable :: extkb_p      (:) !(k, g(mu)/mu) direct solar extinction coefficient
   real(r8), allocatable :: extkd_p      (:) !diffuse and scattered diffuse PAR extinction coefficient
   !TODO@yuan: to check the below for PC whether they are needed
   real(r8), allocatable :: tref_p       (:) !2 m height air temperature [kelvin]
   real(r8), allocatable :: t2m_wmo_p    (:) !2 m WMO air temperature [kelvin]
   real(r8), allocatable :: qref_p       (:) !2 m height air specific humidity
   real(r8), allocatable :: rst_p        (:) !canopy stomatal resistance (s/m)
   real(r8), allocatable :: z0m_p        (:) !effective roughness [m]
! Plant Hydraulic variables
   real(r8), allocatable :: vegwp_p    (:,:) !vegetation water potential [mm]
   real(r8), allocatable :: gs0sun_p     (:) !working copy of sunlit stomata conductance
   real(r8), allocatable :: gs0sha_p     (:) !working copy of shaded stomata conductance
! END plant hydraulic variables
! Ozone Stress Variables
   real(r8), allocatable :: o3coefv_sun_p(:) !Ozone stress factor for photosynthesis on sunlit leaf
   real(r8), allocatable :: o3coefv_sha_p(:) !Ozone stress factor for photosynthesis on shaded leaf
   real(r8), allocatable :: o3coefg_sun_p(:) !Ozone stress factor for stomata on sunlit leaf
   real(r8), allocatable :: o3coefg_sha_p(:) !Ozone stress factor for stomata on shaded leaf
   real(r8), allocatable :: lai_old_p    (:) !lai in last time step
   real(r8), allocatable :: o3uptakesun_p(:) !Ozone does, sunlit leaf (mmol O3/m^2)
   real(r8), allocatable :: o3uptakesha_p(:) !Ozone does, shaded leaf (mmol O3/m^2)
! END Ozone Stress Variables
! irrigation variables
   integer , allocatable :: irrig_method_p(:)!irrigation method
! END irrigation variables

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: allocate_PFTimeVariables
   PUBLIC :: deallocate_PFTimeVariables
   PUBLIC :: READ_PFTimeVariables
   PUBLIC :: WRITE_PFTimeVariables
#ifdef RangeCheck
   PUBLIC :: check_PFTimeVariables
#endif

! PRIVATE MEMBER FUNCTIONS:

!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------

   SUBROUTINE allocate_PFTimeVariables ()
   !--------------------------------------------------------------------
   ! Allocates memory for CoLM 1d [numpft] variables
   !--------------------------------------------------------------------
   USE MOD_Precision
   USE MOD_MPAS_MPI
   USE MOD_LandPFT
   USE MOD_Vars_Global
   IMPLICIT NONE

      IF (.true.) THEN
         IF (numpft > 0) THEN
            allocate (tleaf_p      (numpft)) ; tleaf_p      (:) = spval !leaf temperature [K]
            allocate (ldew_p       (numpft)) ; ldew_p       (:) = spval !depth of water on foliage [mm]
            allocate (ldew_rain_p  (numpft)) ; ldew_rain_p  (:) = spval !depth of rain on foliage [mm]
            allocate (ldew_snow_p  (numpft)) ; ldew_snow_p  (:) = spval !depth of snow on foliage [mm]
            allocate (fwet_snow_p  (numpft)) ; fwet_snow_p  (:) = spval !vegetation snow fractional cover [-]
            allocate (sigf_p       (numpft)) ; sigf_p       (:) = spval !fraction of veg cover, excluding snow-covered veg [-]
            allocate (tlai_p       (numpft)) ; tlai_p       (:) = spval !leaf area index
            allocate (lai_p        (numpft)) ; lai_p        (:) = spval !leaf area index
            allocate (laisun_p     (numpft)) ; laisun_p     (:) = spval !leaf area index
            allocate (laisha_p     (numpft)) ; laisha_p     (:) = spval !leaf area index
            allocate (tsai_p       (numpft)) ; tsai_p       (:) = spval !stem area index
            allocate (sai_p        (numpft)) ; sai_p        (:) = spval !stem area index
            allocate (ssun_p   (2,2,numpft)) ; ssun_p   (:,:,:) = spval !sunlit canopy absorption for solar radiation (0-1)
            allocate (ssha_p   (2,2,numpft)) ; ssha_p   (:,:,:) = spval !shaded canopy absorption for solar radiation (0-1)

#ifdef HYPERSPECTRAL
            allocate (ssun_hires_p (211,2,numpft)) ; ssun_hires_p   (:,:,:) = spval !sunlit canopy absorption for solar radiation (0-1)
            allocate (ssha_hires_p (211,2,numpft)) ; ssha_hires_p   (:,:,:) = spval !shaded canopy absorption for solar radiation (0-1)
#endif

            allocate (thermk_p     (numpft)) ; thermk_p     (:) = spval !canopy gap fraction for tir radiation
            allocate (fshade_p     (numpft)) ; fshade_p     (:) = spval !canopy shade fraction for tir radiation
            allocate (extkb_p      (numpft)) ; extkb_p      (:) = spval !(k, g(mu)/mu) direct solar extinction coefficient
            allocate (extkd_p      (numpft)) ; extkd_p      (:) = spval !diffuse and scattered diffuse PAR extinction coefficient
            allocate (tref_p       (numpft)) ; tref_p       (:) = spval !2 m height air temperature [kelvin]
            allocate (t2m_wmo_p    (numpft)) ; t2m_wmo_p    (:) = spval !2 m WMO air temperature [kelvin]
            allocate (qref_p       (numpft)) ; qref_p       (:) = spval !2 m height air specific humidity
            allocate (rst_p        (numpft)) ; rst_p        (:) = spval !canopy stomatal resistance (s/m)
            allocate (z0m_p        (numpft)) ; z0m_p        (:) = spval !effective roughness [m]
! Plant Hydraulic variables
            allocate (vegwp_p(1:nvegwcs,numpft)); vegwp_p (:,:) = spval
            allocate (gs0sun_p     (numpft)); gs0sun_p      (:) = spval
            allocate (gs0sha_p     (numpft)); gs0sha_p      (:) = spval
! END plant hydraulic variables
! Allocate Ozone Stress Variables
            allocate (o3coefv_sun_p(numpft)) ; o3coefv_sun_p(:) = spval !Ozone stress factor for photosynthesis on sunlit leaf
            allocate (o3coefv_sha_p(numpft)) ; o3coefv_sha_p(:) = spval !Ozone stress factor for photosynthesis on shaded leaf
            allocate (o3coefg_sun_p(numpft)) ; o3coefg_sun_p(:) = spval !Ozone stress factor for stomata on sunlit leaf
            allocate (o3coefg_sha_p(numpft)) ; o3coefg_sha_p(:) = spval !Ozone stress factor for stomata on shaded leaf
            allocate (lai_old_p    (numpft)) ; lai_old_p    (:) = spval !lai in last time step
            allocate (o3uptakesun_p(numpft)) ; o3uptakesun_p(:) = spval !Ozone does, sunlit leaf (mmol O3/m^2)
            allocate (o3uptakesha_p(numpft)) ; o3uptakesha_p(:) = spval !Ozone does, shaded leaf (mmol O3/m^2)
! END allocate Ozone Stress Variables
            allocate (irrig_method_p(numpft)); irrig_method_p(:) = 0! irrigation method

         ENDIF
      ENDIF

#ifdef BGC
      CALL allocate_BGCPFTimeVariables
#endif

   END SUBROUTINE allocate_PFTimeVariables

   SUBROUTINE READ_PFTimeVariables (file_restart)

   USE MOD_Namelist, only: DEF_USE_PLANTHYDRAULICS, DEF_USE_OZONESTRESS, DEF_USE_IRRIGATION
   USE MOD_NetCDFVector
   USE MOD_LandPFT
   USE MOD_Vars_Global

   IMPLICIT NONE

      character(len=*), intent(in) :: file_restart

#ifdef MPAS_EMBEDDED_COLM
      IF (numpft <= 0) RETURN
#endif

      CALL ncio_read_vector (file_restart, 'tleaf_p  ',  landpft, tleaf_p     )
      CALL ncio_read_vector (file_restart, 'ldew_p   ',  landpft, ldew_p      )
      CALL ncio_read_vector (file_restart, 'ldew_rain_p',landpft, ldew_rain_p )
      CALL ncio_read_vector (file_restart, 'ldew_snow_p',landpft, ldew_snow_p )
      CALL ncio_read_vector (file_restart, 'fwet_snow_p',landpft, fwet_snow_p )
      CALL ncio_read_vector (file_restart, 'sigf_p   ',  landpft, sigf_p      )
      CALL ncio_read_vector (file_restart, 'tlai_p   ',  landpft, tlai_p      )
      CALL ncio_read_vector (file_restart, 'lai_p    ',  landpft, lai_p       )
!     CALL ncio_read_vector (file_restart, 'laisun_p ',  landpft, laisun_p    )
!     CALL ncio_read_vector (file_restart, 'laisha_p ',  landpft, laisha_p    )
      CALL ncio_read_vector (file_restart, 'tsai_p   ',  landpft, tsai_p      )
      CALL ncio_read_vector (file_restart, 'sai_p    ',  landpft, sai_p       )
      CALL ncio_read_vector (file_restart, 'ssun_p   ',  2,2, landpft, ssun_p )
      CALL ncio_read_vector (file_restart, 'ssha_p   ',  2,2, landpft, ssha_p )

#ifdef HYPERSPECTRAL
      CALL ncio_read_vector (file_restart, 'ssun_hires_p   ',  211,2, landpft, ssun_hires_p )
      CALL ncio_read_vector (file_restart, 'ssha_hires_p   ',  211,2, landpft, ssha_hires_p )
#endif

      CALL ncio_read_vector (file_restart, 'thermk_p ',  landpft, thermk_p    )
      CALL ncio_read_vector (file_restart, 'fshade_p ',  landpft, fshade_p    )
      CALL ncio_read_vector (file_restart, 'extkb_p  ',  landpft, extkb_p     )
      CALL ncio_read_vector (file_restart, 'extkd_p  ',  landpft, extkd_p     )
      CALL ncio_read_vector (file_restart, 'tref_p   ',  landpft, tref_p      )
      CALL ncio_read_vector (file_restart, 'qref_p   ',  landpft, qref_p      )
      CALL ncio_read_vector (file_restart, 'rst_p    ',  landpft, rst_p       )
      CALL ncio_read_vector (file_restart, 'z0m_p    ',  landpft, z0m_p       )
IF(DEF_USE_PLANTHYDRAULICS)THEN
      CALL ncio_read_vector (file_restart, 'vegwp_p  ',  nvegwcs, landpft, vegwp_p )
      CALL ncio_read_vector (file_restart, 'gs0sun_p ',  landpft, gs0sun_p   )
      CALL ncio_read_vector (file_restart, 'gs0sha_p ',  landpft, gs0sha_p   )
ENDIF
IF(DEF_USE_OZONESTRESS)THEN
      CALL ncio_read_vector (file_restart, 'lai_old_p    ', landpft, lai_old_p    , defval = 0._r8)
      CALL ncio_read_vector (file_restart, 'o3uptakesun_p', landpft, o3uptakesun_p, defval = 0._r8)
      CALL ncio_read_vector (file_restart, 'o3uptakesha_p', landpft, o3uptakesha_p, defval = 0._r8)
ENDIF
IF(DEF_USE_IRRIGATION)THEN
      CALL ncio_read_vector (file_restart,'irrig_method_p', landpft,irrig_method_p, defval = 0)
ENDIF

#ifdef BGC
      CALL read_BGCPFTimeVariables (file_restart)
#endif

   END SUBROUTINE READ_PFTimeVariables

   SUBROUTINE WRITE_PFTimeVariables (file_restart, checkpoint_family_id)

   USE MOD_Namelist, only: DEF_REST_CompressLevel, DEF_USE_PLANTHYDRAULICS, DEF_USE_OZONESTRESS, &
                           DEF_USE_IRRIGATION
   USE MOD_LandPFT
   USE MOD_NetCDFVector
   USE MOD_Vars_Global
#ifdef MPAS_EMBEDDED_COLM
   USE MOD_MPAS_MPI, only: CoLM_stop
#endif
   IMPLICIT NONE

   character(len=*), intent(in) :: file_restart
   character(len=*), intent(in), optional :: checkpoint_family_id

   ! Local variables
   integer :: compress

	      compress = DEF_REST_CompressLevel

#ifdef MPAS_EMBEDDED_COLM
	      IF (numpft <= 0) RETURN
#endif

#ifdef MPAS_EMBEDDED_COLM
	      IF (.not. present(checkpoint_family_id)) THEN
	         CALL CoLM_stop('Embedded CoLM PFT restart write is missing its checkpoint family ID.')
	      ENDIF
	      CALL ncio_create_file_vector(file_restart, landpft, checkpoint_family_id, 'pft')
#else
	      CALL ncio_create_file_vector(file_restart, landpft)
#endif
	      CALL ncio_define_dimension_vector (file_restart, landpft, 'pft')
      CALL ncio_define_dimension_vector (file_restart, landpft, 'band', 2)
      CALL ncio_define_dimension_vector (file_restart, landpft, 'rtyp', 2)

      CALL ncio_define_dimension_vector (file_restart, landpft, 'wavelength', 211)

IF(DEF_USE_PLANTHYDRAULICS)THEN
      CALL ncio_define_dimension_vector (file_restart, landpft, 'vegnodes', nvegwcs)
ENDIF

      CALL ncio_write_vector (file_restart, 'tleaf_p  ', 'pft', landpft, tleaf_p  , compress)
      CALL ncio_write_vector (file_restart, 'ldew_p   ', 'pft', landpft, ldew_p   , compress)
      CALL ncio_write_vector (file_restart, 'ldew_rain_p','pft',landpft,ldew_rain_p,compress)
      CALL ncio_write_vector (file_restart, 'ldew_snow_p','pft',landpft,ldew_snow_p,compress)
      CALL ncio_write_vector (file_restart, 'fwet_snow_p','pft',landpft,fwet_snow_p,compress)
      CALL ncio_write_vector (file_restart, 'sigf_p   ', 'pft', landpft, sigf_p   , compress)
      CALL ncio_write_vector (file_restart, 'tlai_p   ', 'pft', landpft, tlai_p   , compress)
      CALL ncio_write_vector (file_restart, 'lai_p    ', 'pft', landpft, lai_p    , compress)
!     CALL ncio_write_vector (file_restart, 'laisun_p ', 'pft', landpft, laisun_p , compress)
!     CALL ncio_write_vector (file_restart, 'laisha_p ', 'pft', landpft, laisha_p , compress)
      CALL ncio_write_vector (file_restart, 'tsai_p   ', 'pft', landpft, tsai_p   , compress)
      CALL ncio_write_vector (file_restart, 'sai_p    ', 'pft', landpft, sai_p    , compress)
      CALL ncio_write_vector (file_restart, 'ssun_p   ', 'band', 2, 'rtyp', 2, 'pft', landpft, ssun_p, compress)
      CALL ncio_write_vector (file_restart, 'ssha_p   ', 'band', 2, 'rtyp', 2, 'pft', landpft, ssha_p, compress)

#ifdef HYPERSPECTRAL
      CALL ncio_write_vector (file_restart, 'ssun_hires_p', 'wavelength', 211, 'rtyp', 2, 'pft', landpft, ssun_hires_p, compress)
      CALL ncio_write_vector (file_restart, 'ssha_hires_p', 'wavelength', 211, 'rtyp', 2, 'pft', landpft, ssha_hires_p, compress)
#endif

      CALL ncio_write_vector (file_restart, 'thermk_p ', 'pft', landpft, thermk_p , compress)
      CALL ncio_write_vector (file_restart, 'fshade_p ', 'pft', landpft, fshade_p , compress)
      CALL ncio_write_vector (file_restart, 'extkb_p  ', 'pft', landpft, extkb_p  , compress)
      CALL ncio_write_vector (file_restart, 'extkd_p  ', 'pft', landpft, extkd_p  , compress)
      CALL ncio_write_vector (file_restart, 'tref_p   ', 'pft', landpft, tref_p   , compress)
      CALL ncio_write_vector (file_restart, 'qref_p   ', 'pft', landpft, qref_p   , compress)
      CALL ncio_write_vector (file_restart, 'rst_p    ', 'pft', landpft, rst_p    , compress)
      CALL ncio_write_vector (file_restart, 'z0m_p    ', 'pft', landpft, z0m_p    , compress)
IF(DEF_USE_PLANTHYDRAULICS)THEN
      CALL ncio_write_vector (file_restart, 'vegwp_p  ', 'vegnodes', nvegwcs,  'pft', landpft, vegwp_p, compress)
      CALL ncio_write_vector (file_restart, 'gs0sun_p ', 'pft', landpft, gs0sun_p   , compress)
      CALL ncio_write_vector (file_restart, 'gs0sha_p ', 'pft', landpft, gs0sha_p   , compress)
ENDIF
IF(DEF_USE_OZONESTRESS)THEN
      CALL ncio_write_vector (file_restart, 'lai_old_p    ', 'pft', landpft, lai_old_p    , compress)
      CALL ncio_write_vector (file_restart, 'o3uptakesun_p', 'pft', landpft, o3uptakesun_p, compress)
      CALL ncio_write_vector (file_restart, 'o3uptakesha_p', 'pft', landpft, o3uptakesha_p, compress)
ENDIF
IF(DEF_USE_IRRIGATION)THEN
      CALL ncio_write_vector (file_restart,'irrig_method_p','pft', landpft, irrig_method_p, compress)
ENDIF

#ifdef BGC
      CALL WRITE_BGCPFTimeVariables (file_restart)
#endif

   END SUBROUTINE WRITE_PFTimeVariables


   SUBROUTINE deallocate_PFTimeVariables
   !--------------------------------------------------------------------
   ! Deallocates memory for CoLM 1d [numpft/numpc] variables
   !--------------------------------------------------------------------
   USE MOD_MPAS_MPI
   USE MOD_LandPFT

      IF (.true.) THEN
         IF (numpft > 0) THEN
            deallocate (tleaf_p        )  ! leaf temperature [K]
            deallocate (ldew_p         )  ! depth of water on foliage [mm]
            deallocate (ldew_rain_p    )  ! depth of rain on foliage [mm]
            deallocate (ldew_snow_p    )  ! depth of snow on foliage [mm]
            deallocate (fwet_snow_p    )  ! vegetation snow fractional cover [-]
            deallocate (sigf_p         )  ! fraction of veg cover, excluding snow-covered veg [-]
            deallocate (tlai_p         )  ! leaf area index
            deallocate (lai_p          )  ! leaf area index
            deallocate (laisun_p       )  ! leaf area index
            deallocate (laisha_p       )  ! leaf area index
            deallocate (tsai_p         )  ! stem area index
            deallocate (sai_p          )  ! stem area index
            deallocate (ssun_p         )  ! sunlit canopy absorption for solar radiation (0-1)
            deallocate (ssha_p         )  ! shaded canopy absorption for solar radiation (0-1)

#ifdef HYPERSPECTRAL
            deallocate (ssun_hires_p   )  ! sunlit canopy absorption for solar radiation (0-1)
            deallocate (ssha_hires_p   )  ! shaded canopy absorption for solar radiation (0-1)
#endif

            deallocate (thermk_p       )  ! canopy gap fraction for tir radiation
            deallocate (fshade_p       )  ! canopy gap fraction for tir radiation
            deallocate (extkb_p        )  ! (k, g(mu)/mu) direct solar extinction coefficient
            deallocate (extkd_p        )  ! diffuse and scattered diffuse PAR extinction coefficient
            deallocate (tref_p         )  ! 2 m height air temperature [kelvin]
            deallocate (t2m_wmo_p      )  ! 2 m WMO air temperature [kelvin]
            deallocate (qref_p         )  ! 2 m height air specific humidity
            deallocate (rst_p          )  ! canopy stomatal resistance (s/m)
            deallocate (z0m_p          )  ! effective roughness [m]
! Plant Hydraulic variables
            deallocate (vegwp_p        )  ! vegetation water potential [mm]
            deallocate (gs0sun_p       )  ! working copy of sunlit stomata conductance
            deallocate (gs0sha_p       )  ! working copy of shaded stomata conductance
! END plant hydraulic variables
! Ozone Stress variables
            deallocate (o3coefv_sun_p  )  ! Ozone stress factor for photosynthesis on sunlit leaf
            deallocate (o3coefv_sha_p  )  ! Ozone stress factor for photosynthesis on shaded leaf
            deallocate (o3coefg_sun_p  )  ! Ozone stress factor for stomata on sunlit leaf
            deallocate (o3coefg_sha_p  )  ! Ozone stress factor for stomata on shaded leaf
            deallocate (lai_old_p      )  ! lai in last time step
            deallocate (o3uptakesun_p  )  ! Ozone does, sunlit leaf (mmol O3/m^2)
            deallocate (o3uptakesha_p  )  ! Ozone does, shaded leaf (mmol O3/m^2)
            deallocate (irrig_method_p )
! Ozone Stress variables
         ENDIF
      ENDIF

#ifdef BGC
      CALL deallocate_BGCPFTimeVariables
#endif

   END SUBROUTINE deallocate_PFTimeVariables

#ifdef RangeCheck
   SUBROUTINE check_PFTimeVariables

   USE MOD_RangeCheck
   USE MOD_Namelist, only: DEF_USE_PLANTHYDRAULICS, DEF_USE_OZONESTRESS, DEF_USE_IRRIGATION

   IMPLICIT NONE

      CALL check_vector_data ('       tleaf_p', tleaf_p        )
      CALL check_vector_data ('        ldew_p', ldew_p         )
      CALL check_vector_data ('   ldew_rain_p', ldew_rain_p    )
      CALL check_vector_data ('   ldew_snow_p', ldew_snow_p    )
      CALL check_vector_data ('   fwet_snow_p', fwet_snow_p    )
      CALL check_vector_data ('        sigf_p', sigf_p         )
      CALL check_vector_data ('        tlai_p', tlai_p         )
      CALL check_vector_data ('         lai_p', lai_p          )
      CALL check_vector_data ('      laisun_p', lai_p          )
      CALL check_vector_data ('      laisha_p', lai_p          )
      CALL check_vector_data ('        tsai_p', tsai_p         )
      CALL check_vector_data ('         sai_p', sai_p          )
      CALL check_vector_data ('        ssun_p', ssun_p         )
      CALL check_vector_data ('        ssha_p', ssha_p         )
      CALL check_vector_data ('      thermk_p', thermk_p       )
      CALL check_vector_data ('      fshade_p', fshade_p       )
      CALL check_vector_data ('       extkb_p', extkb_p        )
      CALL check_vector_data ('       extkd_p', extkd_p        )
      CALL check_vector_data ('        tref_p', tref_p         )
      CALL check_vector_data ('     t2m_wmo_p', t2m_wmo_p      )
      CALL check_vector_data ('        qref_p', qref_p         )
      CALL check_vector_data ('         rst_p', rst_p          )
      CALL check_vector_data ('         z0m_p', z0m_p          )
IF(DEF_USE_PLANTHYDRAULICS)THEN
      CALL check_vector_data ('       vegwp_p', vegwp_p        )
      CALL check_vector_data ('      gs0sun_p', gs0sun_p       )
      CALL check_vector_data ('      gs0sha_p', gs0sha_p       )
ENDIF
IF(DEF_USE_OZONESTRESS)THEN
      CALL check_vector_data (' o3coefv_sun_p', o3coefv_sun_p  )
      CALL check_vector_data (' o3coefv_sha_p', o3coefv_sha_p  )
      CALL check_vector_data (' o3coefg_sun_p', o3coefg_sun_p  )
      CALL check_vector_data (' o3coefg_sha_p', o3coefg_sha_p  )
      CALL check_vector_data ('     lai_old_p', lai_old_p      )
      CALL check_vector_data (' o3uptakesun_p', o3uptakesun_p  )
      CALL check_vector_data (' o3uptakesha_p', o3uptakesha_p  )
ENDIF
IF(DEF_USE_IRRIGATION)THEN
      CALL check_vector_data ('irrig_method_p', irrig_method_p )
ENDIF

#ifdef BGC
      CALL check_BGCPFTimeVariables
#endif

   END SUBROUTINE check_PFTimeVariables
#endif

END MODULE MOD_Vars_PFTimeVariables
#endif


MODULE MOD_Vars_TimeVariables
! -------------------------------
! Created by Yongjiu Dai, 03/2014
! -------------------------------

   USE MOD_Precision
   USE MOD_TimeManager
#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
   USE MOD_Vars_PFTimeVariables
#endif
#ifdef BGC
   USE MOD_BGC_Vars_TimeVariables
#endif
#ifdef CatchLateralFlow
   USE MOD_Catch_Vars_TimeVariables
#endif
#ifdef GridRiverLakeFlow
   USE MOD_Grid_RiverLakeTimeVars
#endif
#ifdef URBAN_MODEL
   USE MOD_Urban_Vars_TimeVariables
#endif
#ifdef EXTERNAL_LAKE
   USE MOD_Lake_TimeVars
#endif
#ifdef DataAssimilation
   USE MOD_DA_Vars_TimeVariables
#endif

   IMPLICIT NONE
   SAVE
! -----------------------------------------------------------------
! Time-varying state variables which required by restart run
   real(r8), allocatable :: z_sno       (:,:) ! node depth [m]
   real(r8), allocatable :: dz_sno      (:,:) ! interface depth [m]
   real(r8), allocatable :: t_soisno    (:,:) ! soil temperature [K]
   real(r8), allocatable :: wliq_soisno (:,:) ! liquid water in layers [kg/m2]
   real(r8), allocatable :: wice_soisno (:,:) ! ice lens in layers [kg/m2]
   real(r8), allocatable :: h2osoi      (:,:) ! volumetric soil water in layers [m3/m3]
   real(r8), allocatable :: smp         (:,:) ! soil matrix potential [mm]
   real(r8), allocatable :: hk          (:,:) ! hydraulic conductivity [mm h2o/s]
   real(r8), allocatable :: rootr       (:,:) ! transpiration contribution fraction from different layers
   real(r8), allocatable :: rootflux    (:,:) ! water exchange between soil and root. Positive: soil->root [?]
!Plant Hydraulic variables
   real(r8), allocatable :: vegwp       (:,:) ! vegetation water potential [mm]
   real(r8), allocatable :: gs0sun        (:) ! working copy of sunlit stomata conductance
   real(r8), allocatable :: gs0sha        (:) ! working copy of shaded stomata conductance
!END plant hydraulic variables
!Ozone stress variables
   real(r8), allocatable :: o3coefv_sun   (:) ! Ozone stress factor for photosynthesis on sunlit leaf
   real(r8), allocatable :: o3coefv_sha   (:) ! Ozone stress factor for photosynthesis on shaded leaf
   real(r8), allocatable :: o3coefg_sun   (:) ! Ozone stress factor for stomata on sunlit leaf
   real(r8), allocatable :: o3coefg_sha   (:) ! Ozone stress factor for stomata on shaded leaf
   real(r8), allocatable :: lai_old       (:) ! lai in last time step
   real(r8), allocatable :: o3uptakesun   (:) ! Ozone does, sunlit leaf (mmol O3/m^2)
   real(r8), allocatable :: o3uptakesha   (:) ! Ozone does, shaded leaf (mmol O3/m^2)
!END ozone stress variables
   real(r8), allocatable :: rstfacsun_out (:) ! factor of soil water stress on sunlit leaf
   real(r8), allocatable :: rstfacsha_out (:) ! factor of soil water stress on shaded leaf
   real(r8), allocatable :: gssun_out     (:) ! stomata conductance on sunlit leaf
   real(r8), allocatable :: gssha_out     (:) ! stomata conductance on shaded leaf
   real(r8), allocatable :: t_grnd        (:) ! ground surface temperature [K]

   real(r8), allocatable :: assimsun_out  (:) ! diagnostic sunlit leaf assim value for output
   real(r8), allocatable :: assimsha_out  (:) ! diagnostic sunlit leaf etr value for output
   real(r8), allocatable :: etrsun_out    (:) ! diagnostic shaded leaf assim for output
   real(r8), allocatable :: etrsha_out    (:) ! diagnostic shaded leaf etr for output

   real(r8), allocatable :: tleaf         (:) ! leaf temperature [K]
   real(r8), allocatable :: ldew          (:) ! depth of water on foliage [mm]
   real(r8), allocatable :: ldew_rain     (:) ! depth of rain on foliage [mm]
   real(r8), allocatable :: ldew_snow     (:) ! depth of rain on foliage [mm]
   real(r8), allocatable :: fwet_snow     (:) ! vegetation snow fractional cover [-]
   real(r8), allocatable :: sag           (:) ! non dimensional snow age [-]
   real(r8), allocatable :: scv           (:) ! snow cover, water equivalent [mm]
   real(r8), allocatable :: snowdp        (:) ! snow depth [meter]
   real(r8), allocatable :: fveg          (:) ! fraction of vegetation cover
   real(r8), allocatable :: fsno          (:) ! fraction of snow cover on ground
   real(r8), allocatable :: sigf          (:) ! fraction of veg cover, excluding snow-covered veg [-]
   real(r8), allocatable :: green         (:) ! leaf greenness
   real(r8), allocatable :: tlai          (:) ! leaf area index
   real(r8), allocatable :: lai           (:) ! leaf area index
   real(r8), allocatable :: laisun        (:) ! leaf area index for sunlit leaf
   real(r8), allocatable :: laisha        (:) ! leaf area index for shaded leaf
   real(r8), allocatable :: tsai          (:) ! stem area index
   real(r8), allocatable :: sai           (:) ! stem area index
#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
   real(r8), allocatable :: lai_enftemp   (:) ! lai for needleleaf evergreen temperate tree (m2 m-2)
   real(r8), allocatable :: lai_enfboreal (:) ! lai for needleleaf evergreen boreal tree (m2 m-2)
   real(r8), allocatable :: lai_dnfboreal (:) ! lai for needleleaf deciduous boreal tree (m2 m-2)
   real(r8), allocatable :: lai_ebftrop   (:) ! lai for broadleaf evergreen tropical tree (m2 m-2)
   real(r8), allocatable :: lai_ebftemp   (:) ! lai for broadleaf evergreen temperate tree (m2 m-2)
   real(r8), allocatable :: lai_dbftrop   (:) ! lai for broadleaf deciduous tropical tree (m2 m-2)
   real(r8), allocatable :: lai_dbftemp   (:) ! lai for broadleaf deciduous temperate tree (m2 m-2)
   real(r8), allocatable :: lai_dbfboreal (:) ! lai for broadleaf deciduous boreal tree (m2 m-2)
   real(r8), allocatable :: lai_ebstemp   (:) ! lai for broadleaf evergreen temperate shrub (m2 m-2)
   real(r8), allocatable :: lai_dbstemp   (:) ! lai for broadleaf deciduous temperate shrub (m2 m-2)
   real(r8), allocatable :: lai_dbsboreal (:) ! lai for broadleaf deciduous boreal shrub (m2 m-2)
   real(r8), allocatable :: lai_c3arcgrass(:) ! lai for c3 arctic grass (m2 m-2)
   real(r8), allocatable :: lai_c3grass   (:) ! lai for c3 grass (m2 m-2)
   real(r8), allocatable :: lai_c4grass   (:) ! lai for c4 grass (m2 m-2)
#endif

   real(r8), allocatable :: coszen        (:) ! cosine of solar zenith angle
   real(r8), allocatable :: alb       (:,:,:) ! averaged albedo [-]
   real(r8), allocatable :: ssun      (:,:,:) ! sunlit canopy absorption for solar radiation (0-1)
   real(r8), allocatable :: ssha      (:,:,:) ! shaded canopy absorption for solar radiation (0-1)
   real(r8), allocatable :: ssoi      (:,:,:) ! soil absorption for solar radiation (0-1)
   real(r8), allocatable :: ssno      (:,:,:) ! snow absorption for solar radiation (0-1)
#ifdef HYPERSPECTRAL
   ! new variables for hyperspectral scheme
   real(r8), allocatable :: alb_hires (:,:,:) ! averaged albedo [-]
   real(r8), allocatable :: reflectance_out   (:,:,:) ! averaged albedo [-]
   real(r8), allocatable :: transmittance_out (:,:,:) ! averaged albedo [-]
#endif
   real(r8), allocatable :: thermk        (:) ! canopy gap fraction for tir radiation
   real(r8), allocatable :: extkb         (:) ! (k, g(mu)/mu) direct solar extinction coefficient
   real(r8), allocatable :: extkd         (:) ! diffuse and scattered diffuse PAR extinction coefficient
   real(r8), allocatable :: zwt           (:) ! the depth to water table [m]
   real(r8), allocatable :: wa            (:) ! water storage in aquifer [mm]
   real(r8), allocatable :: wetwat        (:) ! water storage in wetland [mm]
   real(r8), allocatable :: wat           (:) ! total water storage [mm]
   real(r8), allocatable :: wdsrf         (:) ! depth of surface water [mm]
   real(r8), allocatable :: rss           (:) ! soil surface resistance [s/m]

   real(r8), allocatable :: t_lake      (:,:) ! lake layer temperature [K]
   real(r8), allocatable :: lake_icefrac(:,:) ! lake mass fraction of lake layer that is frozen
   real(r8), allocatable :: savedtke1     (:) ! top level eddy conductivity (W/m K)

   real(r8), allocatable :: snw_rds     (:,:) ! effective grain radius (col,lyr) [microns, m-6]
   real(r8), allocatable :: mss_bcpho   (:,:) ! mass of hydrophobic BC in snow  (col,lyr) [kg]
   real(r8), allocatable :: mss_bcphi   (:,:) ! mass of hydrophillic BC in snow (col,lyr) [kg]
   real(r8), allocatable :: mss_ocpho   (:,:) ! mass of hydrophobic OC in snow  (col,lyr) [kg]
   real(r8), allocatable :: mss_ocphi   (:,:) ! mass of hydrophillic OC in snow (col,lyr) [kg]
   real(r8), allocatable :: mss_dst1    (:,:) ! mass of dust species 1 in snow  (col,lyr) [kg]
   real(r8), allocatable :: mss_dst2    (:,:) ! mass of dust species 2 in snow  (col,lyr) [kg]
   real(r8), allocatable :: mss_dst3    (:,:) ! mass of dust species 3 in snow  (col,lyr) [kg]
   real(r8), allocatable :: mss_dst4    (:,:) ! mass of dust species 4 in snow  (col,lyr) [kg]
   real(r8), allocatable :: ssno_lyr(:,:,:,:) ! snow layer absorption [-]

   real(r8), allocatable :: trad          (:) ! radiative temperature of surface [K]
	   real(r8), allocatable :: tref          (:) ! 2 m height air temperature [kelvin]
	   real(r8), allocatable :: t2m_wmo       (:) ! 2 m WMO air temperature [kelvin]
	   real(r8), allocatable :: qref          (:) ! 2 m height air specific humidity
	   real(r8), allocatable :: qsfc          (:) ! bulk surface/ground specific humidity [kg/kg]
	   real(r8), allocatable :: rst           (:) ! canopy stomatal resistance (s/m)
   real(r8), allocatable :: emis          (:) ! averaged bulk surface emissivity
   real(r8), allocatable :: z0m           (:) ! effective roughness [m]
   real(r8), allocatable :: displa        (:) ! zero displacement height [m]
   real(r8), allocatable :: zol           (:) ! dimensionless height (z/L) used in Monin-Obukhov theory
   real(r8), allocatable :: rib           (:) ! bulk Richardson number in surface layer
   real(r8), allocatable :: ustar         (:) ! u* in similarity theory [m/s]
   real(r8), allocatable :: qstar         (:) ! q* in similarity theory [kg/kg]
   real(r8), allocatable :: tstar         (:) ! t* in similarity theory [K]
   real(r8), allocatable :: fm            (:) ! integral of profile FUNCTION for momentum
   real(r8), allocatable :: fh            (:) ! integral of profile FUNCTION for heat
   real(r8), allocatable :: fq            (:) ! integral of profile FUNCTION for moisture
   real(r8), allocatable :: fh2m          (:) ! relation for temperature at 2 m
   real(r8), allocatable :: fq2m          (:) ! relation for specific humidity at 2 m
   real(r8), allocatable :: fm10m         (:) ! integral of profile FUNCTION for momentum at 10 m
   real(r8), allocatable :: diag_u10m     (:) ! 10 m zonal wind derived at the PFT level
   real(r8), allocatable :: diag_v10m     (:) ! 10 m meridional wind derived at the PFT level
   real(r8), allocatable :: diag_chs2     (:) ! 2 m heat exchange velocity
   real(r8), allocatable :: diag_cqs2     (:) ! 2 m moisture exchange velocity
   real(r8), allocatable :: diag_cd10     (:) ! 10 m momentum coefficient

   real(r8), allocatable :: irrig_rate           (:) ! irrigation rate [mm s-1]
   real(r8), allocatable :: actual_irrig         (:) ! actual irrigation amount [kg/m2]
   real(r8), allocatable :: deficit_irrig        (:) ! irrigation amount [kg/m2]
   real(r8), allocatable :: sum_irrig            (:) ! total irrigation amount [kg/m2]
   real(r8), allocatable :: sum_deficit_irrig    (:) ! total irrigation amount demand [kg/m2]
   real(r8), allocatable :: sum_irrig_count      (:) ! total irrigation counts [-]
   integer , allocatable :: n_irrig_steps_left   (:) ! left steps for once irrigation [-]
   real(r8), allocatable :: waterstorage         (:) ! water of water storage pool (from reservoir and river) [kg/m2]
   real(r8), allocatable :: waterstorage_supply  (:) ! irrigation supply from water storage pool [kg/m2]
   real(r8), allocatable :: groundwater_demand   (:) ! irrigation demand for ground water [kg/m2]
   real(r8), allocatable :: groundwater_supply   (:) ! irrigation supply from ground water [kg/m2]
   real(r8), allocatable :: reservoirriver_demand(:)! irrigation demand for reservoir or river [kg/m2]
   real(r8), allocatable :: reservoirriver_supply(:)! irrigation supply from reservoir or river [kg/m2]
   real(r8), allocatable :: reservoir_supply     (:)! irrigation supply from reservoir [kg/m2]
   real(r8), allocatable :: river_supply         (:)! irrigation supply from river [kg/m2]
   real(r8), allocatable :: runoff_supply        (:)! irrigation supply from runoff [kg/m2]
   real(r8), allocatable :: tairday                       (:) ! daily mean temperature [degree C]
   real(r8), allocatable :: usday                         (:) ! daily mean wind component in eastward direction [m/s]
   real(r8), allocatable :: vsday                         (:) ! daily mean wind component in northward direction [m/s]
   real(r8), allocatable :: pairday                       (:) ! daily mean pressure [kPa]
   real(r8), allocatable :: rnetday                       (:) ! daily net radiation flux [MJ/m2/day]
   real(r8), allocatable :: fgrndday                      (:) ! daily ground heat flux [MJ/m2/day]
   real(r8), allocatable :: potential_evapotranspiration  (:) ! daily potential evapotranspiration [mm/day]

   integer , allocatable :: irrig_method_corn      (:) ! irrigation method for corn (0-3)
   integer , allocatable :: irrig_method_swheat    (:) ! irrigation method for spring wheat (0-3)
   integer , allocatable :: irrig_method_wwheat    (:) ! irrigation method for winter wheat (0-3)
   integer , allocatable :: irrig_method_soybean   (:) ! irrigation method for soybean (0-3)
   integer , allocatable :: irrig_method_cotton    (:) ! irrigation method for cotton (0-3)
   integer , allocatable :: irrig_method_rice1     (:) ! irrigation method for rice1 (0-3)
   integer , allocatable :: irrig_method_rice2     (:) ! irrigation method for rice2 (0-3)
   integer , allocatable :: irrig_method_sugarcane (:) ! irrigation method for sugarcane (0-3)

   real(r8), allocatable :: irrig_gw_alloc         (:) ! irrigation demand allocated to groundwater [kg/kg]
   real(r8), allocatable :: irrig_sw_alloc         (:) ! irrigation demand allocated to surfacewater [kg/kg]
   real(r8), allocatable :: zwt_stand              (:) ! initial the depth to water table [m]

   ! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: allocate_TimeVariables
   PUBLIC :: deallocate_TimeVariables
   PUBLIC :: READ_TimeVariables
   PUBLIC :: WRITE_TimeVariables
#ifdef RangeCheck
   PUBLIC :: check_TimeVariables
#endif


!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------

   SUBROUTINE allocate_TimeVariables
   !--------------------------------------------------------------------
   ! Allocates memory for CoLM 1d [numpatch] variables
   !--------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_Vars_Global
   USE MOD_MPAS_MPI
   USE MOD_LandPatch, only: numpatch
   IMPLICIT NONE


      IF (.true.) THEN

         IF (numpatch > 0) THEN

            allocate (z_sno      (maxsnl+1:0,      numpatch)); z_sno       (:,:) = spval
            allocate (dz_sno     (maxsnl+1:0,      numpatch)); dz_sno      (:,:) = spval
            allocate (t_soisno   (maxsnl+1:nl_soil,numpatch)); t_soisno    (:,:) = spval
            allocate (wliq_soisno(maxsnl+1:nl_soil,numpatch)); wliq_soisno (:,:) = spval
            allocate (wice_soisno(maxsnl+1:nl_soil,numpatch)); wice_soisno (:,:) = spval
            allocate (smp               (1:nl_soil,numpatch)); smp         (:,:) = spval
            allocate (hk                (1:nl_soil,numpatch)); hk          (:,:) = spval
            allocate (h2osoi            (1:nl_soil,numpatch)); h2osoi      (:,:) = spval
            allocate (rootr             (1:nl_soil,numpatch)); rootr       (:,:) = spval
            allocate (rootflux          (1:nl_soil,numpatch)); rootflux    (:,:) = spval
!Plant Hydraulic variables
            allocate (vegwp             (1:nvegwcs,numpatch)); vegwp       (:,:) = spval
            allocate (gs0sun                      (numpatch)); gs0sun        (:) = spval
            allocate (gs0sha                      (numpatch)); gs0sha        (:) = spval
!END plant hydraulic variables
!Ozone Stress variables
            allocate (o3coefv_sun                 (numpatch)); o3coefv_sun   (:) = spval
            allocate (o3coefv_sha                 (numpatch)); o3coefv_sha   (:) = spval
            allocate (o3coefg_sun                 (numpatch)); o3coefg_sun   (:) = spval
            allocate (o3coefg_sha                 (numpatch)); o3coefg_sha   (:) = spval
            allocate (lai_old                     (numpatch)); lai_old       (:) = spval
            allocate (o3uptakesun                 (numpatch)); o3uptakesun   (:) = spval
            allocate (o3uptakesha                 (numpatch)); o3uptakesha   (:) = spval
!END ozone stress variables

            allocate (rstfacsun_out               (numpatch)); rstfacsun_out (:) = spval
            allocate (rstfacsha_out               (numpatch)); rstfacsha_out (:) = spval
            allocate (gssun_out                   (numpatch)); gssun_out     (:) = spval
            allocate (gssha_out                   (numpatch)); gssha_out     (:) = spval
            allocate (assimsun_out                (numpatch)); assimsun_out  (:) = spval
            allocate (assimsha_out                (numpatch)); assimsha_out  (:) = spval
            allocate (etrsun_out                  (numpatch)); etrsun_out    (:) = spval
            allocate (etrsha_out                  (numpatch)); etrsha_out    (:) = spval

            allocate (t_grnd                      (numpatch)); t_grnd        (:) = spval
            allocate (tleaf                       (numpatch)); tleaf         (:) = spval
            allocate (ldew                        (numpatch)); ldew          (:) = spval
            allocate (ldew_rain                   (numpatch)); ldew_rain     (:) = spval
            allocate (ldew_snow                   (numpatch)); ldew_snow     (:) = spval
            allocate (fwet_snow                   (numpatch)); fwet_snow     (:) = spval
            allocate (sag                         (numpatch)); sag           (:) = spval
            allocate (scv                         (numpatch)); scv           (:) = spval
            allocate (snowdp                      (numpatch)); snowdp        (:) = spval
            allocate (fveg                        (numpatch)); fveg          (:) = spval
            allocate (fsno                        (numpatch)); fsno          (:) = spval
            allocate (sigf                        (numpatch)); sigf          (:) = spval
            allocate (green                       (numpatch)); green         (:) = spval
            allocate (tlai                        (numpatch)); tlai          (:) = spval
            allocate (lai                         (numpatch)); lai           (:) = spval
            allocate (laisun                      (numpatch)); laisun        (:) = spval
            allocate (laisha                      (numpatch)); laisha        (:) = spval
            allocate (tsai                        (numpatch)); tsai          (:) = spval
            allocate (sai                         (numpatch)); sai           (:) = spval

#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
            allocate (lai_enftemp                 (numpatch)); lai_enftemp   (:) = spval
            allocate (lai_enfboreal               (numpatch)); lai_enfboreal (:) = spval
            allocate (lai_dnfboreal               (numpatch)); lai_dnfboreal (:) = spval
            allocate (lai_ebftrop                 (numpatch)); lai_ebftrop   (:) = spval
            allocate (lai_ebftemp                 (numpatch)); lai_ebftemp   (:) = spval
            allocate (lai_dbftrop                 (numpatch)); lai_dbftrop   (:) = spval
            allocate (lai_dbftemp                 (numpatch)); lai_dbftemp   (:) = spval
            allocate (lai_dbfboreal               (numpatch)); lai_dbfboreal (:) = spval
            allocate (lai_ebstemp                 (numpatch)); lai_ebstemp   (:) = spval
            allocate (lai_dbstemp                 (numpatch)); lai_dbstemp   (:) = spval
            allocate (lai_dbsboreal               (numpatch)); lai_dbsboreal (:) = spval
            allocate (lai_c3arcgrass              (numpatch)); lai_c3arcgrass(:) = spval
            allocate (lai_c3grass                 (numpatch)); lai_c3grass   (:) = spval
            allocate (lai_c4grass                 (numpatch)); lai_c4grass   (:) = spval
#endif
            allocate (coszen                      (numpatch)); coszen        (:) = spval
            allocate (alb                     (2,2,numpatch)); alb       (:,:,:) = spval
            allocate (ssun                    (2,2,numpatch)); ssun      (:,:,:) = spval
            allocate (ssha                    (2,2,numpatch)); ssha      (:,:,:) = spval
            allocate (ssoi                    (2,2,numpatch)); ssoi      (:,:,:) = spval
            allocate (ssno                    (2,2,numpatch)); ssno      (:,:,:) = spval
#ifdef HYPERSPECTRAL
            ! high resolution parameters
            allocate (alb_hires             (211,2,numpatch)); alb_hires        (:,:,:) = spval
            allocate (reflectance_out       (211,0:15,numpatch)); reflectance_out  (:,:,:) = spval
            allocate (transmittance_out     (211,0:15,numpatch)); transmittance_out(:,:,:) = spval
#endif
            allocate (thermk                      (numpatch)); thermk        (:) = spval
            allocate (extkb                       (numpatch)); extkb         (:) = spval
            allocate (extkd                       (numpatch)); extkd         (:) = spval
            allocate (zwt                         (numpatch)); zwt           (:) = spval
            allocate (wa                          (numpatch)); wa            (:) = spval
            allocate (wetwat                      (numpatch)); wetwat        (:) = spval
            allocate (wat                         (numpatch)); wat           (:) = spval
            allocate (wdsrf                       (numpatch)); wdsrf         (:) = spval
            allocate (rss                         (numpatch)); rss           (:) = spval
            allocate (t_lake              (nl_lake,numpatch)); t_lake      (:,:) = spval
            allocate (lake_icefrac        (nl_lake,numpatch)); lake_icefrac(:,:) = spval
            allocate (savedtke1                   (numpatch)); savedtke1     (:) = spval

            allocate (snw_rds          (maxsnl+1:0,numpatch)); snw_rds     (:,:) = spval
            allocate (mss_bcpho        (maxsnl+1:0,numpatch)); mss_bcpho   (:,:) = spval
            allocate (mss_bcphi        (maxsnl+1:0,numpatch)); mss_bcphi   (:,:) = spval
            allocate (mss_ocpho        (maxsnl+1:0,numpatch)); mss_ocpho   (:,:) = spval
            allocate (mss_ocphi        (maxsnl+1:0,numpatch)); mss_ocphi   (:,:) = spval
            allocate (mss_dst1         (maxsnl+1:0,numpatch)); mss_dst1    (:,:) = spval
            allocate (mss_dst2         (maxsnl+1:0,numpatch)); mss_dst2    (:,:) = spval
            allocate (mss_dst3         (maxsnl+1:0,numpatch)); mss_dst3    (:,:) = spval
            allocate (mss_dst4         (maxsnl+1:0,numpatch)); mss_dst4    (:,:) = spval
            allocate (ssno_lyr     (2,2,maxsnl+1:1,numpatch)); ssno_lyr(:,:,:,:) = spval

            allocate (trad                        (numpatch)); trad          (:) = spval
	            allocate (tref                        (numpatch)); tref          (:) = spval
	            allocate (t2m_wmo                     (numpatch)); t2m_wmo       (:) = spval
	            allocate (qref                        (numpatch)); qref          (:) = spval
	            allocate (qsfc                        (numpatch)); qsfc          (:) = spval
	            allocate (rst                         (numpatch)); rst           (:) = spval
            allocate (emis                        (numpatch)); emis          (:) = spval
            allocate (z0m                         (numpatch)); z0m           (:) = spval
            allocate (displa                      (numpatch)); displa        (:) = spval
            allocate (zol                         (numpatch)); zol           (:) = spval
            allocate (rib                         (numpatch)); rib           (:) = spval
            allocate (ustar                       (numpatch)); ustar         (:) = spval
            allocate (qstar                       (numpatch)); qstar         (:) = spval
            allocate (tstar                       (numpatch)); tstar         (:) = spval
            allocate (fm                          (numpatch)); fm            (:) = spval
            allocate (fh                          (numpatch)); fh            (:) = spval
            allocate (fq                          (numpatch)); fq            (:) = spval
            allocate (fh2m                        (numpatch)); fh2m          (:) = spval
            allocate (fq2m                        (numpatch)); fq2m          (:) = spval
            allocate (fm10m                       (numpatch)); fm10m         (:) = spval
            allocate (diag_u10m                   (numpatch)); diag_u10m     (:) = spval
            allocate (diag_v10m                   (numpatch)); diag_v10m     (:) = spval
            allocate (diag_chs2                   (numpatch)); diag_chs2     (:) = spval
            allocate (diag_cqs2                   (numpatch)); diag_cqs2     (:) = spval
            allocate (diag_cd10                   (numpatch)); diag_cd10     (:) = spval

            allocate ( irrig_rate                 (numpatch)); irrig_rate             (:) = spval
            allocate ( deficit_irrig              (numpatch)); deficit_irrig          (:) = spval
            allocate ( actual_irrig               (numpatch)); actual_irrig           (:) = spval
            allocate ( sum_irrig                  (numpatch)); sum_irrig              (:) = spval
            allocate ( sum_deficit_irrig          (numpatch)); sum_deficit_irrig      (:) = spval
            allocate ( sum_irrig_count            (numpatch)); sum_irrig_count        (:) = spval
            allocate ( n_irrig_steps_left         (numpatch)); n_irrig_steps_left     (:) = spval_i4
            allocate ( waterstorage               (numpatch)); waterstorage           (:) = spval
            allocate ( waterstorage_supply        (numpatch)); waterstorage_supply    (:) = spval
            allocate ( groundwater_demand         (numpatch)); groundwater_demand     (:) = spval
            allocate ( groundwater_supply         (numpatch)); groundwater_supply     (:) = spval
            allocate ( reservoirriver_demand      (numpatch)); reservoirriver_demand  (:) = spval
            allocate ( reservoirriver_supply      (numpatch)); reservoirriver_supply  (:) = spval
            allocate ( reservoir_supply           (numpatch)); reservoir_supply       (:) = spval
            allocate ( river_supply               (numpatch)); river_supply           (:) = spval
            allocate ( runoff_supply              (numpatch)); runoff_supply          (:) = spval
            allocate ( tairday                    (numpatch)); tairday                (:) = spval
            allocate ( usday                      (numpatch)); usday                  (:) = spval
            allocate ( vsday                      (numpatch)); vsday                  (:) = spval
            allocate ( pairday                    (numpatch)); pairday                (:) = spval
            allocate ( rnetday                    (numpatch)); rnetday                (:) = spval
            allocate ( fgrndday                   (numpatch)); fgrndday               (:) = spval
            allocate ( potential_evapotranspiration(numpatch)); potential_evapotranspiration(:) = spval

            allocate ( irrig_method_corn          (numpatch)); irrig_method_corn      (:) = spval_i4
            allocate ( irrig_method_swheat        (numpatch)); irrig_method_swheat    (:) = spval_i4
            allocate ( irrig_method_wwheat        (numpatch)); irrig_method_wwheat    (:) = spval_i4
            allocate ( irrig_method_soybean       (numpatch)); irrig_method_soybean   (:) = spval_i4
            allocate ( irrig_method_cotton        (numpatch)); irrig_method_cotton    (:) = spval_i4
            allocate ( irrig_method_rice1         (numpatch)); irrig_method_rice1     (:) = spval_i4
            allocate ( irrig_method_rice2         (numpatch)); irrig_method_rice2     (:) = spval_i4
            allocate ( irrig_method_sugarcane     (numpatch)); irrig_method_sugarcane (:) = spval_i4

            allocate ( irrig_gw_alloc             (numpatch)); irrig_gw_alloc         (:) = spval
            allocate ( irrig_sw_alloc             (numpatch)); irrig_sw_alloc         (:) = spval
            allocate ( zwt_stand                  (numpatch)); zwt_stand              (:) = spval
         ENDIF
      ENDIF

#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
      CALL allocate_PFTimeVariables
#endif

#ifdef BGC
      CALL allocate_BGCTimeVariables
#endif

#ifdef CatchLateralFlow
      CALL allocate_CatchTimeVariables
#endif

#ifdef GridRiverLakeFlow
      CALL allocate_GridRiverLakeTimeVars
#endif

#ifdef URBAN_MODEL
      CALL allocate_UrbanTimeVariables
#endif

#ifdef EXTERNAL_LAKE
      CALL allocate_LakeTimeVars
#endif

#ifdef DataAssimilation
      CALL allocate_DATimeVariables
#endif

   END SUBROUTINE allocate_TimeVariables



   SUBROUTINE deallocate_TimeVariables ()

   USE MOD_MPAS_MPI
   USE MOD_LandPatch, only: numpatch
   IMPLICIT NONE

   !--------------------------------------------------------------------
   ! Deallocates memory for CoLM 1d [numpatch] variables
   !--------------------------------------------------------------------

      IF (.true.) THEN

         IF (numpatch > 0) THEN

            deallocate (z_sno                  )
            deallocate (dz_sno                 )
            deallocate (t_soisno               )
            deallocate (wliq_soisno            )
            deallocate (wice_soisno            )
            deallocate (smp                    )
            deallocate (hk                     )
            deallocate (h2osoi                 )
            deallocate (rootr                  )
            deallocate (rootflux               )
!Plant Hydraulic variables
            deallocate (vegwp                  )
            deallocate (gs0sun                 )
            deallocate (gs0sha                 )
!END plant hydraulic variables
!Ozone stress variables
            deallocate (o3coefv_sun            ) ! Ozone stress factor for photosynthesis on sunlit leaf
            deallocate (o3coefv_sha            ) ! Ozone stress factor for photosynthesis on shaded leaf
            deallocate (o3coefg_sun            ) ! Ozone stress factor for stomata on sunlit leaf
            deallocate (o3coefg_sha            ) ! Ozone stress factor for stomata on shaded leaf
            deallocate (lai_old                ) ! lai in last time step
            deallocate (o3uptakesun            ) ! Ozone does, sunlit leaf (mmol O3/m^2)
            deallocate (o3uptakesha            ) ! Ozone does, shaded leaf (mmol O3/m^2)
!END Ozone stress variables
            deallocate (rstfacsun_out          )
            deallocate (rstfacsha_out          )
            deallocate (gssun_out              )
            deallocate (gssha_out              )
            deallocate (assimsun_out           )
            deallocate (assimsha_out           )
            deallocate (etrsun_out             )
            deallocate (etrsha_out             )

            deallocate (t_grnd                 )
            deallocate (tleaf                  )
            deallocate (ldew                   )
            deallocate (ldew_rain              )
            deallocate (ldew_snow              )
            deallocate (fwet_snow              )
            deallocate (sag                    )
            deallocate (scv                    )
            deallocate (snowdp                 )
            deallocate (fveg                   )
            deallocate (fsno                   )
            deallocate (sigf                   )
            deallocate (green                  )
            deallocate (tlai                   )
            deallocate (lai                    )
            deallocate (laisun                 )
            deallocate (laisha                 )
            deallocate (tsai                   )
            deallocate (sai                    )
#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
            deallocate (lai_enftemp            )
            deallocate (lai_enfboreal          )
            deallocate (lai_dnfboreal          )
            deallocate (lai_ebftrop            )
            deallocate (lai_ebftemp            )
            deallocate (lai_dbftrop            )
            deallocate (lai_dbftemp            )
            deallocate (lai_dbfboreal          )
            deallocate (lai_ebstemp            )
            deallocate (lai_dbstemp            )
            deallocate (lai_dbsboreal          )
            deallocate (lai_c3arcgrass         )
            deallocate (lai_c3grass            )
            deallocate (lai_c4grass            )
#endif
            deallocate (coszen                 )
            deallocate (alb                    )
#ifdef HYPERSPECTRAL
            deallocate (alb_hires              )
            deallocate (reflectance_out        )
            deallocate (transmittance_out      )
#endif
            deallocate (ssun                   )
            deallocate (ssha                   )
            deallocate (ssoi                   )
            deallocate (ssno                   )
            deallocate (thermk                 )
            deallocate (extkb                  )
            deallocate (extkd                  )
            deallocate (zwt                    )
            deallocate (wa                     )
            deallocate (wetwat                 )
            deallocate (wat                    )
            deallocate (wdsrf                  )
            deallocate (rss                    )

            deallocate (t_lake                 ) ! new lake scheme
            deallocate (lake_icefrac           ) ! new lake scheme
            deallocate (savedtke1              ) ! new lake scheme

            deallocate (snw_rds                )
            deallocate (mss_bcpho              )
            deallocate (mss_bcphi              )
            deallocate (mss_ocpho              )
            deallocate (mss_ocphi              )
            deallocate (mss_dst1               )
            deallocate (mss_dst2               )
            deallocate (mss_dst3               )
            deallocate (mss_dst4               )
            deallocate (ssno_lyr               )

            deallocate (trad                   )
	            deallocate (tref                   )
	            deallocate (t2m_wmo                )
	            deallocate (qref                   )
	            deallocate (qsfc                   )
	            deallocate (rst                    )
            deallocate (emis                   )
            deallocate (z0m                    )
            deallocate (displa                 )
            deallocate (zol                    )
            deallocate (rib                    )
            deallocate (ustar                  )
            deallocate (qstar                  )
            deallocate (tstar                  )
            deallocate (fm                     )
            deallocate (fh                     )
            deallocate (fq                     )
            deallocate (fh2m                   )
            deallocate (fq2m                   )
            deallocate (fm10m                  )
            deallocate (diag_u10m              )
            deallocate (diag_v10m              )
            deallocate (diag_chs2              )
            deallocate (diag_cqs2              )
            deallocate (diag_cd10              )

            deallocate (irrig_rate             )
            deallocate (deficit_irrig          )
            deallocate (actual_irrig           )
            deallocate (sum_irrig              )
            deallocate (sum_deficit_irrig      )
            deallocate (sum_irrig_count        )
            deallocate (n_irrig_steps_left     )
            deallocate (waterstorage           )
            deallocate (waterstorage_supply    )
            deallocate (groundwater_demand     )
            deallocate (groundwater_supply     )
            deallocate (reservoirriver_demand  )
            deallocate (reservoirriver_supply  )
            deallocate (reservoir_supply       )
            deallocate (river_supply           )
            deallocate (runoff_supply          )
            deallocate (tairday                )
            deallocate (usday                  )
            deallocate (vsday                  )
            deallocate (pairday                )
            deallocate (rnetday                )
            deallocate (fgrndday               )
            deallocate (potential_evapotranspiration)

            deallocate (irrig_method_corn      )
            deallocate (irrig_method_swheat    )
            deallocate (irrig_method_wwheat    )
            deallocate (irrig_method_soybean   )
            deallocate (irrig_method_cotton    )
            deallocate (irrig_method_rice1     )
            deallocate (irrig_method_rice2     )
            deallocate (irrig_method_sugarcane )

            deallocate (irrig_gw_alloc         )
            deallocate (irrig_sw_alloc         )
            deallocate (zwt_stand              )

         ENDIF
      ENDIF

#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
      CALL deallocate_PFTimeVariables
#endif

#if (defined BGC)
      CALL deallocate_BGCTimeVariables
#endif

#ifdef CatchLateralFlow
      CALL deallocate_CatchTimeVariables
#endif

#ifdef GridRiverLakeFlow
      CALL deallocate_GridRiverLakeTimeVars
#endif

#if (defined URBAN_MODEL)
      CALL deallocate_UrbanTimeVariables
#endif

#ifdef EXTERNAL_LAKE
      CALL deallocate_LakeTimeVars
#endif

#ifdef DataAssimilation
      CALL deallocate_DATimeVariables
#endif

   END SUBROUTINE deallocate_TimeVariables


   !---------------------------------------
   FUNCTION save_to_restart (idate, deltim, itstamp, ptstamp, etstamp) result(rwrite)

   USE MOD_Namelist
   IMPLICIT NONE

   logical :: rwrite

   integer,  intent(in) :: idate(3)
   real(r8), intent(in) :: deltim
   type(timestamp), intent(in) :: itstamp, ptstamp, etstamp


      ! added by yuan, 08/31/2014
      SELECTCASE (trim(adjustl(DEF_WRST_FREQ)))
      CASE ('TIMESTEP')
         rwrite = .true.
      CASE ('HOURLY')
         rwrite = isendofhour (idate, deltim)
      CASE ('DAILY')
         rwrite = isendofday(idate, deltim)
      CASE ('MONTHLY')
         rwrite = isendofmonth(idate, deltim)
      CASE ('YEARLY')
         rwrite = isendofyear(idate, deltim)
      CASE default
         rwrite = .false.
         write(*,*) 'Warning: Please USE one of TIMESTEP/HOURLY/DAILY/MONTHLY/YEARLY for restart frequency.'
         write(*,*) '         Set to FALSE by default.                                                     '
      ENDSELECT

      IF (rwrite) THEN
         rwrite = ((ptstamp <= itstamp) .or. isendofyear(idate,deltim))
      ENDIF

      rwrite = rwrite .or. (.not. (itstamp < etstamp))

   END FUNCTION save_to_restart


#ifdef MPAS_EMBEDDED_COLM
   SUBROUTINE configure_mpas_restart_identity(site)

   USE MOD_NetCDFVector, only: ncio_require_distributed_identity
   IMPLICIT NONE

   character(len=*), intent(in) :: site

      CALL ncio_require_distributed_identity(site)

   END SUBROUTINE configure_mpas_restart_identity
#endif

   SUBROUTINE WRITE_TimeVariables (idate, lc_year, site, dir_restart)

   !====================================================================
   ! Original version: Yongjiu Dai, September 15, 1999, 03/2014
   !====================================================================

   USE MOD_MPAS_MPI
   USE MOD_Namelist, only: DEF_REST_CompressLevel, DEF_USE_PLANTHYDRAULICS, DEF_USE_OZONESTRESS, &
                           DEF_USE_IRRIGATION, DEF_USE_Dynamic_Lake, SITE_landtype
	   USE MOD_LandPatch
	   USE MOD_NetCDFVector
	   USE MOD_Utils, only: make_directory
   USE MOD_Vars_Global
   USE MOD_Vars_TimeInvariants, only: dz_lake
   USE MOD_Const_LC, only: patchtypes
   IMPLICIT NONE

   integer, intent(in) :: idate(3)
   integer, intent(in) :: lc_year      !year of land cover type data
   character(len=*), intent(in) :: site
   character(len=*), intent(in) :: dir_restart

   ! Local variables
	   character(len=:), allocatable :: file_restart
	   character(len=:), allocatable :: file_restart_vector
#ifdef MPAS_EMBEDDED_COLM
	   character(len=:), allocatable :: file_restart_pft_manifest
	   character(len=:), allocatable :: file_restart_gridriver_manifest
	   character(len=96) :: checkpoint_family_id
	   logical :: write_pft_restart
#endif
#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
#ifdef MPAS_EMBEDDED_COLM
	   character(len=:), allocatable :: file_restart_pft
#endif
#endif
	   character(len=14)  :: cdate
	   character(len=4) :: cyear         !character for lc_year
   integer :: compress

      compress = DEF_REST_CompressLevel

      ! land cover type year
      write(cyear,'(i4.4)') lc_year
	      write(cdate,'(i4.4,"-",i3.3,"-",i5.5)') idate(1), idate(2), idate(3)

#ifdef MPAS_EMBEDDED_COLM
	      CALL configure_mpas_restart_identity(site)
	      CALL ncio_create_checkpoint_family_id(checkpoint_family_id)
	      file_restart_pft_manifest = 'NONE'
	      file_restart_gridriver_manifest = 'NONE'
	      write_pft_restart = .false.
#endif

      IF (mpas_is_root) THEN
         CALL make_directory(trim(dir_restart)//'/'//trim(cdate))
      ENDIF
#ifdef MPAS_MPI
      CALL mpi_barrier (mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('time-variable restart directory creation')
#endif

	      file_restart = trim(dir_restart)// '/'//trim(cdate)//'/' // trim(site) //'_restart_'//trim(cdate)//'_lc'//trim(cyear)//'.nc'
	      file_restart_vector = file_restart
	      CALL ncio_begin_distributed_write(file_restart_vector)
#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
#ifdef MPAS_EMBEDDED_COLM
	      file_restart_pft = trim(dir_restart)// '/'//trim(cdate)//'/' // trim(site) //'_restart_pft_'// &
	                         trim(cdate)//'_lc'//trim(cyear)//'.nc'
#ifdef SinglePoint
	      write_pft_restart = patchtypes(SITE_landtype) == 0
#else
	      write_pft_restart = .true.
#endif
	      IF (write_pft_restart) THEN
	         file_restart_pft_manifest = file_restart_pft
	         CALL ncio_begin_distributed_write(file_restart_pft)
	      ENDIF
#endif
#endif
#ifdef GridRiverLakeFlow
#ifdef MPAS_EMBEDDED_COLM
	      file_restart_gridriver_manifest = trim(dir_restart)// '/'//trim(cdate)//'/' // trim(site) // &
	         '_restart_gridriver_'//trim(cdate)//'_lc'//trim(cyear)//'.nc'
#endif
#endif

#ifdef MPAS_EMBEDDED_COLM
	      IF (numpatch > 0) THEN
#endif

#ifdef MPAS_EMBEDDED_COLM
	      CALL ncio_create_file_vector(file_restart, landpatch, checkpoint_family_id, 'patch')
#else
	      CALL ncio_create_file_vector(file_restart, landpatch)
#endif
	      CALL ncio_define_dimension_vector (file_restart, landpatch, 'patch')

      CALL ncio_define_dimension_vector (file_restart, landpatch, 'snow',     -maxsnl       )
      CALL ncio_define_dimension_vector (file_restart, landpatch, 'snowp1',   -maxsnl+1     )
      CALL ncio_define_dimension_vector (file_restart, landpatch, 'soilsnow', nl_soil-maxsnl)
      CALL ncio_define_dimension_vector (file_restart, landpatch, 'soil',     nl_soil)
      CALL ncio_define_dimension_vector (file_restart, landpatch, 'lake',     nl_lake)

IF(DEF_USE_PLANTHYDRAULICS)THEN
      CALL ncio_define_dimension_vector (file_restart, landpatch, 'vegnodes', nvegwcs)
ENDIF

      CALL ncio_define_dimension_vector (file_restart, landpatch, 'band', 2)
      CALL ncio_define_dimension_vector (file_restart, landpatch, 'rtyp', 2)
#ifdef HYPERSPECTRAL
      CALL ncio_define_dimension_vector (file_restart, landpatch, 'wavelength', 211)
      CALL ncio_define_dimension_vector (file_restart, landpatch, 'PFT', 16)
#endif
      ! Time-varying state variables which required by restart run
      CALL ncio_write_vector (file_restart, 'z_sno   '   , 'snow', -maxsnl, 'patch', landpatch, z_sno , compress)                 ! node depth [m]
      CALL ncio_write_vector (file_restart, 'dz_sno  '   , 'snow', -maxsnl, 'patch', landpatch, dz_sno, compress)                 ! interface depth [m]
      CALL ncio_write_vector (file_restart, 't_soisno'   , 'soilsnow', nl_soil-maxsnl, 'patch', landpatch, t_soisno   , compress) ! soil temperature [K]
      CALL ncio_write_vector (file_restart, 'wliq_soisno', 'soilsnow', nl_soil-maxsnl, 'patch', landpatch, wliq_soisno, compress) ! liquid water in layers [kg/m2]
      CALL ncio_write_vector (file_restart, 'wice_soisno', 'soilsnow', nl_soil-maxsnl, 'patch', landpatch, wice_soisno, compress) ! ice lens in layers [kg/m2]
      CALL ncio_write_vector (file_restart, 'smp',         'soil', nl_soil, 'patch', landpatch, smp, compress)                    ! soil matrix potential [mm]
      CALL ncio_write_vector (file_restart, 'hk',          'soil', nl_soil, 'patch', landpatch, hk, compress)                     ! hydraulic conductivity [mm h2o/s]
IF(DEF_USE_PLANTHYDRAULICS)THEN
      CALL ncio_write_vector (file_restart, 'vegwp',  'vegnodes', nvegwcs,  'patch', landpatch, vegwp, compress)        ! vegetation water potential [mm]
      CALL ncio_write_vector (file_restart, 'gs0sun',    'patch', landpatch, gs0sun, compress)                          ! working copy of sunlit stomata conductance
      CALL ncio_write_vector (file_restart, 'gs0sha',    'patch', landpatch, gs0sha, compress)                          ! working copy of shalit stomata conductance
ENDIF
IF(DEF_USE_OZONESTRESS)THEN
      CALL ncio_write_vector (file_restart, 'lai_old    ', 'patch', landpatch, lai_old    , compress)
      CALL ncio_write_vector (file_restart, 'o3uptakesun', 'patch', landpatch, o3uptakesun, compress)
      CALL ncio_write_vector (file_restart, 'o3uptakesha', 'patch', landpatch, o3uptakesha, compress)
      CALL ncio_write_vector (file_restart, 'o3coefv_sun', 'patch', landpatch, o3coefv_sun, compress)
      CALL ncio_write_vector (file_restart, 'o3coefv_sha', 'patch', landpatch, o3coefv_sha, compress)
      CALL ncio_write_vector (file_restart, 'o3coefg_sun', 'patch', landpatch, o3coefg_sun, compress)
      CALL ncio_write_vector (file_restart, 'o3coefg_sha', 'patch', landpatch, o3coefg_sha, compress)
ENDIF
      CALL ncio_write_vector (file_restart, 't_grnd  '   , 'patch', landpatch, t_grnd    , compress)                    ! ground surface temperature [K]
      CALL ncio_write_vector (file_restart, 'tleaf   '   , 'patch', landpatch, tleaf     , compress)                    ! leaf temperature [K]
      CALL ncio_write_vector (file_restart, 'ldew    '   , 'patch', landpatch, ldew      , compress)                    ! depth of water on foliage [mm]
      CALL ncio_write_vector (file_restart, 'ldew_rain'  , 'patch', landpatch, ldew_rain , compress)                    ! depth of water on foliage [mm]
      CALL ncio_write_vector (file_restart, 'ldew_snow'  , 'patch', landpatch, ldew_snow , compress)                    ! depth of water on foliage [mm]
      CALL ncio_write_vector (file_restart, 'fwet_snow'  , 'patch', landpatch, fwet_snow , compress)                    ! vegetation snow fractional cover [-]
      CALL ncio_write_vector (file_restart, 'sag     '   , 'patch', landpatch, sag       , compress)                    ! non dimensional snow age [-]
      CALL ncio_write_vector (file_restart, 'scv     '   , 'patch', landpatch, scv       , compress)                    ! snow cover, water equivalent [mm]
      CALL ncio_write_vector (file_restart, 'snowdp  '   , 'patch', landpatch, snowdp    , compress)                    ! snow depth [meter]
      CALL ncio_write_vector (file_restart, 'fveg    '   , 'patch', landpatch, fveg      , compress)                    ! fraction of vegetation cover
      CALL ncio_write_vector (file_restart, 'fsno    '   , 'patch', landpatch, fsno      , compress)                    ! fraction of snow cover on ground
      CALL ncio_write_vector (file_restart, 'sigf    '   , 'patch', landpatch, sigf      , compress)                    ! fraction of veg cover, excluding snow-covered veg [-]
      CALL ncio_write_vector (file_restart, 'green   '   , 'patch', landpatch, green     , compress)                    ! leaf greenness
      CALL ncio_write_vector (file_restart, 'lai     '   , 'patch', landpatch, lai       , compress)                    ! leaf area index
      CALL ncio_write_vector (file_restart, 'tlai    '   , 'patch', landpatch, tlai      , compress)                    ! leaf area index
      CALL ncio_write_vector (file_restart, 'sai     '   , 'patch', landpatch, sai       , compress)                    ! stem area index
      CALL ncio_write_vector (file_restart, 'tsai    '   , 'patch', landpatch, tsai      , compress)                    ! stem area index
      CALL ncio_write_vector (file_restart, 'coszen  '   , 'patch', landpatch, coszen    , compress)                    ! cosine of solar zenith angle
      CALL ncio_write_vector (file_restart, 'alb     '   , 'band', 2, 'rtyp', 2, 'patch', landpatch, alb , compress)    ! averaged albedo [-]
      CALL ncio_write_vector (file_restart, 'ssun    '   , 'band', 2, 'rtyp', 2, 'patch', landpatch, ssun, compress)    ! sunlit canopy absorption for solar radiation (0-1)
      CALL ncio_write_vector (file_restart, 'ssha    '   , 'band', 2, 'rtyp', 2, 'patch', landpatch, ssha, compress)    ! shaded canopy absorption for solar radiation (0-1)
      CALL ncio_write_vector (file_restart, 'ssoi    '   , 'band', 2, 'rtyp', 2, 'patch', landpatch, ssoi, compress)    ! shaded canopy absorption for solar radiation (0-1)
      CALL ncio_write_vector (file_restart, 'ssno    '   , 'band', 2, 'rtyp', 2, 'patch', landpatch, ssno, compress)    ! shaded canopy absorption for solar radiation (0-1)
#ifdef HYPERSPECTRAL
      CALL ncio_write_vector (file_restart, 'alb_hires'   , 'wavelength', 211, 'rtyp', 2, 'patch', landpatch, alb_hires , compress)    ! averaged albedo [-]
      CALL ncio_write_vector (file_restart, 'reflectance_out'  , 'wavelength', 211, 'PFT', 16, 'patch', landpatch, reflectance_out  , compress)    ! averaged albedo [-]
      CALL ncio_write_vector (file_restart, 'transmittance_out', 'wavelength', 211, 'PFT', 16, 'patch', landpatch, transmittance_out, compress)    ! averaged albedo [-]
#endif

      CALL ncio_write_vector (file_restart, 'thermk  '   , 'patch', landpatch, thermk    , compress)                    ! canopy gap fraction for tir radiation
      CALL ncio_write_vector (file_restart, 'extkb   '   , 'patch', landpatch, extkb     , compress)                    ! (k, g(mu)/mu) direct solar extinction coefficient
      CALL ncio_write_vector (file_restart, 'extkd   '   , 'patch', landpatch, extkd     , compress)                    ! diffuse and scattered diffuse PAR extinction coefficient
      CALL ncio_write_vector (file_restart, 'zwt     '   , 'patch', landpatch, zwt       , compress)                    ! the depth to water table [m]
      CALL ncio_write_vector (file_restart, 'wa      '   , 'patch', landpatch, wa        , compress)                    ! water storage in aquifer [mm]
      CALL ncio_write_vector (file_restart, 'wetwat  '   , 'patch', landpatch, wetwat    , compress)                    ! water storage in wetland [mm]
      CALL ncio_write_vector (file_restart, 'wdsrf   '   , 'patch', landpatch, wdsrf     , compress)                    ! depth of surface water [mm]
      CALL ncio_write_vector (file_restart, 'rss     '   , 'patch', landpatch, rss       , compress)                    ! soil surface resistance [s/m]

IF (DEF_USE_Dynamic_Lake) THEN
      CALL ncio_write_vector (file_restart, 'dz_lake'    , 'lake', nl_lake, 'patch', landpatch, dz_lake     , compress)
ENDIF
      CALL ncio_write_vector (file_restart, 't_lake  '   , 'lake', nl_lake, 'patch', landpatch, t_lake      , compress)
      CALL ncio_write_vector (file_restart, 'lake_icefrc', 'lake', nl_lake, 'patch', landpatch, lake_icefrac, compress)
      CALL ncio_write_vector (file_restart, 'savedtke1  ', 'patch', landpatch, savedtke1   , compress)
      CALL ncio_write_vector (file_restart, 'snw_rds  ', 'snow', -maxsnl, 'patch', landpatch, snw_rds  , compress)
      CALL ncio_write_vector (file_restart, 'mss_bcpho', 'snow', -maxsnl, 'patch', landpatch, mss_bcpho, compress)
      CALL ncio_write_vector (file_restart, 'mss_bcphi', 'snow', -maxsnl, 'patch', landpatch, mss_bcphi, compress)
      CALL ncio_write_vector (file_restart, 'mss_ocpho', 'snow', -maxsnl, 'patch', landpatch, mss_ocpho, compress)
      CALL ncio_write_vector (file_restart, 'mss_ocphi', 'snow', -maxsnl, 'patch', landpatch, mss_ocphi, compress)
      CALL ncio_write_vector (file_restart, 'mss_dst1 ', 'snow', -maxsnl, 'patch', landpatch, mss_dst1 , compress)
      CALL ncio_write_vector (file_restart, 'mss_dst2 ', 'snow', -maxsnl, 'patch', landpatch, mss_dst2 , compress)
      CALL ncio_write_vector (file_restart, 'mss_dst3 ', 'snow', -maxsnl, 'patch', landpatch, mss_dst3 , compress)
      CALL ncio_write_vector (file_restart, 'mss_dst4 ', 'snow', -maxsnl, 'patch', landpatch, mss_dst4 , compress)
      CALL ncio_write_vector (file_restart, 'ssno_lyr', 'band', 2, 'rtyp', 2, 'snowp1', -maxsnl+1, 'patch', landpatch, ssno_lyr, compress)

      ! Additional va_vectorriables required by regional model (such as WRF ) RSM)
      CALL ncio_write_vector (file_restart, 'trad ', 'patch', landpatch, trad , compress) ! radiative temperature of surface [K]
      CALL ncio_write_vector (file_restart, 'tref ', 'patch', landpatch, tref , compress) ! 2 m height air temperature [kelvin]
      CALL ncio_write_vector (file_restart, 'qref ', 'patch', landpatch, qref , compress) ! 2 m height air specific humidity
      CALL ncio_write_vector (file_restart, 'rst  ', 'patch', landpatch, rst  , compress) ! canopy stomatal resistance (s/m)
      CALL ncio_write_vector (file_restart, 'emis ', 'patch', landpatch, emis , compress) ! averaged bulk surface emissivity
      CALL ncio_write_vector (file_restart, 'z0m  ', 'patch', landpatch, z0m  , compress) ! effective roughness [m]
      CALL ncio_write_vector (file_restart, 'zol  ', 'patch', landpatch, zol  , compress) ! dimensionless height (z/L) used in Monin-Obukhov theory
      CALL ncio_write_vector (file_restart, 'rib  ', 'patch', landpatch, rib  , compress) ! bulk Richardson number in surface layer
      CALL ncio_write_vector (file_restart, 'ustar', 'patch', landpatch, ustar, compress) ! u* in similarity theory [m/s]
      CALL ncio_write_vector (file_restart, 'qstar', 'patch', landpatch, qstar, compress) ! q* in similarity theory [kg/kg]
      CALL ncio_write_vector (file_restart, 'tstar', 'patch', landpatch, tstar, compress) ! t* in similarity theory [K]
      CALL ncio_write_vector (file_restart, 'fm   ', 'patch', landpatch, fm   , compress) ! integral of profile FUNCTION for momentum
      CALL ncio_write_vector (file_restart, 'fh   ', 'patch', landpatch, fh   , compress) ! integral of profile FUNCTION for heat
      CALL ncio_write_vector (file_restart, 'fq   ', 'patch', landpatch, fq   , compress) ! integral of profile FUNCTION for moisture

IF (DEF_USE_IRRIGATION) THEN
      CALL ncio_write_vector (file_restart, 'irrig_rate            ' , 'patch',landpatch,irrig_rate            , compress)
      CALL ncio_write_vector (file_restart, 'sum_irrig             ' , 'patch',landpatch,sum_irrig             , compress)
      CALL ncio_write_vector (file_restart, 'sum_deficit_irrig     ' , 'patch',landpatch,sum_deficit_irrig     , compress)
      CALL ncio_write_vector (file_restart, 'sum_irrig_count       ' , 'patch',landpatch,sum_irrig_count       , compress)
      CALL ncio_write_vector (file_restart, 'n_irrig_steps_left    ' , 'patch',landpatch,n_irrig_steps_left    , compress)
      CALL ncio_write_vector (file_restart, 'waterstorage          ' , 'patch',landpatch,waterstorage          , compress)
      CALL ncio_write_vector (file_restart, 'irrig_method_corn     ' , 'patch',landpatch,irrig_method_corn     , compress)
      CALL ncio_write_vector (file_restart, 'irrig_method_swheat   ' , 'patch',landpatch,irrig_method_swheat   , compress)
      CALL ncio_write_vector (file_restart, 'irrig_method_wwheat   ' , 'patch',landpatch,irrig_method_wwheat   , compress)
      CALL ncio_write_vector (file_restart, 'irrig_method_soybean  ' , 'patch',landpatch,irrig_method_soybean  , compress)
      CALL ncio_write_vector (file_restart, 'irrig_method_cotton   ' , 'patch',landpatch,irrig_method_cotton   , compress)
      CALL ncio_write_vector (file_restart, 'irrig_method_rice1    ' , 'patch',landpatch,irrig_method_rice1    , compress)
      CALL ncio_write_vector (file_restart, 'irrig_method_rice2    ' , 'patch',landpatch,irrig_method_rice2    , compress)
      CALL ncio_write_vector (file_restart, 'irrig_method_sugarcane' , 'patch',landpatch,irrig_method_sugarcane, compress)
      CALL ncio_write_vector (file_restart, 'irrig_gw_alloc        ' , 'patch',landpatch,irrig_gw_alloc        , compress)
      CALL ncio_write_vector (file_restart, 'irrig_sw_alloc        ' , 'patch',landpatch,irrig_sw_alloc        , compress)
      CALL ncio_write_vector (file_restart, 'zwt_stand             ' , 'patch',landpatch,zwt_stand             , compress)
ENDIF

#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
#ifdef SinglePoint
      IF (patchtypes(SITE_landtype) == 0) THEN
#ifdef MPAS_EMBEDDED_COLM
         file_restart = file_restart_pft
#else
         file_restart = trim(dir_restart)// '/'//trim(cdate)//'/' // trim(site) //'_restart_pft_'//trim(cdate)//'_lc'//trim(cyear)//'.nc'
#endif
#ifdef MPAS_EMBEDDED_COLM
         CALL WRITE_PFTimeVariables(file_restart, checkpoint_family_id)
#else
         CALL WRITE_PFTimeVariables(file_restart)
#endif
      ENDIF
#else
#ifdef MPAS_EMBEDDED_COLM
      file_restart = file_restart_pft
#else
      file_restart = trim(dir_restart)// '/'//trim(cdate)//'/' // trim(site) //'_restart_pft_'//trim(cdate)//'_lc'//trim(cyear)//'.nc'
#endif
#ifdef MPAS_EMBEDDED_COLM
      CALL WRITE_PFTimeVariables(file_restart, checkpoint_family_id)
#else
      CALL WRITE_PFTimeVariables(file_restart)
#endif
#endif
#endif

#if (defined BGC)
      file_restart = trim(dir_restart)// '/'//trim(cdate)//'/' // trim(site) //'_restart_bgc_'//trim(cdate)//'_lc'//trim(cyear)//'.nc'
      CALL WRITE_BGCTimeVariables (file_restart)
#endif

#if (defined CatchLateralFlow)
	      file_restart = trim(dir_restart)// '/'//trim(cdate)//'/' // trim(site) //'_restart_basin_'//trim(cdate)//'_lc'//trim(cyear)//'.nc'
	      CALL WRITE_CatchTimeVariables (file_restart)
#endif

#if (defined URBAN_MODEL)
	      file_restart = trim(dir_restart)// '/'//trim(cdate)//'/' // trim(site) //'_restart_urban_'//trim(cdate)//'_lc'//trim(cyear)//'.nc'
	      CALL WRITE_UrbanTimeVariables (file_restart)
#endif

#ifdef EXTERNAL_LAKE
      CALL WRITE_LakeTimeVars (idate, lc_year, site, dir_restart)
#endif

#ifdef DataAssimilation
	      CALL WRITE_DATimeVariables (idate, lc_year, site, dir_restart)
#endif

#ifdef MPAS_EMBEDDED_COLM
	      ENDIF
#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
	      IF (write_pft_restart) THEN
	         CALL ncio_complete_distributed_write(file_restart_pft, cdate, checkpoint_family_id, 'pft', &
	            file_restart_vector, file_restart_pft_manifest, file_restart_gridriver_manifest)
	      ENDIF
#endif
#endif

#ifdef GridRiverLakeFlow
#ifdef MPAS_EMBEDDED_COLM
	      file_restart = file_restart_gridriver_manifest
	      CALL WRITE_GridRiverLakeTimeVars(file_restart, checkpoint_family_id, file_restart_vector, &
	                                      file_restart_pft_manifest, file_restart_gridriver_manifest)
#else
	      file_restart = trim(dir_restart)// '/'//trim(cdate)//'/' // trim(site) //'_restart_gridriver_'//trim(cdate)//'_lc'//trim(cyear)//'.nc'
	      CALL WRITE_GridRiverLakeTimeVars(file_restart)
#endif
#endif

#ifdef MPAS_EMBEDDED_COLM
	      CALL ncio_complete_distributed_write(file_restart_vector, cdate, checkpoint_family_id, 'patch', &
	         file_restart_vector, file_restart_pft_manifest, file_restart_gridriver_manifest)
#else
	      CALL ncio_complete_distributed_write(file_restart_vector, cdate)
#endif

	   END SUBROUTINE WRITE_TimeVariables


   SUBROUTINE READ_TimeVariables (idate, lc_year, site, dir_restart, require_complete_restart)

   !====================================================================
   ! Original version: Yongjiu Dai, September 15, 1999, 03/2014
   !====================================================================

   USE MOD_Namelist
   USE MOD_MPAS_MPI
   USE MOD_NetCDFVector
#ifdef RangeCheck
   USE MOD_RangeCheck
#endif
   USE MOD_LandPatch
   USE MOD_Vars_Global
   USE MOD_Vars_TimeInvariants, only: dz_lake
   USE MOD_Const_LC, only: patchtypes

   IMPLICIT NONE

   integer, intent(in) :: idate(3)
   integer, intent(in) :: lc_year      !year of land cover type data
   character(len=*), intent(in) :: site
   character(len=*), intent(in) :: dir_restart
	   logical, intent(in) :: require_complete_restart

   ! Local variables
	   character(len=:), allocatable :: file_restart
	   character(len=14)  :: cdate, cyear
#ifdef MPAS_EMBEDDED_COLM
	   character(len=:), allocatable :: file_restart_vector
	   character(len=:), allocatable :: file_restart_pft_manifest
	   character(len=:), allocatable :: file_restart_gridriver_manifest
	   character(len=96) :: checkpoint_family_id
	   logical :: read_pft_restart
#endif

#ifdef MPAS_MPI
      CALL mpi_barrier (mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('time-variable restart read entry')
#endif

      IF (mpas_is_root) THEN
         write(*,*) 'Loading Time Variables ...'
      ENDIF

      ! land cover type year
      write(cyear,'(i4.4)') lc_year

	      write(cdate,'(i4.4,"-",i3.3,"-",i5.5)') idate(1), idate(2), idate(3)
	      file_restart = trim(dir_restart)// '/'//trim(cdate)//'/' // trim(site) //'_restart_'//trim(cdate)//'_lc'//trim(cyear)//'.nc'

#ifdef MPAS_EMBEDDED_COLM
	      CALL configure_mpas_restart_identity(site)
	      IF (require_complete_restart) THEN
	         file_restart_vector = file_restart
	         file_restart_pft_manifest = 'NONE'
	         file_restart_gridriver_manifest = 'NONE'
	         read_pft_restart = .false.
#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
#ifdef SinglePoint
	         read_pft_restart = patchtypes(SITE_landtype) == 0
#else
	         read_pft_restart = .true.
#endif
	         IF (read_pft_restart) THEN
	            file_restart_pft_manifest = trim(dir_restart)// '/'//trim(cdate)//'/' // trim(site) // &
	               '_restart_pft_'//trim(cdate)//'_lc'//trim(cyear)//'.nc'
	         ENDIF
#endif
#ifdef GridRiverLakeFlow
	         file_restart_gridriver_manifest = trim(dir_restart)// '/'//trim(cdate)//'/' // trim(site) // &
	            '_restart_gridriver_'//trim(cdate)//'_lc'//trim(cyear)//'.nc'
#endif
	         CALL ncio_validate_distributed_restart(file_restart_vector, cdate, 'patch', &
	            expected_patch_file=file_restart_vector, expected_pft_file=file_restart_pft_manifest, &
	            expected_gridriver_file=file_restart_gridriver_manifest, &
	            checkpoint_family_id=checkpoint_family_id)
#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
	         IF (read_pft_restart) THEN
	            CALL ncio_validate_distributed_restart(file_restart_pft_manifest, cdate, 'pft', &
	               checkpoint_family_id, file_restart_vector, file_restart_pft_manifest, &
	               file_restart_gridriver_manifest)
	         ENDIF
#endif
#ifdef GridRiverLakeFlow
	         CALL VALIDATE_GridRiverRestart(file_restart_gridriver_manifest, checkpoint_family_id, &
	            file_restart_vector, file_restart_pft_manifest, file_restart_gridriver_manifest)
#endif
	      ENDIF
#endif

#ifdef MPAS_EMBEDDED_COLM
      IF (numpatch > 0) THEN
#endif

      ! Time-varying state variables which required by restart run
      CALL ncio_read_vector (file_restart, 'z_sno   '   , -maxsnl, landpatch, z_sno )             ! node depth [m]
      CALL ncio_read_vector (file_restart, 'dz_sno  '   , -maxsnl, landpatch, dz_sno)             ! interface depth [m]
      CALL ncio_read_vector (file_restart, 't_soisno'   , nl_soil-maxsnl, landpatch, t_soisno   ) ! soil temperature [K]
      CALL ncio_read_vector (file_restart, 'wliq_soisno', nl_soil-maxsnl, landpatch, wliq_soisno) ! liquid water in layers [kg/m2]
      CALL ncio_read_vector (file_restart, 'wice_soisno', nl_soil-maxsnl, landpatch, wice_soisno) ! ice lens in layers [kg/m2]
      CALL ncio_read_vector (file_restart, 'smp',         nl_soil,        landpatch, smp        ) ! soil matrix potential [mm]
      CALL ncio_read_vector (file_restart, 'hk',          nl_soil,        landpatch, hk         ) ! hydraulic conductivity [mm h2o/s]
IF(DEF_USE_PLANTHYDRAULICS)THEN
      CALL ncio_read_vector (file_restart, 'vegwp',       nvegwcs,        landpatch, vegwp      ) ! vegetation water potential [mm]
      CALL ncio_read_vector (file_restart, 'gs0sun  ',    landpatch, gs0sun     ) ! working copy of sunlit stomata conductance
      CALL ncio_read_vector (file_restart, 'gs0sha  ',    landpatch, gs0sha     ) ! working copy of shaded stomata conductance
ENDIF
      CALL ncio_read_vector (file_restart, 't_grnd  '   , landpatch, t_grnd     ) ! ground surface temperature [K]
      CALL ncio_read_vector (file_restart, 'tleaf   '   , landpatch, tleaf      ) ! leaf temperature [K]
      CALL ncio_read_vector (file_restart, 'ldew    '   , landpatch, ldew       ) ! depth of water on foliage [mm]
      CALL ncio_read_vector (file_restart, 'ldew_rain'  , landpatch, ldew_rain  ) ! depth of rain on foliage [mm]
      CALL ncio_read_vector (file_restart, 'ldew_snow'  , landpatch, ldew_snow  ) ! depth of snow on foliage [mm]
      CALL ncio_read_vector (file_restart, 'fwet_snow'  , landpatch, fwet_snow  ) ! vegetation snow fractional cover [-]
      CALL ncio_read_vector (file_restart, 'sag     '   , landpatch, sag        ) ! non dimensional snow age [-]
      CALL ncio_read_vector (file_restart, 'scv     '   , landpatch, scv        ) ! snow cover, water equivalent [mm]
      CALL ncio_read_vector (file_restart, 'snowdp  '   , landpatch, snowdp     ) ! snow depth [meter]
      CALL ncio_read_vector (file_restart, 'fveg    '   , landpatch, fveg       ) ! fraction of vegetation cover
      CALL ncio_read_vector (file_restart, 'fsno    '   , landpatch, fsno       ) ! fraction of snow cover on ground
      CALL ncio_read_vector (file_restart, 'sigf    '   , landpatch, sigf       ) ! fraction of veg cover, excluding snow-covered veg [-]
      CALL ncio_read_vector (file_restart, 'green   '   , landpatch, green      ) ! leaf greenness
      CALL ncio_read_vector (file_restart, 'lai     '   , landpatch, lai        ) ! leaf area index
      CALL ncio_read_vector (file_restart, 'tlai    '   , landpatch, tlai       ) ! leaf area index
      CALL ncio_read_vector (file_restart, 'sai     '   , landpatch, sai        ) ! stem area index
      CALL ncio_read_vector (file_restart, 'tsai    '   , landpatch, tsai       ) ! stem area index
      CALL ncio_read_vector (file_restart, 'coszen  '   , landpatch, coszen     ) ! cosine of solar zenith angle
IF(DEF_USE_OZONESTRESS)THEN
      CALL ncio_read_vector (file_restart, 'lai_old    ', landpatch, lai_old    )
      CALL ncio_read_vector (file_restart, 'o3uptakesun', landpatch, o3uptakesun)
      CALL ncio_read_vector (file_restart, 'o3uptakesha', landpatch, o3uptakesha)
      CALL ncio_read_vector (file_restart, 'o3coefv_sun', landpatch, o3coefv_sun)
      CALL ncio_read_vector (file_restart, 'o3coefv_sha', landpatch, o3coefv_sha)
      CALL ncio_read_vector (file_restart, 'o3coefg_sun', landpatch, o3coefg_sun)
      CALL ncio_read_vector (file_restart, 'o3coefg_sha', landpatch, o3coefg_sha)
ENDIF
      CALL ncio_read_vector (file_restart, 'alb     '   , 2, 2, landpatch, alb  ) ! averaged albedo [-]
      CALL ncio_read_vector (file_restart, 'ssun    '   , 2, 2, landpatch, ssun ) ! sunlit canopy absorption for solar radiation (0-1)
      CALL ncio_read_vector (file_restart, 'ssha    '   , 2, 2, landpatch, ssha ) ! shaded canopy absorption for solar radiation (0-1)
      CALL ncio_read_vector (file_restart, 'ssoi    '   , 2, 2, landpatch, ssoi ) ! soil absorption for solar radiation (0-1)
      CALL ncio_read_vector (file_restart, 'ssno    '   , 2, 2, landpatch, ssno ) ! snow absorption for solar radiation (0-1)
#ifdef HYPERSPECTRAL
      CALL ncio_read_vector (file_restart, 'alb_hires ' , 211, 2, landpatch, alb_hires  ) ! averaged albedo [-]
      CALL ncio_read_vector (file_restart, 'reflectance_out'  , 211, 16, landpatch, reflectance_out  ) ! averaged albedo [-]
      CALL ncio_read_vector (file_restart, 'transmittance_out', 211, 16, landpatch, transmittance_out) ! averaged albedo [-]
#endif

      CALL ncio_read_vector (file_restart, 'thermk  '   , landpatch, thermk     ) ! canopy gap fraction for tir radiation
      CALL ncio_read_vector (file_restart, 'extkb   '   , landpatch, extkb      ) ! (k, g(mu)/mu) direct solar extinction coefficient
      CALL ncio_read_vector (file_restart, 'extkd   '   , landpatch, extkd      ) ! diffuse and scattered diffuse PAR extinction coefficient
      CALL ncio_read_vector (file_restart, 'zwt     '   , landpatch, zwt        ) ! the depth to water table [m]
      CALL ncio_read_vector (file_restart, 'wa      '   , landpatch, wa         ) ! water storage in aquifer [mm]
      CALL ncio_read_vector (file_restart, 'wetwat  '   , landpatch, wetwat     ) ! water storage in wetland [mm]
      CALL ncio_read_vector (file_restart, 'wdsrf   '   , landpatch, wdsrf      ) ! depth of surface water [mm]
      CALL ncio_read_vector (file_restart, 'rss     '   , landpatch, rss        ) ! soil surface resistance [s/m]

IF (DEF_USE_Dynamic_Lake) THEN
      CALL ncio_read_vector (file_restart, 'dz_lake'    , nl_lake, landpatch, dz_lake     )
ENDIF
      CALL ncio_read_vector (file_restart, 't_lake  '   , nl_lake, landpatch, t_lake      )
      CALL ncio_read_vector (file_restart, 'lake_icefrc', nl_lake, landpatch, lake_icefrac)
      CALL ncio_read_vector (file_restart, 'savedtke1', landpatch, savedtke1)

      CALL ncio_read_vector (file_restart, 'snw_rds  ', -maxsnl, landpatch, snw_rds  )
      CALL ncio_read_vector (file_restart, 'mss_bcpho', -maxsnl, landpatch, mss_bcpho)
      CALL ncio_read_vector (file_restart, 'mss_bcphi', -maxsnl, landpatch, mss_bcphi)
      CALL ncio_read_vector (file_restart, 'mss_ocpho', -maxsnl, landpatch, mss_ocpho)
      CALL ncio_read_vector (file_restart, 'mss_ocphi', -maxsnl, landpatch, mss_ocphi)
      CALL ncio_read_vector (file_restart, 'mss_dst1 ', -maxsnl, landpatch, mss_dst1 )
      CALL ncio_read_vector (file_restart, 'mss_dst2 ', -maxsnl, landpatch, mss_dst2 )
      CALL ncio_read_vector (file_restart, 'mss_dst3 ', -maxsnl, landpatch, mss_dst3 )
      CALL ncio_read_vector (file_restart, 'mss_dst4 ', -maxsnl, landpatch, mss_dst4 )
      CALL ncio_read_vector (file_restart, 'ssno_lyr', 2,2, -maxsnl+1, landpatch, ssno_lyr)

      ! Additional variables required by regional model (such as WRF ) RSM)
      CALL ncio_read_vector (file_restart, 'trad ', landpatch, trad ) ! radiative temperature of surface [K]
      CALL ncio_read_vector (file_restart, 'tref ', landpatch, tref ) ! 2 m height air temperature [kelvin]
      CALL ncio_read_vector (file_restart, 'qref ', landpatch, qref ) ! 2 m height air specific humidity
      CALL ncio_read_vector (file_restart, 'rst  ', landpatch, rst  ) ! canopy stomatal resistance (s/m)
      CALL ncio_read_vector (file_restart, 'emis ', landpatch, emis ) ! averaged bulk surface emissivity
      CALL ncio_read_vector (file_restart, 'z0m  ', landpatch, z0m  ) ! effective roughness [m]
      CALL ncio_read_vector (file_restart, 'zol  ', landpatch, zol  ) ! dimensionless height (z/L) used in Monin-Obukhov theory
      CALL ncio_read_vector (file_restart, 'rib  ', landpatch, rib  ) ! bulk Richardson number in surface layer
      CALL ncio_read_vector (file_restart, 'ustar', landpatch, ustar) ! u* in similarity theory [m/s]
      CALL ncio_read_vector (file_restart, 'qstar', landpatch, qstar) ! q* in similarity theory [kg/kg]
      CALL ncio_read_vector (file_restart, 'tstar', landpatch, tstar) ! t* in similarity theory [K]
      CALL ncio_read_vector (file_restart, 'fm   ', landpatch, fm   ) ! integral of profile FUNCTION for momentum
      CALL ncio_read_vector (file_restart, 'fh   ', landpatch, fh   ) ! integral of profile FUNCTION for heat
      CALL ncio_read_vector (file_restart, 'fq   ', landpatch, fq   ) ! integral of profile FUNCTION for moisture

IF (DEF_USE_IRRIGATION) THEN
      CALL ncio_read_vector (file_restart, 'irrig_rate            ' , landpatch, irrig_rate            )
      CALL ncio_read_vector (file_restart, 'sum_irrig             ' , landpatch, sum_irrig             )
      CALL ncio_read_vector (file_restart, 'sum_deficit_irrig     ' , landpatch, sum_deficit_irrig     )
      CALL ncio_read_vector (file_restart, 'sum_irrig_count       ' , landpatch, sum_irrig_count       )
      CALL ncio_read_vector (file_restart, 'n_irrig_steps_left    ' , landpatch, n_irrig_steps_left    )
      CALL ncio_read_vector (file_restart, 'waterstorage          ' , landpatch, waterstorage          )
      CALL ncio_read_vector (file_restart, 'irrig_method_corn     ' , landpatch, irrig_method_corn     )
      CALL ncio_read_vector (file_restart, 'irrig_method_swheat   ' , landpatch, irrig_method_swheat   )
      CALL ncio_read_vector (file_restart, 'irrig_method_wwheat   ' , landpatch, irrig_method_wwheat   )
      CALL ncio_read_vector (file_restart, 'irrig_method_soybean  ' , landpatch, irrig_method_soybean  )
      CALL ncio_read_vector (file_restart, 'irrig_method_cotton   ' , landpatch, irrig_method_cotton   )
      CALL ncio_read_vector (file_restart, 'irrig_method_rice1    ' , landpatch, irrig_method_rice1    )
      CALL ncio_read_vector (file_restart, 'irrig_method_rice2    ' , landpatch, irrig_method_rice2    )
      CALL ncio_read_vector (file_restart, 'irrig_method_sugarcane' , landpatch, irrig_method_sugarcane)
      CALL ncio_read_vector (file_restart, 'irrig_gw_alloc        ' , landpatch, irrig_gw_alloc        )
      CALL ncio_read_vector (file_restart, 'irrig_sw_alloc        ' , landpatch, irrig_sw_alloc        )
      CALL ncio_read_vector (file_restart, 'zwt_stand             ' , landpatch, zwt_stand             )
ENDIF

#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
#ifdef SinglePoint
      IF (patchtypes(SITE_landtype) == 0) THEN
         file_restart = trim(dir_restart)// '/'//trim(cdate)//'/' // trim(site) //'_restart_pft_'//trim(cdate)//'_lc'//trim(cyear)//'.nc'
         CALL READ_PFTimeVariables (file_restart)
      ENDIF
#else
      file_restart = trim(dir_restart)// '/'//trim(cdate)//'/' // trim(site) //'_restart_pft_'//trim(cdate)//'_lc'//trim(cyear)//'.nc'
      CALL READ_PFTimeVariables (file_restart)
#endif
#endif

#if (defined BGC)
      file_restart = trim(dir_restart)// '/'//trim(cdate)//'/' // trim(site) //'_restart_bgc_'//trim(cdate)//'_lc'//trim(cyear)//'.nc'
      CALL READ_BGCTimeVariables (file_restart)
#endif

#if (defined CatchLateralFlow)
      file_restart = trim(dir_restart)// '/'//trim(cdate)//'/' // trim(site) //'_restart_basin_'//trim(cdate)//'_lc'//trim(cyear)//'.nc'
      CALL READ_CatchTimeVariables (file_restart)
#endif

#ifdef MPAS_EMBEDDED_COLM
      ENDIF
#endif

#ifdef GridRiverLakeFlow
      file_restart = trim(dir_restart)// '/'//trim(cdate)//'/' // trim(site) //'_restart_gridriver_'//trim(cdate)//'_lc'//trim(cyear)//'.nc'
#ifdef MPAS_EMBEDDED_COLM
	      IF (require_complete_restart) THEN
	         CALL READ_GridRiverLakeTimeVars(file_restart, require_complete_restart, checkpoint_family_id, &
	            file_restart_vector, file_restart_pft_manifest, file_restart_gridriver_manifest)
	      ELSE
	         CALL READ_GridRiverLakeTimeVars(file_restart, require_complete_restart)
	      ENDIF
#else
	      CALL READ_GridRiverLakeTimeVars(file_restart, require_complete_restart)
#endif
#endif

#ifdef MPAS_EMBEDDED_COLM
      IF (numpatch > 0) THEN
#endif

#if (defined URBAN_MODEL)
      file_restart = trim(dir_restart)// '/'//trim(cdate)//'/' // trim(site) //'_restart_urban_'//trim(cdate)//'_lc'//trim(cyear)//'.nc'
      CALL READ_UrbanTimeVariables (file_restart)
#endif

#ifdef EXTERNAL_LAKE
      CALL READ_LakeTimeVars(idate, lc_year, site, dir_restart)
#endif

#ifdef DataAssimilation
      CALL READ_DATimeVariables (idate, lc_year, site, dir_restart)
#endif

#ifdef RangeCheck
      CALL check_TimeVariables
#endif

#ifdef MPAS_EMBEDDED_COLM
      ENDIF
#endif

      IF (mpas_is_root) THEN
         write(*,*) 'Loading Time Variables done.'
      ENDIF

   END SUBROUTINE READ_TimeVariables


#ifdef RangeCheck
   SUBROUTINE check_TimeVariables ()

   USE MOD_MPAS_MPI
   USE MOD_RangeCheck
   USE MOD_Namelist, only: DEF_USE_PLANTHYDRAULICS, DEF_USE_OZONESTRESS, DEF_USE_IRRIGATION, &
                           DEF_USE_SNICAR, DEF_USE_Dynamic_Lake
   USE MOD_Vars_TimeInvariants, only: dz_lake

   IMPLICIT NONE

#ifdef MPAS_MPI
      CALL mpi_barrier (mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('time-variable range-check entry')
#endif
      IF (mpas_is_root) THEN
         write(*,'(/,A27)') 'Checking Time Variables ...'
      ENDIF

      CALL check_vector_data ('t_grnd      [K]    ', t_grnd     ) ! ground surface temperature [K]
      CALL check_vector_data ('tleaf       [K]    ', tleaf      ) ! leaf temperature [K]
      CALL check_vector_data ('ldew        [mm]   ', ldew       ) ! depth of water on foliage [mm]
      CALL check_vector_data ('ldew_rain   [mm]   ', ldew_rain  ) ! depth of rain on foliage [mm]
      CALL check_vector_data ('ldew_snow   [mm]   ', ldew_snow  ) ! depth of snow on foliage [mm]
      CALL check_vector_data ('fwet_snow   [mm]   ', fwet_snow  ) ! vegetation snow fractional cover [-]
      CALL check_vector_data ('sag         [-]    ', sag        ) ! non dimensional snow age [-]
      CALL check_vector_data ('scv         [mm]   ', scv        ) ! snow cover, water equivalent [mm]
      CALL check_vector_data ('snowdp      [m]    ', snowdp     ) ! snow depth [meter]
      CALL check_vector_data ('fveg        [-]    ', fveg       ) ! fraction of vegetation cover
      CALL check_vector_data ('fsno        [-]    ', fsno       ) ! fraction of snow cover on ground
      CALL check_vector_data ('sigf        [-]    ', sigf       ) ! fraction of veg cover, excluding snow-covered veg [-]
      CALL check_vector_data ('green       [-]    ', green      ) ! leaf greenness
      CALL check_vector_data ('lai         [-]    ', lai        ) ! leaf area index
      CALL check_vector_data ('tlai        [-]    ', tlai       ) ! leaf area index
      CALL check_vector_data ('sai         [-]    ', sai        ) ! stem area index
      CALL check_vector_data ('tsai        [-]    ', tsai       ) ! stem area index
      CALL check_vector_data ('coszen      [-]    ', coszen     ) ! cosine of solar zenith angle
      CALL check_vector_data ('alb         [-]    ', alb        ) ! averaged albedo [-]
      CALL check_vector_data ('ssun        [-]    ', ssun       ) ! sunlit canopy absorption for solar radiation (0-1)
      CALL check_vector_data ('ssha        [-]    ', ssha       ) ! shaded canopy absorption for solar radiation (0-1)
      CALL check_vector_data ('ssoi        [-]    ', ssoi       ) ! soil absorption for solar radiation (0-1)
      CALL check_vector_data ('ssno        [-]    ', ssno       ) ! snow absorption for solar radiation (0-1)
      CALL check_vector_data ('thermk      [-]    ', thermk     ) ! canopy gap fraction for tir radiation
      CALL check_vector_data ('extkb       [-]    ', extkb      ) ! (k, g(mu)/mu) direct solar extinction coefficient
      CALL check_vector_data ('extkd       [-]    ', extkd      ) ! diffuse and scattered diffuse PAR extinction coefficient
      CALL check_vector_data ('zwt         [m]    ', zwt        ) ! the depth to water table [m]
      CALL check_vector_data ('wa          [mm]   ', wa         ) ! water storage in aquifer [mm]
      CALL check_vector_data ('wetwat      [mm]   ', wetwat     ) ! water storage in wetland [mm]
      CALL check_vector_data ('wdsrf       [mm]   ', wdsrf      ) ! depth of surface water [mm]
      CALL check_vector_data ('rss         [s/m]  ', rss        ) ! soil surface resistance [s/m]
IF (DEF_USE_Dynamic_Lake) THEN
      CALL check_vector_data ('dz_lake     [m]    ', dz_lake     )!
ENDIF
      CALL check_vector_data ('t_lake      [K]    ', t_lake      )!
      CALL check_vector_data ('lake_icefrc [-]    ', lake_icefrac)!
      CALL check_vector_data ('savedtke1   [W/m K]', savedtke1   )!
      CALL check_vector_data ('z_sno       [m]    ', z_sno )      ! node depth [m]
      CALL check_vector_data ('dz_sno      [m]    ', dz_sno)      ! interface depth [m]
      CALL check_vector_data ('t_soisno    [K]    ', t_soisno   ) ! soil temperature [K]
      CALL check_vector_data ('wliq_soisno [kg/m2]', wliq_soisno) ! liquid water in layers [kg/m2]
	      CALL check_vector_data ('wice_soisno [kg/m2]', wice_soisno) ! ice lens in layers [kg/m2]
	      CALL check_vector_data ('smp         [mm]   ', smp        ) ! soil matrix potential [mm]
	      CALL check_vector_data ('hk          [mm/s] ', hk         ) ! hydraulic conductivity [mm h2o/s]
	      CALL check_vector_data ('qsfc        [kg/kg]', qsfc       ) ! bulk surface/ground specific humidity
	IF(DEF_USE_PLANTHYDRAULICS)THEN
      CALL check_vector_data ('vegwp       [m]    ', vegwp      ) ! vegetation water potential [mm]
      CALL check_vector_data ('gs0sun      []     ', gs0sun     ) ! working copy of sunlit stomata conductance
      CALL check_vector_data ('gs0sha      []     ', gs0sha     ) ! working copy of shaded stomata conductance
ENDIF
IF(DEF_USE_OZONESTRESS)THEN
      CALL check_vector_data ('o3coefv_sun        ', o3coefv_sun)
      CALL check_vector_data ('o3coefv_sha        ', o3coefv_sha)
      CALL check_vector_data ('o3coefg_sun        ', o3coefg_sun)
      CALL check_vector_data ('o3coefg_sha        ', o3coefg_sha)
      CALL check_vector_data ('lai_old            ', lai_old    )
      CALL check_vector_data ('o3uptakesun        ', o3uptakesun)
      CALL check_vector_data ('o3uptakesha        ', o3uptakesha)
ENDIF

IF (DEF_USE_SNICAR) THEN
      CALL check_vector_data ('snw_rds     [m-6]  ',  snw_rds   )
      CALL check_vector_data ('mss_bcpho   [Kg]   ',  mss_bcpho )
      CALL check_vector_data ('mss_bcphi   [Kg]   ',  mss_bcphi )
      CALL check_vector_data ('mss_ocpho   [Kg]   ',  mss_ocpho )
      CALL check_vector_data ('mss_ocphi   [Kg]   ',  mss_ocphi )
      CALL check_vector_data ('mss_dst1    [Kg]   ',  mss_dst1  )
      CALL check_vector_data ('mss_dst2    [Kg]   ',  mss_dst2  )
      CALL check_vector_data ('mss_dst3    [Kg]   ',  mss_dst3  )
      CALL check_vector_data ('mss_dst4    [Kg]   ',  mss_dst4  )
      CALL check_vector_data ('ssno_lyr    [-]    ',  ssno_lyr  )
ENDIF

IF (DEF_USE_IRRIGATION) THEN
      CALL check_vector_data ('irrig_rate            ' , irrig_rate            )
      CALL check_vector_data ('deficit_irrig         ' , deficit_irrig         )
      CALL check_vector_data ('actual_irrig          ' , actual_irrig          )
      CALL check_vector_data ('sum_irrig             ' , sum_irrig             )
      CALL check_vector_data ('sum_deficit_irrig     ' , sum_deficit_irrig     )
      CALL check_vector_data ('sum_irrig_count       ' , sum_irrig_count       )
      CALL check_vector_data ('n_irrig_steps_left    ' , n_irrig_steps_left    )
      CALL check_vector_data ('waterstorage          ' , waterstorage          )
      CALL check_vector_data ('waterstorage_supply   ' , waterstorage_supply   )
      CALL check_vector_data ('groundwater_demand    ' , groundwater_demand    )
      CALL check_vector_data ('groundwater_supply    ' , groundwater_supply    )
      CALL check_vector_data ('reservoirriver_demand ' , reservoirriver_demand )
      CALL check_vector_data ('reservoirriver_supply ' , reservoirriver_supply )
      CALL check_vector_data ('irrig_method_corn     ' , irrig_method_corn     )
      CALL check_vector_data ('irrig_method_swheat   ' , irrig_method_swheat   )
      CALL check_vector_data ('irrig_method_wwheat   ' , irrig_method_wwheat   )
      CALL check_vector_data ('irrig_method_soybean  ' , irrig_method_soybean  )
      CALL check_vector_data ('irrig_method_cotton   ' , irrig_method_cotton   )
      CALL check_vector_data ('irrig_method_rice1    ' , irrig_method_rice1    )
      CALL check_vector_data ('irrig_method_rice2    ' , irrig_method_rice2    )
      CALL check_vector_data ('irrig_method_sugarcane' , irrig_method_sugarcane)
      CALL check_vector_data ('irrig_gw_alloc        ' , irrig_gw_alloc        )
      CALL check_vector_data ('irrig_sw_alloc        ' , irrig_sw_alloc        )
      CALL check_vector_data ('zwt_stand             ' , zwt_stand             )
ENDIF

#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
      CALL check_PFTimeVariables
#endif

#if (defined BGC)
      CALL check_BGCTimeVariables
#endif

#ifdef EXTERNAL_LAKE
      CALL CHECK_LakeTimeVars
#endif

#ifdef DataAssimilation
      IF (DEF_DA_ENS_NUM > 1) CALL check_DATimeVariables
#endif

#ifdef MPAS_MPI
      CALL mpi_barrier (mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('time-variable range-check completion')
#endif

  END SUBROUTINE check_TimeVariables
#endif


END MODULE MOD_Vars_TimeVariables
! ---------- EOP ------------
