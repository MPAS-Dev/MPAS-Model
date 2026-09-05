#include <define.h>

#ifdef URBAN_MODEL
MODULE MOD_Urban_Vars_TimeInvariants

!-----------------------------------------------------------------------
! !DESCRIPTION:
!
!  Define urban model time invariant variables.
!
!  Created by Hua Yuan, 12/2020
!-----------------------------------------------------------------------

   USE MOD_Precision
   IMPLICIT NONE
   SAVE

   !integer , allocatable :: urbclass     (:) !urban type
   !integer , allocatable :: patch2urb    (:) !projection from patch to Urban
   !integer , allocatable :: urb2patch    (:) !projection from Urban to patch

   real(r8), allocatable :: pop_den       (:) !pop density
   real(r8), allocatable :: vehicle     (:,:) !vehicle numbers per thousand people
   real(r8), allocatable :: week_holiday(:,:) !week holidays
   real(r8), allocatable :: weh_prof    (:,:) !Diurnal traffic flow profile of weekend
   real(r8), allocatable :: wdh_prof    (:,:) !Diurnal traffic flow profile of weekday
   real(r8), allocatable :: hum_prof    (:,:) !Diurnal metabolic heat profile
   real(r8), allocatable :: fix_holiday (:,:) !Fixed public holidays, holiday (0) or workday(1)

   ! Vegetations
   real(r8), allocatable :: fveg_urb      (:) !tree coverage of urban patch [-]
   real(r8), allocatable :: htop_urb      (:) !tree crown top height of urban patch [m]
   real(r8), allocatable :: hbot_urb      (:) !tree crown bottom height of urban patch [m]

   ! Urban morphology
   real(r8), allocatable :: froof         (:) !roof fractional cover [-]
   real(r8), allocatable :: fgper         (:) !impervious fraction to ground area [-]
   real(r8), allocatable :: flake         (:) !lake fraction to ground area [-]
   real(r8), allocatable :: hroof         (:) !average building height [m]
   real(r8), allocatable :: hlr           (:) !average building height to their side length [-]

   real(r8), allocatable :: z_roof      (:,:) !depth of each roof layer [m]
   real(r8), allocatable :: z_wall      (:,:) !depth of each wall layer [m]
   real(r8), allocatable :: dz_roof     (:,:) !thickness of each roof layer [m]
   real(r8), allocatable :: dz_wall     (:,:) !thickness of each wall layer [m]

   ! albedo
   real(r8), allocatable :: alb_roof  (:,:,:) !albedo of roof [-]
   real(r8), allocatable :: alb_wall  (:,:,:) !albedo of walls [-]
   real(r8), allocatable :: alb_gimp  (:,:,:) !albedo of impervious [-]
   real(r8), allocatable :: alb_gper  (:,:,:) !albedo of pervious [-]

   ! emissivity
   real(r8), allocatable :: em_roof       (:) !emissivity of roof [-]
   real(r8), allocatable :: em_wall       (:) !emissivity of walls [-]
   real(r8), allocatable :: em_gimp       (:) !emissivity of impervious [-]
   real(r8), allocatable :: em_gper       (:) !emissivity of pervious [-]

   ! thermal pars of roof, wall, impervious
   real(r8), allocatable :: cv_roof     (:,:) !heat capacity of roof [J/(m2 K)]
   real(r8), allocatable :: cv_wall     (:,:) !heat capacity of wall [J/(m2 K)]
   real(r8), allocatable :: cv_gimp     (:,:) !heat capacity of impervious [J/(m2 K)]

   real(r8), allocatable :: tk_roof     (:,:) !thermal conductivity of roof [W/m-K]
   real(r8), allocatable :: tk_wall     (:,:) !thermal conductivity of wall [W/m-K]
   real(r8), allocatable :: tk_gimp     (:,:) !thermal conductivity of impervious [W/m-K]

   ! room maximum and minimum temperature
   real(r8), allocatable :: t_roommax     (:) !maximum temperature of inner room [K]
   real(r8), allocatable :: t_roommin     (:) !minimum temperature of inner room [K]

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: allocate_UrbanTimeInvariants
   PUBLIC :: deallocate_UrbanTimeInvariants
   PUBLIC :: READ_UrbanTimeInvariants
   PUBLIC :: WRITE_UrbanTimeInvariants

! PRIVATE MEMBER FUNCTIONS:

!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------

   SUBROUTINE allocate_UrbanTimeInvariants ()
! ------------------------------------------------------
! Allocates memory for CLM 1d [numurban] variants
! ------------------------------------------------------
   USE MOD_Precision
   USE MOD_MPAS_MPI
   USE MOD_LandUrban
   USE MOD_Vars_Global
   IMPLICIT NONE

      IF (.true.) THEN
         IF (numurban > 0) THEN
            allocate (fveg_urb             (numurban))
            allocate (htop_urb             (numurban))
            allocate (hbot_urb             (numurban))
            allocate (froof                (numurban))
            allocate (fgper                (numurban))
            allocate (flake                (numurban))
            allocate (hroof                (numurban))
            allocate (hlr                  (numurban))

            allocate (alb_roof         (2,2,numurban))
            allocate (alb_wall         (2,2,numurban))
            allocate (alb_gimp         (2,2,numurban))
            allocate (alb_gper         (2,2,numurban))

            allocate (em_roof              (numurban))
            allocate (em_wall              (numurban))
            allocate (em_gimp              (numurban))
            allocate (em_gper              (numurban))

            allocate (z_roof     (1:nl_roof,numurban))
            allocate (z_wall     (1:nl_wall,numurban))
            allocate (dz_roof    (1:nl_roof,numurban))
            allocate (dz_wall    (1:nl_wall,numurban))

            allocate (cv_roof    (1:nl_roof,numurban))
            allocate (cv_wall    (1:nl_wall,numurban))
            allocate (cv_gimp    (1:nl_soil,numurban))
            allocate (tk_roof    (1:nl_roof,numurban))
            allocate (tk_wall    (1:nl_wall,numurban))
            allocate (tk_gimp    (1:nl_soil,numurban))

            allocate (t_roommax            (numurban))
            allocate (t_roommin            (numurban))
            allocate (pop_den              (numurban))

            allocate (vehicle          (3  ,numurban))
            allocate (week_holiday     (7  ,numurban))
            allocate (weh_prof         (24 ,numurban))
            allocate (wdh_prof         (24 ,numurban))
            allocate (hum_prof         (24 ,numurban))
            allocate (fix_holiday      (365,numurban))
         ENDIF
      ENDIF

   END SUBROUTINE allocate_UrbanTimeInvariants

   SUBROUTINE READ_UrbanTimeInvariants (file_restart)

   USE MOD_NetCDFVector
   USE MOD_LandUrban

   IMPLICIT NONE

   integer, parameter :: ns = 2
   integer, parameter :: nr = 2
   integer, parameter :: ulev = 10
   character(len=*), intent(in) :: file_restart

      ! vegetation
      CALL ncio_read_vector (file_restart, 'PCT_Tree'      , landurban, fveg_urb )
      CALL ncio_read_vector (file_restart, 'URBAN_TREE_TOP', landurban, htop_urb )
      CALL ncio_read_vector (file_restart, 'URBAN_TREE_BOT', landurban, hbot_urb )
      CALL ncio_read_vector (file_restart, 'PCT_Water'     , landurban, flake    )

      ! LUCY paras !TODO: variable name can be optimized
      CALL ncio_read_vector (file_restart, 'POP_DEN'     ,      landurban, pop_den      )
      CALL ncio_read_vector (file_restart, 'VEHC_NUM'    , 3  , landurban, vehicle      )
      CALL ncio_read_vector (file_restart, 'week_holiday', 7  , landurban, week_holiday )
      CALL ncio_read_vector (file_restart, 'weekendhour' , 24 , landurban, weh_prof     )
      CALL ncio_read_vector (file_restart, 'weekdayhour' , 24 , landurban, wdh_prof     )
      CALL ncio_read_vector (file_restart, 'metabolism'  , 24 , landurban, hum_prof     )
      CALL ncio_read_vector (file_restart, 'holiday'     , 365, landurban, fix_holiday  )

      ! morphological paras
      CALL ncio_read_vector (file_restart, 'WT_ROOF'       , landurban, froof     )
      CALL ncio_read_vector (file_restart, 'HT_ROOF'       , landurban, hroof     )
      CALL ncio_read_vector (file_restart, 'BUILDING_HLR'  , landurban, hlr       )
      CALL ncio_read_vector (file_restart, 'WTROAD_PERV'   , landurban, fgper     )
      CALL ncio_read_vector (file_restart, 'EM_ROOF'       , landurban, em_roof   )
      CALL ncio_read_vector (file_restart, 'EM_WALL'       , landurban, em_wall   )
      CALL ncio_read_vector (file_restart, 'EM_IMPROAD'    , landurban, em_gimp   )
      CALL ncio_read_vector (file_restart, 'EM_PERROAD'    , landurban, em_gper   )
      CALL ncio_read_vector (file_restart, 'T_BUILDING_MIN', landurban, t_roommin )
      CALL ncio_read_vector (file_restart, 'T_BUILDING_MAX', landurban, t_roommax )

      CALL ncio_read_vector (file_restart, 'ROOF_DEPTH_L'  , ulev, landurban, z_roof  )
      CALL ncio_read_vector (file_restart, 'ROOF_THICK_L'  , ulev, landurban, dz_roof )
      CALL ncio_read_vector (file_restart, 'WALL_DEPTH_L'  , ulev, landurban, z_wall  )
      CALL ncio_read_vector (file_restart, 'WALL_THICK_L'  , ulev, landurban, dz_wall )

      ! thermal paras
      CALL ncio_read_vector (file_restart, 'CV_ROOF'   , ulev, landurban, cv_roof )
      CALL ncio_read_vector (file_restart, 'CV_WALL'   , ulev, landurban, cv_wall )
      CALL ncio_read_vector (file_restart, 'TK_ROOF'   , ulev, landurban, tk_roof )
      CALL ncio_read_vector (file_restart, 'TK_WALL'   , ulev, landurban, tk_wall )
      CALL ncio_read_vector (file_restart, 'TK_IMPROAD', ulev, landurban, tk_gimp )
      CALL ncio_read_vector (file_restart, 'CV_IMPROAD', ulev, landurban, cv_gimp )

      CALL ncio_read_vector (file_restart, 'ALB_ROOF'   , ns, nr, landurban, alb_roof )
      CALL ncio_read_vector (file_restart, 'ALB_WALL'   , ns, nr, landurban, alb_wall )
      CALL ncio_read_vector (file_restart, 'ALB_IMPROAD', ns, nr, landurban, alb_gimp )
      CALL ncio_read_vector (file_restart, 'ALB_PERROAD', ns, nr, landurban, alb_gper )

   END SUBROUTINE READ_UrbanTimeInvariants

   SUBROUTINE WRITE_UrbanTimeInvariants (file_restart)

   USE MOD_NetCDFVector
   USE MOD_LandUrban
   USE MOD_Namelist
   USE MOD_Vars_Global

   IMPLICIT NONE

   integer, parameter :: ns    = 2
   integer, parameter :: nr    = 2
   integer, parameter :: ulev  = 10
   integer, parameter :: ityp  = 3
   integer, parameter :: ihour = 24
   integer, parameter :: iweek = 7
   integer, parameter :: iday  = 365
   ! Local variables
   character(len=*), intent(in) :: file_restart
   integer :: compress

      compress = DEF_REST_CompressLevel

      CALL ncio_create_file_vector (file_restart, landurban)
      CALL ncio_define_dimension_vector (file_restart, landurban, 'urban')

      CALL ncio_define_dimension_vector (file_restart, landurban, 'urban')
      CALL ncio_define_dimension_vector (file_restart, landurban, 'numsolar', nr  )
      CALL ncio_define_dimension_vector (file_restart, landurban, 'numrad'  , ns  )
      CALL ncio_define_dimension_vector (file_restart, landurban, 'ulev'    , ulev)
      CALL ncio_define_dimension_vector (file_restart, landurban, 'ityp'    , 3   )
      CALL ncio_define_dimension_vector (file_restart, landurban, 'iweek'   , 7   )
      CALL ncio_define_dimension_vector (file_restart, landurban, 'ihour'   , 24  )
      CALL ncio_define_dimension_vector (file_restart, landurban, 'iday'    , 365 )

      ! vegetation
      CALL ncio_write_vector (file_restart, 'PCT_Tree'      , 'urban', landurban, fveg_urb, DEF_REST_CompressLevel)
      CALL ncio_write_vector (file_restart, 'URBAN_TREE_TOP', 'urban', landurban, htop_urb, DEF_REST_CompressLevel)
      CALL ncio_write_vector (file_restart, 'URBAN_TREE_BOT', 'urban', landurban, hbot_urb, DEF_REST_CompressLevel)
      CALL ncio_write_vector (file_restart, 'PCT_Water'     , 'urban', landurban, flake   , DEF_REST_CompressLevel)

      ! LUCY paras
      CALL ncio_write_vector (file_restart, 'POP_DEN'     , 'urban',                 landurban, pop_den     , DEF_REST_CompressLevel)
      CALL ncio_write_vector (file_restart, 'VEHC_NUM'    , 'ityp' , ityp , 'urban', landurban, vehicle     , DEF_REST_CompressLevel)
      CALL ncio_write_vector (file_restart, 'week_holiday', 'iweek', iweek, 'urban', landurban, week_holiday, DEF_REST_CompressLevel)
      CALL ncio_write_vector (file_restart, 'weekendhour' , 'ihour', ihour, 'urban', landurban, weh_prof    , DEF_REST_CompressLevel)
      CALL ncio_write_vector (file_restart, 'weekdayhour' , 'ihour', ihour, 'urban', landurban, wdh_prof    , DEF_REST_CompressLevel)
      CALL ncio_write_vector (file_restart, 'metabolism'  , 'ihour', ihour, 'urban', landurban, hum_prof    , DEF_REST_CompressLevel)
      CALL ncio_write_vector (file_restart, 'holiday'     , 'iday' , iday , 'urban', landurban, fix_holiday , DEF_REST_CompressLevel)

      ! morphological paras
      CALL ncio_write_vector (file_restart, 'WT_ROOF'       , 'urban', landurban, froof    , DEF_REST_CompressLevel)
      CALL ncio_write_vector (file_restart, 'HT_ROOF'       , 'urban', landurban, hroof    , DEF_REST_CompressLevel)
      CALL ncio_write_vector (file_restart, 'BUILDING_HLR'  , 'urban', landurban, hlr      , DEF_REST_CompressLevel)
      CALL ncio_write_vector (file_restart, 'WTROAD_PERV'   , 'urban', landurban, fgper    , DEF_REST_CompressLevel)
      CALL ncio_write_vector (file_restart, 'EM_ROOF'       , 'urban', landurban, em_roof  , DEF_REST_CompressLevel)
      CALL ncio_write_vector (file_restart, 'EM_WALL'       , 'urban', landurban, em_wall  , DEF_REST_CompressLevel)
      CALL ncio_write_vector (file_restart, 'EM_IMPROAD'    , 'urban', landurban, em_gimp  , DEF_REST_CompressLevel)
      CALL ncio_write_vector (file_restart, 'EM_PERROAD'    , 'urban', landurban, em_gper  , DEF_REST_CompressLevel)
      CALL ncio_write_vector (file_restart, 'T_BUILDING_MIN', 'urban', landurban, t_roommin, DEF_REST_CompressLevel)
      CALL ncio_write_vector (file_restart, 'T_BUILDING_MAX', 'urban', landurban, t_roommax, DEF_REST_CompressLevel)

      CALL ncio_write_vector (file_restart, 'ROOF_DEPTH_L', 'ulev', ulev, 'urban', landurban, z_roof , DEF_REST_CompressLevel)
      CALL ncio_write_vector (file_restart, 'ROOF_THICK_L', 'ulev', ulev, 'urban', landurban, dz_roof, DEF_REST_CompressLevel)
      CALL ncio_write_vector (file_restart, 'WALL_DEPTH_L', 'ulev', ulev, 'urban', landurban, z_wall , DEF_REST_CompressLevel)
      CALL ncio_write_vector (file_restart, 'WALL_THICK_L', 'ulev', ulev, 'urban', landurban, dz_wall, DEF_REST_CompressLevel)

      ! thermal paras
      CALL ncio_write_vector (file_restart, 'CV_ROOF'   , 'ulev', ulev, 'urban', landurban, cv_roof, DEF_REST_CompressLevel)
      CALL ncio_write_vector (file_restart, 'CV_WALL'   , 'ulev', ulev, 'urban', landurban, cv_wall, DEF_REST_CompressLevel)
      CALL ncio_write_vector (file_restart, 'TK_ROOF'   , 'ulev', ulev, 'urban', landurban, tk_roof, DEF_REST_CompressLevel)
      CALL ncio_write_vector (file_restart, 'TK_WALL'   , 'ulev', ulev, 'urban', landurban, tk_wall, DEF_REST_CompressLevel)
      CALL ncio_write_vector (file_restart, 'TK_IMPROAD', 'ulev', ulev, 'urban', landurban, tk_gimp, DEF_REST_CompressLevel)
      CALL ncio_write_vector (file_restart, 'CV_IMPROAD', 'ulev', ulev, 'urban', landurban, cv_gimp, DEF_REST_CompressLevel)

      CALL ncio_write_vector (file_restart, 'ALB_ROOF'   , 'numsolar', ns, 'numrad', nr, 'urban', landurban, alb_roof, DEF_REST_CompressLevel)
      CALL ncio_write_vector (file_restart, 'ALB_WALL'   , 'numsolar', ns, 'numrad', nr, 'urban', landurban, alb_wall, DEF_REST_CompressLevel)
      CALL ncio_write_vector (file_restart, 'ALB_IMPROAD', 'numsolar', ns, 'numrad', nr, 'urban', landurban, alb_gimp, DEF_REST_CompressLevel)
      CALL ncio_write_vector (file_restart, 'ALB_PERROAD', 'numsolar', ns, 'numrad', nr, 'urban', landurban, alb_gper, DEF_REST_CompressLevel)

   END SUBROUTINE WRITE_UrbanTimeInvariants

   SUBROUTINE deallocate_UrbanTimeInvariants

   USE MOD_MPAS_MPI
   USE MOD_LandUrban

      ! deallocate (urbclass  )

      IF (.true.) THEN
         IF (numurban > 0) THEN
            deallocate (fveg_urb     )
            deallocate (htop_urb     )
            deallocate (hbot_urb     )
            deallocate (froof        )
            deallocate (fgper        )
            deallocate (flake        )
            deallocate (hroof        )
            deallocate (hlr          )

            deallocate (alb_roof     )
            deallocate (alb_wall     )
            deallocate (alb_gimp     )
            deallocate (alb_gper     )

            deallocate (em_roof      )
            deallocate (em_wall      )
            deallocate (em_gimp      )
            deallocate (em_gper      )

            deallocate (z_roof       )
            deallocate (z_wall       )
            deallocate (dz_roof      )
            deallocate (dz_wall      )

            deallocate (cv_roof      )
            deallocate (cv_wall      )
            deallocate (cv_gimp      )
            deallocate (tk_roof      )
            deallocate (tk_wall      )
            deallocate (tk_gimp      )

            deallocate (t_roommax    )
            deallocate (t_roommin    )

            deallocate (pop_den      )
            deallocate (vehicle      )
            deallocate (week_holiday )
            deallocate (weh_prof     )
            deallocate (wdh_prof     )
            deallocate (hum_prof     )
            deallocate (fix_holiday  )
         ENDIF
      ENDIF
   END SUBROUTINE deallocate_UrbanTimeInvariants

END MODULE MOD_Urban_Vars_TimeInvariants
#endif
! ---------- EOP ------------
