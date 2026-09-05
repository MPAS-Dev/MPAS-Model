#include <define.h>

#if (defined URBAN_MODEL)
MODULE MOD_Urban_Vars_TimeVariables

!-----------------------------------------------------------------------
! !DESCRIPTION:
!
!  Define urban model time variant variables.
!
!  Created by Hua Yuan, 12/2020
!-----------------------------------------------------------------------

   USE MOD_Precision
   IMPLICIT NONE
   SAVE
! -----------------------------------------------------------------
! Time-varying state variables which required by restart run

   real(r8), allocatable :: fwsun          (:) !sunlit fraction of walls [-]
   real(r8), allocatable :: dfwsun         (:) !change of sunlit fraction of walls [-]

   ! shortwave absorption
   real(r8), allocatable :: sroof      (:,:,:) !roof absorption [-]
   real(r8), allocatable :: swsun      (:,:,:) !sunlit wall absorption [-]
   real(r8), allocatable :: swsha      (:,:,:) !shaded wall absorption [-]
   real(r8), allocatable :: sgimp      (:,:,:) !impervious absorption [-]
   real(r8), allocatable :: sgper      (:,:,:) !pervious absorption [-]
   real(r8), allocatable :: slake      (:,:,:) !urban lake absorption [-]

   ! net longwave radiation for last time temperature change
   real(r8), allocatable :: lwsun          (:) !net longwave of sunlit wall [W/m2]
   real(r8), allocatable :: lwsha          (:) !net longwave of shaded wall [W/m2]
   real(r8), allocatable :: lgimp          (:) !net longwave of impervious  [W/m2]
   real(r8), allocatable :: lgper          (:) !net longwave of pervious [W/m2]
   real(r8), allocatable :: lveg           (:) !net longwave of vegetation [W/m2]

   real(r8), allocatable :: z_sno_roof   (:,:) !node depth of roof [m]
   real(r8), allocatable :: z_sno_gimp   (:,:) !node depth of impervious [m]
   real(r8), allocatable :: z_sno_gper   (:,:) !node depth pervious [m]
   real(r8), allocatable :: z_sno_lake   (:,:) !node depth lake [m]

   real(r8), allocatable :: dz_sno_roof  (:,:) !interface depth of roof [m]
   real(r8), allocatable :: dz_sno_gimp  (:,:) !interface depth of impervious [m]
   real(r8), allocatable :: dz_sno_gper  (:,:) !interface depth pervious [m]
   real(r8), allocatable :: dz_sno_lake  (:,:) !interface depth lake [m]

   real(r8), allocatable :: troof_inner    (:) !temperature of roof [K]
   real(r8), allocatable :: twsun_inner    (:) !temperature of sunlit wall [K]
   real(r8), allocatable :: twsha_inner    (:) !temperature of shaded wall [K]

   real(r8), allocatable :: t_roofsno    (:,:) !temperature of roof [K]
   real(r8), allocatable :: t_wallsun    (:,:) !temperature of sunlit wall [K]
   real(r8), allocatable :: t_wallsha    (:,:) !temperature of shaded wall [K]
   real(r8), allocatable :: t_gimpsno    (:,:) !temperature of impervious [K]
   real(r8), allocatable :: t_gpersno    (:,:) !temperature of pervious [K]
   real(r8), allocatable :: t_lakesno    (:,:) !temperature of pervious [K]

   real(r8), allocatable :: wliq_roofsno (:,:) !liquid water in layers [kg/m2]
   real(r8), allocatable :: wliq_gimpsno (:,:) !liquid water in layers [kg/m2]
   real(r8), allocatable :: wliq_gpersno (:,:) !liquid water in layers [kg/m2]
   real(r8), allocatable :: wliq_lakesno (:,:) !liquid water in layers [kg/m2]
   real(r8), allocatable :: wice_roofsno (:,:) !ice lens in layers [kg/m2]
   real(r8), allocatable :: wice_gimpsno (:,:) !ice lens in layers [kg/m2]
   real(r8), allocatable :: wice_gpersno (:,:) !ice lens in layers [kg/m2]
   real(r8), allocatable :: wice_lakesno (:,:) !ice lens in layers [kg/m2]

   real(r8), allocatable :: sag_roof       (:) !roof snow age [-]
   real(r8), allocatable :: sag_gimp       (:) !impervious ground snow age [-]
   real(r8), allocatable :: sag_gper       (:) !pervious ground snow age [-]
   real(r8), allocatable :: sag_lake       (:) !urban lake snow age [-]

   real(r8), allocatable :: scv_roof       (:) !roof snow mass [kg/m2]
   real(r8), allocatable :: scv_gimp       (:) !impervious ground snow mass [kg/m2]
   real(r8), allocatable :: scv_gper       (:) !pervious ground snow mass [kg/m2]
   real(r8), allocatable :: scv_lake       (:) !urban lake snow mass [kg/m2]

   real(r8), allocatable :: fsno_roof      (:) !roof snow fraction [-]
   real(r8), allocatable :: fsno_gimp      (:) !impervious ground snow fraction [-]
   real(r8), allocatable :: fsno_gper      (:) !pervious ground snow fraction [-]
   real(r8), allocatable :: fsno_lake      (:) !urban lake snow fraction [-]

   real(r8), allocatable :: snowdp_roof    (:) !roof snow depth [m]
   real(r8), allocatable :: snowdp_gimp    (:) !impervious ground snow depth [m]
   real(r8), allocatable :: snowdp_gper    (:) !pervious ground snow depth [m]
   real(r8), allocatable :: snowdp_lake    (:) !urban lake snow depth [m]

   !TODO: rename the below variables
   real(r8), allocatable :: Fhac           (:) !sensible flux from heat or cool AC [W/m2]
   real(r8), allocatable :: Fwst           (:) !waste heat flux from heat or cool AC [W/m2]
   real(r8), allocatable :: Fach           (:) !flux from inner and outer air exchange [W/m2]
   real(r8), allocatable :: Fahe           (:) !flux from metabolism and vehicle [W/m2]
   real(r8), allocatable :: Fhah           (:) !sensible heat flux from heating [W/m2]
   real(r8), allocatable :: vehc           (:) !flux from vehicle [W/m2]
   real(r8), allocatable :: meta           (:) !flux from metabolism [W/m2]

   real(r8), allocatable :: t_room         (:) !temperature of inner building [K]
   real(r8), allocatable :: t_roof         (:) !temperature of roof [K]
   real(r8), allocatable :: t_wall         (:) !temperature of wall [K]
   real(r8), allocatable :: tafu           (:) !temperature of outer building [K]

   real(r8), allocatable :: urb_green      (:) !fractional of green leaf in urban patch [-]
   real(r8), allocatable :: urb_lai        (:) !urban tree LAI [m2/m2]
   real(r8), allocatable :: urb_sai        (:) !urban tree SAI [m2/m2]


! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: allocate_UrbanTimeVariables
   PUBLIC :: deallocate_UrbanTimeVariables
   PUBLIC :: READ_UrbanTimeVariables
   PUBLIC :: WRITE_UrbanTimeVariables

! PRIVATE MEMBER FUNCTIONS:

!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------

   SUBROUTINE allocate_UrbanTimeVariables ()
! ------------------------------------------------------
! Allocates memory for CLM 1d [numurban] variables
! ------------------------------------------------------
   USE MOD_Precision
   USE MOD_MPAS_MPI
   USE MOD_LandUrban
   USE MOD_Vars_Global
   IMPLICIT NONE

      IF (.true.) THEN
         IF (numurban > 0) THEN
            allocate (fwsun                         (numurban))
            allocate (dfwsun                        (numurban))

            allocate (sroof                     (2,2,numurban))
            allocate (swsun                     (2,2,numurban))
            allocate (swsha                     (2,2,numurban))
            allocate (sgimp                     (2,2,numurban))
            allocate (sgper                     (2,2,numurban))
            allocate (slake                     (2,2,numurban))

            allocate (lwsun                         (numurban))
            allocate (lwsha                         (numurban))
            allocate (lgimp                         (numurban))
            allocate (lgper                         (numurban))
            allocate (lveg                          (numurban))

            allocate (z_sno_roof         (maxsnl+1:0,numurban))
            allocate (z_sno_gimp         (maxsnl+1:0,numurban))
            allocate (z_sno_gper         (maxsnl+1:0,numurban))
            allocate (z_sno_lake         (maxsnl+1:0,numurban))

            allocate (dz_sno_roof        (maxsnl+1:0,numurban))
            allocate (dz_sno_gimp        (maxsnl+1:0,numurban))
            allocate (dz_sno_gper        (maxsnl+1:0,numurban))
            allocate (dz_sno_lake        (maxsnl+1:0,numurban))

            allocate (troof_inner                   (numurban))
            allocate (twsun_inner                   (numurban))
            allocate (twsha_inner                   (numurban))

            allocate (t_roofsno    (maxsnl+1:nl_roof,numurban))
            allocate (t_wallsun    (maxsnl+1:nl_wall,numurban))
            allocate (t_wallsha    (maxsnl+1:nl_wall,numurban))
            allocate (t_gimpsno    (maxsnl+1:nl_soil,numurban))
            allocate (t_gpersno    (maxsnl+1:nl_soil,numurban))
            allocate (t_lakesno    (maxsnl+1:nl_soil,numurban))

            allocate (wliq_roofsno (maxsnl+1:nl_roof,numurban))
            allocate (wice_roofsno (maxsnl+1:nl_roof,numurban))
            allocate (wliq_gimpsno (maxsnl+1:nl_soil,numurban))
            allocate (wice_gimpsno (maxsnl+1:nl_soil,numurban))
            allocate (wliq_gpersno (maxsnl+1:nl_soil,numurban))
            allocate (wice_gpersno (maxsnl+1:nl_soil,numurban))
            allocate (wliq_lakesno (maxsnl+1:nl_soil,numurban))
            allocate (wice_lakesno (maxsnl+1:nl_soil,numurban))

            allocate (sag_roof                      (numurban))
            allocate (sag_gimp                      (numurban))
            allocate (sag_gper                      (numurban))
            allocate (sag_lake                      (numurban))
            allocate (scv_roof                      (numurban))
            allocate (scv_gimp                      (numurban))
            allocate (scv_gper                      (numurban))
            allocate (scv_lake                      (numurban))
            allocate (fsno_roof                     (numurban))
            allocate (fsno_gimp                     (numurban))
            allocate (fsno_gper                     (numurban))
            allocate (fsno_lake                     (numurban))
            allocate (snowdp_roof                   (numurban))
            allocate (snowdp_gimp                   (numurban))
            allocate (snowdp_gper                   (numurban))
            allocate (snowdp_lake                   (numurban))

            allocate (Fhac                          (numurban))
            allocate (Fwst                          (numurban))
            allocate (Fach                          (numurban))
            allocate (Fahe                          (numurban))
            allocate (Fhah                          (numurban))
            allocate (vehc                          (numurban))
            allocate (meta                          (numurban))

            allocate (t_room                        (numurban))
            allocate (t_roof                        (numurban))
            allocate (t_wall                        (numurban))
            allocate (tafu                          (numurban))

            allocate (urb_green                     (numurban))
            allocate (urb_lai                       (numurban))
            allocate (urb_sai                       (numurban))
         ENDIF
      ENDIF
   END SUBROUTINE allocate_UrbanTimeVariables

   SUBROUTINE READ_UrbanTimeVariables (file_restart)

   USE MOD_NetCDFVector
   USE MOD_LandUrban
   USE MOD_Vars_Global

   IMPLICIT NONE

   character(len=*), intent(in) :: file_restart

      CALL ncio_read_vector (file_restart, 'fwsun' , landurban, fwsun )
      CALL ncio_read_vector (file_restart, 'dfwsun', landurban, dfwsun)

      CALL ncio_read_vector (file_restart, 'sroof', 2, 2, landurban, sroof)
      CALL ncio_read_vector (file_restart, 'swsun', 2, 2, landurban, swsun)
      CALL ncio_read_vector (file_restart, 'swsha', 2, 2, landurban, swsha)
      CALL ncio_read_vector (file_restart, 'sgimp', 2, 2, landurban, sgimp)
      CALL ncio_read_vector (file_restart, 'sgper', 2, 2, landurban, sgper)
      CALL ncio_read_vector (file_restart, 'slake', 2, 2, landurban, slake)

      CALL ncio_read_vector (file_restart, 'lwsun', landurban, lwsun)
      CALL ncio_read_vector (file_restart, 'lwsha', landurban, lwsha)
      CALL ncio_read_vector (file_restart, 'lgimp', landurban, lgimp)
      CALL ncio_read_vector (file_restart, 'lgper', landurban, lgper)
      CALL ncio_read_vector (file_restart, 'lveg' , landurban, lveg )

      CALL ncio_read_vector (file_restart, 'z_sno_roof' , -maxsnl, landurban, z_sno_roof )
      CALL ncio_read_vector (file_restart, 'z_sno_gimp' , -maxsnl, landurban, z_sno_gimp )
      CALL ncio_read_vector (file_restart, 'z_sno_gper' , -maxsnl, landurban, z_sno_gper )
      CALL ncio_read_vector (file_restart, 'z_sno_lake' , -maxsnl, landurban, z_sno_lake )

      CALL ncio_read_vector (file_restart, 'dz_sno_roof', -maxsnl, landurban, dz_sno_roof)
      CALL ncio_read_vector (file_restart, 'dz_sno_gimp', -maxsnl, landurban, dz_sno_gimp)
      CALL ncio_read_vector (file_restart, 'dz_sno_gper', -maxsnl, landurban, dz_sno_gper)
      CALL ncio_read_vector (file_restart, 'dz_sno_lake', -maxsnl, landurban, dz_sno_lake)

      CALL ncio_read_vector (file_restart, 'troof_inner', landurban, troof_inner)
      CALL ncio_read_vector (file_restart, 'twsun_inner', landurban, twsun_inner)
      CALL ncio_read_vector (file_restart, 'twsha_inner', landurban, twsha_inner)

      CALL ncio_read_vector (file_restart, 't_roofsno', nl_roof-maxsnl, landurban, t_roofsno)
      CALL ncio_read_vector (file_restart, 't_wallsun', nl_wall-maxsnl, landurban, t_wallsun)
      CALL ncio_read_vector (file_restart, 't_wallsha', nl_wall-maxsnl, landurban, t_wallsha)
      CALL ncio_read_vector (file_restart, 't_gimpsno', nl_soil-maxsnl, landurban, t_gimpsno)
      CALL ncio_read_vector (file_restart, 't_gpersno', nl_soil-maxsnl, landurban, t_gpersno)
      CALL ncio_read_vector (file_restart, 't_lakesno', nl_soil-maxsnl, landurban, t_lakesno)

      CALL ncio_read_vector (file_restart, 'wliq_roofsno', nl_roof-maxsnl, landurban, wliq_roofsno)
      CALL ncio_read_vector (file_restart, 'wliq_gimpsno', nl_soil-maxsnl, landurban, wliq_gimpsno)
      CALL ncio_read_vector (file_restart, 'wliq_gpersno', nl_soil-maxsnl, landurban, wliq_gpersno)
      CALL ncio_read_vector (file_restart, 'wliq_lakesno', nl_soil-maxsnl, landurban, wliq_lakesno)
      CALL ncio_read_vector (file_restart, 'wice_roofsno', nl_roof-maxsnl, landurban, wice_roofsno)
      CALL ncio_read_vector (file_restart, 'wice_gimpsno', nl_soil-maxsnl, landurban, wice_gimpsno)
      CALL ncio_read_vector (file_restart, 'wice_gpersno', nl_soil-maxsnl, landurban, wice_gpersno)
      CALL ncio_read_vector (file_restart, 'wice_lakesno', nl_soil-maxsnl, landurban, wice_lakesno)

      CALL ncio_read_vector (file_restart, 'sag_roof'   , landurban, sag_roof   )
      CALL ncio_read_vector (file_restart, 'sag_gimp'   , landurban, sag_gimp   )
      CALL ncio_read_vector (file_restart, 'sag_gper'   , landurban, sag_gper   )
      CALL ncio_read_vector (file_restart, 'sag_lake'   , landurban, sag_lake   )
      CALL ncio_read_vector (file_restart, 'scv_roof'   , landurban, scv_roof   )
      CALL ncio_read_vector (file_restart, 'scv_gimp'   , landurban, scv_gimp   )
      CALL ncio_read_vector (file_restart, 'scv_gper'   , landurban, scv_gper   )
      CALL ncio_read_vector (file_restart, 'scv_lake'   , landurban, scv_lake   )
      CALL ncio_read_vector (file_restart, 'fsno_roof'  , landurban, fsno_roof  )
      CALL ncio_read_vector (file_restart, 'fsno_gimp'  , landurban, fsno_gimp  )
      CALL ncio_read_vector (file_restart, 'fsno_gper'  , landurban, fsno_gper  )
      CALL ncio_read_vector (file_restart, 'fsno_lake'  , landurban, fsno_lake  )
      CALL ncio_read_vector (file_restart, 'snowdp_roof', landurban, snowdp_roof)
      CALL ncio_read_vector (file_restart, 'snowdp_gimp', landurban, snowdp_gimp)
      CALL ncio_read_vector (file_restart, 'snowdp_gper', landurban, snowdp_gper)
      CALL ncio_read_vector (file_restart, 'snowdp_lake', landurban, snowdp_lake)
      CALL ncio_read_vector (file_restart, 'Fhac'       , landurban, Fhac       )
      CALL ncio_read_vector (file_restart, 'Fwst'       , landurban, Fwst       )
      CALL ncio_read_vector (file_restart, 'Fach'       , landurban, Fach       )
      CALL ncio_read_vector (file_restart, 'Fahe'       , landurban, Fahe       )
      CALL ncio_read_vector (file_restart, 'Fhah'       , landurban, Fhah       )
      CALL ncio_read_vector (file_restart, 'vehc'       , landurban, vehc       )
      CALL ncio_read_vector (file_restart, 'meta'       , landurban, meta       )
      CALL ncio_read_vector (file_restart, 't_room '    , landurban, t_room     )
      CALL ncio_read_vector (file_restart, 't_roof'     , landurban, t_roof     )
      CALL ncio_read_vector (file_restart, 't_wall'     , landurban, t_wall     )
      CALL ncio_read_vector (file_restart, 'tafu'       , landurban, tafu       )
      CALL ncio_read_vector (file_restart, 'urb_green'  , landurban, urb_green  )
      CALL ncio_read_vector (file_restart, 'tree_lai'   , landurban, urb_lai    )
      CALL ncio_read_vector (file_restart, 'tree_sai'   , landurban, urb_sai    )

   END SUBROUTINE READ_UrbanTimeVariables

   SUBROUTINE WRITE_UrbanTimeVariables (file_restart)

   USE MOD_Namelist, only: DEF_REST_CompressLevel
   USE MOD_LandUrban
   USE MOD_NetCDFVector
   USE MOD_Vars_Global
   IMPLICIT NONE

   character(len=*), intent(in) :: file_restart

   ! Local variables
   integer :: compress

      compress = DEF_REST_CompressLevel

      CALL ncio_create_file_vector (file_restart, landurban)
      CALL ncio_define_dimension_vector (file_restart, landurban, 'urban')

      CALL ncio_define_dimension_vector (file_restart, landurban, 'snow'    , -maxsnl       )
      CALL ncio_define_dimension_vector (file_restart, landurban, 'soil'    , nl_soil       )
      CALL ncio_define_dimension_vector (file_restart, landurban, 'roof'    , nl_roof       )
      CALL ncio_define_dimension_vector (file_restart, landurban, 'wall'    , nl_wall       )
      CALL ncio_define_dimension_vector (file_restart, landurban, 'soilsnow', nl_soil-maxsnl)
      CALL ncio_define_dimension_vector (file_restart, landurban, 'roofsnow', nl_roof-maxsnl)
      CALL ncio_define_dimension_vector (file_restart, landurban, 'wallsnow', nl_wall-maxsnl)

      CALL ncio_define_dimension_vector (file_restart, landurban, 'band', 2)
      CALL ncio_define_dimension_vector (file_restart, landurban, 'rtyp', 2)

      CALL ncio_write_vector (file_restart, 'fwsun' , 'urban', landurban, fwsun , compress)
      CALL ncio_write_vector (file_restart, 'dfwsun', 'urban', landurban, dfwsun, compress)

      CALL ncio_write_vector (file_restart, 'sroof', 'band', 2, 'rtyp', 2, 'urban', landurban, sroof, compress)
      CALL ncio_write_vector (file_restart, 'swsun', 'band', 2, 'rtyp', 2, 'urban', landurban, swsun, compress)
      CALL ncio_write_vector (file_restart, 'swsha', 'band', 2, 'rtyp', 2, 'urban', landurban, swsha, compress)
      CALL ncio_write_vector (file_restart, 'sgimp', 'band', 2, 'rtyp', 2, 'urban', landurban, sgimp, compress)
      CALL ncio_write_vector (file_restart, 'sgper', 'band', 2, 'rtyp', 2, 'urban', landurban, sgper, compress)
      CALL ncio_write_vector (file_restart, 'slake', 'band', 2, 'rtyp', 2, 'urban', landurban, slake, compress)

      CALL ncio_write_vector (file_restart, 'lwsun', 'urban', landurban, lwsun, compress)
      CALL ncio_write_vector (file_restart, 'lwsha', 'urban', landurban, lwsha, compress)
      CALL ncio_write_vector (file_restart, 'lgimp', 'urban', landurban, lgimp, compress)
      CALL ncio_write_vector (file_restart, 'lgper', 'urban', landurban, lgper, compress)
      CALL ncio_write_vector (file_restart, 'lveg' , 'urban', landurban, lveg , compress)

      CALL ncio_write_vector (file_restart, 'z_sno_roof' , 'snow', -maxsnl, 'urban', landurban, z_sno_roof , compress)
      CALL ncio_write_vector (file_restart, 'z_sno_gimp' , 'snow', -maxsnl, 'urban', landurban, z_sno_gimp , compress)
      CALL ncio_write_vector (file_restart, 'z_sno_gper' , 'snow', -maxsnl, 'urban', landurban, z_sno_gper , compress)
      CALL ncio_write_vector (file_restart, 'z_sno_lake' , 'snow', -maxsnl, 'urban', landurban, z_sno_lake , compress)

      CALL ncio_write_vector (file_restart, 'dz_sno_roof', 'snow', -maxsnl, 'urban', landurban, dz_sno_roof, compress)
      CALL ncio_write_vector (file_restart, 'dz_sno_gimp', 'snow', -maxsnl, 'urban', landurban, dz_sno_gimp, compress)
      CALL ncio_write_vector (file_restart, 'dz_sno_gper', 'snow', -maxsnl, 'urban', landurban, dz_sno_gper, compress)
      CALL ncio_write_vector (file_restart, 'dz_sno_lake', 'snow', -maxsnl, 'urban', landurban, dz_sno_lake, compress)

      CALL ncio_write_vector (file_restart, 'troof_inner', 'urban', landurban, troof_inner, compress)
      CALL ncio_write_vector (file_restart, 'twsun_inner', 'urban', landurban, twsun_inner, compress)
      CALL ncio_write_vector (file_restart, 'twsha_inner', 'urban', landurban, twsha_inner, compress)

      CALL ncio_write_vector (file_restart, 't_roofsno', 'roofsnow', nl_roof-maxsnl, 'urban', landurban, t_roofsno, compress)
      CALL ncio_write_vector (file_restart, 't_wallsun', 'wallsnow', nl_wall-maxsnl, 'urban', landurban, t_wallsun, compress)
      CALL ncio_write_vector (file_restart, 't_wallsha', 'wallsnow', nl_wall-maxsnl, 'urban', landurban, t_wallsha, compress)
      CALL ncio_write_vector (file_restart, 't_gimpsno', 'soilsnow', nl_soil-maxsnl, 'urban', landurban, t_gimpsno, compress)
      CALL ncio_write_vector (file_restart, 't_gpersno', 'soilsnow', nl_soil-maxsnl, 'urban', landurban, t_gpersno, compress)
      CALL ncio_write_vector (file_restart, 't_lakesno', 'soilsnow', nl_soil-maxsnl, 'urban', landurban, t_lakesno, compress)

      CALL ncio_write_vector (file_restart, 'wliq_roofsno', 'roofsnow', nl_roof-maxsnl, 'urban', landurban, wliq_roofsno, compress)
      CALL ncio_write_vector (file_restart, 'wliq_gimpsno', 'soilsnow', nl_soil-maxsnl, 'urban', landurban, wliq_gimpsno, compress)
      CALL ncio_write_vector (file_restart, 'wliq_gpersno', 'soilsnow', nl_soil-maxsnl, 'urban', landurban, wliq_gpersno, compress)
      CALL ncio_write_vector (file_restart, 'wliq_lakesno', 'soilsnow', nl_soil-maxsnl, 'urban', landurban, wliq_lakesno, compress)
      CALL ncio_write_vector (file_restart, 'wice_roofsno', 'roofsnow', nl_roof-maxsnl, 'urban', landurban, wice_roofsno, compress)
      CALL ncio_write_vector (file_restart, 'wice_gimpsno', 'soilsnow', nl_soil-maxsnl, 'urban', landurban, wice_gimpsno, compress)
      CALL ncio_write_vector (file_restart, 'wice_gpersno', 'soilsnow', nl_soil-maxsnl, 'urban', landurban, wice_gpersno, compress)
      CALL ncio_write_vector (file_restart, 'wice_lakesno', 'soilsnow', nl_soil-maxsnl, 'urban', landurban, wice_lakesno, compress)

      CALL ncio_write_vector (file_restart, 'sag_roof'   , 'urban', landurban, sag_roof   , compress)
      CALL ncio_write_vector (file_restart, 'sag_gimp'   , 'urban', landurban, sag_gimp   , compress)
      CALL ncio_write_vector (file_restart, 'sag_gper'   , 'urban', landurban, sag_gper   , compress)
      CALL ncio_write_vector (file_restart, 'sag_lake'   , 'urban', landurban, sag_lake   , compress)
      CALL ncio_write_vector (file_restart, 'scv_roof'   , 'urban', landurban, scv_roof   , compress)
      CALL ncio_write_vector (file_restart, 'scv_gimp'   , 'urban', landurban, scv_gimp   , compress)
      CALL ncio_write_vector (file_restart, 'scv_gper'   , 'urban', landurban, scv_gper   , compress)
      CALL ncio_write_vector (file_restart, 'scv_lake'   , 'urban', landurban, scv_lake   , compress)
      CALL ncio_write_vector (file_restart, 'fsno_roof'  , 'urban', landurban, fsno_roof  , compress)
      CALL ncio_write_vector (file_restart, 'fsno_gimp'  , 'urban', landurban, fsno_gimp  , compress)
      CALL ncio_write_vector (file_restart, 'fsno_gper'  , 'urban', landurban, fsno_gper  , compress)
      CALL ncio_write_vector (file_restart, 'fsno_lake'  , 'urban', landurban, fsno_lake  , compress)
      CALL ncio_write_vector (file_restart, 'snowdp_roof', 'urban', landurban, snowdp_roof, compress)
      CALL ncio_write_vector (file_restart, 'snowdp_gimp', 'urban', landurban, snowdp_gimp, compress)
      CALL ncio_write_vector (file_restart, 'snowdp_gper', 'urban', landurban, snowdp_gper, compress)
      CALL ncio_write_vector (file_restart, 'snowdp_lake', 'urban', landurban, snowdp_lake, compress)
      CALL ncio_write_vector (file_restart, 't_room'     , 'urban', landurban, t_room     , compress)
      CALL ncio_write_vector (file_restart, 't_roof'     , 'urban', landurban, t_roof     , compress)
      CALL ncio_write_vector (file_restart, 't_wall'     , 'urban', landurban, t_wall     , compress)
      CALL ncio_write_vector (file_restart, 'tafu'       , 'urban', landurban, tafu       , compress)
      CALL ncio_write_vector (file_restart, 'Fhac'       , 'urban', landurban, Fhac       , compress)
      CALL ncio_write_vector (file_restart, 'Fwst'       , 'urban', landurban, Fwst       , compress)
      CALL ncio_write_vector (file_restart, 'Fach'       , 'urban', landurban, Fach       , compress)
      CALL ncio_write_vector (file_restart, 'Fahe'       , 'urban', landurban, Fahe       , compress)
      CALL ncio_write_vector (file_restart, 'Fhah'       , 'urban', landurban, Fhah       , compress)
      CALL ncio_write_vector (file_restart, 'vehc'       , 'urban', landurban, vehc       , compress)
      CALL ncio_write_vector (file_restart, 'meta'       , 'urban', landurban, meta       , compress)
      CALL ncio_write_vector (file_restart, 'tree_lai'   , 'urban', landurban, urb_lai    , compress)
      CALL ncio_write_vector (file_restart, 'tree_sai'   , 'urban', landurban, urb_sai    , compress)
      CALL ncio_write_vector (file_restart, 'urb_green'  , 'urban', landurban, urb_green  , compress)

   END SUBROUTINE WRITE_UrbanTimeVariables

   SUBROUTINE deallocate_UrbanTimeVariables

   USE MOD_MPAS_MPI
   USE MOD_LandUrban

      IF (.true.) THEN
         IF (numurban > 0) THEN
            deallocate (fwsun        )
            deallocate (dfwsun       )

            deallocate (sroof        )
            deallocate (swsun        )
            deallocate (swsha        )
            deallocate (sgimp        )
            deallocate (sgper        )
            deallocate (slake        )

            deallocate (lwsun        )
            deallocate (lwsha        )
            deallocate (lgimp        )
            deallocate (lgper        )
            deallocate (lveg         )

            deallocate (z_sno_roof   )
            deallocate (z_sno_gimp   )
            deallocate (z_sno_gper   )
            deallocate (z_sno_lake   )

            deallocate (dz_sno_roof  )
            deallocate (dz_sno_gimp  )
            deallocate (dz_sno_gper  )
            deallocate (dz_sno_lake  )

            deallocate (t_roofsno    )
            deallocate (t_wallsun    )
            deallocate (t_wallsha    )
            deallocate (t_gimpsno    )
            deallocate (t_gpersno    )
            deallocate (t_lakesno    )

            deallocate (troof_inner  )
            deallocate (twsun_inner  )
            deallocate (twsha_inner  )

            deallocate (wliq_roofsno )
            deallocate (wice_roofsno )
            deallocate (wliq_gimpsno )
            deallocate (wice_gimpsno )
            deallocate (wliq_gpersno )
            deallocate (wice_gpersno )
            deallocate (wliq_lakesno )
            deallocate (wice_lakesno )

            deallocate (sag_roof     )
            deallocate (sag_gimp     )
            deallocate (sag_gper     )
            deallocate (sag_lake     )
            deallocate (scv_roof     )
            deallocate (scv_gimp     )
            deallocate (scv_gper     )
            deallocate (scv_lake     )
            deallocate (fsno_roof    )
            deallocate (fsno_gimp    )
            deallocate (fsno_gper    )
            deallocate (fsno_lake    )
            deallocate (snowdp_roof  )
            deallocate (snowdp_gimp  )
            deallocate (snowdp_gper  )
            deallocate (snowdp_lake  )

            deallocate (Fhac         )
            deallocate (Fwst         )
            deallocate (Fach         )
            deallocate (Fahe         )
            deallocate (Fhah         )
            deallocate (vehc         )
            deallocate (meta         )

            deallocate (t_room       )
            deallocate (t_roof       )
            deallocate (t_wall       )
            deallocate (tafu         )

            deallocate (urb_green    )
            deallocate (urb_lai      )
            deallocate (urb_sai      )
         ENDIF
      ENDIF

   END SUBROUTINE deallocate_UrbanTimeVariables

END MODULE MOD_Urban_Vars_TimeVariables
#endif
! ---------- EOP ------------
