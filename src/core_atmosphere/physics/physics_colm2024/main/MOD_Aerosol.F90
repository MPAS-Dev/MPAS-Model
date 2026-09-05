#include <define.h>

MODULE MOD_Aerosol

!-----------------------------------------------------------------------
   USE MOD_Precision
   USE MOD_Grid
   USE MOD_DataType
   USE MOD_SpatialMapping
   USE MOD_Vars_Global, only: maxsnl
   IMPLICIT NONE
   SAVE

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: AerosolMasses
   PUBLIC :: AerosolFluxes
   PUBLIC :: AerosolDepInit
   PUBLIC :: AerosolDepReadin

! PUBLIC DATA MEMBERS:
!-----------------------------------------------------------------------

   logical,  parameter :: use_extrasnowlayers = .false.
   real(r8), parameter :: snw_rds_min = 54.526_r8         ! minimum allowed snow effective radius
                                                          ! (also "fresh snow" value) [microns]
   real(r8), parameter :: fresh_snw_rds_max = 204.526_r8  ! maximum warm fresh snow effective radius

   character(len=256)  :: file_aerosol

   type(grid_type) :: grid_aerosol
   type(block_data_real8_2d)  :: f_aerdep
   type(spatial_mapping_type) :: mg2p_aerdep

   integer, parameter :: start_year = 1849
   integer, parameter :: end_year   = 2001

   integer :: month_p

CONTAINS

   SUBROUTINE AerosolMasses( dtime         ,snl            ,do_capsnow    ,&
              h2osno_ice    ,h2osno_liq    ,qflx_snwcp_ice ,snw_rds       ,&

              mss_bcpho     ,mss_bcphi     ,mss_ocpho      ,mss_ocphi     ,&
              mss_dst1      ,mss_dst2      ,mss_dst3       ,mss_dst4      ,&

              mss_cnc_bcphi ,mss_cnc_bcpho ,mss_cnc_ocphi  ,mss_cnc_ocpho ,&
              mss_cnc_dst1  ,mss_cnc_dst2  ,mss_cnc_dst3   ,mss_cnc_dst4  )

!-----------------------------------------------------------------------
! !DESCRIPTION:
!  Calculate column-integrated aerosol masses, and
!  mass concentrations for radiative calculations and output
!  (based on new snow level state, after SnowFilter is rebuilt.
!  NEEDS TO BE AFTER SnowFiler is rebuilt in Hydrology2, otherwise there
!  can be zero snow layers but an active column in filter)
!-----------------------------------------------------------------------

   IMPLICIT NONE

   ! !ARGUMENTS:
   !
   real(r8),intent(in)     ::  dtime            ! seconds in a time step [second]
   integer, intent(in)     ::  snl              ! number of snow layers

   logical,  intent(in)    ::  do_capsnow       ! true => do snow capping
   real(r8), intent(in)    ::  h2osno_ice    ( maxsnl+1:0 ) ! ice lens (kg/m2)
   real(r8), intent(in)    ::  h2osno_liq    ( maxsnl+1:0 ) ! liquid water (kg/m2)
   real(r8), intent(in)    ::  qflx_snwcp_ice   ! excess snowfall due to snow capping (mm H2O /s)

   real(r8), intent(inout) ::  snw_rds       ( maxsnl+1:0 ) ! effective snow grain radius
                                                            ! [microns, m^-6]

   real(r8), intent(inout) ::  mss_bcpho     ( maxsnl+1:0 ) ! mass of hydrophobic BC in snow [kg]
   real(r8), intent(inout) ::  mss_bcphi     ( maxsnl+1:0 ) ! mass of hydrophillic BC in snow [kg]
   real(r8), intent(inout) ::  mss_ocpho     ( maxsnl+1:0 ) ! mass of hydrophobic OC in snow [kg]
   real(r8), intent(inout) ::  mss_ocphi     ( maxsnl+1:0 ) ! mass of hydrophillic OC in snow [kg]
   real(r8), intent(inout) ::  mss_dst1      ( maxsnl+1:0 ) ! mass of dust species 1 in snow [kg]
   real(r8), intent(inout) ::  mss_dst2      ( maxsnl+1:0 ) ! mass of dust species 2 in snow [kg]
   real(r8), intent(inout) ::  mss_dst3      ( maxsnl+1:0 ) ! mass of dust species 3 in snow [kg]
   real(r8), intent(inout) ::  mss_dst4      ( maxsnl+1:0 ) ! mass of dust species 4 in snow [kg]

   real(r8), intent(out)   ::  mss_cnc_bcphi ( maxsnl+1:0 ) ! mass concentration of BC 1 [kg/kg]
   real(r8), intent(out)   ::  mss_cnc_bcpho ( maxsnl+1:0 ) ! mass concentration of BC 2 [kg/kg]
   real(r8), intent(out)   ::  mss_cnc_ocphi ( maxsnl+1:0 ) ! mass concentration of OC 1 [kg/kg]
   real(r8), intent(out)   ::  mss_cnc_ocpho ( maxsnl+1:0 ) ! mass concentration of OC 2 [kg/kg]
   real(r8), intent(out)   ::  mss_cnc_dst1  ( maxsnl+1:0 ) ! mass concentration of dust 1 [kg/kg]
   real(r8), intent(out)   ::  mss_cnc_dst2  ( maxsnl+1:0 ) ! mass concentration of dust 2 [kg/kg]
   real(r8), intent(out)   ::  mss_cnc_dst3  ( maxsnl+1:0 ) ! mass concentration of dust 3 [kg/kg]
   real(r8), intent(out)   ::  mss_cnc_dst4  ( maxsnl+1:0 ) ! mass concentration of dust 4 [kg/kg]

   ! !LOCAL VARIABLES:
   integer  :: c,j             ! indices
   real(r8) :: snowmass        ! liquid+ice snow mass in a layer [kg/m2]
   real(r8) :: snowcap_scl_fct ! temporary factor used to correct for snow capping

      !-----------------------------------------------------------------------

      DO j = maxsnl+1, 0

         ! layer mass of snow:
         snowmass = h2osno_ice(j) + h2osno_liq(j)

         IF (.not. use_extrasnowlayers) THEN
            ! Correct the top layer aerosol mass to account for snow capping.
            ! This approach conserves the aerosol mass concentration
            ! (but not the aerosol mass) when snow-capping is invoked

            IF (j == snl+1) THEN
               IF (do_capsnow) THEN

                  snowcap_scl_fct = snowmass / (snowmass + (qflx_snwcp_ice*dtime))

                  mss_bcpho(j) = mss_bcpho(j)*snowcap_scl_fct
                  mss_bcphi(j) = mss_bcphi(j)*snowcap_scl_fct
                  mss_ocpho(j) = mss_ocpho(j)*snowcap_scl_fct
                  mss_ocphi(j) = mss_ocphi(j)*snowcap_scl_fct

                  mss_dst1(j)  = mss_dst1(j)*snowcap_scl_fct
                  mss_dst2(j)  = mss_dst2(j)*snowcap_scl_fct
                  mss_dst3(j)  = mss_dst3(j)*snowcap_scl_fct
                  mss_dst4(j)  = mss_dst4(j)*snowcap_scl_fct
               ENDIF
            ENDIF
         ENDIF

         IF (j >= snl+1) THEN

            mss_cnc_bcphi(j) = mss_bcphi(j) / snowmass
            mss_cnc_bcpho(j) = mss_bcpho(j) / snowmass

            mss_cnc_ocphi(j) = mss_ocphi(j) / snowmass
            mss_cnc_ocpho(j) = mss_ocpho(j) / snowmass

            mss_cnc_dst1(j)  = mss_dst1(j)  / snowmass
            mss_cnc_dst2(j)  = mss_dst2(j)  / snowmass
            mss_cnc_dst3(j)  = mss_dst3(j)  / snowmass
            mss_cnc_dst4(j)  = mss_dst4(j)  / snowmass

         ELSE
            ! 01/10/2023, yuan: set empty snow layers to snw_rds_min
            snw_rds(j)       = snw_rds_min

            mss_bcpho(j)     = 0._r8
            mss_bcphi(j)     = 0._r8
            mss_cnc_bcphi(j) = 0._r8
            mss_cnc_bcpho(j) = 0._r8

            mss_ocpho(j)     = 0._r8
            mss_ocphi(j)     = 0._r8
            mss_cnc_ocphi(j) = 0._r8
            mss_cnc_ocpho(j) = 0._r8

            mss_dst1(j)      = 0._r8
            mss_dst2(j)      = 0._r8
            mss_dst3(j)      = 0._r8
            mss_dst4(j)      = 0._r8
            mss_cnc_dst1(j)  = 0._r8
            mss_cnc_dst2(j)  = 0._r8
            mss_cnc_dst3(j)  = 0._r8
            mss_cnc_dst4(j)  = 0._r8
         ENDIF
      ENDDO

   END SUBROUTINE AerosolMasses



   SUBROUTINE AerosolFluxes( dtime, snl, forc_aer, &
                             mss_bcphi  ,mss_bcpho  ,mss_ocphi  ,mss_ocpho ,&
                             mss_dst1   ,mss_dst2   ,mss_dst3   ,mss_dst4   )
!-----------------------------------------------------------------------
! !DESCRIPTION:
!  Compute aerosol fluxes through snowpack and aerosol deposition fluxes
!  into top layer
!
!-----------------------------------------------------------------------
   IMPLICIT NONE
   !
   !-----------------------------------------------------------------------
   ! !ARGUMENTS:
   real(r8),intent(in)  :: dtime           ! seconds in a time step [second]
   integer, intent(in)  :: snl             ! number of snow layers

   real(r8), intent(in) :: forc_aer (14 )  ! aerosol deposition from atmosphere [kg m-1 s-1]

   real(r8), intent(inout) :: mss_bcphi  (maxsnl+1:0 )  ! hydrophillic BC mass in snow [kg]
   real(r8), intent(inout) :: mss_bcpho  (maxsnl+1:0 )  ! hydrophobic  BC mass in snow [kg]
   real(r8), intent(inout) :: mss_ocphi  (maxsnl+1:0 )  ! hydrophillic OC mass in snow [kg]
   real(r8), intent(inout) :: mss_ocpho  (maxsnl+1:0 )  ! hydrophobic  OC mass in snow [kg]
   real(r8), intent(inout) :: mss_dst1   (maxsnl+1:0 )  ! mass of dust species 1 in snow [kg]
   real(r8), intent(inout) :: mss_dst2   (maxsnl+1:0 )  ! mass of dust species 2 in snow [kg]
   real(r8), intent(inout) :: mss_dst3   (maxsnl+1:0 )  ! mass of dust species 3 in snow [kg]
   real(r8), intent(inout) :: mss_dst4   (maxsnl+1:0 )  ! mass of dust species 4 in snow [kg]

   ! !LOCAL VARIABLES:
   real(r8) :: flx_bc_dep          ! total BC deposition (col) [kg m-2 s-1]
   real(r8) :: flx_bc_dep_phi      ! hydrophillic BC deposition (col) [kg m-1 s-1]
   real(r8) :: flx_bc_dep_pho      ! hydrophobic BC deposition (col) [kg m-1 s-1]
   real(r8) :: flx_oc_dep          ! total OC deposition (col) [kg m-2 s-1]
   real(r8) :: flx_oc_dep_phi      ! hydrophillic OC deposition (col) [kg m-1 s-1]
   real(r8) :: flx_oc_dep_pho      ! hydrophobic OC deposition (col) [kg m-1 s-1]
   real(r8) :: flx_dst_dep         ! total dust deposition (col) [kg m-2 s-1]

   real(r8) :: flx_dst_dep_wet1    ! wet dust (species 1) deposition (col) [kg m-2 s-1]
   real(r8) :: flx_dst_dep_dry1    ! dry dust (species 1) deposition (col) [kg m-2 s-1]
   real(r8) :: flx_dst_dep_wet2    ! wet dust (species 2) deposition (col) [kg m-2 s-1]
   real(r8) :: flx_dst_dep_dry2    ! dry dust (species 2) deposition (col) [kg m-2 s-1]
   real(r8) :: flx_dst_dep_wet3    ! wet dust (species 3) deposition (col) [kg m-2 s-1]
   real(r8) :: flx_dst_dep_dry3    ! dry dust (species 3) deposition (col) [kg m-2 s-1]
   real(r8) :: flx_dst_dep_wet4    ! wet dust (species 4) deposition (col) [kg m-2 s-1]
   real(r8) :: flx_dst_dep_dry4    ! dry dust (species 4) deposition (col) [kg m-2 s-1]

   integer  :: c

      !-----------------------------------------------------------------------
      ! set aerosol deposition fluxes from forcing array
      ! The forcing array is either set from an external file
      ! or from fluxes received from the atmosphere model
#ifdef MODAL_AER
      ! Mapping for modal aerosol scheme where within-hydrometeor and
      ! interstitial aerosol fluxes are differentiated. Here, "phi"
      ! flavors of BC and OC correspond to within-hydrometeor
      ! (cloud-borne) aerosol, and "pho" flavors are interstitial
      ! aerosol. "wet" and "dry" fluxes of BC and OC specified here are
      ! purely diagnostic
      !
      ! NOTE: right now the macro 'MODAL_AER' is not defined anywhere, i.e.,
      ! the below (modal aerosol scheme) is not available and can not be
      ! active either. It depends on the specific input aerosol deposition
      ! data which is suitable for modal scheme. [06/15/2023, Hua Yuan]


      flx_bc_dep_phi   = forc_aer(3)
      flx_bc_dep_pho   = forc_aer(1) + forc_aer(2)
      flx_bc_dep       = forc_aer(1) + forc_aer(2) + forc_aer(3)

      flx_oc_dep_phi   = forc_aer(6)
      flx_oc_dep_pho   = forc_aer(4) + forc_aer(5)
      flx_oc_dep       = forc_aer(4) + forc_aer(5) + forc_aer(6)

      flx_dst_dep_wet1 = forc_aer(7)
      flx_dst_dep_dry1 = forc_aer(8)
      flx_dst_dep_wet2 = forc_aer(9)
      flx_dst_dep_dry2 = forc_aer(10)
      flx_dst_dep_wet3 = forc_aer(11)
      flx_dst_dep_dry3 = forc_aer(12)
      flx_dst_dep_wet4 = forc_aer(13)
      flx_dst_dep_dry4 = forc_aer(14)
      flx_dst_dep      = forc_aer(7)  + forc_aer(8)  + forc_aer(9)  + &
                         forc_aer(10) + forc_aer(11) + forc_aer(12) + &
                         forc_aer(13) + forc_aer(14)
#else

      ! Original mapping for bulk aerosol deposition. phi and pho BC/OC
      ! species are distinguished in model, other fluxes (e.g., dry and
      ! wet BC/OC) are purely diagnostic.

      flx_bc_dep_phi   = forc_aer(1) + forc_aer(3)
      flx_bc_dep_pho   = forc_aer(2)
      flx_bc_dep       = forc_aer(1) + forc_aer(2) + forc_aer(3)

      flx_oc_dep_phi   = forc_aer(4) + forc_aer(6)
      flx_oc_dep_pho   = forc_aer(5)
      flx_oc_dep       = forc_aer(4) + forc_aer(5) + forc_aer(6)

      flx_dst_dep_wet1 = forc_aer(7)
      flx_dst_dep_dry1 = forc_aer(8)
      flx_dst_dep_wet2 = forc_aer(9)
      flx_dst_dep_dry2 = forc_aer(10)
      flx_dst_dep_wet3 = forc_aer(11)
      flx_dst_dep_dry3 = forc_aer(12)
      flx_dst_dep_wet4 = forc_aer(13)
      flx_dst_dep_dry4 = forc_aer(14)
      flx_dst_dep      = forc_aer(7)  + forc_aer(8)  + forc_aer(9)  + &
                         forc_aer(10) + forc_aer(11) + forc_aer(12) + &
                         forc_aer(13) + forc_aer(14)
#endif

      ! aerosol deposition fluxes into top layer
      ! This is done after the inter-layer fluxes so that some aerosol
      ! is in the top layer after deposition, and is not immediately
      ! washed out before radiative calculations are done

      mss_bcphi(snl+1) = mss_bcphi(snl+1) + (flx_bc_dep_phi*dtime)
      mss_bcpho(snl+1) = mss_bcpho(snl+1) + (flx_bc_dep_pho*dtime)
      mss_ocphi(snl+1) = mss_ocphi(snl+1) + (flx_oc_dep_phi*dtime)
      mss_ocpho(snl+1) = mss_ocpho(snl+1) + (flx_oc_dep_pho*dtime)

      mss_dst1(snl+1) = mss_dst1(snl+1) + (flx_dst_dep_dry1 + flx_dst_dep_wet1)*dtime
      mss_dst2(snl+1) = mss_dst2(snl+1) + (flx_dst_dep_dry2 + flx_dst_dep_wet2)*dtime
      mss_dst3(snl+1) = mss_dst3(snl+1) + (flx_dst_dep_dry3 + flx_dst_dep_wet3)*dtime
      mss_dst4(snl+1) = mss_dst4(snl+1) + (flx_dst_dep_dry4 + flx_dst_dep_wet4)*dtime

   END SUBROUTINE AerosolFluxes


   SUBROUTINE AerosolDepInit ()

   USE MOD_Namelist
   USE MOD_Grid
   USE MOD_NetCDFSerial
   USE MOD_NetCDFBlock
   USE MOD_LandPatch
   IMPLICIT NONE

   real(r8), allocatable :: lat(:), lon(:)

      IF (DEF_Aerosol_Clim) THEN
         ! climatology data
         file_aerosol = trim(DEF_dir_runtime) // &
            '/aerosol/aerosoldep_monthly_2000_mean_0.9x1.25_c090529.nc'
      ELSE
         ! yearly change data
         file_aerosol = trim(DEF_dir_runtime) // &
            '/aerosol/aerosoldep_monthly_1849-2001_0.9x1.25_c090529.nc'
      ENDIF

      CALL ncio_read_bcast_serial (file_aerosol, 'lat', lat)
      CALL ncio_read_bcast_serial (file_aerosol, 'lon', lon)

      CALL grid_aerosol%define_by_center (lat, lon)

      CALL allocate_block_data (grid_aerosol, f_aerdep)

      CALL mg2p_aerdep%build_arealweighted (grid_aerosol, landpatch)

      month_p = -1

   END SUBROUTINE AerosolDepInit


   SUBROUTINE AerosolDepReadin (idate)

   USE MOD_TimeManager
   USE MOD_NetCDFBlock
   USE MOD_Namelist
   USE MOD_Vars_1DForcing
#ifdef RangeCheck
   USE MOD_RangeCheck
#endif
   IMPLICIT NONE

   integer, intent(in) :: idate(3)

   integer :: itime, year, month, mday

      year = idate(1)
      CALL julian2monthday (idate(1), idate(2), month, mday)

      ! data before the start year, will use the start year
      IF (year < start_year) year = start_year
      ! data after the end year, will use the end year
      IF (year > end_year  ) year = end_year

      IF (month.eq.month_p) RETURN

      month_p = month

      ! calculate itime
      ! NOTE: aerosol deposition is monthly data
      IF (DEF_Aerosol_Clim) THEN
         ! for climatology data
         itime = month
      ELSE
         ! for yearly change data
         itime = (year-start_year)*12 + month
      ENDIF

      ! BCPHIDRY , hydrophilic BC dry deposition
      CALL ncio_read_block_time (file_aerosol, 'BCPHIDRY', grid_aerosol, itime, f_aerdep)
      CALL mg2p_aerdep%grid2pset (f_aerdep, forc_aerdep(1,:))

      ! BCPHODRY , hydrophobic BC dry deposition
      CALL ncio_read_block_time (file_aerosol, 'BCPHODRY', grid_aerosol, itime, f_aerdep)
      CALL mg2p_aerdep%grid2pset (f_aerdep, forc_aerdep(2,:))

      ! BCDEPWET , hydrophilic BC wet deposition
      CALL ncio_read_block_time (file_aerosol, 'BCDEPWET', grid_aerosol, itime, f_aerdep)
      CALL mg2p_aerdep%grid2pset (f_aerdep, forc_aerdep(3,:))

      ! OCPHIDRY , hydrophilic OC dry deposition
      CALL ncio_read_block_time (file_aerosol, 'OCPHIDRY', grid_aerosol, itime, f_aerdep)
      CALL mg2p_aerdep%grid2pset (f_aerdep, forc_aerdep(4,:))

      ! OCPHODRY , hydrophobic OC dry deposition
      CALL ncio_read_block_time (file_aerosol, 'OCPHODRY', grid_aerosol, itime, f_aerdep)
      CALL mg2p_aerdep%grid2pset (f_aerdep, forc_aerdep(5,:))

      ! OCDEPWET , hydrophilic OC wet deposition
      CALL ncio_read_block_time (file_aerosol, 'OCDEPWET', grid_aerosol, itime, f_aerdep)
      CALL mg2p_aerdep%grid2pset (f_aerdep, forc_aerdep(6,:))

      ! DSTX01WD , DSTX01 wet deposition flux at bottom
      CALL ncio_read_block_time (file_aerosol, 'DSTX01WD', grid_aerosol, itime, f_aerdep)
      CALL mg2p_aerdep%grid2pset (f_aerdep, forc_aerdep(7,:))

      ! DSTX01DD , DSTX01 dry deposition flux at bottom
      CALL ncio_read_block_time (file_aerosol, 'DSTX01DD', grid_aerosol, itime, f_aerdep)
      CALL mg2p_aerdep%grid2pset (f_aerdep, forc_aerdep(8,:))

      ! DSTX02WD , DSTX02 wet deposition flux at bottom
      CALL ncio_read_block_time (file_aerosol, 'DSTX02WD', grid_aerosol, itime, f_aerdep)
      CALL mg2p_aerdep%grid2pset (f_aerdep, forc_aerdep(9,:))

      ! DSTX02DD , DSTX02 dry deposition flux at bottom
      CALL ncio_read_block_time (file_aerosol, 'DSTX02DD', grid_aerosol, itime, f_aerdep)
      CALL mg2p_aerdep%grid2pset (f_aerdep, forc_aerdep(10,:))

      ! DSTX03WD , DSTX03 wet deposition flux at bottom
      CALL ncio_read_block_time (file_aerosol, 'DSTX03WD', grid_aerosol, itime, f_aerdep)
      CALL mg2p_aerdep%grid2pset (f_aerdep, forc_aerdep(11,:))

      ! DSTX03DD , DSTX03 dry deposition flux at bottom
      CALL ncio_read_block_time (file_aerosol, 'DSTX03DD', grid_aerosol, itime, f_aerdep)
      CALL mg2p_aerdep%grid2pset (f_aerdep, forc_aerdep(12,:))

      ! DSTX04WD , DSTX04 wet deposition flux at bottom
      CALL ncio_read_block_time (file_aerosol, 'DSTX04WD', grid_aerosol, itime, f_aerdep)
      CALL mg2p_aerdep%grid2pset (f_aerdep, forc_aerdep(13,:))

      ! DSTX04DD , DSTX04 dry deposition flux at bottom
      CALL ncio_read_block_time (file_aerosol, 'DSTX04DD', grid_aerosol, itime, f_aerdep)
      CALL mg2p_aerdep%grid2pset (f_aerdep, forc_aerdep(14,:))

#ifdef RangeCheck
      !CALL check_block_data  ('aerosol', f_aerdep)
      CALL check_vector_data ('      aerosol [kg/m/s]', forc_aerdep)
#endif


   END SUBROUTINE AerosolDepReadin

END MODULE MOD_Aerosol
