#include <define.h>

MODULE MOD_HighRes_Parameters

!-----------------------------------------------------------------------
! USES:
USE MOD_Precision
USE MOD_NetCDFSerial
USE MOD_Namelist, only : DEF_USE_SNICAR, DEF_HighResSoil, DEF_HighResVeg, DEF_PROSPECT  !, DEF_Satellite_Params
USE MOD_Namelist, only : DEF_HighResUrban_albedo
! USE MOD_Namelist, only : DEF_file_soiloptics, DEF_file_satellite_params, DEF_sla_varname
IMPLICIT NONE
SAVE

REAL(r8) :: reflectance    ( 0:15, 211, 2 )   ! (species, leaf/stem, wavelength)
REAL(r8) :: transmittance  ( 0:15, 211, 2 )   ! (species, leaf/stem, wavelength)
! REAL(r8) :: soil_alb       ( 211 )          ! (wavelength)
REAL(r8) :: kw             ( 211 )          ! (wavelength)
REAL(r8) :: nw             ( 211 )          ! (wavelength)
REAL(r8) :: clr_frac       ( 211, 89, 5 )   ! (wvl_ctr, SZA, loc )
REAL(r8) :: cld_frac       ( 211, 5 )       ! (wvl_ctr, loc, )
! Urban hyperspectral albedo
REAL(r8), ALLOCATABLE :: urban_albedo( :, :, : )    ! (cluster_id, season wavelength)
REAL(r8), ALLOCATABLE :: mean_albedo ( :, : )       ! (season, wavelength)
REAL(r8), ALLOCATABLE :: lat_north   ( :    )       ! (cluster_id)
REAL(r8), ALLOCATABLE :: lat_south   ( :    )       ! (cluster_id)
REAL(r8), ALLOCATABLE :: lon_east    ( :    )       ! (cluster_id)
REAL(r8), ALLOCATABLE :: lon_west    ( :    )       ! (cluster_id)

! ! ONLY VALID when DEF_Satellite_Params is TRUE
! REAL(r8) :: chl_satellite(366)
! REAL(r8) :: sla_satellite

PUBLIC :: flux_frac_init
PUBLIC :: leaf_property_init
! PUBLIC :: surface_albedo_single_init
PUBLIC :: get_water_optical_properties
PUBLIC :: get_loc_params
! PUBLIC :: satellite_PROSPECT_init

CONTAINS
!-----------------------------------------------------------------------

subroutine flux_frac_init( )

    USE NETCDF
    IMPLICIT NONE
    !-------------------------------------------------------------------
    INTEGER :: i, ncid, varid
    CHARACTER(len=256) :: file_path
    !-------------------------------------------------------------------

    ! open the file base on the SZA and latitude
    file_path = "/ddn_lustre/weiliren/data/CoLM_hires_params/fsds/swnb_480bnd_fsds.nc"

    ! open the file
    CALL nccheck( nf90_open(trim(file_path), NF90_NOWRITE, ncid) , trace=trim(file_path)//' cannot open' )

    ! get the variable id

    CALL nccheck( nf90_open(trim(file_path), NF90_NOWRITE, ncid) , trace=trim(file_path)//' cannot open' )

    ! get the variable id
    CALL nccheck( nf90_inq_varid(ncid, 'flx_frc_cld', varid) , trace=trim(file_path)//' cannot get varid' )
    CALL nccheck( nf90_get_var(ncid, varid, cld_frac) , trace=trim(file_path)//' cannot get data' )

    CALL nccheck( nf90_inq_varid(ncid, 'flx_frc_clr', varid) , trace=trim(file_path)//' cannot get varid' )
    CALL nccheck( nf90_get_var(ncid, varid, clr_frac) , trace=trim(file_path)//' cannot get data' )

    ! close the file
    CALL nccheck( nf90_close(ncid) , trace=trim(file_path)//' cannot close' )
end subroutine flux_frac_init


subroutine leaf_property_init( rho_p, tau_p )
    ! Plant Functional Type classification
    !---------------------------
    ! 0  not vegetated
    ! 1  needleleaf evergreen temperate tree
    ! 2  needleleaf evergreen boreal tree
    ! 3  needleleaf deciduous boreal tree
    ! 4  broadleaf evergreen tropical tree
    ! 5  broadleaf evergreen temperate tree
    ! 6  broadleaf deciduous tropical tree
    ! 7  broadleaf deciduous temperate tree
    ! 8  broadleaf deciduous boreal tree
    ! 9  broadleaf evergreen shrub
    !10  broadleaf deciduous temperate shrub
    !11  broadleaf deciduous boreal shrub
    !12  c3 arctic grass
    !13  c3 non-arctic grass
    !14  c4 grass
    !15  c3 crop
    !16  c3_irrigated

    USE NETCDF
    IMPLICIT NONE
    !-------------------------------------------------------------------
    REAL(r8), INTENT(IN) :: rho_p(2, 2, 0:15), tau_p(2, 2, 0:15)    ! (leaf/stem, vis/nir, species)
    INTEGER :: i, j, ncid, varid, ndims, dimids(3), dimlen(3)
    CHARACTER(len=256) :: file_path
    REAL(r8), allocatable :: reflectance_temp(:,:,:), transmittance_temp(:,:,:)
    !-------------------------------------------------------------------
    ! open the file base on the SZA and latitude

    ! Parameters for hyperspectral leaf properties
    IF ( DEF_HighResVeg .OR. DEF_PROSPECT ) THEN

        file_path = "/ddn_lustre/weiliren/data/CoLM_hires_params/leaf_optical_properties/colm_PFT_params.nc"

        ! open the file
        CALL nccheck( nf90_open(trim(file_path), NF90_NOWRITE, ncid) , trace=trim(file_path)//' cannot open' )

        ! get the variable id
        CALL nccheck( nf90_inq_varid(ncid, 'reflectance', varid) , trace=trim(file_path)//' cannot get varid' )
        CALL nccheck( nf90_inquire_variable(ncid, varid, dimids=dimids) , trace=trim(file_path)//' cannot get var dims' )

        do i = 1, size(dimlen)
            CALL nccheck( nf90_inquire_dimension(ncid, dimids(i), len=dimlen(i)) )
        end do

        ! allocate the memory, and get the reflectance data
        allocate( reflectance_temp( dimlen(1), dimlen(2), dimlen(3) ) )
        CALL nccheck( nf90_get_var(ncid, varid, reflectance_temp) )

        CALL nccheck( nf90_inq_varid(ncid, 'transmittance', varid) , trace=trim(file_path)//' cannot get varid' )
        CALL nccheck( nf90_inquire_variable(ncid, varid, dimids=dimids) , trace=trim(file_path)//' cannot get var dims' )

        ! allocate the memory, and get the transmittance data
        allocate( transmittance_temp( dimlen(1), dimlen(2), dimlen(3) ) )
        CALL nccheck( nf90_get_var(ncid, varid, transmittance_temp) )

        ! close the file
        CALL nccheck( nf90_close(ncid) , trace=trim(file_path)//' cannot close' )

        do i = 0, 15
            do j = 1, 2
                reflectance  (i, :, j) = reflectance_temp  (:, j, i+1)
                transmittance(i, :, j) = transmittance_temp(:, j, i+1)
            end do
        end do

        DEALLOCATE( reflectance_temp )
        DEALLOCATE( transmittance_temp )

    ELSE

        do i = 0, 15
            reflectance(i, 1 :29 , 1) = rho_p(1, 1, i)
            reflectance(i, 1 :29 , 2) = rho_p(1, 2, i)
            reflectance(i, 30:211, 1) = rho_p(2, 1, i)
            reflectance(i, 30:211, 2) = rho_p(2, 2, i)

            transmittance(i, 1 :29 , 1) = tau_p(1, 1, i)
            transmittance(i, 1 :29 , 2) = tau_p(1, 2, i)
            transmittance(i, 30:211, 1) = tau_p(2, 1, i)
            transmittance(i, 30:211, 2) = tau_p(2, 2, i)
        end do

    END IF

    ! 3. Close file

end subroutine leaf_property_init


! No longer used
! subroutine surface_albedo_single_init( )
!     IMPLICIT NONE
!     INTEGER :: i, unit
!     !-------------------------------------------------------------------

!     unit = 10

!     if (DEF_file_soiloptics == 'Null') then
!         write(*,*) "Error: soiloptics file is not defined"
!         stop
!     end if
!     open(unit=unit, file=DEF_file_soiloptics, status='old', action='read')

!     do i = 1, 211
!         read(unit,*) soil_alb(i)
!     end do
!     close(unit)
! end subroutine surface_albedo_single_init


SUBROUTINE get_water_optical_properties( )
    IMPLICIT NONE
    INTEGER :: i, unit

    unit = 10
    open(unit=unit, file='/ddn_lustre/weiliren/data/CoLM_hires_params/water_params.txt', status='old')

    do i = 1, 211
        read(unit,*) kw(i), nw(i)
    end do
    close(unit)
END SUBROUTINE get_water_optical_properties


real function rad2deg(angle_in_rad)
    real, intent(in) :: angle_in_rad
    rad2deg = angle_in_rad * (180.0 / 3.14159265358979323846)
end function rad2deg


SUBROUTINE get_loc_params( fsds, idate, coszen, lat, lon, clr_frac_all, cld_frac_all, dir_frac, dif_frac )
    USE MOD_OrbCoszen
    USE MOD_TimeManager

    implicit none
    real(r8), intent(in) :: fsds
    integer, INTENT(in) :: idate(3)
    real(r8), intent(in) :: coszen
    real(r8), intent(in) :: lat, lon
    real(r8), intent(in) :: clr_frac_all( 211, 89, 5 )
    real(r8), intent(in) :: cld_frac_all( 211, 5 )

    real, intent(out) :: dir_frac(211)
    real, intent(out) :: dif_frac(211)

    INTEGER :: loc_index, sza
    real(r8) :: lat_deg, sunang, cloud, difrat, vnrat, calday, a

    ! index = 1 - 90
    sza = int(rad2deg(acos(min(1._r8,max(-1._r8,coszen))))) + 1
    sza = max(1, min(ubound(clr_frac_all,2), sza))   ! 自动适配

    ! combine cloud and clear sky fraction
    a = max(0., fsds)
    calday = calendarday(idate)
    sunang = orb_coszen(calday, lon, lat)

    ! turn lat from radians to degrees
    lat_deg = abs(rad2deg(lat))

    ! check if the lat in tropical/temperate/polar
    IF (lat_deg >= 0.0 .AND. lat_deg < 23.5) THEN
        loc_index = 5

    ELSE IF (lat_deg >= 23.5 .AND. lat_deg < 66.5) THEN
        ! temperate summer
        IF (calday > 91 .AND. calday < 274) THEN
            loc_index = 4
        ELSE
            loc_index = 3
        END IF

    ELSE IF (lat_deg >= 66.5 .AND. lat_deg <= 90.0) THEN
        IF (calday > 91 .AND. calday < 274) THEN
            loc_index = 2
        ELSE
            loc_index = 1
        END IF

    ENDIF

    dir_frac = clr_frac_all(:, sza, loc_index)
    dif_frac = cld_frac_all(:, loc_index)

END SUBROUTINE get_loc_params

! SUBROUTINE satellite_PROSPECT_init()
!     USE NETCDF
!     IMPLICIT NONE
!     !-------------------------------------------------------------------
!     INTEGER :: i, j, ncid, varid
!     INTEGER, ALLOCATABLE :: dimids(:)
!     !-------------------------------------------------------------------
!     if (DEF_file_satellite_params == 'null') then
!         write(*,*) "ERROR: DEF_file_satellite_params is not set"
!         stop
!     end if

!     ! 1. Open file
!     CALL nccheck( nf90_open(trim(DEF_file_satellite_params), NF90_NOWRITE, ncid), trace=trim(DEF_file_satellite_params)//' cannot open' )

!     ! 2. Read data: chl
!     CALL nccheck( nf90_inq_varid(ncid, 'chl', varid), trace=trim(DEF_file_satellite_params)//' cannot find variable' )
!     CALL nccheck( nf90_inquire_variable(ncid, varid, dimids=dimids), trace=trim(DEF_file_satellite_params)//' cannot inquire variable' )
!     CALL nccheck( nf90_get_var(ncid, varid, chl_satellite), trace='chl cannot get variable' )

!     ! 2. Read data: sla
!     CALL nccheck( nf90_inq_varid(ncid, DEF_sla_varname, varid), trace=trim(DEF_file_satellite_params)//' cannot find variable' )
!     CALL nccheck( nf90_inquire_variable(ncid, varid, dimids=dimids), trace=trim(DEF_file_satellite_params)//' cannot inquire variable' )
!     CALL nccheck( nf90_get_var(ncid, varid, sla_satellite), trace=trim(DEF_sla_varname)//' cannot get variable' )

!     ! 3. Close file
!     call nccheck( nf90_close(ncid) )
! END SUBROUTINE satellite_PROSPECT_init


! ======== Calculate Reflectance & Transmittance using PROSPECT ========
SUBROUTINE update_params_PROSPECT(ipft, reflectance_in, transmittance_in,&
                                  reflectance_p, transmittance_p        ,&
                                  soilmoisture                   )
    USE MOD_prospect_DB
    USE MOD_dataSpec_PDB, only : nw
    IMPLICIT NONE
    !-------------------------------------------------------------------
    REAL(r8), PARAMETER :: SLA(0: 15) &         ! unit g/m^2
        = (/ 0.0   , 0.0100, 0.0100, 0.0202, 0.0190, 0.0190, 0.0308, 0.0308 &
           , 0.0308, 0.0180, 0.0307, 0.0307, 0.0402, 0.0402, 0.0385, 0.0402 /)

    REAL(r8), PARAMETER :: vmax25_p(0: 15) &         !
        = (/ 52.0, 55.0, 42.0, 29.0, 41.0, 51.0, 36.0, 30.0 &
           , 40.0, 36.0, 30.0, 19.0, 21.0, 26.0, 25.0, 57.0 /) * 1.e-6

    INTEGER , INTENT(IN) :: ipft
    REAL(r8), INTENT(IN) :: reflectance_in(16, 211, 2), transmittance_in(16, 211, 2)
    REAL(r8), INTENT(IN) :: soilmoisture

    REAL(r8), INTENT(OUT) :: reflectance_p(211, 2), transmittance_p(211, 2)

    ! Params for PROSPECT
    REAL(r8) :: N       ! leaf structure coefficient (n_layer)
    REAL(r8) :: Cab     ! Chlorophyll Content
    REAL(r8) :: Car     ! Carotenoid, unit [μg cm^−2]
    REAL(r8) :: Anth    ! Anthocyanin
    REAL(r8) :: Cbrown  ! Brown Pigment
    REAL(r8) :: Cw      ! Equivalent Water Thickness
    REAL(r8) :: Cm      ! Dry Matter Content (g cm^-2)
    REAL(r8) :: RT(nw, 2)   ! nw = 2101, defined in dataSpec_PDB

    ! temporary variables
    REAL(r8) :: vmax25 ! maximum carboxylation rate at 25 C at canopy top, unit [mol m-2 s-1]

    INTEGER :: i, j
    !-------------------------------------------------------------------
    ! 1. Set Car, Cbrown, Anth
    Car = 8.0
    Cbrown = 0.01
    Anth = 0.0

    ! 2. Calculate N & Cm
    !    SLA: Specific Leaf Area, unit [m^2 g-1] -> [cm^2 mg-1]
    N = (0.9 * (SLA(ipft) * 10.) + 0.025) / ((SLA(ipft) * 10.) - 0.01)
    !    Cm: Dry Matter Content, unit [g cm^-2] <- SLA [m^2 g-1]
    Cm = 1.0 / (SLA(ipft) * 1.e4)

    ! 3. Calculate Cab
    vmax25 = vmax25_p(ipft)
    Cab = ((vmax25 * 1.e6) - 3.72) / 1.3

    ! 4. Calculate Cw
    Cw = 0.01 - (( 0.01 - 0. ) * exp( -5.5 * soilmoisture ))

    ! 5. Calculate reflectance & transmittance
    CALL prospect_DB(N,Cab,Car,Anth,Cbrown,Cw,Cm,RT)

    j = 1
    do i = 1, nw, 10
        reflectance_p(j, 1) = RT(i, 1)
        transmittance_p(j, 1) = RT(i, 2)
        j = j + 1
    end do

    reflectance_p(:,2) = reflectance_in(ipft, :, 2)
    transmittance_p(:,2) = transmittance_in(ipft, :, 2)
END SUBROUTINE update_params_PROSPECT
! ======================================================================

! SUBROUTINE satellite_PROSPECT(ipft, reflectance_in, transmittance_in,&
!                               reflectance_p, transmittance_p        ,&
!                               soilmoisture, doy )
!     USE MOD_prospect_DB
!     USE MOD_dataSpec_PDB, only : nw
!     IMPLICIT NONE
!     !-------------------------------------------------------------------

!     INTEGER , INTENT(IN) :: ipft
!     REAL(r8), INTENT(IN) :: reflectance_in(16, 211, 2), transmittance_in(16, 211, 2)
!     REAL(r8), INTENT(IN) :: soilmoisture
!     INTEGER, INTENT(IN)  :: doy

!     REAL(r8), INTENT(OUT) :: reflectance_p(211, 2), transmittance_p(211, 2)

!     ! Params for PROSPECT
!     REAL(r8) :: N       ! leaf structure coefficient (n_layer)
!     REAL(r8) :: Cab     ! Chlorophyll Content
!     REAL(r8) :: Car     ! Carotenoid, unit [μg cm^−2]
!     REAL(r8) :: Anth    ! Anthocyanin
!     REAL(r8) :: Cbrown  ! Brown Pigment
!     REAL(r8) :: Cw      ! Equivalent Water Thickness
!     REAL(r8) :: Cm      ! Dry Matter Content (g cm^-2)
!     REAL(r8) :: RT(nw, 2)   ! nw = 2101, defined in dataSpec_PDB
!     INTEGER  :: i, j

!     !NOTE: All from Wang 2025 NC
!     ! 1. Set Car, Cbrown, Anth
!     Cbrown = 0.0
!     Anth = 0.0

!     ! 2. Calculate N & Cm
!     !    SLA: Specific Leaf Area, unit [cm^2 mg-1]
!     N = 1.4
!     ! N = (0.9 * sla_satellite + 0.025) / (sla_satellite - 0.01)
!     !    Cm: Dry Matter Content, unit [g cm^-2] <- SLA [cm^2 mg-1]
!     Cm = 1.0 / (sla_satellite * 1.e3)

!     ! 3. Calculate Cab
!     Cab = chl_satellite(doy)
!     Car = Cab / 7.

!     ! 4. Calculate Cw
!     Cw = 0.009   ! cm
!     ! Cw = 0.01 - (( 0.01 - 0. ) * exp( -5.5 * soilmoisture ))

!     ! 5. Calculate reflectance & transmittance
!     CALL prospect_DB(N,Cab,Car,Anth,Cbrown,Cw,Cm,RT)

!     j = 1
!     do i = 1, nw, 10
!         reflectance_p(j, 1) = RT(i, 1)
!         transmittance_p(j, 1) = RT(i, 2)
!         j = j + 1
!     end do

!     reflectance_p(:,2) = reflectance_in(ipft, :, 2)
!     transmittance_p(:,2) = transmittance_in(ipft, :, 2)

! END SUBROUTINE satellite_PROSPECT

SUBROUTINE readin_urban_albedo()

    USE NETCDF
    IMPLICIT NONE
    INTEGER :: ncid, ndims, dimids(3), dimlen(3)
    INTEGER :: albedo_varid, mean_albedo_varid, &
            lat_north_varid, lat_south_varid, &
            lon_east_varid , lon_west_varid
    CHARACTER(len=256) :: file_path
    INTEGER :: i

    ! 设置文件路径
    file_path = DEF_HighResUrban_albedo

    ! 打开 NetCDF 文件
    CALL nccheck( nf90_open(trim(file_path), NF90_NOWRITE, ncid) , &
                  trace=trim(file_path)//' cannot open' )

    ! 获取变量 ID
    CALL nccheck( nf90_inq_varid(ncid, 'urban_albedo', albedo_varid) , &
                  trace=trim(file_path)//' cannot get varid' )

    CALL nccheck( nf90_inq_varid(ncid, 'mean_albedo', mean_albedo_varid) , &
                  trace=trim(file_path)//' cannot get varid' )

    CALL nccheck( nf90_inq_varid(ncid, 'lat_north', lat_north_varid) , &
                  trace=trim(file_path)//' cannot get varid' )

    CALL nccheck( nf90_inq_varid(ncid, 'lat_south', lat_south_varid) , &
                  trace=trim(file_path)//' cannot get varid' )

    CALL nccheck( nf90_inq_varid(ncid, 'lon_east', lon_east_varid) , &
                  trace=trim(file_path)//' cannot get varid' )

    CALL nccheck( nf90_inq_varid(ncid, 'lon_west', lon_west_varid) , &
                  trace=trim(file_path)//' cannot get varid' )

    ! 获取变量维度信息
    CALL nccheck( nf90_inquire_variable(ncid, albedo_varid, ndims=ndims, dimids=dimids) , &
                  trace=trim(file_path)//' cannot get var dims' )

    ! 获取各维度长度
    do i = 1, ndims
        CALL nccheck( nf90_inquire_dimension(ncid, dimids(i), len=dimlen(i)) )
    end do

    ! 分配内存
    if (.not. allocated(urban_albedo)) then
        allocate( urban_albedo(dimlen(1), dimlen(2), dimlen(3)) )   ! (cluster_id, season, wavelength)
    end if

    if (.not. allocated(mean_albedo)) then
        allocate( mean_albedo(dimlen(2), dimlen(3)) )   ! (season, wavelength)
    end if

    if (.not. allocated(lat_north)) then
        allocate( lat_north(dimlen(1)) )   ! (cluster_id)
    end if

    if (.not. allocated(lat_south)) then
        allocate( lat_south(dimlen(1)) )   ! (cluster_id)
    end if

    if (.not. allocated(lon_east)) then
        allocate( lon_east(dimlen(1)) )   ! (cluster_id)
    end if

    if (.not. allocated(lon_west)) then
        allocate( lon_west(dimlen(1)) )   ! (cluster_id)
    end if

    ! 读取数据
    CALL nccheck( nf90_get_var(ncid, albedo_varid, urban_albedo) , &
                  trace=trim(file_path)//' cannot get data' )

    CALL nccheck( nf90_get_var(ncid, mean_albedo_varid, mean_albedo) , &
                  trace=trim(file_path)//' cannot get data' )

    CALL nccheck( nf90_get_var(ncid, lat_north_varid, lat_north) , &
                  trace=trim(file_path)//' cannot get data' )

    CALL nccheck( nf90_get_var(ncid, lat_south_varid, lat_south) , &
                  trace=trim(file_path)//' cannot get data' )

    CALL nccheck( nf90_get_var(ncid, lon_east_varid, lon_east) , &
                  trace=trim(file_path)//' cannot get data' )

    CALL nccheck( nf90_get_var(ncid, lon_west_varid, lon_west) , &
                  trace=trim(file_path)//' cannot get data' )

    ! 关闭文件
    CALL nccheck( nf90_close(ncid) , &
                  trace=trim(file_path)//' cannot close' )

END SUBROUTINE readin_urban_albedo

SUBROUTINE deallocate_urban_albedo()
    IMPLICIT NONE

    IF (ALLOCATED(urban_albedo)) THEN
        DEALLOCATE(urban_albedo)
    END IF

    IF (ALLOCATED(mean_albedo)) THEN
        DEALLOCATE(mean_albedo)
    END IF

    IF (ALLOCATED(lat_north)) THEN
        DEALLOCATE(lat_north)
    END IF

    IF (ALLOCATED(lat_south)) THEN
        DEALLOCATE(lat_south)
    END IF

    IF (ALLOCATED(lon_east)) THEN
        DEALLOCATE(lon_east)
    END IF

    IF (ALLOCATED(lon_west)) THEN
        DEALLOCATE(lon_west)
    END IF

END SUBROUTINE deallocate_urban_albedo

END MODULE MOD_HighRes_Parameters
