 module NoahmpDriverMainMod

 use Machine
 use NoahmpVarType
 use NoahmpIOVarType
 use ConfigVarInitMod
 use EnergyVarInitMod
 use ForcingVarInitMod
 use WaterVarInitMod
 use BiochemVarInitMod
 use ConfigVarInTransferMod
 use EnergyVarInTransferMod
 use ForcingVarInTransferMod
 use WaterVarInTransferMod
 use BiochemVarInTransferMod
 use ConfigVarOutTransferMod
 use ForcingVarOutTransferMod
 use EnergyVarOutTransferMod
 use WaterVarOutTransferMod
 use BiochemVarOutTransferMod
 use NoahmpMainMod
 use NoahmpMainGlacierMod

 use mpas_log

 implicit none

 contains

 subroutine NoahmpDriverMain(NoahmpIO)

! ------------------------ Code history -------------------------------------
! Original Noah-MP subroutine: noahmplsm
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ---------------------------------------------------------------------------

 implicit none

 type(NoahmpIO_type), intent(inout)  :: NoahmpIO

! local variables
 type(noahmp_type)                   :: noahmp
 integer                             :: i,k
 integer                             :: jmonth,jday
 real(kind=kind_noahmp)              :: solar_time
 real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil ) :: sand
 real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil ) :: clay
 real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil ) :: orgm
! ---------------------------------------------------------------------------
!call mpas_log_write(' ')
!call mpas_log_write('--- enter subroutine NoahmpDriverMain:')

!---------------------------------------------------------------------
!  Treatment of Noah-MP soil timestep
!---------------------------------------------------------------------
 NoahmpIO%calculate_soil    = .false.
 NoahmpIO%soil_update_steps = nint(NoahmpIO%soiltstep / NoahmpIO%dtbl)
 NoahmpIO%soil_update_steps = max(NoahmpIO%soil_update_steps,1)

 if ( NoahmpIO%soil_update_steps == 1 ) then
    NoahmpIO%acc_ssoilxy  = 0.0
    NoahmpIO%acc_qinsurxy = 0.0
    NoahmpIO%acc_qsevaxy  = 0.0
    NoahmpIO%acc_etranixy = 0.0
    NoahmpIO%acc_dwaterxy = 0.0
    NoahmpIO%acc_prcpxy   = 0.0
    NoahmpIO%acc_ecanxy   = 0.0
    NoahmpIO%acc_etranxy  = 0.0
    NoahmpIO%acc_edirxy   = 0.0
 endif

 if ( NoahmpIO%soil_update_steps > 1 ) then
    if ( mod(NoahmpIO%itimestep, NoahmpIO%soil_update_steps) == 1 ) then
       NoahmpIO%acc_ssoilxy  = 0.0
       NoahmpIO%acc_qinsurxy = 0.0
       NoahmpIO%acc_qsevaxy  = 0.0
       NoahmpIO%acc_etranixy = 0.0
       NoahmpIO%acc_dwaterxy = 0.0
       NoahmpIO%acc_prcpxy   = 0.0
       NoahmpIO%acc_ecanxy   = 0.0
       NoahmpIO%acc_etranxy  = 0.0
       NoahmpIO%acc_edirxy   = 0.0
    end if
 endif

 if ( mod(NoahmpIO%itimestep, NoahmpIO%soil_update_steps) == 0 ) NoahmpIO%calculate_soil = .true.
!call mpas_log_write(' ')
!call mpas_log_write('--- enter subroutine noahmpdrivermain:')
!call mpas_log_write('--- NoahmpIO%itimestep         = $i',intArgs=(/NoahmpIO%itimestep/))
!call mpas_log_write('--- NoahmpIO%soiltstep         = $r',realArgs=(/NoahmpIO%soiltstep/))
!call mpas_log_write('--- NoahmpIO%dtbl              = $r',realArgs=(/NoahmpIO%dtbl/))
!call mpas_log_write('--- NoahmpIO%soil_update_steps = $i',intArgs=(/NoahmpIO%soil_update_steps/))
!call mpas_log_write('--- NoahmpIO%calculate_soil    = $l',logicArgs=(/NoahmpIO%calculate_soil/))
!call mpas_log_write(' ')
!call mpas_log_write('--- NoahmpIO%isurban_table     = $i',intArgs=(/NoahmpIO%isurban_table/))
!call mpas_log_write('--- NoahmpIO%urbtype_beg       = $i',intArgs=(/NoahmpIO%urbtype_beg/))
!call mpas_log_write('--- NoahmpIO%sf_urban_physics  = $i',intArgs=(/NoahmpIO%sf_urban_physics/))
!call mpas_log_write('--- NoahmpIO%iri_urban         = $i',intArgs=(/NoahmpIO%iri_urban/))
!call mpas_log_write(' ')

!---------------------------------------------------------------------
!  Prepare Noah-MP driver
!---------------------------------------------------------------------

! find length of year for phenology (also S Hemisphere):
 NoahmpIO%yearlen = 365
 if (mod(NoahmpIO%yr,4) == 0)then
    NoahmpIO%yearlen = 366
    if (mod(NoahmpIO%yr,100) == 0)then
       NoahmpIO%yearlen = 365
       if (mod(NoahmpIO%yr,400) == 0)then
          NoahmpIO%yearlen = 366
       endif
    endif
 endif

! initialize jmonth and jday:
 jmonth = NoahmpIO%month
 jday   = NoahmpIO%day
!call mpas_log_write('--- NoahmpIO%yearlen           = $i',intargs=(/NoahmpIO%yearlen/))
!call mpas_log_write('--- NoahmpIO%yr                = $i',intargs=(/NoahmpIO%yr/))
!call mpas_log_write('--- NoahmpIO%month             = $i',intargs=(/jmonth/))
!call mpas_log_write('--- NoahmpIO%day               = $i',intargs=(/jday/))
!call mpas_log_write('--- NoahmpIO%julian            = $r',realargs=(/NoahmpIO%julian/))
!call mpas_log_write('--- NoahmpIO%xice_threshold    = $r',realargs=(/NoahmpIO%xice_threshold/))
!call mpas_log_write(' ')

! depth to soil interfaces (<0) [m]
 NoahmpIO%zsoil(1) = -NoahmpIO%dzs(1)
 do k = 2, NoahmpIO%nsoil
    NoahmpIO%zsoil(k) = -NoahmpIO%dzs(k) + NoahmpIO%zsoil(k-1)
 enddo

 if ( NoahmpIO%itimestep == 1 ) then
    do i = NoahmpIO%its, NoahmpIO%ite
       if ( (NoahmpIO%xland(i)-1.5) >= 0.0 ) then  ! open water point
          if ( NoahmpIO%xice(i) == 1.0 ) print*,' sea-ice at water point, i=',i
          NoahmpIO%smstav(i) = 1.0
          NoahmpIO%smstot(i) = 1.0
          do k = 1, NoahmpIO%nsoil
             NoahmpIO%smois(i,k) = 1.0
             NoahmpIO%tslb(i,k)  = 273.16
          enddo
       else
          if ( NoahmpIO%xice(i) == 1.0 ) then      ! sea-ice case
             NoahmpIO%smstav(i) = 1.0
             NoahmpIO%smstot(i) = 1.0
             do k = 1, NoahmpIO%nsoil
                NoahmpIO%smois(i,k) = 1.0
             enddo
          endif
       endif
    enddo
 endif  ! end of initialization over ocean

 iloop : do i = NoahmpIO%its, NoahmpIO%ite

    NoahmpIO%j = 1
    NoahmpIO%i = i
    if ( NoahmpIO%xice(i) >= NoahmpIO%xice_threshold ) then  ! sea-ice point
       NoahmpIO%ice                      = 1
       NoahmpIO%sh2o(i,1:NoahmpIO%nsoil) = 1.0
       NoahmpIO%lai (i)                  = 0.01
       cycle iloop                                           ! skip any sea-ice points
    else
       if ( (NoahmpIO%xland(i)-1.5) >= 0.0 ) cycle ILOOP     ! skip any open water points
       !------------------------------------------------------------------------------------
       !  initialize Data Types and transfer all the inputs from 2-D to 1-D column variables
       !------------------------------------------------------------------------------------
       call ConfigVarInitDefault  (noahmp)
       call ConfigVarInTransfer   (noahmp, NoahmpIO)
       call ForcingVarInitDefault (noahmp)
       call ForcingVarInTransfer  (noahmp, NoahmpIO)
       call EnergyVarInitDefault  (noahmp)
       call EnergyVarInTransfer   (noahmp, NoahmpIO)
       call WaterVarInitDefault   (noahmp)
       call WaterVarInTransfer    (noahmp, NoahmpIO)
       call BiochemVarInitDefault (noahmp)
       call BiochemVarInTransfer  (noahmp, NoahmpIO)

       !----------------------------------------------------------------------
       !  hydrological processes for vegetation in urban model
       !  irrigate vegetation only in urban area, MAY-SEP, 9-11pm
       !  need to be separated from Noah-MP into outside urban specific module
       !----------------------------------------------------------------------
       if ( (NoahmpIO%ivgtyp(i) == NoahmpIO%isurban_table) .or. &
            (NoahmpIO%ivgtyp(i) > NoahmpIO%urbtype_beg) ) then
          if ( (NoahmpIO%sf_urban_physics > 0) .and. (NoahmpIO%iri_urban == 1) ) then
             solar_time = (NoahmpIO%julian - int(NoahmpIO%julian))*24 + NoahmpIO%xlong(i)/15.0
             if ( solar_time < 0.0 ) solar_time = solar_time + 24.0
             if ( (solar_time >= 21.0) .and. (solar_time <= 23.0) .and. &
                  (jmonth >= 5) .and. (jmonth <= 9) ) then
                noahmp%water%state%SoilMoisture(1) = &
                      max(noahmp%water%state%SoilMoisture(1),noahmp%water%param%SoilMoistureFieldCap(1))
                 noahmp%water%state%SoilMoisture(2) = &
                       max(noahmp%water%state%SoilMoisture(2),noahmp%water%param%SoilMoistureFieldCap(2))
             endif
          endif
       endif

       !------------------------------------------------------------------------
       !  Call 1D Noah-MP LSM
       !------------------------------------------------------------------------

       if (noahmp%config%domain%VegType == noahmp%config%domain%IndexIcePoint ) then
          noahmp%config%domain%IndicatorIceSfc = -1  ! Land-ice point
          noahmp%forcing%TemperatureSoilBottom = min(noahmp%forcing%TemperatureSoilBottom,263.15) ! set deep glacier temp to >= -10C
          call NoahmpMainGlacier(noahmp)
          ! non-glacier land
       else
          noahmp%config%domain%IndicatorIceSfc = 0   ! land soil point.
          call NoahmpMain(noahmp)
       endif ! glacial split ends

       !---------------------------------------------------------------------
       !  Transfer 1-D Noah-MP column variables to 2-D output variables
       !---------------------------------------------------------------------
       call ConfigVarOutTransfer (noahmp, NoahmpIO)
       call ForcingVarOutTransfer(noahmp, NoahmpIO)
       call EnergyVarOutTransfer (noahmp, NoahmpIO)
       call WaterVarOutTransfer  (noahmp, NoahmpIO)
       call BiochemVarOutTransfer(noahmp, NoahmpIO)

    endif     ! land-sea split ends

 enddo iloop  ! i loop

 end subroutine NoahmpDriverMain

 end module NoahmpDriverMainMod
