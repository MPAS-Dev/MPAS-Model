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

  implicit none
  
contains  

  subroutine NoahmpDriverMain(NoahmpIO)
  
! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: noahmplsm
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ------------------------------------------------------------------------- 
 
    implicit none 
    
    type(NoahmpIO_type), intent(inout)  :: NoahmpIO
    
    ! local variables
    type(noahmp_type)                   :: noahmp
    integer                             :: I
    integer                             :: J
    integer                             :: K
    integer                             :: JMONTH, JDAY
    real(kind=kind_noahmp)              :: SOLAR_TIME 
    real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil ) :: SAND
    real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil ) :: CLAY
    real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil ) :: ORGM
! ------------------------------------------------------------------------- 

    !---------------------------------------------------------------------
    !  Treatment of Noah-MP soil timestep
    !---------------------------------------------------------------------
    NoahmpIO%CALCULATE_SOIL    = .false.
    NoahmpIO%SOIL_UPDATE_STEPS = nint(NoahmpIO%SOILTSTEP / NoahmpIO%DTBL)
    NoahmpIO%SOIL_UPDATE_STEPS = max(NoahmpIO%SOIL_UPDATE_STEPS,1)

    if ( NoahmpIO%SOIL_UPDATE_STEPS == 1 ) then
       NoahmpIO%ACC_SSOILXY  = 0.0
       NoahmpIO%ACC_QINSURXY = 0.0
       NoahmpIO%ACC_QSEVAXY  = 0.0
       NoahmpIO%ACC_ETRANIXY = 0.0
       NoahmpIO%ACC_DWATERXY = 0.0
       NoahmpIO%ACC_PRCPXY   = 0.0
       NoahmpIO%ACC_ECANXY   = 0.0
       NoahmpIO%ACC_ETRANXY  = 0.0
       NoahmpIO%ACC_EDIRXY   = 0.0
    endif

    if ( NoahmpIO%SOIL_UPDATE_STEPS > 1 ) then
       if ( mod(NoahmpIO%ITIMESTEP, NoahmpIO%SOIL_UPDATE_STEPS) == 1 ) then
          NoahmpIO%ACC_SSOILXY  = 0.0
          NoahmpIO%ACC_QINSURXY = 0.0
          NoahmpIO%ACC_QSEVAXY  = 0.0
          NoahmpIO%ACC_ETRANIXY = 0.0
          NoahmpIO%ACC_DWATERXY = 0.0
          NoahmpIO%ACC_PRCPXY   = 0.0
          NoahmpIO%ACC_ECANXY   = 0.0
          NoahmpIO%ACC_ETRANXY  = 0.0
          NoahmpIO%ACC_EDIRXY   = 0.0
       end if
    endif

    if ( mod(NoahmpIO%ITIMESTEP, NoahmpIO%SOIL_UPDATE_STEPS) == 0 ) NoahmpIO%CALCULATE_SOIL = .true.
 
    !---------------------------------------------------------------------
    !  Prepare Noah-MP driver
    !---------------------------------------------------------------------
    
    ! find length of year for phenology (also S Hemisphere)
    NoahmpIO%YEARLEN = 365
    if (mod(NoahmpIO%YR,4) == 0)then
       NoahmpIO%YEARLEN = 366
       if (mod(NoahmpIO%YR,100) == 0)then
          NoahmpIO%YEARLEN = 365
          if (mod(NoahmpIO%YR,400) == 0)then
             NoahmpIO%YEARLEN = 366
          endif
       endif
    endif

    ! depth to soil interfaces (<0) [m]
    NoahmpIO%ZSOIL(1) = -NoahmpIO%DZS(1)
    do K = 2, NoahmpIO%NSOIL
       NoahmpIO%ZSOIL(K) = -NoahmpIO%DZS(K) + NoahmpIO%ZSOIL(K-1)
    enddo
    
    JLOOP : do J = NoahmpIO%JTS, NoahmpIO%JTE

       NoahmpIO%J = J
       if ( NoahmpIO%ITIMESTEP == 1 ) then
          do I = NoahmpIO%ITS, NoahmpIO%ITE
             if ( (NoahmpIO%XLAND(I,J)-1.5) >= 0.0 ) then  ! Open water point
                if ( NoahmpIO%XICE(I,J) == 1.0 ) print*,' sea-ice at water point, I=',I,'J=',J
                NoahmpIO%SMSTAV(I,J) = 1.0
                NoahmpIO%SMSTOT(I,J) = 1.0
                do K = 1, NoahmpIO%NSOIL
                   NoahmpIO%SMOIS(I,K,J) = 1.0
                   NoahmpIO%TSLB(I,K,J)  = 273.16
                enddo
             else
                if ( NoahmpIO%XICE(I,J) == 1.0 ) then      ! Sea-ice case
                   NoahmpIO%SMSTAV(I,J) = 1.0
                   NoahmpIO%SMSTOT(I,J) = 1.0
                   do K = 1, NoahmpIO%NSOIL
                      NoahmpIO%SMOIS(I,K,J) = 1.0
                   enddo
                endif
             endif
          enddo
       endif  ! end of initialization over ocean

       ILOOP : do I = NoahmpIO%ITS, NoahmpIO%ITE

          NoahmpIO%I = I
          if ( NoahmpIO%XICE(I,J) >= NoahmpIO%XICE_THRESHOLD ) then  ! Sea-ice point
             NoahmpIO%ICE                         = 1
             NoahmpIO%SH2O(I,1:NoahmpIO%NSOIL,J) = 1.0
             NoahmpIO%LAI (I,J)                  = 0.01
             cycle ILOOP                                             ! Skip any sea-ice points
          else
             if ( (NoahmpIO%XLAND(I,J)-1.5) >= 0.0 ) cycle ILOOP     ! Skip any open water points

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

             !---------------------------------------------------------------------
             !  hydrological processes for vegetation in urban model
             !  irrigate vegetaion only in urban area, MAY-SEP, 9-11pm
             ! need to be separated from Noah-MP into outside urban specific module 
             !---------------------------------------------------------------------
             if ( (NoahmpIO%IVGTYP(I,J) == NoahmpIO%ISURBAN_TABLE) .or. &
                  (NoahmpIO%IVGTYP(I,J) > NoahmpIO%URBTYPE_beg) ) then
                if ( (NoahmpIO%SF_URBAN_PHYSICS > 0) .and. (NoahmpIO%IRI_URBAN == 1) ) then
                   SOLAR_TIME = (NoahmpIO%JULIAN - int(NoahmpIO%JULIAN))*24 + NoahmpIO%XLONG(I,J)/15.0
                   if ( SOLAR_TIME < 0.0 ) SOLAR_TIME = SOLAR_TIME + 24.0
                   call CAL_MON_DAY(int(NoahmpIO%JULIAN), NoahmpIO%YR, JMONTH, JDAY)
                   if ( (SOLAR_TIME >= 21.0) .and. (SOLAR_TIME <= 23.0) .and. &
                        (JMONTH >= 5) .and. (JMONTH <= 9) ) then
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
         
             ! glacier ice
             if (noahmp%config%domain%VegType == noahmp%config%domain%IndexIcePoint ) then
                 noahmp%config%domain%IndicatorIceSfc = -1  ! Land-ice point      
                 noahmp%forcing%TemperatureSoilBottom = min(noahmp%forcing%TemperatureSoilBottom,263.15) ! set deep glaicer temp to >= -10C
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

       enddo ILOOP  ! I loop
    enddo  JLOOP    ! J loop
              
  end subroutine NoahmpDriverMain
  
end module NoahmpDriverMainMod  
