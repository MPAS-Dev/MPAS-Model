module BalanceErrorCheckGlacierMod

!!! Check glacier water and energy balance and report error

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

!!!! Water balance check initialization
  subroutine BalanceWaterInitGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: None (embedded in NOAHMP_GLACIER)
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! --------------------------------------------------------------------
    associate(                                                             &
              SnowWaterEquiv     => noahmp%water%state%SnowWaterEquiv     ,& ! in,  snow water equivalent [mm]
              WaterStorageTotBeg => noahmp%water%state%WaterStorageTotBeg  & ! out, total water storage [mm] at the beginning
             )
! ----------------------------------------------------------------------

    ! compute total glacier water storage before NoahMP processes
    ! need more work on including glacier ice mass underneath snow
    WaterStorageTotBeg = SnowWaterEquiv

    end associate

  end subroutine BalanceWaterInitGlacier


!!!! Water balance check and report error
  subroutine BalanceWaterCheckGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: ERROR_GLACIER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! --------------------------------------------------------------------
    associate(                                                             &
              GridIndexI         => noahmp%config%domain%GridIndexI       ,& ! in,  grid index in x-direction
              GridIndexJ         => noahmp%config%domain%GridIndexJ       ,& ! in,  grid index in y-direction
              MainTimeStep       => noahmp%config%domain%MainTimeStep     ,& ! in,  main noahmp timestep [s]
              SnowWaterEquiv     => noahmp%water%state%SnowWaterEquiv     ,& ! in,  snow water equivalent [mm]
              WaterStorageTotBeg => noahmp%water%state%WaterStorageTotBeg ,& ! in,  total water storage [mm] at the beginning
              PrecipTotRefHeight => noahmp%water%flux%PrecipTotRefHeight  ,& ! in,  total precipitation [mm/s] at reference height
              EvapGroundNet      => noahmp%water%flux%EvapGroundNet       ,& ! in,  net ground evaporation [mm/s]
              RunoffSurface      => noahmp%water%flux%RunoffSurface       ,& ! in,  surface runoff [mm/s]
              RunoffSubsurface   => noahmp%water%flux%RunoffSubsurface    ,& ! in,  subsurface runoff [mm/s]
              WaterStorageTotEnd => noahmp%water%state%WaterStorageTotEnd ,& ! out, total water storage [mm] at the end
              WaterBalanceError  => noahmp%water%state%WaterBalanceError   & ! out, water balance error [mm] per time step
             )
! ----------------------------------------------------------------------

    ! Error in water balance should be < 0.1 mm
    ! compute total glacier water storage before NoahMP processes
    ! need more work on including glacier ice mass underneath snow
    WaterStorageTotEnd = SnowWaterEquiv
    WaterBalanceError  = WaterStorageTotEnd - WaterStorageTotBeg - &
                         (PrecipTotRefHeight - EvapGroundNet - RunoffSurface - RunoffSubsurface) * MainTimeStep

#ifndef WRF_HYDRO
    if ( abs(WaterBalanceError) > 0.1 ) then
       if ( WaterBalanceError > 0) then
          write(*,*) "The model is gaining water (WaterBalanceError is positive)"
       else
          write(*,*) "The model is losing water (WaterBalanceError is negative)"
       endif
       write(*,*) "WaterBalanceError = ",WaterBalanceError, "kg m{-2} timestep{-1}"
       write(*, &
           '("  GridIndexI   GridIndexJ     WaterStorageTotEnd  WaterStorageTotBeg  PrecipTotRefHeight  &
                EvapGroundNet  RunoffSurface  RunoffSubsurface")')
       write(*,'(i6,1x,i6,1x,2f15.3,9f11.5)') GridIndexI, GridIndexJ, WaterStorageTotEnd, WaterStorageTotBeg, &
                                              PrecipTotRefHeight*MainTimeStep, EvapGroundNet*MainTimeStep,    &
                                              RunoffSurface*MainTimeStep, RunoffSubsurface*MainTimeStep
       stop "Error: Water budget problem in NoahMP LSM"
    endif
#endif

    end associate

  end subroutine BalanceWaterCheckGlacier


!!!! Energy balance check and error report
  subroutine BalanceEnergyCheckGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: ERROR_GLACIER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! --------------------------------------------------------------------
    associate(                                                              &
              GridIndexI         => noahmp%config%domain%GridIndexI        ,& ! in,  grid index in x-direction
              GridIndexJ         => noahmp%config%domain%GridIndexJ        ,& ! in,  grid index in y-direction
              RadSwDownRefHeight => noahmp%forcing%RadSwDownRefHeight      ,& ! in,  downward shortwave radiation [W/m2] at reference height
              RadSwAbsSfc        => noahmp%energy%flux%RadSwAbsSfc         ,& ! in,  total absorbed solar radiation [W/m2]
              RadSwReflSfc       => noahmp%energy%flux%RadSwReflSfc        ,& ! in,  total reflected solar radiation [W/m2]
              RadLwNetSfc        => noahmp%energy%flux%RadLwNetSfc         ,& ! in,  total net longwave rad [W/m2] (+ to atm)
              HeatSensibleSfc    => noahmp%energy%flux%HeatSensibleSfc     ,& ! in,  total sensible heat [W/m2] (+ to atm)
              HeatLatentGrd      => noahmp%energy%flux%HeatLatentGrd       ,& ! in,  total ground latent heat [W/m2] (+ to atm)
              HeatGroundTot      => noahmp%energy%flux%HeatGroundTot       ,& ! in,  total ground heat flux [W/m2] (+ to soil/snow)
              RadSwAbsGrd        => noahmp%energy%flux%RadSwAbsGrd         ,& ! in,  solar radiation absorbed by ground [W/m2]
              HeatPrecipAdvSfc   => noahmp%energy%flux%HeatPrecipAdvSfc    ,& ! in,  precipitation advected heat - total [W/m2]
              EnergyBalanceError => noahmp%energy%state%EnergyBalanceError ,& ! out, error in surface energy balance [W/m2]
              RadSwBalanceError  => noahmp%energy%state%RadSwBalanceError   & ! out, error in shortwave radiation balance [W/m2]
             )
! ----------------------------------------------------------------------

    ! error in shortwave radiation balance should be <0.01 W/m2
    RadSwBalanceError = RadSwDownRefHeight - (RadSwAbsSfc + RadSwReflSfc)
    ! print out diagnostics when error is large
    if ( abs(RadSwBalanceError) > 0.01 ) then
       write(*,*) "GridIndexI, GridIndexJ = ", GridIndexI, GridIndexJ
       write(*,*) "RadSwBalanceError      = ", RadSwBalanceError
       write(*,*) "RadSwDownRefHeight     = ", RadSwDownRefHeight
       write(*,*) "RadSwReflSfc           = ", RadSwReflSfc
       write(*,*) "RadSwAbsGrd            = ", RadSwAbsGrd
       write(*,*) "RadSwAbsSfc            = ", RadSwAbsSfc
       stop "Error: Solar radiation budget problem in NoahMP LSM"
    endif

    ! error in surface energy balance should be <0.01 W/m2
    EnergyBalanceError = RadSwAbsGrd + HeatPrecipAdvSfc - (RadLwNetSfc + HeatSensibleSfc + HeatLatentGrd + HeatGroundTot)
    ! print out diagnostics when error is large
    if ( abs(EnergyBalanceError) > 0.01 ) then
       write(*,*) 'EnergyBalanceError = ', EnergyBalanceError, ' at GridIndexI,GridIndexJ: ', GridIndexI, GridIndexJ
       write(*,'(a17,F10.4)' ) "Net longwave:       ", RadLwNetSfc
       write(*,'(a17,F10.4)' ) "Total sensible:     ", HeatSensibleSfc
       write(*,'(a17,F10.4)' ) "Ground evap:        ", HeatLatentGrd
       write(*,'(a17,F10.4)' ) "Total ground:       ", HeatGroundTot
       write(*,'(a17,4F10.4)') "Precip advected:    ", HeatPrecipAdvSfc
       write(*,'(a17,F10.4)' ) "absorbed shortwave: ", RadSwAbsGrd
       stop "Error: Surface energy budget problem in NoahMP LSM"
    endif

    end associate

  end subroutine BalanceEnergyCheckGlacier

end module BalanceErrorCheckGlacierMod
