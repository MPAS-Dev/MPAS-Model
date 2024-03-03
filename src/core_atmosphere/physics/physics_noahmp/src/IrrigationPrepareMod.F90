module IrrigationPrepareMod

!!! Prepare dynamic irrigation variables and trigger irrigation based on conditions

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use IrrigationTriggerMod, only : IrrigationTrigger

  implicit none

contains

  subroutine IrrigationPrepare(noahmp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: None (embedded in NOAHMP_SFLX
! Original code: P. Valayamkunnath (NCAR) <prasanth@ucar.edu> (08/06/2020)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable

! ----------------------------------------------------------------------
    associate(                                                                       &
              LandUseDataName         => noahmp%config%domain%LandUseDataName       ,& ! in,    landuse data name (USGS or MODIS_IGBP)
              VegType                 => noahmp%config%domain%VegType               ,& ! in,    vegetation type
              FlagSoilProcess         => noahmp%config%domain%FlagSoilProcess       ,& ! in,    flag to calculate soil processes
              OptIrrigationMethod     => noahmp%config%nmlist%OptIrrigationMethod   ,& ! in,    irrigation method option
              IrriFracThreshold       => noahmp%water%param%IrriFracThreshold       ,& ! in,    irrigation fraction threshold
              IrriStopPrecipThr       => noahmp%water%param%IrriStopPrecipThr       ,& ! in,    maximum precipitation to stop irrigation trigger
              IrrigationFracGrid      => noahmp%water%state%IrrigationFracGrid      ,& ! in,    total input irrigation fraction of a grid
              IrrigationAmtSprinkler  => noahmp%water%state%IrrigationAmtSprinkler  ,& ! inout, irrigation water amount [m] to be applied, Sprinkler
              IrrigationAmtFlood      => noahmp%water%state%IrrigationAmtFlood      ,& ! inout, flood irrigation water amount [m]
              IrrigationAmtMicro      => noahmp%water%state%IrrigationAmtMicro      ,& ! inout, micro irrigation water amount [m]
              RainfallRefHeight       => noahmp%water%flux%RainfallRefHeight        ,& ! inout, rainfall [mm/s] at reference height
              FlagCropland            => noahmp%config%domain%FlagCropland          ,& ! out,   flag to identify croplands
              IrrigationFracSprinkler => noahmp%water%state%IrrigationFracSprinkler ,& ! out,   sprinkler irrigation fraction (0 to 1)
              IrrigationFracMicro     => noahmp%water%state%IrrigationFracMicro     ,& ! out,   fraction of grid under micro irrigation (0 to 1)
              IrrigationFracFlood     => noahmp%water%state%IrrigationFracFlood      & ! out,   fraction of grid under flood irrigation (0 to 1)
             )
! ----------------------------------------------------------------------

    ! initialize
    FlagCropland = .false.

    ! determine cropland
    if ( trim(LandUseDataName) == "USGS" ) then
       if ( (VegType >= 3) .and. (VegType <= 6) ) FlagCropland = .true.
    elseif ( trim(LandUseDataName) == "MODIFIED_IGBP_MODIS_NOAH") then
       if ( (VegType == 12) .or. (VegType == 14) ) FlagCropland = .true.
    endif

    ! if OptIrrigationMethod = 0 and if methods are unknown for certain area, then use sprinkler irrigation method
    if ( (OptIrrigationMethod == 0) .and. (IrrigationFracSprinkler == 0.0) .and. (IrrigationFracMicro == 0.0) &
         .and. (IrrigationFracFlood == 0.0) .and. (IrrigationFracGrid >= IrriFracThreshold) ) then
       IrrigationFracSprinkler = 1.0
    endif

    ! choose method based on user namelist choice
    if ( OptIrrigationMethod == 1 ) then
       IrrigationFracSprinkler = 1.0
       IrrigationFracMicro     = 0.0
       IrrigationFracFlood     = 0.0
    elseif ( OptIrrigationMethod == 2 ) then
       IrrigationFracSprinkler = 0.0
       IrrigationFracMicro     = 1.0
       IrrigationFracFlood     = 0.0
    elseif ( OptIrrigationMethod == 3 ) then
       IrrigationFracSprinkler = 0.0
       IrrigationFracMicro     = 0.0
       IrrigationFracFlood     = 1.0
    endif

    ! trigger irrigation only at soil water timestep to be consistent for solving soil water
    if ( FlagSoilProcess .eqv. .true. ) then
       if ( (FlagCropland .eqv. .true.) .and. (IrrigationFracGrid >= IrriFracThreshold) .and. &
            (RainfallRefHeight < (IrriStopPrecipThr/3600.0)) .and. &
            ((IrrigationAmtSprinkler+IrrigationAmtMicro+IrrigationAmtFlood) == 0.0) ) then
          call IrrigationTrigger(noahmp)
       endif

       ! set irrigation off if larger than IrriStopPrecipThr mm/h for this time step and irr triggered last time step
       if ( (RainfallRefHeight >= (IrriStopPrecipThr/3600.0)) .or. (IrrigationFracGrid < IrriFracThreshold) ) then
          IrrigationAmtSprinkler = 0.0
          IrrigationAmtMicro     = 0.0
          IrrigationAmtFlood     = 0.0
       endif
    endif

    end associate

  end subroutine IrrigationPrepare

end module IrrigationPrepareMod
