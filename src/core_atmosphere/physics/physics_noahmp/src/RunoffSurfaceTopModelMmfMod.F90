module RunoffSurfaceTopModelMmfMod

!!! Calculate surface runoff based on TOPMODEL with MMF groundwater scheme

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine RunoffSurfaceTopModelMMF(noahmp)

! ------------------------ Code history --------------------------------------------------
! Originally embeded in SOILWATER subroutine instead of as a separate subroutine
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! --------------------------------------------------------------------
    associate(                                                           &
              SoilSfcInflowMean => noahmp%water%flux%SoilSfcInflowMean  ,& ! in,  mean water input on soil surface [m/s]
              RunoffDecayFac    => noahmp%water%param%RunoffDecayFac    ,& ! in,  runoff decay factor [1/m]
              SoilSfcSatFracMax => noahmp%water%param%SoilSfcSatFracMax ,& ! in,  maximum surface saturated fraction (global mean)
              SoilImpervFrac    => noahmp%water%state%SoilImpervFrac    ,& ! in,  impervious fraction due to frozen soil
              WaterTableDepth   => noahmp%water%state%WaterTableDepth   ,& ! in,  water table depth [m]
              SoilSaturateFrac  => noahmp%water%state%SoilSaturateFrac  ,& ! out, fractional saturated area for soil moisture
              RunoffSurface     => noahmp%water%flux%RunoffSurface      ,& ! out, surface runoff [m/s]
              InfilRateSfc      => noahmp%water%flux%InfilRateSfc        & ! out, infiltration rate at surface [m/s]
             )
! ----------------------------------------------------------------------

    ! set up key parameter
    RunoffDecayFac = 6.0

    ! compute saturated area fraction
    SoilSaturateFrac = SoilSfcSatFracMax * exp(-0.5 * RunoffDecayFac * max(-2.0-WaterTableDepth,0.0))

    ! compute surface runoff and infiltration  m/s
    if ( SoilSfcInflowMean > 0.0 ) then
       RunoffSurface = SoilSfcInflowMean * ((1.0-SoilImpervFrac(1)) * SoilSaturateFrac + SoilImpervFrac(1))
       InfilRateSfc  = SoilSfcInflowMean - RunoffSurface 
    endif

    end associate

  end subroutine RunoffSurfaceTopModelMMF

end module RunoffSurfaceTopModelMmfMod
