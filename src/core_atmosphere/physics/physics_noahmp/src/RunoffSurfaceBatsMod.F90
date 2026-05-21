module RunoffSurfaceBatsMod

!!! Calculate surface runoff based on BATS scheme

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine RunoffSurfaceBATS(noahmp)

! ------------------------ Code history --------------------------------------------------
! Originally embeded in SOILWATER subroutine instead of as a separate subroutine
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variables
    integer                          :: LoopInd             ! loop index
    real(kind=kind_noahmp)           :: SoilMoistureTmp     ! 2-m averaged soil moisture (m3/m3)
    real(kind=kind_noahmp)           :: SoilDepthTmp        ! 2-m soil depth (m)

! --------------------------------------------------------------------
    associate(                                                                       &
              NumSoilLayer           => noahmp%config%domain%NumSoilLayer           ,& ! in,  number of soil layers
              ThicknessSnowSoilLayer => noahmp%config%domain%ThicknessSnowSoilLayer ,& ! in,  thickness of snow/soil layers [m]
              SoilMoisture           => noahmp%water%state%SoilMoisture             ,& ! in,  total soil water content [m3/m3]
              SoilImpervFrac         => noahmp%water%state%SoilImpervFrac           ,& ! in,  impervious fraction due to frozen soil
              SoilSfcInflowMean      => noahmp%water%flux%SoilSfcInflowMean         ,& ! in,  water input on soil surface [m/s]
              SoilMoistureSat        => noahmp%water%param%SoilMoistureSat          ,& ! in,  saturated value of soil moisture [m3/m3]
              SoilSaturateFrac       => noahmp%water%state%SoilSaturateFrac         ,& ! out, fractional saturated area for soil moisture
              RunoffSurface          => noahmp%water%flux%RunoffSurface             ,& ! out, surface runoff [m/s]
              InfilRateSfc           => noahmp%water%flux%InfilRateSfc               & ! out, infiltration rate at surface [m/s]
             )
! ----------------------------------------------------------------------

    ! initialization
    SoilMoistureTmp = 0.0
    SoilDepthTmp    = 0.0

    ! compute mean soil moisture, depth and saturation fraction
    do LoopInd = 1, NumSoilLayer
       SoilDepthTmp    = SoilDepthTmp + ThicknessSnowSoilLayer(LoopInd)
       SoilMoistureTmp = SoilMoistureTmp + &
                         SoilMoisture(LoopInd) / SoilMoistureSat(LoopInd) * ThicknessSnowSoilLayer(LoopInd)
       if ( SoilDepthTmp >= 2.0 ) exit
    enddo
    SoilMoistureTmp  = SoilMoistureTmp / SoilDepthTmp
    SoilSaturateFrac = max(0.01, SoilMoistureTmp)**4.0  ! BATS

    ! compute surface runoff and infiltration m/s
    if ( SoilSfcInflowMean > 0.0 ) then
       RunoffSurface = SoilSfcInflowMean * ((1.0-SoilImpervFrac(1)) * SoilSaturateFrac + SoilImpervFrac(1))
       InfilRateSfc  = SoilSfcInflowMean - RunoffSurface 
    endif

    end associate

  end subroutine RunoffSurfaceBATS

end module RunoffSurfaceBatsMod
