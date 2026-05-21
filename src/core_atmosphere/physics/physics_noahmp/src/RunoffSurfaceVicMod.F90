module RunoffSurfaceVicMod

!!! Compute saturated area, surface infiltration, and surface runoff based on VIC runoff scheme
!!! This scheme is adopted from VIC model

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine RunoffSurfaceVIC(noahmp, TimeStep)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: COMPUTE_VIC_SURFRUNOFF
! Original code: Prasanth Valayamkunnath <prasanth@ucar.edu>
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

! IN & OUT variabls
    type(noahmp_type)     , intent(inout) :: noahmp
    real(kind=kind_noahmp), intent(in)    :: TimeStep          ! timestep (may not be the same as model timestep)

! local variable
    integer                               :: LoopInd           ! do-loop index
    real(kind=kind_noahmp)                :: InfilExpFac       ! infitration exponential factor
    real(kind=kind_noahmp)                :: WaterDepthInit    ! initial water depth [m]
    real(kind=kind_noahmp)                :: WaterDepthMax     ! Maximum water depth [m]
    real(kind=kind_noahmp)                :: InfilVarTmp       ! temporary infiltration variable
    real(kind=kind_noahmp)                :: SoilMoistTop      ! top layer soil moisture [m]
    real(kind=kind_noahmp)                :: SoilMoistTopMax   ! top layer max soil moisture [m]

! --------------------------------------------------------------------
    associate(                                                          &
              NumSoilLayer      => noahmp%config%domain%NumSoilLayer   ,& ! in,  number of soil layers
              DepthSoilLayer    => noahmp%config%domain%DepthSoilLayer ,& ! in,  depth [m] of layer-bottom from soil surface
              SoilMoisture      => noahmp%water%state%SoilMoisture     ,& ! in,  total soil moisture [m3/m3]
              SoilSfcInflowMean => noahmp%water%flux%SoilSfcInflowMean ,& ! in,  mean water input on soil surface [m/s]
              SoilMoistureSat   => noahmp%water%param%SoilMoistureSat  ,& ! in,  saturated value of soil moisture [m3/m3]
              InfilFacVic       => noahmp%water%param%InfilFacVic      ,& ! in,  VIC model infiltration parameter
              RunoffSurface     => noahmp%water%flux%RunoffSurface     ,& ! out, surface runoff [m/s]
              InfilRateSfc      => noahmp%water%flux%InfilRateSfc      ,& ! out, infiltration rate at surface [m/s]
              SoilSaturateFrac  => noahmp%water%state%SoilSaturateFrac  & ! out, fractional saturated area for soil moisture
             )
! ----------------------------------------------------------------------

    ! Initialization
    InfilExpFac      = 0.0
    SoilSaturateFrac = 0.0
    WaterDepthMax    = 0.0
    WaterDepthInit   = 0.0
    InfilVarTmp      = 0.0
    SoilMoistTop     = 0.0
    SoilMoistTopMax  = 0.0
    RunoffSurface    = 0.0
    InfilRateSfc     = 0.0

    do LoopInd = 1, NumSoilLayer-2
       SoilMoistTop    = SoilMoistTop + SoilMoisture(LoopInd) * (-1.0) * DepthSoilLayer(LoopInd)
       SoilMoistTopMax = SoilMoistTopMax + SoilMoistureSat(LoopInd) * (-1.0) * DepthSoilLayer(LoopInd)
    enddo

    ! fractional saturated area from soil moisture
    InfilExpFac      = InfilFacVic / ( 1.0 + InfilFacVic )
    SoilSaturateFrac = 1.0 - (max(0.0, (1.0-(SoilMoistTop/SoilMoistTopMax))))**InfilExpFac
    SoilSaturateFrac = max(0.0, SoilSaturateFrac)
    SoilSaturateFrac = min(1.0, SoilSaturateFrac)

    ! Infiltration for the previous time-step soil moisture based on SoilSaturateFrac
    WaterDepthMax  = (1.0 + InfilFacVic) * SoilMoistTopMax
    WaterDepthInit = WaterDepthMax * (1.0 - (1.0 - SoilSaturateFrac)**(1.0/InfilFacVic))

    ! Solve for surface runoff
    if ( SoilSfcInflowMean == 0.0 ) then
       RunoffSurface = 0.0
    else if ( WaterDepthMax == 0.0 ) then
       RunoffSurface = SoilSfcInflowMean * TimeStep
    else if ( (WaterDepthInit + (SoilSfcInflowMean*TimeStep)) > WaterDepthMax ) then
       RunoffSurface = SoilSfcInflowMean * TimeStep - SoilMoistTopMax + SoilMoistTop
    else
       InfilVarTmp  = 1.0 - ((WaterDepthInit + (SoilSfcInflowMean * TimeStep) ) / WaterDepthMax)
       RunoffSurface = SoilSfcInflowMean * TimeStep - SoilMoistTopMax + SoilMoistTop + &
                       SoilMoistTopMax * (InfilVarTmp**(1.0+InfilFacVic))
    endif

    RunoffSurface = RunoffSurface / TimeStep
    if ( RunoffSurface < 0.0 ) RunoffSurface = 0.0
    if ( RunoffSurface > SoilSfcInflowMean) RunoffSurface = SoilSfcInflowMean

    InfilRateSfc = SoilSfcInflowMean - RunoffSurface

    end associate

  end subroutine RunoffSurfaceVIC

end module RunoffSurfaceVicMod
