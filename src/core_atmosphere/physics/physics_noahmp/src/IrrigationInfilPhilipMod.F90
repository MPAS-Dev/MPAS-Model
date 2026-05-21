module IrrigationInfilPhilipMod

!!! Estimate infiltration rate based on Philip's two parameter equation
!!! Reference: Eq.2 in Valiantzas (2010): New linearized two-parameter infiltration equation for direct
!!! determination of conductivity and sorptivity, J. Hydrology.

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use SoilHydraulicPropertyMod, only : SoilDiffusivityConductivityOpt2

  implicit none

contains

  subroutine IrrigationInfilPhilip(noahmp, TimeStep, InfilRateSfc)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: IRR_PHILIP_INFIL
! Original code: P. Valayamkunnath (NCAR) <prasanth@ucar.edu> (08/06/2020)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

! IN & OUT variables
    type(noahmp_type)     , intent(inout)  :: noahmp
    real(kind=kind_noahmp), intent(in)     :: TimeStep             ! time step [s]
    real(kind=kind_noahmp), intent(out)    :: InfilRateSfc         ! surface infiltration rate [m/s]

! local variables
    integer                                :: LoopInd              ! loop indices
    integer                                :: IndSoilLayer         ! soil layer index
    real(kind=kind_noahmp)                 :: SoilSorptivity       ! sorptivity [m s^-1/2]
    real(kind=kind_noahmp)                 :: SoilWatConductInit   ! intial hydraulic conductivity [m/s]
    real(kind=kind_noahmp)                 :: SoilWatConductivity  ! soil water conductivity [m/s]
    real(kind=kind_noahmp)                 :: SoilWatDiffusivity   ! soil water diffusivity [m2/s]
    real(kind=kind_noahmp)                 :: SoilIceMaxTmp        ! maximum soil ice content [m3/m3]

! --------------------------------------------------------------------
    associate(                                                                     &
              NumSoilLayer           => noahmp%config%domain%NumSoilLayer         ,& ! in, number of soil layers
              SoilMoisture           => noahmp%water%state%SoilMoisture           ,& ! in, total soil moisture [m3/m3]
              SoilLiqWater           => noahmp%water%state%SoilLiqWater           ,& ! in, soil water content [m3/m3]
              SoilIce                => noahmp%water%state%SoilIce                ,& ! in, soil ice content [m3/m3]
              SoilMoistureSat        => noahmp%water%param%SoilMoistureSat        ,& ! in, saturated value of soil moisture [m3/m3]
              SoilWatDiffusivitySat  => noahmp%water%param%SoilWatDiffusivitySat  ,& ! in, saturated soil hydraulic diffusivity [m2/s]
              SoilWatConductivitySat => noahmp%water%param%SoilWatConductivitySat  & ! in, saturated soil hydraulic conductivity [m/s]
             )
! ----------------------------------------------------------------------

    ! initialize out-only and local variables
    SoilWatConductivity = 0.0
    SoilWatDiffusivity  = 0.0
    SoilIceMaxTmp       = 0.0
    SoilSorptivity      = 0.0
    SoilWatConductInit  = 0.0

    ! maximum ice fraction
    do LoopInd = 1, NumSoilLayer
       if ( SoilIce(LoopInd) > SoilIceMaxTmp ) SoilIceMaxTmp = SoilIce(LoopInd)
    enddo

    ! estimate initial soil hydraulic conductivty and diffusivity (Ki, D(theta) in the equation)
    IndSoilLayer = 1
    call SoilDiffusivityConductivityOpt2(noahmp, SoilWatDiffusivity, SoilWatConductivity, &
                                         SoilLiqWater(IndSoilLayer), SoilIceMaxTmp, IndSoilLayer)

    ! sorptivity based on Eq. 10b from Kutilek, Miroslav, and Jana Valentova (1986) 
    ! sorptivity approximations. Transport in Porous Media 1.1, 57-62.
    SoilSorptivity = sqrt(2.0 * max(0.0, (SoilMoistureSat(IndSoilLayer) - SoilMoisture(IndSoilLayer))) * &
                          (SoilWatDiffusivitySat(IndSoilLayer) - SoilWatDiffusivity))

    ! parameter A in Eq. 9 of Valiantzas (2010) is given by
    SoilWatConductInit = min(SoilWatConductivity, (2.0/3.0) * SoilWatConductivitySat(IndSoilLayer))
    SoilWatConductInit = max(SoilWatConductInit , (1.0/3.0) * SoilWatConductivitySat(IndSoilLayer))

    ! maximun infiltration rate, m/s
    InfilRateSfc = 0.5 * SoilSorptivity * (TimeStep**(-0.5)) + SoilWatConductInit ! m/s
    InfilRateSfc = max(0.0, InfilRateSfc)

    end associate

  end subroutine IrrigationInfilPhilip

end module IrrigationInfilPhilipMod
