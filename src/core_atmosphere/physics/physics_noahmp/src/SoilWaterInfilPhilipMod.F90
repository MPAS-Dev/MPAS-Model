module SoilWaterInfilPhilipMod

!!! Compute soil surface infiltration rate based on Philip's two parameter equation
!!! Reference: Valiantzas (2010): New linearized two-parameter infiltration equation 
!!! for direct determination of conductivity and sorptivity, J. Hydrology.

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use SoilHydraulicPropertyMod, only : SoilDiffusivityConductivityOpt2

  implicit none

contains

  subroutine SoilWaterInfilPhilip(noahmp, TimeStep, IndInfilMax, InfilSfcAcc, InfilSfcTmp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: PHILIP_INFIL
! Original code: Prasanth Valayamkunnath <prasanth@ucar.edu>
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

! in & out variabls
    type(noahmp_type)     , intent(inout) :: noahmp
    integer               , intent(in)    :: IndInfilMax            ! check for maximum infiltration at SoilMoistureWilt 
    real(kind=kind_noahmp), intent(in)    :: TimeStep               ! timestep (may not be the same as model timestep)
    real(kind=kind_noahmp), intent(inout) :: InfilSfcAcc            ! accumulated infiltration rate [m/s]
    real(kind=kind_noahmp), intent(out)   :: InfilSfcTmp            ! surface infiltration rate [m/s]

! local variable
    integer                               :: IndSoil                ! soil layer index
    real(kind=kind_noahmp)                :: SoilWatDiffusivity     ! soil water diffusivity [m2/s]
    real(kind=kind_noahmp)                :: SoilWatConductivity    ! soil water conductivity [m/s]
    real(kind=kind_noahmp)                :: SoilSorptivity         ! sorptivity [m s^-1/2]
    real(kind=kind_noahmp)                :: SoilWatConductTmp      ! intial hydraulic conductivity [m/s]

! --------------------------------------------------------------------
    associate(                                                                     &
              SoilMoisture           => noahmp%water%state%SoilMoisture           ,& ! in, total soil moisture [m3/m3]
              SoilIce                => noahmp%water%state%SoilIce                ,& ! in, soil ice content [m3/m3] 
              SoilSfcInflowMean      => noahmp%water%flux%SoilSfcInflowMean       ,& ! in, mean water input on soil surface [m/s]
              SoilMoistureSat        => noahmp%water%param%SoilMoistureSat        ,& ! in, saturated value of soil moisture [m3/m3]
              SoilMoistureWilt       => noahmp%water%param%SoilMoistureWilt       ,& ! in, wilting point soil moisture [m3/m3]
              SoilWatDiffusivitySat  => noahmp%water%param%SoilWatDiffusivitySat  ,& ! in, saturated soil hydraulic diffusivity [m2/s]
              SoilWatConductivitySat => noahmp%water%param%SoilWatConductivitySat  & ! in, saturated soil hydraulic conductivity [m/s]
              )
! ----------------------------------------------------------------------

    IndSoil = 1
    if ( IndInfilMax == 1) then

       ! estimate initial soil hydraulic conductivty and diffusivity (Ki, D(theta) in the equation)
       call SoilDiffusivityConductivityOpt2(noahmp, SoilWatDiffusivity, SoilWatConductivity, &
                                            SoilMoistureWilt(IndSoil), 0.0, IndSoil)

       ! Sorptivity based on Eq. 10b from Kutílek, Miroslav, and Jana Valentová (1986)
       ! Sorptivity approximations. Transport in Porous Media 1.1, 57-62.
       SoilSorptivity = sqrt(2.0 * (SoilMoistureSat(IndSoil) - SoilMoistureWilt(IndSoil)) * &
                             (SoilWatDiffusivitySat(IndSoil) - SoilWatDiffusivity))

       ! Parameter A in Eq. 9 of Valiantzas (2010) is given by
       SoilWatConductTmp = min(SoilWatConductivity, (2.0/3.0)*SoilWatConductivitySat(IndSoil))
       SoilWatConductTmp = max(SoilWatConductTmp,   (1.0/3.0)*SoilWatConductivitySat(IndSoil))

       ! Maximun infiltration rate
       InfilSfcTmp = (1.0/2.0) * SoilSorptivity * (TimeStep**(-1.0/2.0)) + SoilWatConductTmp
       if ( InfilSfcTmp < 0.0) InfilSfcTmp = SoilWatConductivity

    else

       ! estimate initial soil hydraulic conductivty and diffusivity (Ki, D(theta) in the equation)
       call SoilDiffusivityConductivityOpt2(noahmp, SoilWatDiffusivity, SoilWatConductivity, &
                                            SoilMoisture(IndSoil), SoilIce(IndSoil), IndSoil)

       ! Sorptivity based on Eq. 10b from Kutílek, Miroslav, and Jana Valentová (1986) 
       ! Sorptivity approximations. Transport in Porous Media 1.1, 57-62.
       SoilSorptivity = sqrt(2.0 * max(0.0, (SoilMoistureSat(IndSoil)-SoilMoisture(IndSoil))) * &
                             (SoilWatDiffusivitySat(IndSoil) - SoilWatDiffusivity))
       ! Parameter A in Eq. 9 of Valiantzas (2010) is given by
       SoilWatConductTmp = min(SoilWatConductivity, (2.0/3.0)*SoilWatConductivitySat(IndSoil))
       SoilWatConductTmp = max(SoilWatConductTmp,   (1.0/3.0)*SoilWatConductivitySat(IndSoil))

       ! Maximun infiltration rate
       InfilSfcTmp = (1.0/2.0) * SoilSorptivity * (TimeStep**(-1.0/2.0)) + SoilWatConductTmp

       ! infiltration rate at surface
       if ( SoilWatConductivitySat(IndSoil) < SoilSfcInflowMean ) then
          InfilSfcTmp = min(SoilSfcInflowMean, InfilSfcTmp)
       else
          InfilSfcTmp = SoilSfcInflowMean
       endif
       ! accumulated infiltration function
       InfilSfcAcc = InfilSfcAcc + InfilSfcTmp

    endif

    end associate

  end subroutine SoilWaterInfilPhilip

end module SoilWaterInfilPhilipMod
