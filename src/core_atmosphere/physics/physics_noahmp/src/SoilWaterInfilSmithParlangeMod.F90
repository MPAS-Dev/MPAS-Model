module SoilWaterInfilSmithParlangeMod

!!! Compute soil surface infiltration rate based on Smith-Parlange equation
!!! Reference: Smith, R.E. (2002), Infiltration Theory for Hydrologic Applications

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use SoilHydraulicPropertyMod, only : SoilDiffusivityConductivityOpt2

  implicit none

contains

  subroutine SoilWaterInfilSmithParlange(noahmp, IndInfilMax, InfilSfcAcc, InfilSfcTmp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: SMITH_PARLANGE_INFIL
! Original code: Prasanth Valayamkunnath <prasanth@ucar.edu>
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

! in & out variabls
    type(noahmp_type)     , intent(inout) :: noahmp
    integer               , intent(in)    :: IndInfilMax           ! check for maximum infiltration at SoilMoistureWilt 
    real(kind=kind_noahmp), intent(inout) :: InfilSfcAcc           ! accumulated infiltration rate [m/s]
    real(kind=kind_noahmp), intent(out)   :: InfilSfcTmp           ! surface infiltration rate [m/s]

! local variables
    integer                               :: IndSoil               ! soil layer index
    real(kind=kind_noahmp)                :: SoilWatDiffusivity    ! soil water diffusivity [m2/s]
    real(kind=kind_noahmp)                :: SoilWatConductivity   ! soil water conductivity [m/s]
    real(kind=kind_noahmp)                :: InfilFacTmp           ! temporary infiltrability variable
    real(kind=kind_noahmp)                :: WeighFac              ! smith-parlang weighing parameter

! --------------------------------------------------------------------
    associate(                                                                     &
              DepthSoilLayer         => noahmp%config%domain%DepthSoilLayer       ,& ! in, depth [m] of layer-bottom from soil surface
              SoilMoisture           => noahmp%water%state%SoilMoisture           ,& ! in, total soil moisture [m3/m3]
              SoilIce                => noahmp%water%state%SoilIce                ,& ! in, soil ice content [m3/m3] 
              SoilSfcInflowMean      => noahmp%water%flux%SoilSfcInflowMean       ,& ! in, mean water input on soil surface [m/s]
              SoilMoistureSat        => noahmp%water%param%SoilMoistureSat        ,& ! in, saturated value of soil moisture [m3/m3]
              SoilMoistureWilt       => noahmp%water%param%SoilMoistureWilt       ,& ! in, wilting point soil moisture [m3/m3]
              SoilWatConductivitySat => noahmp%water%param%SoilWatConductivitySat ,& ! in, saturated soil hydraulic conductivity [m/s]
              InfilCapillaryDynVic   => noahmp%water%param%InfilCapillaryDynVic    & ! in, DVIC Mean Capillary Drive [m] for infiltration models
              )
! ----------------------------------------------------------------------

    ! smith-parlang weighing parameter, Gamma
    WeighFac = 0.82
    IndSoil  = 1

    ! check whether we are estimating infiltration for current SoilMoisture or SoilMoistureWilt
    if ( IndInfilMax == 1 ) then ! not active for now as the maximum infiltration is estimated based on table values

       ! estimate initial soil hydraulic conductivty (Ki in the equation) (m/s)
       call SoilDiffusivityConductivityOpt2(noahmp, SoilWatDiffusivity, SoilWatConductivity, &
                                            SoilMoistureWilt(IndSoil), 0.0, IndSoil)

       ! Maximum infiltrability based on the Eq. 6.25. (m/s)
       InfilFacTmp = InfilCapillaryDynVic * (SoilMoistureSat(IndSoil) - SoilMoistureWilt(IndSoil)) * &
                     (-1.0) * DepthSoilLayer(IndSoil)
       InfilSfcTmp = SoilWatConductivitySat(IndSoil) + (WeighFac*(SoilWatConductivitySat(IndSoil)-SoilWatConductivity) / &
                                                       (exp(WeighFac*1.0e-05/InfilFacTmp) - 1.0))

       ! infiltration rate at surface
       if ( SoilWatConductivitySat(IndSoil) < SoilSfcInflowMean ) then
          InfilSfcTmp = min(SoilSfcInflowMean, InfilSfcTmp)
       else
          InfilSfcTmp = SoilSfcInflowMean
       endif
       if ( InfilSfcTmp < 0.0 ) InfilSfcTmp = SoilWatConductivity

    else

       ! estimate initial soil hydraulic conductivty (Ki in the equation) (m/s)
       call SoilDiffusivityConductivityOpt2(noahmp, SoilWatDiffusivity, SoilWatConductivity, &
                                            SoilMoisture(IndSoil), SoilIce(IndSoil), IndSoil)

       ! Maximum infiltrability based on the Eq. 6.25. (m/s)
       InfilFacTmp = InfilCapillaryDynVic * max(0.0, (SoilMoistureSat(IndSoil) - SoilMoisture(IndSoil))) * &
                     (-1.0) * DepthSoilLayer(IndSoil)
       if ( InfilFacTmp == 0.0 ) then ! infiltration at surface == saturated hydraulic conductivity
          InfilSfcTmp = SoilWatConductivity
       else
          InfilSfcTmp = SoilWatConductivitySat(IndSoil) + (WeighFac*(SoilWatConductivitySat(IndSoil)-SoilWatConductivity) / &
                                                          (exp(WeighFac*InfilSfcAcc/InfilFacTmp) - 1.0))
       endif

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

  end subroutine SoilWaterInfilSmithParlange

end module SoilWaterInfilSmithParlangeMod
