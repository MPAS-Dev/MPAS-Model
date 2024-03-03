module SoilWaterSupercoolNiu06Mod

!!! Calculate amount of supercooled liquid soil water content if soil temperature < freezing point
!!! This solution does not use iteration (Niu and Yang, 2006 JHM).

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SoilWaterSupercoolNiu06(noahmp, IndSoil, SoilWatSupercool, SoilTemperature)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: embedded in PHASECHANGE
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type)     , intent(inout) :: noahmp
    integer               , intent(in   ) :: IndSoil              ! soil layer index
    real(kind=kind_noahmp), intent(in   ) :: SoilTemperature      ! soil temperature [K]
    real(kind=kind_noahmp), intent(out  ) :: SoilWatSupercool     ! soil supercooled liquid water content [m3/m3]

! local variable
    real(kind=kind_noahmp)                :: SoilWatPotFrz                  ! frozen water potential [mm]

! -----------------------------------------------------------------------------
    associate(                                                                 &
              SoilExpCoeffB       => noahmp%water%param%SoilExpCoeffB         ,& ! in,  soil B parameter
              SoilMatPotentialSat => noahmp%water%param%SoilMatPotentialSat   ,& ! in,  saturated soil matric potential [m]
              SoilMoistureSat     => noahmp%water%param%SoilMoistureSat        & ! in,  saturated value of soil moisture [m3/m3]
             )
! -----------------------------------------------------------------------------

    SoilWatPotFrz    = ConstLatHeatFusion * (ConstFreezePoint - SoilTemperature) / (ConstGravityAcc * SoilTemperature)
    SoilWatSupercool = SoilMoistureSat(IndSoil) * (SoilWatPotFrz / SoilMatPotentialSat(IndSoil))**(-1.0/SoilExpCoeffB(IndSoil))

    end associate

  end subroutine SoilWaterSupercoolNiu06

end module SoilWaterSupercoolNiu06Mod
