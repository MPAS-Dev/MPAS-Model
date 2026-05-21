module SoilHydraulicPropertyMod

!!! Two methods for calculating soil water diffusivity and soil hydraulic conductivity
!!! Option 1: linear effects (more permeable, Niu and Yang,2006); Option 2: nonlinear effects (less permeable)

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SoilDiffusivityConductivityOpt1(noahmp, SoilWatDiffusivity, SoilWatConductivity, &
                                             SoilMoisture, SoilImpervFrac, IndLayer)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: WDFCND1
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

! IN and OUT variables
    type(noahmp_type)     , intent(inout) :: noahmp
    integer               , intent(in)    :: IndLayer               ! soil layer index
    real(kind=kind_noahmp), intent(in)    :: SoilMoisture           ! soil moisture [m3/m3]
    real(kind=kind_noahmp), intent(in)    :: SoilImpervFrac         ! impervious fraction due to frozen soil
    real(kind=kind_noahmp), intent(out)   :: SoilWatConductivity    ! soil water conductivity [m/s]
    real(kind=kind_noahmp), intent(out)   :: SoilWatDiffusivity     ! soil water diffusivity [m2/s]

! local variable
    real(kind=kind_noahmp)                :: SoilExpTmp             ! exponential local factor
    real(kind=kind_noahmp)                :: SoilPreFac             ! pre-factor

! --------------------------------------------------------------------
    associate(                                                                     &
              SoilMoistureSat        => noahmp%water%param%SoilMoistureSat        ,& ! in, saturated value of soil moisture [m3/m3]
              SoilExpCoeffB          => noahmp%water%param%SoilExpCoeffB          ,& ! in, soil B parameter
              SoilWatDiffusivitySat  => noahmp%water%param%SoilWatDiffusivitySat  ,& ! in, saturated soil hydraulic diffusivity [m2/s]
              SoilWatConductivitySat => noahmp%water%param%SoilWatConductivitySat  & ! in, saturated soil hydraulic conductivity [m/s]
             )
! ----------------------------------------------------------------------

    SoilPreFac = max(0.01, SoilMoisture/SoilMoistureSat(IndLayer))

    ! soil water diffusivity
    SoilExpTmp         = SoilExpCoeffB(IndLayer) + 2.0
    SoilWatDiffusivity = SoilWatDiffusivitySat(IndLayer) * SoilPreFac ** SoilExpTmp
    SoilWatDiffusivity = SoilWatDiffusivity * (1.0 - SoilImpervFrac)

    ! soil hydraulic conductivity
    SoilExpTmp          = 2.0 * SoilExpCoeffB(IndLayer) + 3.0
    SoilWatConductivity = SoilWatConductivitySat(IndLayer) * SoilPreFac ** SoilExpTmp
    SoilWatConductivity = SoilWatConductivity * (1.0 - SoilImpervFrac)

    end associate

  end subroutine SoilDiffusivityConductivityOpt1


  subroutine SoilDiffusivityConductivityOpt2(noahmp, SoilWatDiffusivity, SoilWatConductivity, &
                                             SoilMoisture, SoilIce, IndLayer)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: WDFCND2
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

! IN and OUT variables
    type(noahmp_type)     , intent(inout) :: noahmp
    integer               , intent(in)    :: IndLayer              ! soil layer index
    real(kind=kind_noahmp), intent(in)    :: SoilMoisture          ! soil moisture [m3/m3]
    real(kind=kind_noahmp), intent(in)    :: SoilIce               ! soil ice content [m3/m3]
    real(kind=kind_noahmp), intent(out)   :: SoilWatConductivity   ! soil water conductivity [m/s]
    real(kind=kind_noahmp), intent(out)   :: SoilWatDiffusivity    ! soil water diffusivity [m2/s]

! local variable
    real(kind=kind_noahmp)                :: SoilExpTmp            ! exponential local factor
    real(kind=kind_noahmp)                :: SoilPreFac1           ! pre-factor
    real(kind=kind_noahmp)                :: SoilPreFac2           ! pre-factor
    real(kind=kind_noahmp)                :: SoilIceWgt            ! weights

! --------------------------------------------------------------------
    associate(                                                                     &
              SoilMoistureSat        => noahmp%water%param%SoilMoistureSat        ,& ! in, saturated value of soil moisture [m3/m3]
              SoilExpCoeffB          => noahmp%water%param%SoilExpCoeffB          ,& ! in, soil B parameter
              SoilWatDiffusivitySat  => noahmp%water%param%SoilWatDiffusivitySat  ,& ! in, saturated soil hydraulic diffusivity [m2/s]
              SoilWatConductivitySat => noahmp%water%param%SoilWatConductivitySat  & ! in, saturated soil hydraulic conductivity [m/s]
             )
! ----------------------------------------------------------------------

    SoilPreFac1 = 0.05 / SoilMoistureSat(IndLayer)
    SoilPreFac2 = max(0.01, SoilMoisture/SoilMoistureSat(IndLayer))
    SoilPreFac1 = min(SoilPreFac1, SoilPreFac2)

    ! soil water diffusivity
    SoilExpTmp         = SoilExpCoeffB(IndLayer) + 2.0
    SoilWatDiffusivity = SoilWatDiffusivitySat(IndLayer) * SoilPreFac2 ** SoilExpTmp
    if ( SoilIce > 0.0 ) then
       SoilIceWgt         = 1.0 / (1.0 + (500.0 * SoilIce)**3.0)
       SoilWatDiffusivity = SoilIceWgt * SoilWatDiffusivity + &
                            (1.0-SoilIceWgt) * SoilWatDiffusivitySat(IndLayer) * SoilPreFac1**SoilExpTmp
    endif

    ! soil hydraulic conductivity
    SoilExpTmp          = 2.0 * SoilExpCoeffB(IndLayer) + 3.0
    SoilWatConductivity = SoilWatConductivitySat(IndLayer) * SoilPreFac2 ** SoilExpTmp

    end associate

  end subroutine SoilDiffusivityConductivityOpt2

end module SoilHydraulicPropertyMod
