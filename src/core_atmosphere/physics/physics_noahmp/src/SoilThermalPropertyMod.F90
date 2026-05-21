module SoilThermalPropertyMod

!!! Compute soil thermal conductivity based on Peters-Lidard et al. (1998)

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SoilThermalProperty(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: TDFCND
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! If the soil has any moisture content compute a partial sum/product
! otherwise use a constant value which works well with most soils
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                          :: LoopInd                       ! loop index
    real(kind=kind_noahmp)           :: KerstenFac                    ! Kersten number
    real(kind=kind_noahmp)           :: SoilGamFac                    ! temporary soil GAMMD factor
    real(kind=kind_noahmp)           :: ThermConductSoilDry           ! thermal conductivity for dry soil
    real(kind=kind_noahmp)           :: ThermConductSoilSat           ! thermal conductivity for saturated soil
    real(kind=kind_noahmp)           :: ThermConductSolid             ! thermal conductivity for the solids
    real(kind=kind_noahmp)           :: SoilSatRatio                  ! saturation ratio
    real(kind=kind_noahmp)           :: SoilWatFracSat                ! saturated soil water fraction
    real(kind=kind_noahmp)           :: SoilWatFrac                   ! soil water fraction
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilIceTmp   ! temporal soil ice

! --------------------------------------------------------------------
    associate(                                                          &
              NumSoilLayer     => noahmp%config%domain%NumSoilLayer    ,& ! in,  number of soil layers
              SoilMoistureSat  => noahmp%water%param%SoilMoistureSat   ,& ! in,  saturated value of soil moisture [m3/m3]
              SoilHeatCapacity => noahmp%energy%param%SoilHeatCapacity ,& ! in,  soil volumetric specific heat [J/m3/K]
              SoilQuartzFrac   => noahmp%energy%param%SoilQuartzFrac   ,& ! in,  soil quartz content
              SoilMoisture     => noahmp%water%state%SoilMoisture      ,& ! in,  total soil moisture [m3/m3]
              SoilLiqWater     => noahmp%water%state%SoilLiqWater      ,& ! in,  soil water content [m3/m3] 
              HeatCapacVolSoil => noahmp%energy%state%HeatCapacVolSoil ,& ! out, soil layer volumetric specific heat [J/m3/K]
              ThermConductSoil => noahmp%energy%state%ThermConductSoil  & ! out, soil layer thermal conductivity [W/m/K]
             )
! ----------------------------------------------------------------------

    ! initiazliation
    if (.not. allocated(SoilIceTmp)) allocate(SoilIceTmp(1:NumSoilLayer))
    SoilIceTmp(:)       = 0.0

    do LoopInd = 1, NumSoilLayer

       ! ==== soil heat capacity
       SoilIceTmp(LoopInd)       = SoilMoisture(LoopInd) - SoilLiqWater(LoopInd)
       HeatCapacVolSoil(LoopInd) = SoilLiqWater(LoopInd) * ConstHeatCapacWater +                            &
                                   (1.0 - SoilMoistureSat(LoopInd)) * SoilHeatCapacity +                    &
                                   (SoilMoistureSat(LoopInd) - SoilMoisture(LoopInd)) * ConstHeatCapacAir + &
                                   SoilIceTmp(LoopInd) * ConstHeatCapacIce

       ! ==== soil thermal conductivity
       SoilSatRatio = SoilMoisture(LoopInd) / SoilMoistureSat(LoopInd) ! SATURATION RATIO

       ! UNFROZEN FRACTION (FROM 1., i.e., 100%LIQUID, TO 0. (100% FROZEN))
       ThermConductSolid = (ConstThermConductQuartz ** SoilQuartzFrac(LoopInd)) * &
                           (ConstThermConductSoilOth ** (1.0 - SoilQuartzFrac(LoopInd)))

       ! UNFROZEN VOLUME FOR SATURATION (POROSITY*SoilWatFrac)
       SoilWatFrac = 1.0    ! Prevent divide by zero (suggested by D. Mocko)
       if ( SoilMoisture(LoopInd) > 0.0 ) SoilWatFrac = SoilLiqWater(LoopInd) / SoilMoisture(LoopInd)
       SoilWatFracSat = SoilWatFrac * SoilMoistureSat(LoopInd)

       ! SATURATED THERMAL CONDUCTIVITY
       ThermConductSoilSat = ThermConductSolid ** (1.0-SoilMoistureSat(LoopInd)) * &
                             ConstThermConductIce ** (SoilMoistureSat(LoopInd)-SoilWatFracSat) * &
                             ConstThermConductWater ** (SoilWatFracSat)

       ! DRY THERMAL CONDUCTIVITY IN W.M-1.K-1
       SoilGamFac          = (1.0 - SoilMoistureSat(LoopInd)) * 2700.0
       ThermConductSoilDry = (0.135 * SoilGamFac + 64.7) / (2700.0 - 0.947 * SoilGamFac)

       ! THE KERSTEN NUMBER KerstenFac
       if ( (SoilLiqWater(LoopInd)+0.0005) < SoilMoisture(LoopInd) ) then ! FROZEN 
          KerstenFac = SoilSatRatio
       else  ! UNFROZEN
          ! KERSTEN NUMBER (USING "FINE" FORMULA, VALID FOR SOILS CONTAINING AT
          ! LEAST 5% OF PARTICLES WITH DIAMETER LESS THAN 2.E-6 METERS.)
          ! (FOR "COARSE" FORMULA, SEE PETERS-LIDARD ET AL., 1998).
          if ( SoilSatRatio > 0.1 ) then
             KerstenFac = log10(SoilSatRatio) + 1.0
          else
             KerstenFac = 0.0
          endif
       endif

       !  THERMAL CONDUCTIVITY
       ThermConductSoil(LoopInd) = KerstenFac*(ThermConductSoilSat-ThermConductSoilDry) + ThermConductSoilDry

    enddo ! LoopInd

    ! deallocate local arrays to avoid memory leaks
    deallocate(SoilIceTmp)

    end associate

  end subroutine SoilThermalProperty

end module SoilThermalPropertyMod
