module SnowpackCompactionMod

!!! Snowpack compaction process
!!! Update snow depth via compaction due to destructive metamorphism, overburden, & melt

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SnowpackCompaction(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: COMPACT
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                          :: LoopInd                ! snow layer loop index
    real(kind=kind_noahmp)           :: SnowBurden             ! pressure of overlying snow [kg/m2]
    real(kind=kind_noahmp)           :: SnowCompactAgeExpFac   ! EXPF=exp(-c4*(273.15-TemperatureSoilSnow))
    real(kind=kind_noahmp)           :: TempDiff               ! ConstFreezePoint - TemperatureSoilSnow[K]
    real(kind=kind_noahmp)           :: SnowVoid               ! void (1 - SnowIce - SnowLiqWater)
    real(kind=kind_noahmp)           :: SnowWatTotTmp          ! water mass (ice + liquid) [kg/m2]
    real(kind=kind_noahmp)           :: SnowIceDens            ! partial density of ice [kg/m3]

! --------------------------------------------------------------------
    associate(                                                                       &
              MainTimeStep           => noahmp%config%domain%MainTimeStep           ,& ! in,    noahmp main time step [s]
              TemperatureSoilSnow    => noahmp%energy%state%TemperatureSoilSnow     ,& ! in,    snow and soil layer temperature [K]
              SnowIce                => noahmp%water%state%SnowIce                  ,& ! in,    snow layer ice [mm]
              SnowLiqWater           => noahmp%water%state%SnowLiqWater             ,& ! in,    snow layer liquid water [mm]
              IndexPhaseChange       => noahmp%water%state%IndexPhaseChange         ,& ! in,    phase change index [0-none;1-melt;2-refreeze]
              SnowIceFracPrev        => noahmp%water%state%SnowIceFracPrev          ,& ! in,    ice fraction in snow layers at previous timestep
              SnowCompactBurdenFac   => noahmp%water%param%SnowCompactBurdenFac     ,& ! in,    snow overburden compaction parameter [m3/kg]
              SnowCompactAgingFac1   => noahmp%water%param%SnowCompactAgingFac1     ,& ! in,    snow desctructive metamorphism compaction factor1 [1/s]
              SnowCompactAgingFac2   => noahmp%water%param%SnowCompactAgingFac2     ,& ! in,    snow desctructive metamorphism compaction factor2 [1/k]
              SnowCompactAgingFac3   => noahmp%water%param%SnowCompactAgingFac3     ,& ! in,    snow desctructive metamorphism compaction factor3 
              SnowCompactAgingMax    => noahmp%water%param%SnowCompactAgingMax      ,& ! in,    maximum destructive metamorphism compaction [kg/m3]
              SnowViscosityCoeff     => noahmp%water%param%SnowViscosityCoeff       ,& ! in,    snow viscosity coeff [kg s/m2],Anderson1979:0.52e6~1.38e6
              NumSnowLayerNeg        => noahmp%config%domain%NumSnowLayerNeg        ,& ! inout, actual number of snow layers (negative)
              ThicknessSnowSoilLayer => noahmp%config%domain%ThicknessSnowSoilLayer ,& ! inout, thickness of snow/soil layers [m]
              CompactionSnowAging    => noahmp%water%flux%CompactionSnowAging       ,& ! out,   rate of compaction due to destructive metamorphism [1/s]
              CompactionSnowBurden   => noahmp%water%flux%CompactionSnowBurden      ,& ! out,   rate of compaction of snowpack due to overburden [1/s]
              CompactionSnowMelt     => noahmp%water%flux%CompactionSnowMelt        ,& ! out,   rate of compaction of snowpack due to melt [1/s]
              CompactionSnowTot      => noahmp%water%flux%CompactionSnowTot         ,& ! out,   change in fractional-thickness due to compaction [1/s]
              SnowIceFrac            => noahmp%water%state%SnowIceFrac               & ! out,   fraction of ice in snow layers at current time step 
             )
! ----------------------------------------------------------------------

! initialization for out-only variables
    CompactionSnowAging(:)  = 0.0
    CompactionSnowBurden(:) = 0.0
    CompactionSnowMelt(:)   = 0.0
    CompactionSnowTot(:)    = 0.0
    SnowIceFrac(:)          = 0.0

! start snow compaction
    SnowBurden = 0.0
    do LoopInd = NumSnowLayerNeg+1, 0

       SnowWatTotTmp        = SnowIce(LoopInd) + SnowLiqWater(LoopInd)
       SnowIceFrac(LoopInd) = SnowIce(LoopInd) / SnowWatTotTmp
       SnowVoid             = 1.0 - (SnowIce(LoopInd)/ConstDensityIce + SnowLiqWater(LoopInd)/ConstDensityWater) / &
                                    ThicknessSnowSoilLayer(LoopInd)

       ! Allow compaction only for non-saturated node and higher ice lens node.
       if ( (SnowVoid > 0.001) .and. (SnowIce(LoopInd) > 0.1) ) then
          SnowIceDens = SnowIce(LoopInd) / ThicknessSnowSoilLayer(LoopInd)
          TempDiff    = max(0.0, ConstFreezePoint-TemperatureSoilSnow(LoopInd))

          ! Settling/compaction as a result of destructive metamorphism
          SnowCompactAgeExpFac         = exp(-SnowCompactAgingFac2 * TempDiff)
          CompactionSnowAging(LoopInd) = -SnowCompactAgingFac1 * SnowCompactAgeExpFac
          if ( SnowIceDens > SnowCompactAgingMax ) &
             CompactionSnowAging(LoopInd) = CompactionSnowAging(LoopInd) * exp(-46.0e-3*(SnowIceDens-SnowCompactAgingMax))
          if ( SnowLiqWater(LoopInd) > (0.01*ThicknessSnowSoilLayer(LoopInd)) ) &
             CompactionSnowAging(LoopInd) = CompactionSnowAging(LoopInd) * SnowCompactAgingFac3                ! Liquid water term

          ! Compaction due to overburden
          CompactionSnowBurden(LoopInd) = -(SnowBurden + 0.5*SnowWatTotTmp) * &
                                    exp(-0.08*TempDiff-SnowCompactBurdenFac*SnowIceDens) / SnowViscosityCoeff  ! 0.5*SnowWatTotTmp -> self-burden

          ! Compaction occurring during melt
          if ( IndexPhaseChange(LoopInd) == 1 ) then
             CompactionSnowMelt(LoopInd) = max(0.0, (SnowIceFracPrev(LoopInd)-SnowIceFrac(LoopInd)) / &
                                                    max(1.0e-6, SnowIceFracPrev(LoopInd)))
             CompactionSnowMelt(LoopInd) = -CompactionSnowMelt(LoopInd) / MainTimeStep   ! sometimes too large
          else
             CompactionSnowMelt(LoopInd) = 0.0
          endif

          ! Time rate of fractional change in snow thickness (units of s-1)
          CompactionSnowTot(LoopInd) = (CompactionSnowAging(LoopInd) + CompactionSnowBurden(LoopInd) + &
                                        CompactionSnowMelt(LoopInd) ) * MainTimeStep
          CompactionSnowTot(LoopInd) = max(-0.5, CompactionSnowTot(LoopInd))

          ! The change in DZ due to compaction
          ThicknessSnowSoilLayer(LoopInd) = ThicknessSnowSoilLayer(LoopInd) * (1.0 + CompactionSnowTot(LoopInd))
          ThicknessSnowSoilLayer(LoopInd) = max(ThicknessSnowSoilLayer(LoopInd), &
                                            SnowIce(LoopInd)/ConstDensityIce + SnowLiqWater(LoopInd)/ConstDensityWater)

          ! Constrain snow density to a reasonable range (50~500 kg/m3)
          ThicknessSnowSoilLayer(LoopInd) = min( max( ThicknessSnowSoilLayer(LoopInd),&
                                                     (SnowIce(LoopInd)+SnowLiqWater(LoopInd))/500.0 ), &
                                                (SnowIce(LoopInd)+SnowLiqWater(LoopInd))/50.0 )
       endif

       ! Pressure of overlying snow
       SnowBurden = SnowBurden + SnowWatTotTmp

    enddo

    end associate

  end subroutine SnowpackCompaction

end module SnowpackCompactionMod
