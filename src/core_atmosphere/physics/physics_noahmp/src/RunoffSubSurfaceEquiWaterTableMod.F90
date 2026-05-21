module RunoffSubSurfaceEquiWaterTableMod

!!! Calculate subsurface runoff using equilibrium water table depth (Niu et al., 2005)

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use WaterTableEquilibriumMod, only : WaterTableEquilibrium

  implicit none

contains

  subroutine RunoffSubSurfaceEquiWaterTable(noahmp)

! ------------------------ Code history --------------------------------------------------
! Originally embeded in SOILWATER subroutine instead of as a separate subroutine
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! --------------------------------------------------------------------
    associate(                                                           &
              SoilImpervFracMax => noahmp%water%state%SoilImpervFracMax ,& ! in,    maximum soil imperviousness fraction
              GridTopoIndex     => noahmp%water%param%GridTopoIndex     ,& ! in,    gridcell mean topgraphic index (global mean)
              RunoffDecayFac    => noahmp%water%param%RunoffDecayFac    ,& ! in,    runoff decay factor [m-1]
              BaseflowCoeff     => noahmp%water%param%BaseflowCoeff     ,& ! inout, baseflow coefficient [mm/s]
              WaterTableDepth   => noahmp%water%state%WaterTableDepth   ,& ! out,   water table depth [m]
              RunoffSubsurface  => noahmp%water%flux%RunoffSubsurface    & ! out,   subsurface runoff [mm/s] 
             )
! ----------------------------------------------------------------------

    ! set parameter values specific for this scheme
    RunoffDecayFac = 2.0
    BaseflowCoeff  = 4.0

    ! compute equilibrium water table depth
    call WaterTableEquilibrium(noahmp)

    ! compuate subsurface runoff mm/s
    RunoffSubsurface = (1.0 - SoilImpervFracMax) * BaseflowCoeff * &
                       exp(-GridTopoIndex) * exp(-RunoffDecayFac * WaterTableDepth)

    end associate

  end subroutine RunoffSubSurfaceEquiWaterTable

end module RunoffSubSurfaceEquiWaterTableMod
