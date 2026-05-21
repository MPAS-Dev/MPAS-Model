module RunoffSubSurfaceDrainageMod

!!! Calculate subsurface runoff using derived soil water drainage rate

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine RunoffSubSurfaceDrainage(noahmp)

! ------------------------ Code history --------------------------------------------------
! Originally embeded in WATER subroutine instead of as a separate subroutine
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! --------------------------------------------------------------------
    associate(                                                        &
              DrainSoilBot     => noahmp%water%flux%DrainSoilBot     ,& ! in,    soil bottom drainage [mm/s]
              RunoffSubsurface => noahmp%water%flux%RunoffSubsurface  & ! inout, subsurface runoff [mm/s] 
             )
! ----------------------------------------------------------------------

    ! compuate subsurface runoff mm/s
    RunoffSubsurface = RunoffSubsurface + DrainSoilBot

    end associate

  end subroutine RunoffSubSurfaceDrainage

end module RunoffSubSurfaceDrainageMod
