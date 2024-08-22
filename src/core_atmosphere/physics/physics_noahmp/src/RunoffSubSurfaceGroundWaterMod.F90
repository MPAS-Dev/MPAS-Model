module RunoffSubSurfaceGroundWaterMod

!!! Calculate subsurface runoff based on TOPMODEL with groundwater (Niu et al 2007)

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use GroundWaterTopModelMod, only :  GroundWaterTopModel

  implicit none

contains

  subroutine RunoffSubSurfaceGroundWater(noahmp)

! ------------------------ Code history --------------------------------------------------
! Originally embeded in WATER subroutine instead of as a separate subroutine
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! --------------------------------------------------------------------
    associate(                                                        &
              DischargeGw      => noahmp%water%flux%DischargeGw      ,& ! out, groundwater discharge [mm/s]
              RunoffSubsurface => noahmp%water%flux%RunoffSubsurface  & ! out, subsurface runoff [mm/s] 
             )
! ----------------------------------------------------------------------

    ! compute ground water
    call GroundWaterTopModel(noahmp)

    ! compute subsurface runoff as groundwater discharge
    RunoffSubsurface = DischargeGw

    end associate

  end subroutine RunoffSubSurfaceGroundWater

end module RunoffSubSurfaceGroundWaterMod
