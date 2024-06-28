module SnowCoverGlacierMod

!!! Compute glacier ground snow cover fraction

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SnowCoverGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: None (embedded in RADIATION_GLACIER subroutine)
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable

! --------------------------------------------------------------------
    associate(                                                     &
              SnowWaterEquiv => noahmp%water%state%SnowWaterEquiv ,& ! in,  snow water equivalent [mm]
              SnowCoverFrac  => noahmp%water%state%SnowCoverFrac   & ! out, snow cover fraction
             )
! ----------------------------------------------------------------------

    SnowCoverFrac = 0.0
    if ( SnowWaterEquiv > 0.0 ) SnowCoverFrac = 1.0

    end associate

  end subroutine SnowCoverGlacier

end module SnowCoverGlacierMod
