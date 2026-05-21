module SurfaceEmissivityGlacierMod

!!! Compute glacier surface longwave emissivity

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SurfaceEmissivityGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: None (embedded in ENERGY_GLACIER subroutine)
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type), intent(inout) :: noahmp

! --------------------------------------------------------------------
    associate(                                                          &
              EmissivitySnow   => noahmp%energy%param%EmissivitySnow   ,& ! in,  snow emissivity
              EmissivityIceSfc => noahmp%energy%param%EmissivityIceSfc ,& ! in,  emissivity ice surface
              SnowCoverFrac    => noahmp%water%state%SnowCoverFrac     ,& ! in,  snow cover fraction
              EmissivityGrd    => noahmp%energy%state%EmissivityGrd    ,& ! out, ground emissivity
              EmissivitySfc    => noahmp%energy%state%EmissivitySfc     & ! out, surface emissivity
             )
! ----------------------------------------------------------------------

    ! ground emissivity
    EmissivityGrd = EmissivityIceSfc * (1.0 - SnowCoverFrac) + EmissivitySnow * SnowCoverFrac

    ! surface emissivity
    EmissivitySfc = EmissivityGrd

    end associate

  end subroutine SurfaceEmissivityGlacier

end module SurfaceEmissivityGlacierMod
