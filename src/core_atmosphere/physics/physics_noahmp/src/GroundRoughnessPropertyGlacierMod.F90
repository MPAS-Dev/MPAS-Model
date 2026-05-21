module GroundRoughnessPropertyGlacierMod

!!! Compute glacier ground roughness length, displacement height, and surface reference height

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine GroundRoughnessPropertyGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: None (embedded in ENERGY_GLACIER subroutine)
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type), intent(inout) :: noahmp

! --------------------------------------------------------------------
    associate(                                                             &
              RefHeightAboveSfc => noahmp%config%domain%RefHeightAboveSfc ,& ! in,  reference height [m] above surface zero plane
              SnowDepth         => noahmp%water%state%SnowDepth           ,& ! in,  snow depth [m]
              RoughLenMomSnow   => noahmp%energy%param%RoughLenMomSnow    ,& ! in,  snow surface roughness length [m]
              RoughLenMomSfc    => noahmp%energy%state%RoughLenMomSfc     ,& ! out, roughness length [m], momentum, surface
              RoughLenMomGrd    => noahmp%energy%state%RoughLenMomGrd     ,& ! out, roughness length [m], momentum, ground
              ZeroPlaneDispSfc  => noahmp%energy%state%ZeroPlaneDispSfc   ,& ! out, surface zero plane displacement [m]
              ZeroPlaneDispGrd  => noahmp%energy%state%ZeroPlaneDispGrd   ,& ! out, ground zero plane displacement [m]
              RefHeightAboveGrd => noahmp%energy%state%RefHeightAboveGrd   & ! out, reference height [m] above ground
             )
! ----------------------------------------------------------------------

    ! ground roughness length
    RoughLenMomGrd    = RoughLenMomSnow
    RoughLenMomSfc    = RoughLenMomGrd

    ! surface roughness length and displacement height
    ZeroPlaneDispGrd  = SnowDepth
    ZeroPlaneDispSfc  = ZeroPlaneDispGrd

    ! reference height above ground
    RefHeightAboveGrd = ZeroPlaneDispSfc + RefHeightAboveSfc

    end associate

  end subroutine GroundRoughnessPropertyGlacier

end module GroundRoughnessPropertyGlacierMod
