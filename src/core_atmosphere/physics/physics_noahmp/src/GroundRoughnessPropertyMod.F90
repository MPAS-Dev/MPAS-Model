module GroundRoughnessPropertyMod

!!! Compute ground roughness length, displacement height, and surface reference height

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine GroundRoughnessProperty(noahmp, FlagVegSfc)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: None (embedded in ENERGY subroutine)
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type), intent(inout) :: noahmp
    logical          , intent(in   ) :: FlagVegSfc          ! flag: true if vegetated surface

! --------------------------------------------------------------------
    associate(                                                             &
              SurfaceType       => noahmp%config%domain%SurfaceType       ,& ! in,  surface type 1-soil; 2-lake
              RefHeightAboveSfc => noahmp%config%domain%RefHeightAboveSfc ,& ! in,  reference height [m] above surface zero plane
              FlagUrban         => noahmp%config%domain%FlagUrban         ,& ! in,  logical flag for urban grid
              SnowCoverFrac     => noahmp%water%state%SnowCoverFrac       ,& ! in,  snow cover fraction
              SnowDepth         => noahmp%water%state%SnowDepth           ,& ! in,  snow depth [m]
              HeightCanopyTop   => noahmp%energy%param%HeightCanopyTop    ,& ! in,  top of canopy [m]
              RoughLenMomVeg    => noahmp%energy%param%RoughLenMomVeg     ,& ! in,  momentum roughness length vegetated [m]
              RoughLenMomSnow   => noahmp%energy%param%RoughLenMomSnow    ,& ! in,  snow surface roughness length [m]
              RoughLenMomSoil   => noahmp%energy%param%RoughLenMomSoil    ,& ! in,  bare-soil roughness length [m]
              RoughLenMomLake   => noahmp%energy%param%RoughLenMomLake    ,& ! in,  lake surface roughness length [m]
              TemperatureGrd    => noahmp%energy%state%TemperatureGrd     ,& ! in,  ground temperature [K]
              RoughLenMomSfc    => noahmp%energy%state%RoughLenMomSfc     ,& ! out, roughness length [m], momentum, surface
              RoughLenMomGrd    => noahmp%energy%state%RoughLenMomGrd     ,& ! out, roughness length [m], momentum, ground
              ZeroPlaneDispSfc  => noahmp%energy%state%ZeroPlaneDispSfc   ,& ! out, surface zero plane displacement [m]
              ZeroPlaneDispGrd  => noahmp%energy%state%ZeroPlaneDispGrd   ,& ! out, ground zero plane displacement [m]
              RefHeightAboveGrd => noahmp%energy%state%RefHeightAboveGrd   & ! out, reference height [m] above ground
             )
! ----------------------------------------------------------------------

    ! ground roughness length
    if ( SurfaceType == 2 ) then ! Lake 
       if ( TemperatureGrd <= ConstFreezePoint ) then
          RoughLenMomGrd = RoughLenMomLake * (1.0-SnowCoverFrac) + SnowCoverFrac * RoughLenMomSnow
       else
          RoughLenMomGrd = RoughLenMomLake
       endif
    else                         ! soil
       RoughLenMomGrd    = RoughLenMomSoil * (1.0-SnowCoverFrac) + SnowCoverFrac * RoughLenMomSnow
    endif

    ! surface roughness length and displacement height
    ZeroPlaneDispGrd     = SnowDepth
    if ( FlagVegSfc .eqv. .true. ) then
       RoughLenMomSfc    = RoughLenMomVeg
       ZeroPlaneDispSfc  = 0.65 * HeightCanopyTop
       if ( SnowDepth > ZeroPlaneDispSfc ) ZeroPlaneDispSfc = SnowDepth
    else
       RoughLenMomSfc    = RoughLenMomGrd
       ZeroPlaneDispSfc  = ZeroPlaneDispGrd
    endif

    ! special case for urban
    if ( FlagUrban .eqv. .true. ) then
       RoughLenMomGrd    = RoughLenMomVeg
       ZeroPlaneDispGrd  = 0.65 * HeightCanopyTop
       RoughLenMomSfc    = RoughLenMomGrd
       ZeroPlaneDispSfc  = ZeroPlaneDispGrd
    endif

    ! reference height above ground
    RefHeightAboveGrd    = max(ZeroPlaneDispSfc, HeightCanopyTop) + RefHeightAboveSfc
    if ( ZeroPlaneDispGrd >= RefHeightAboveGrd ) RefHeightAboveGrd = ZeroPlaneDispGrd + RefHeightAboveSfc

    end associate

  end subroutine GroundRoughnessProperty

end module GroundRoughnessPropertyMod
