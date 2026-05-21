module SurfaceEmissivityMod

!!! Compute ground, vegetation, and total surface longwave emissivity

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SurfaceEmissivity(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: None (embedded in ENERGY subroutine)
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type), intent(inout) :: noahmp

! --------------------------------------------------------------------
    associate(                                                              &
              IndicatorIceSfc    => noahmp%config%domain%IndicatorIceSfc   ,& ! in,  indicator for ice point: 1->seaice; -1->land ice; 0->soil
              SurfaceType        => noahmp%config%domain%SurfaceType       ,& ! in,  surface type 1-soil; 2-lake
              EmissivitySnow     => noahmp%energy%param%EmissivitySnow     ,& ! in,  snow emissivity
              EmissivitySoilLake => noahmp%energy%param%EmissivitySoilLake ,& ! in,  emissivity soil surface
              EmissivityIceSfc   => noahmp%energy%param%EmissivityIceSfc   ,& ! in,  emissivity ice surface
              SnowCoverFrac      => noahmp%water%state%SnowCoverFrac       ,& ! in,  snow cover fraction
              LeafAreaIndEff     => noahmp%energy%state%LeafAreaIndEff     ,& ! in,  leaf area index, after burying by snow
              StemAreaIndEff     => noahmp%energy%state%StemAreaIndEff     ,& ! in,  stem area index, after burying by snow
              VegFrac            => noahmp%energy%state%VegFrac            ,& ! in,  greeness vegetation fraction
              EmissivityVeg      => noahmp%energy%state%EmissivityVeg      ,& ! out, vegetation emissivity
              EmissivityGrd      => noahmp%energy%state%EmissivityGrd      ,& ! out, ground emissivity
              EmissivitySfc      => noahmp%energy%state%EmissivitySfc       & ! out, surface emissivity
             )
! ----------------------------------------------------------------------

    ! vegetation emissivity
    EmissivityVeg = 1.0 - exp(-(LeafAreaIndEff + StemAreaIndEff) / 1.0)

    ! ground emissivity
    if ( IndicatorIceSfc == 1 ) then
       EmissivityGrd = EmissivityIceSfc * (1.0-SnowCoverFrac) + EmissivitySnow * SnowCoverFrac
    else
       EmissivityGrd = EmissivitySoilLake(SurfaceType) * (1.0-SnowCoverFrac) + EmissivitySnow * SnowCoverFrac
    endif

    ! net surface emissivity
    EmissivitySfc = VegFrac * (EmissivityGrd*(1-EmissivityVeg) + EmissivityVeg + &
                    EmissivityVeg*(1-EmissivityVeg)*(1-EmissivityGrd)) + (1-VegFrac) * EmissivityGrd

    end associate

  end subroutine SurfaceEmissivity

end module SurfaceEmissivityMod
