module GroundAlbedoGlacierMod

!!! Compute glacier ground albedo based on snow and ice albedo

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine GroundAlbedoGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: RADIATION_GLACIER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                          :: IndSwBnd      ! solar radiation band index

! --------------------------------------------------------------------
    associate(                                                    &
              NumSwRadBand  => noahmp%config%domain%NumSwRadBand ,& ! in,  number of solar radiation wave bands
              SnowCoverFrac => noahmp%water%state%SnowCoverFrac  ,& ! in,  snow cover fraction
              AlbedoLandIce => noahmp%energy%param%AlbedoLandIce ,& ! in,  albedo land ice: 1=vis, 2=nir
              AlbedoSnowDir => noahmp%energy%state%AlbedoSnowDir ,& ! in,  snow albedo for direct(1=vis, 2=nir)
              AlbedoSnowDif => noahmp%energy%state%AlbedoSnowDif ,& ! in,  snow albedo for diffuse(1=vis, 2=nir)
              AlbedoGrdDir  => noahmp%energy%state%AlbedoGrdDir  ,& ! out, ground albedo (direct beam: vis, nir)
              AlbedoGrdDif  => noahmp%energy%state%AlbedoGrdDif   & ! out, ground albedo (diffuse: vis, nir)
             )
! ----------------------------------------------------------------------

    do IndSwBnd = 1, NumSwRadBand

       AlbedoGrdDir(IndSwBnd) = AlbedoLandIce(IndSwBnd)*(1.0-SnowCoverFrac) + AlbedoSnowDir(IndSwBnd)*SnowCoverFrac
       AlbedoGrdDif(IndSwBnd) = AlbedoLandIce(IndSwBnd)*(1.0-SnowCoverFrac) + AlbedoSnowDif(IndSwBnd)*SnowCoverFrac

    enddo

    end associate

  end subroutine GroundAlbedoGlacier

end module GroundAlbedoGlacierMod
