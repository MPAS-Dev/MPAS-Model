module SnowAlbedoClassMod

!!! Compute snow albedo based on the CLASS scheme (Verseghy, 1991)

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SnowAlbedoClass(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: SNOWALB_CLASS
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    real(kind=kind_noahmp)           :: SnowAlbedoTmp         ! temporary snow albedo

! --------------------------------------------------------------------
    associate(                                                                 &
              NumSwRadBand         => noahmp%config%domain%NumSwRadBand       ,& ! in,  number of solar radiation wave bands
              MainTimeStep         => noahmp%config%domain%MainTimeStep       ,& ! in,  noahmp main time step [s]
              SnowfallGround       => noahmp%water%flux%SnowfallGround        ,& ! in,  snowfall at ground [mm/s]
              SnowMassFullCoverOld => noahmp%water%param%SnowMassFullCoverOld ,& ! in,  new snow mass to fully cover old snow [mm]
              SnowAlbRefClass      => noahmp%energy%param%SnowAlbRefClass     ,& ! in,  reference snow albedo in CLASS scheme
              SnowAgeFacClass      => noahmp%energy%param%SnowAgeFacClass     ,& ! in,  snow aging e-folding time [s]
              SnowAlbFreshClass    => noahmp%energy%param%SnowAlbFreshClass   ,& ! in,  fresh snow albedo
              AlbedoSnowPrev       => noahmp%energy%state%AlbedoSnowPrev      ,& ! in,  snow albedo at last time step
              AlbedoSnowDir        => noahmp%energy%state%AlbedoSnowDir       ,& ! out, snow albedo for direct (1=vis, 2=nir)
              AlbedoSnowDif        => noahmp%energy%state%AlbedoSnowDif        & ! out, snow albedo for diffuse (1=vis, 2=nir)
             )
! ----------------------------------------------------------------------

    ! initialization
    AlbedoSnowDir(1:NumSwRadBand) = 0.0
    AlbedoSnowDif(1:NumSwRadBand) = 0.0

    ! when CosSolarZenithAngle > 0
    SnowAlbedoTmp = SnowAlbRefClass + (AlbedoSnowPrev-SnowAlbRefClass) * exp(-0.01*MainTimeStep/SnowAgeFacClass)

    ! 1 mm fresh snow(SWE) -- 10mm snow depth, assumed the fresh snow density 100kg/m3
    ! here assume 1cm snow depth will fully cover the old snow
    if ( SnowfallGround > 0.0 ) then
       SnowAlbedoTmp = SnowAlbedoTmp + min(SnowfallGround, SnowMassFullCoverOld/MainTimeStep) * &
                                       (SnowAlbFreshClass-SnowAlbedoTmp) / (SnowMassFullCoverOld/MainTimeStep)
    endif

    AlbedoSnowDif(1) = SnowAlbedoTmp
    AlbedoSnowDif(2) = SnowAlbedoTmp
    AlbedoSnowDir(1) = SnowAlbedoTmp
    AlbedoSnowDir(2) = SnowAlbedoTmp

    AlbedoSnowPrev   = SnowAlbedoTmp

    end associate

  end subroutine SnowAlbedoClass

end module SnowAlbedoClassMod
