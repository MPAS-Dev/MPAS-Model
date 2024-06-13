module SnowAlbedoBatsMod

!!! Compute snow albedo based on BATS scheme (Yang et al. (1997) J.of Climate)

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SnowAlbedoBats(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: SNOWALB_BATS
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    real(kind=kind_noahmp)           :: ZenithAngFac        ! solar zenith angle correction factor
    real(kind=kind_noahmp)           :: ZenithAngFacTmp     ! temperary zenith angle correction factor
    real(kind=kind_noahmp)           :: SolarAngleFac2      ! 2.0 * SolarAngleFac
    real(kind=kind_noahmp)           :: SolarAngleFac1      ! 1 / SolarAngleFac
    real(kind=kind_noahmp)           :: SolarAngleFac       ! adjustable solar zenith angle factor

! --------------------------------------------------------------------
    associate(                                                                 &
              NumSwRadBand        => noahmp%config%domain%NumSwRadBand        ,& ! in,  number of solar radiation wave bands
              CosSolarZenithAngle => noahmp%config%domain%CosSolarZenithAngle ,& ! in,  cosine solar zenith angle
              SolarZenithAdjBats  => noahmp%energy%param%SolarZenithAdjBats   ,& ! in,  zenith angle snow albedo adjustment
              FreshSnoAlbVisBats  => noahmp%energy%param%FreshSnoAlbVisBats   ,& ! in,  new snow visible albedo
              FreshSnoAlbNirBats  => noahmp%energy%param%FreshSnoAlbNirBats   ,& ! in,  new snow NIR albedo
              SnoAgeFacDifVisBats => noahmp%energy%param%SnoAgeFacDifVisBats  ,& ! in,  age factor for diffuse visible snow albedo
              SnoAgeFacDifNirBats => noahmp%energy%param%SnoAgeFacDifNirBats  ,& ! in,  age factor for diffuse NIR snow albedo
              SzaFacDirVisBats    => noahmp%energy%param%SzaFacDirVisBats     ,& ! in,  cosz factor for direct visible snow albedo
              SzaFacDirNirBats    => noahmp%energy%param%SzaFacDirNirBats     ,& ! in,  cosz factor for direct NIR snow albedo
              SnowAgeFac          => noahmp%energy%state%SnowAgeFac           ,& ! in,  snow age factor
              AlbedoSnowDir       => noahmp%energy%state%AlbedoSnowDir        ,& ! out, snow albedo for direct(1=vis, 2=nir)
              AlbedoSnowDif       => noahmp%energy%state%AlbedoSnowDif         & ! out, snow albedo for diffuse(1=vis, 2=nir)
             )
! ----------------------------------------------------------------------

    ! initialization
    AlbedoSnowDir(1:NumSwRadBand) = 0.0
    AlbedoSnowDif(1:NumSwRadBand) = 0.0

    ! when CosSolarZenithAngle > 0
    SolarAngleFac    = SolarZenithAdjBats
    SolarAngleFac1   = 1.0 / SolarAngleFac
    SolarAngleFac2   = 2.0 * SolarAngleFac
    ZenithAngFacTmp  = (1.0 + SolarAngleFac1) / (1.0 + SolarAngleFac2*CosSolarZenithAngle) - SolarAngleFac1
    ZenithAngFac     = amax1(ZenithAngFacTmp, 0.0)
    AlbedoSnowDif(1) = FreshSnoAlbVisBats * (1.0 - SnoAgeFacDifVisBats * SnowAgeFac)
    AlbedoSnowDif(2) = FreshSnoAlbNirBats * (1.0 - SnoAgeFacDifNirBats * SnowAgeFac)
    AlbedoSnowDir(1) = AlbedoSnowDif(1) + SzaFacDirVisBats * ZenithAngFac * (1.0 - AlbedoSnowDif(1))
    AlbedoSnowDir(2) = AlbedoSnowDif(2) + SzaFacDirNirBats * ZenithAngFac * (1.0 - AlbedoSnowDif(2))

    end associate

  end subroutine SnowAlbedoBats

end module SnowAlbedoBatsMod
