module SurfaceAlbedoGlacierMod

!!! Compute glacier surface albedo

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use SnowAgingBatsMod,       only : SnowAgingBats
  use SnowAlbedoBatsMod,      only : SnowAlbedoBats
  use SnowAlbedoClassMod,     only : SnowAlbedoClass
  use GroundAlbedoGlacierMod, only : GroundAlbedoGlacier

  implicit none

contains

  subroutine SurfaceAlbedoGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: RADIATION_GLACIER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                          :: IndBand       ! solar band index

! --------------------------------------------------------------------
    associate(                                                                 &
              NumSwRadBand        => noahmp%config%domain%NumSwRadBand        ,& ! in,  number of solar radiation wave bands
              CosSolarZenithAngle => noahmp%config%domain%CosSolarZenithAngle ,& ! in,  cosine solar zenith angle
              OptSnowAlbedo       => noahmp%config%nmlist%OptSnowAlbedo       ,& ! in,  options for ground snow surface albedo
              AlbedoGrdDir        => noahmp%energy%state%AlbedoGrdDir         ,& ! out, ground albedo (direct beam: vis, nir)
              AlbedoGrdDif        => noahmp%energy%state%AlbedoGrdDif         ,& ! out, ground albedo (diffuse: vis, nir)
              AlbedoSnowDir       => noahmp%energy%state%AlbedoSnowDir        ,& ! out, snow albedo for direct(1=vis, 2=nir)
              AlbedoSnowDif       => noahmp%energy%state%AlbedoSnowDif        ,& ! out, snow albedo for diffuse(1=vis, 2=nir)
              AlbedoSfcDir        => noahmp%energy%state%AlbedoSfcDir         ,& ! out, surface albedo (direct)
              AlbedoSfcDif        => noahmp%energy%state%AlbedoSfcDif          & ! out, surface albedo (diffuse)
             )
! ----------------------------------------------------------------------

    ! initialization
    do IndBand = 1, NumSwRadBand
       AlbedoSfcDir (IndBand) = 0.0
       AlbedoSfcDif (IndBand) = 0.0
       AlbedoGrdDir (IndBand) = 0.0
       AlbedoGrdDif (IndBand) = 0.0
       AlbedoSnowDir(IndBand) = 0.0
       AlbedoSnowDif(IndBand) = 0.0
    enddo

    ! solar radiation process is only done if there is light
    if ( CosSolarZenithAngle > 0 ) then

       ! snow aging
       call SnowAgingBats(noahmp)

       ! snow albedo
       if ( OptSnowAlbedo == 1 )  call SnowAlbedoBats(noahmp)
       if ( OptSnowAlbedo == 2 )  call SnowAlbedoClass(noahmp)

       ! ground albedo
       call GroundAlbedoGlacier(noahmp)

       ! surface albedo
       AlbedoSfcDir = AlbedoGrdDir
       AlbedoSfcDif = AlbedoGrdDif

    endif  ! CosSolarZenithAngle > 0

    end associate

  end subroutine SurfaceAlbedoGlacier

end module SurfaceAlbedoGlacierMod
