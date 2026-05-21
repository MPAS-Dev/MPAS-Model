module SurfaceRadiationMod

!!! Compute surface (ground and vegetation) radiative fluxes (absorption and reflection)

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SurfaceRadiation(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: SURRAD
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                          :: IndBand                          ! waveband indices (1=vis, 2=nir)
    real(kind=kind_noahmp)           :: MinThr                           ! prevents overflow for division by zero
    real(kind=kind_noahmp)           :: RadSwAbsGrdTmp                   ! ground absorbed solar radiation [W/m2]
    real(kind=kind_noahmp)           :: RadSwReflSfcNir                  ! surface reflected solar radiation NIR [W/m2]
    real(kind=kind_noahmp)           :: RadSwReflSfcVis                  ! surface reflected solar radiation VIS [W/m2]
    real(kind=kind_noahmp)           :: LeafAreaIndFrac                  ! leaf area fraction of canopy
    real(kind=kind_noahmp)           :: RadSwTranGrdDir                  ! transmitted solar radiation at ground: direct [W/m2]
    real(kind=kind_noahmp)           :: RadSwTranGrdDif                  ! transmitted solar radiation at ground: diffuse [W/m2]
    real(kind=kind_noahmp), allocatable, dimension(:) :: RadSwAbsCanDir  ! direct beam absorbed by canopy [W/m2]
    real(kind=kind_noahmp), allocatable, dimension(:) :: RadSwAbsCanDif  ! diffuse radiation absorbed by canopy [W/m2]

! --------------------------------------------------------------------
    associate(                                                                 &
              NumSwRadBand         => noahmp%config%domain%NumSwRadBand       ,& ! in,  number of solar radiation wave bands
              LeafAreaIndEff       => noahmp%energy%state%LeafAreaIndEff      ,& ! in,  leaf area index, after burying by snow
              VegAreaIndEff        => noahmp%energy%state%VegAreaIndEff       ,& ! in,  one-sided leaf+stem area index [m2/m2]
              CanopySunlitFrac     => noahmp%energy%state%CanopySunlitFrac    ,& ! in,  sunlit fraction of canopy
              CanopyShadeFrac      => noahmp%energy%state%CanopyShadeFrac     ,& ! in,  shaded fraction of canopy
              LeafAreaIndSunlit    => noahmp%energy%state%LeafAreaIndSunlit   ,& ! in,  sunlit leaf area
              LeafAreaIndShade     => noahmp%energy%state%LeafAreaIndShade    ,& ! in,  shaded leaf area
              RadSwDownDir         => noahmp%energy%flux%RadSwDownDir         ,& ! in,  incoming direct solar radiation [W/m2]
              RadSwDownDif         => noahmp%energy%flux%RadSwDownDif         ,& ! in,  incoming diffuse solar radiation [W/m2]
              RadSwAbsVegDir       => noahmp%energy%flux%RadSwAbsVegDir       ,& ! in,  flux abs by veg (per unit direct flux)
              RadSwAbsVegDif       => noahmp%energy%flux%RadSwAbsVegDif       ,& ! in,  flux abs by veg (per unit diffuse flux)
              RadSwDirTranGrdDir   => noahmp%energy%flux%RadSwDirTranGrdDir   ,& ! in,  down direct flux below veg (per unit dir flux)
              RadSwDifTranGrdDir   => noahmp%energy%flux%RadSwDifTranGrdDir   ,& ! in,  down diffuse flux below veg (per unit dir flux)
              RadSwDifTranGrdDif   => noahmp%energy%flux%RadSwDifTranGrdDif   ,& ! in,  down diffuse flux below veg (per unit dif flux)
              AlbedoGrdDir         => noahmp%energy%state%AlbedoGrdDir        ,& ! in,  ground albedo (direct beam: vis, nir)
              AlbedoGrdDif         => noahmp%energy%state%AlbedoGrdDif        ,& ! in,  ground albedo (diffuse: vis, nir)
              AlbedoSfcDir         => noahmp%energy%state%AlbedoSfcDir        ,& ! in,  surface albedo (direct)
              AlbedoSfcDif         => noahmp%energy%state%AlbedoSfcDif        ,& ! in,  surface albedo (diffuse)
              RadSwReflVegDir      => noahmp%energy%flux%RadSwReflVegDir      ,& ! in,  flux reflected by veg layer (per unit direct flux)
              RadSwReflVegDif      => noahmp%energy%flux%RadSwReflVegDif      ,& ! in,  flux reflected by veg layer (per unit diffuse flux)
              RadSwReflGrdDir      => noahmp%energy%flux%RadSwReflGrdDir      ,& ! in,  flux reflected by ground (per unit direct flux)
              RadSwReflGrdDif      => noahmp%energy%flux%RadSwReflGrdDif      ,& ! in,  flux reflected by ground (per unit diffuse flux)
              RadPhotoActAbsSunlit => noahmp%energy%flux%RadPhotoActAbsSunlit ,& ! out, average absorbed par for sunlit leaves [W/m2]
              RadPhotoActAbsShade  => noahmp%energy%flux%RadPhotoActAbsShade  ,& ! out, average absorbed par for shaded leaves [W/m2]
              RadSwAbsVeg          => noahmp%energy%flux%RadSwAbsVeg          ,& ! out, solar radiation absorbed by vegetation [W/m2]
              RadSwAbsGrd          => noahmp%energy%flux%RadSwAbsGrd          ,& ! out, solar radiation absorbed by ground [W/m2]
              RadSwAbsSfc          => noahmp%energy%flux%RadSwAbsSfc          ,& ! out, total absorbed solar radiation [W/m2]
              RadSwReflSfc         => noahmp%energy%flux%RadSwReflSfc         ,& ! out, total reflected solar radiation [W/m2]
              RadSwReflVeg         => noahmp%energy%flux%RadSwReflVeg         ,& ! out, reflected solar radiation by vegetation [W/m2]
              RadSwReflGrd         => noahmp%energy%flux%RadSwReflGrd          & ! out, reflected solar radiation by ground [W/m2]
             )
! ----------------------------------------------------------------------

    ! initialization
    if (.not. allocated(RadSwAbsCanDir)) allocate(RadSwAbsCanDir(1:NumSwRadBand))
    if (.not. allocated(RadSwAbsCanDif)) allocate(RadSwAbsCanDif(1:NumSwRadBand))
    MinThr               = 1.0e-6
    RadSwAbsCanDir       = 0.0
    RadSwAbsCanDif       = 0.0
    RadSwAbsGrd          = 0.0
    RadSwAbsVeg          = 0.0
    RadSwAbsSfc          = 0.0
    RadSwReflSfc         = 0.0
    RadSwReflVeg         = 0.0
    RadSwReflGrd         = 0.0
    RadPhotoActAbsSunlit = 0.0
    RadPhotoActAbsShade  = 0.0

    do IndBand = 1, NumSwRadBand
       ! absorbed by canopy
       RadSwAbsCanDir(IndBand) = RadSwDownDir(IndBand) * RadSwAbsVegDir(IndBand)
       RadSwAbsCanDif(IndBand) = RadSwDownDif(IndBand) * RadSwAbsVegDif(IndBand)
       RadSwAbsVeg             = RadSwAbsVeg + RadSwAbsCanDir(IndBand) + RadSwAbsCanDif(IndBand)
       RadSwAbsSfc             = RadSwAbsSfc + RadSwAbsCanDir(IndBand) + RadSwAbsCanDif(IndBand)
       ! transmitted solar fluxes incident on ground
       RadSwTranGrdDir         = RadSwDownDir(IndBand) * RadSwDirTranGrdDir(IndBand)
       RadSwTranGrdDif         = RadSwDownDir(IndBand) * RadSwDifTranGrdDir(IndBand) + &
                                 RadSwDownDif(IndBand) * RadSwDifTranGrdDif(IndBand)
       ! solar radiation absorbed by ground surface
       RadSwAbsGrdTmp          = RadSwTranGrdDir * (1.0 - AlbedoGrdDir(IndBand)) + &
                                 RadSwTranGrdDif * (1.0 - AlbedoGrdDif(IndBand))
       RadSwAbsGrd             = RadSwAbsGrd + RadSwAbsGrdTmp
       RadSwAbsSfc             = RadSwAbsSfc + RadSwAbsGrdTmp
    enddo

    ! partition visible canopy absorption to sunlit and shaded fractions
    ! to get average absorbed par for sunlit and shaded leaves
    LeafAreaIndFrac = LeafAreaIndEff / max(VegAreaIndEff, MinThr)
    if ( CanopySunlitFrac > 0.0 ) then
       RadPhotoActAbsSunlit = (RadSwAbsCanDir(1) + CanopySunlitFrac * RadSwAbsCanDif(1)) * &
                              LeafAreaIndFrac / max(LeafAreaIndSunlit, MinThr)
       RadPhotoActAbsShade  = (CanopyShadeFrac * RadSwAbsCanDif(1)) * &
                              LeafAreaIndFrac / max(LeafAreaIndShade, MinThr)
    else
       RadPhotoActAbsSunlit = 0.0
       RadPhotoActAbsShade  = (RadSwAbsCanDir(1) + RadSwAbsCanDif(1)) * &
                              LeafAreaIndFrac / max(LeafAreaIndShade, MinThr)
    endif

    ! reflected solar radiation
    RadSwReflSfcVis = AlbedoSfcDir(1) * RadSwDownDir(1) + AlbedoSfcDif(1) * RadSwDownDif(1)
    RadSwReflSfcNir = AlbedoSfcDir(2) * RadSwDownDir(2) + AlbedoSfcDif(2) * RadSwDownDif(2)
    RadSwReflSfc    = RadSwReflSfcVis + RadSwReflSfcNir

    ! reflected solar radiation of veg. and ground (combined ground)
    RadSwReflVeg = RadSwReflVegDir(1)*RadSwDownDir(1) + RadSwReflVegDif(1)*RadSwDownDif(1) + &
                   RadSwReflVegDir(2)*RadSwDownDir(2) + RadSwReflVegDif(2)*RadSwDownDif(2)
    RadSwReflGrd = RadSwReflGrdDir(1)*RadSwDownDir(1) + RadSwReflGrdDif(1)*RadSwDownDif(1) + &
                   RadSwReflGrdDir(2)*RadSwDownDir(2) + RadSwReflGrdDif(2)*RadSwDownDif(2)

    ! deallocate local arrays to avoid memory leaks
    deallocate(RadSwAbsCanDir)
    deallocate(RadSwAbsCanDif)

    end associate

  end subroutine SurfaceRadiation

end module SurfaceRadiationMod
