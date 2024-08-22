module SurfaceRadiationGlacierMod

!!! Compute glacier surface radiative fluxes (absorption and reflection)

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SurfaceRadiationGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: RADIATION_GLACIER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                          :: IndBand             ! waveband indices (1=vis, 2=nir)
    real(kind=kind_noahmp)           :: RadSwAbsGrdTmp      ! ground absorbed solar radiation [W/m2]
    real(kind=kind_noahmp)           :: RadSwReflGrdTmp     ! ground reflected solar radiation [W/m2]

! -----------------------------------------------------------------
    associate(                                                   &
              NumSwRadBand => noahmp%config%domain%NumSwRadBand ,& ! in,  number of solar radiation wave bands
              RadSwDownDir => noahmp%energy%flux%RadSwDownDir   ,& ! in,  incoming direct solar radiation [W/m2]
              RadSwDownDif => noahmp%energy%flux%RadSwDownDif   ,& ! in,  incoming diffuse solar radiation [W/m2]
              AlbedoGrdDir => noahmp%energy%state%AlbedoGrdDir  ,& ! in,  ground albedo (direct beam: vis, nir)
              AlbedoGrdDif => noahmp%energy%state%AlbedoGrdDif  ,& ! in,  ground albedo (diffuse: vis, nir)
              RadSwAbsGrd  => noahmp%energy%flux%RadSwAbsGrd    ,& ! out, solar radiation absorbed by ground [W/m2]
              RadSwAbsSfc  => noahmp%energy%flux%RadSwAbsSfc    ,& ! out, total absorbed solar radiation [W/m2]
              RadSwReflSfc => noahmp%energy%flux%RadSwReflSfc    & ! out, total reflected solar radiation [W/m2]
             )
! ----------------------------------------------------------------------

    ! initialization
    RadSwAbsGrd  = 0.0
    RadSwAbsSfc  = 0.0
    RadSwReflSfc = 0.0

    do IndBand = 1, NumSwRadBand
       ! solar radiation absorbed by glacier surface
       RadSwAbsGrdTmp  = RadSwDownDir(IndBand) * (1.0 - AlbedoGrdDir(IndBand)) + &
                         RadSwDownDif(IndBand) * (1.0 - AlbedoGrdDif(IndBand))
       RadSwAbsGrd     = RadSwAbsGrd + RadSwAbsGrdTmp
       RadSwAbsSfc     = RadSwAbsSfc + RadSwAbsGrdTmp
      
       ! solar radiation reflected by glacier surface
       RadSwReflGrdTmp = RadSwDownDir(IndBand) * AlbedoGrdDir(IndBand) + &
                         RadSwDownDif(IndBand) * AlbedoGrdDif(IndBand)
       RadSwReflSfc    = RadSwReflSfc + RadSwReflGrdTmp
    enddo

    end associate

  end subroutine SurfaceRadiationGlacier

end module SurfaceRadiationGlacierMod
