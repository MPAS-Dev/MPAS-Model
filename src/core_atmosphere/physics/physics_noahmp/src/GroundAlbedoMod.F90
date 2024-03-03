module GroundAlbedoMod

!!! Compute ground albedo based on soil and snow albedo

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine GroundAlbedo(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: GROUNDALB
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                          :: IndSwBnd             ! solar radiation band index
    real(kind=kind_noahmp)           :: AlbedoSoilAdjWet     ! soil water correction factor for soil albedo

! --------------------------------------------------------------------
    associate(                                                                 &
              NumSwRadBand        => noahmp%config%domain%NumSwRadBand        ,& ! in,  number of solar radiation wave bands
              SurfaceType         => noahmp%config%domain%SurfaceType         ,& ! in,  surface type 1-soil; 2-lake
              CosSolarZenithAngle => noahmp%config%domain%CosSolarZenithAngle ,& ! in,  cosine solar zenith angle
              SnowCoverFrac       => noahmp%water%state%SnowCoverFrac         ,& ! in,  snow cover fraction
              SoilMoisture        => noahmp%water%state%SoilMoisture          ,& ! in,  total soil moisture [m3/m3]
              AlbedoSoilSat       => noahmp%energy%param%AlbedoSoilSat        ,& ! in,  saturated soil albedos: 1=vis, 2=nir
              AlbedoSoilDry       => noahmp%energy%param%AlbedoSoilDry        ,& ! in,  dry soil albedos: 1=vis, 2=nir
              AlbedoLakeFrz       => noahmp%energy%param%AlbedoLakeFrz        ,& ! in,  albedo frozen lakes: 1=vis, 2=nir
              TemperatureGrd      => noahmp%energy%state%TemperatureGrd       ,& ! in,  ground temperature [K]
              AlbedoSnowDir       => noahmp%energy%state%AlbedoSnowDir        ,& ! in,  snow albedo for direct(1=vis, 2=nir)
              AlbedoSnowDif       => noahmp%energy%state%AlbedoSnowDif        ,& ! in,  snow albedo for diffuse(1=vis, 2=nir)
              AlbedoSoilDir       => noahmp%energy%state%AlbedoSoilDir        ,& ! out, soil albedo (direct)
              AlbedoSoilDif       => noahmp%energy%state%AlbedoSoilDif        ,& ! out, soil albedo (diffuse)
              AlbedoGrdDir        => noahmp%energy%state%AlbedoGrdDir         ,& ! out, ground albedo (direct beam: vis, nir)
              AlbedoGrdDif        => noahmp%energy%state%AlbedoGrdDif          & ! out, ground albedo (diffuse: vis, nir)
             )
! ----------------------------------------------------------------------

    do IndSwBnd = 1, NumSwRadBand

       AlbedoSoilAdjWet = max(0.11-0.40*SoilMoisture(1), 0.0)

       if ( SurfaceType == 1 )  then                      ! soil
          AlbedoSoilDir(IndSwBnd) = min(AlbedoSoilSat(IndSwBnd)+AlbedoSoilAdjWet, AlbedoSoilDry(IndSwBnd))
          AlbedoSoilDif(IndSwBnd) = AlbedoSoilDir(IndSwBnd)
       elseif ( TemperatureGrd > ConstFreezePoint ) then  ! unfrozen lake, wetland
          AlbedoSoilDir(IndSwBnd) = 0.06 / (max(0.01, CosSolarZenithAngle)**1.7+0.15)
          AlbedoSoilDif(IndSwBnd) = 0.06
       else                                               ! frozen lake, wetland
          AlbedoSoilDir(IndSwBnd) = AlbedoLakeFrz(IndSwBnd)
          AlbedoSoilDif(IndSwBnd) = AlbedoSoilDir(IndSwBnd)
       endif

       AlbedoGrdDir(IndSwBnd) = AlbedoSoilDir(IndSwBnd)*(1.0-SnowCoverFrac) + AlbedoSnowDir(IndSwBnd)*SnowCoverFrac
       AlbedoGrdDif(IndSwBnd) = AlbedoSoilDif(IndSwBnd)*(1.0-SnowCoverFrac) + AlbedoSnowDif(IndSwBnd)*SnowCoverFrac

    enddo

    end associate

  end subroutine GroundAlbedo

end module GroundAlbedoMod
