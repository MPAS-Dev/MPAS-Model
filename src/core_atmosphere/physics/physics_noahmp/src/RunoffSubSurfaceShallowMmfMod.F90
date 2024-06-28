module RunoffSubSurfaceShallowMmfMod

!!! Calculate subsurface runoff based on MMF groundwater scheme

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use ShallowWaterTableMmfMod, only : ShallowWaterTableMMF

  implicit none

contains

  subroutine RunoffSubSurfaceShallowWaterMMF(noahmp)

! ------------------------ Code history --------------------------------------------------
! Originally embeded in WATER subroutine instead of as a separate subroutine
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! --------------------------------------------------------------------
    associate(                                                               &
              NumSoilLayer        => noahmp%config%domain%NumSoilLayer      ,& ! in,    number of soil layers
              SoilIce             => noahmp%water%state%SoilIce             ,& ! in,    soil ice content [m3/m3]
              DrainSoilBot        => noahmp%water%flux%DrainSoilBot         ,& ! in,    soil bottom drainage [mm/s]
              SoilLiqWater        => noahmp%water%state%SoilLiqWater        ,& ! inout, soil water content [m3/m3]
              SoilMoisture        => noahmp%water%state%SoilMoisture        ,& ! inout, total soil water content [m3/m3]
              WaterStorageAquifer => noahmp%water%state%WaterStorageAquifer ,& ! inout, water storage in aquifer [mm]
              RunoffSubsurface    => noahmp%water%flux%RunoffSubsurface      & ! out,   subsurface runoff [mm/s] 
             )
! ----------------------------------------------------------------------

    ! compute shallow water table and moisture
    call ShallowWaterTableMMF(noahmp)

    ! update moisture
    SoilLiqWater(NumSoilLayer) = SoilMoisture(NumSoilLayer) - SoilIce(NumSoilLayer)

    ! compute subsurface runoff
    RunoffSubsurface    = RunoffSubsurface + DrainSoilBot 
    WaterStorageAquifer = 0.0

    end associate

  end subroutine RunoffSubSurfaceShallowWaterMMF

end module RunoffSubSurfaceShallowMmfMod
