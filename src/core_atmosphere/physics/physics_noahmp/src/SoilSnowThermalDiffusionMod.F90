module SoilSnowThermalDiffusionMod

!!! Solve soil and snow layer thermal diffusion
!!! Calculate the right hand side of the time tendency term of the soil
!!! and snow thermal diffusion equation. Currently snow and soil layers
!!! are coupled in solving the equations. Also compute/prepare the matrix
!!! coefficients for the tri-diagonal matrix of the implicit time scheme.

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SoilSnowThermalDiffusion(noahmp, MatLeft1, MatLeft2, MatLeft3, MatRight)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: HRT
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type)     , intent(inout) :: noahmp
    real(kind=kind_noahmp), allocatable, dimension(:), intent(inout) :: MatRight  ! right-hand side term of the matrix
    real(kind=kind_noahmp), allocatable, dimension(:), intent(inout) :: MatLeft1  ! left-hand side term of the matrix
    real(kind=kind_noahmp), allocatable, dimension(:), intent(inout) :: MatLeft2  ! left-hand side term of the matrix
    real(kind=kind_noahmp), allocatable, dimension(:), intent(inout) :: MatLeft3  ! left-hand side term of the matrix

! local variable
    integer                                           :: LoopInd                  ! loop index
    real(kind=kind_noahmp)                            :: DepthSnowSoilTmp         ! temporary snow/soil layer depth [m]
    real(kind=kind_noahmp), allocatable, dimension(:) :: DepthSnowSoilInv         ! inverse of snow/soil layer depth [1/m]
    real(kind=kind_noahmp), allocatable, dimension(:) :: HeatCapacPerArea         ! Heat capacity of soil/snow per area [J/m2/K]
    real(kind=kind_noahmp), allocatable, dimension(:) :: TempGradDepth            ! temperature gradient (derivative) with soil/snow depth [K/m]
    real(kind=kind_noahmp), allocatable, dimension(:) :: EnergyExcess             ! energy flux excess in soil/snow [W/m2]

! --------------------------------------------------------------------
    associate(                                                                           &
              NumSoilLayer             => noahmp%config%domain%NumSoilLayer             ,& ! in,  number of soil layers
              NumSnowLayerMax          => noahmp%config%domain%NumSnowLayerMax          ,& ! in,  maximum number of snow layers
              NumSnowLayerNeg          => noahmp%config%domain%NumSnowLayerNeg          ,& ! in,  actual number of snow layers (negative)
              DepthSnowSoilLayer       => noahmp%config%domain%DepthSnowSoilLayer       ,& ! in,  depth of snow/soil layer-bottom [m]
              OptSoilTemperatureBottom => noahmp%config%nmlist%OptSoilTemperatureBottom ,& ! in,  options for lower boundary condition of soil temp.
              OptSnowSoilTempTime      => noahmp%config%nmlist%OptSnowSoilTempTime      ,& ! in,  options for snow/soil temperature time scheme
              TemperatureSoilBottom    => noahmp%forcing%TemperatureSoilBottom          ,& ! in,  bottom boundary soil temperature [K]
              DepthSoilTempBotToSno    => noahmp%energy%state%DepthSoilTempBotToSno     ,& ! in,  depth of lower boundary condition [m] from snow surface
              TemperatureSoilSnow      => noahmp%energy%state%TemperatureSoilSnow       ,& ! in,  snow and soil layer temperature [K]
              ThermConductSoilSnow     => noahmp%energy%state%ThermConductSoilSnow      ,& ! in,  thermal conductivity [W/m/K] for all soil & snow
              HeatCapacSoilSnow        => noahmp%energy%state%HeatCapacSoilSnow         ,& ! in,  heat capacity [J/m3/K] for all soil & snow
              HeatGroundTotMean        => noahmp%energy%flux%HeatGroundTotMean          ,& ! in,  total ground heat flux [W/m2] averaged during soil timestep
              RadSwPenetrateGrd        => noahmp%energy%flux%RadSwPenetrateGrd          ,& ! in,  light penetrating through soil/snow water [W/m2]
              HeatFromSoilBot          => noahmp%energy%flux%HeatFromSoilBot             & ! out, energy influx from soil bottom [W/m2]
             )
! ----------------------------------------------------------------------

    ! initialization
    if (.not. allocated(DepthSnowSoilInv)) allocate(DepthSnowSoilInv(-NumSnowLayerMax+1:NumSoilLayer))
    if (.not. allocated(HeatCapacPerArea)) allocate(HeatCapacPerArea(-NumSnowLayerMax+1:NumSoilLayer))
    if (.not. allocated(TempGradDepth)   ) allocate(TempGradDepth   (-NumSnowLayerMax+1:NumSoilLayer))
    if (.not. allocated(EnergyExcess)    ) allocate(EnergyExcess    (-NumSnowLayerMax+1:NumSoilLayer))
    MatRight(:)         = 0.0
    MatLeft1(:)         = 0.0
    MatLeft2(:)         = 0.0
    MatLeft3(:)         = 0.0
    DepthSnowSoilInv(:) = 0.0
    HeatCapacPerArea(:) = 0.0
    TempGradDepth(:)    = 0.0
    EnergyExcess(:)     = 0.0

    ! compute gradient and flux of soil/snow thermal diffusion
    do LoopInd = NumSnowLayerNeg+1, NumSoilLayer
       if ( LoopInd == (NumSnowLayerNeg+1) ) then
          HeatCapacPerArea(LoopInd) = - DepthSnowSoilLayer(LoopInd) * HeatCapacSoilSnow(LoopInd)
          DepthSnowSoilTmp          = - DepthSnowSoilLayer(LoopInd+1)
          DepthSnowSoilInv(LoopInd) = 2.0 / DepthSnowSoilTmp
          TempGradDepth(LoopInd)    = 2.0 * (TemperatureSoilSnow(LoopInd) - TemperatureSoilSnow(LoopInd+1)) / DepthSnowSoilTmp
          EnergyExcess(LoopInd)     = ThermConductSoilSnow(LoopInd) * TempGradDepth(LoopInd) - &
                                      HeatGroundTotMean - RadSwPenetrateGrd(LoopInd)
       elseif ( LoopInd < NumSoilLayer ) then
          HeatCapacPerArea(LoopInd) = (DepthSnowSoilLayer(LoopInd-1) - DepthSnowSoilLayer(LoopInd)) * HeatCapacSoilSnow(LoopInd)
          DepthSnowSoilTmp          = DepthSnowSoilLayer(LoopInd-1) - DepthSnowSoilLayer(LoopInd+1)
          DepthSnowSoilInv(LoopInd) = 2.0 / DepthSnowSoilTmp
          TempGradDepth(LoopInd)    = 2.0 * (TemperatureSoilSnow(LoopInd) - TemperatureSoilSnow(LoopInd+1)) / DepthSnowSoilTmp
          EnergyExcess(LoopInd)     = (ThermConductSoilSnow(LoopInd)*TempGradDepth(LoopInd) - &
                                      ThermConductSoilSnow(LoopInd-1) * TempGradDepth(LoopInd-1) ) - RadSwPenetrateGrd(LoopInd)
       elseif ( LoopInd == NumSoilLayer ) then
          HeatCapacPerArea(LoopInd) = (DepthSnowSoilLayer(LoopInd-1) - DepthSnowSoilLayer(LoopInd)) * HeatCapacSoilSnow(LoopInd)
          DepthSnowSoilTmp          =  DepthSnowSoilLayer(LoopInd-1) - DepthSnowSoilLayer(LoopInd)
          if ( OptSoilTemperatureBottom == 1 ) then
             HeatFromSoilBot        = 0.0
          endif
          if ( OptSoilTemperatureBottom == 2 ) then
             TempGradDepth(LoopInd) = (TemperatureSoilSnow(LoopInd) - TemperatureSoilBottom) / &
                                      (0.5*(DepthSnowSoilLayer(LoopInd-1)+DepthSnowSoilLayer(LoopInd)) - DepthSoilTempBotToSno)
             HeatFromSoilBot        = -ThermConductSoilSnow(LoopInd) * TempGradDepth(LoopInd)
          endif
          EnergyExcess(LoopInd)     = (-HeatFromSoilBot - ThermConductSoilSnow(LoopInd-1) * TempGradDepth(LoopInd-1)) - &
                                      RadSwPenetrateGrd(LoopInd)
       endif
    enddo

    ! prepare the matrix coefficients for the tri-diagonal matrix
    do LoopInd = NumSnowLayerNeg+1, NumSoilLayer
       if ( LoopInd == (NumSnowLayerNeg+1) ) then
          MatLeft1(LoopInd) = 0.0
          MatLeft3(LoopInd) = - ThermConductSoilSnow(LoopInd) * DepthSnowSoilInv(LoopInd) / HeatCapacPerArea(LoopInd)
          if ( (OptSnowSoilTempTime == 1) .or. (OptSnowSoilTempTime == 3) ) then
             MatLeft2(LoopInd) = - MatLeft3(LoopInd)
          endif
          if ( OptSnowSoilTempTime == 2 ) then
             MatLeft2(LoopInd) = - MatLeft3(LoopInd) + ThermConductSoilSnow(LoopInd) / &
                                (0.5*DepthSnowSoilLayer(LoopInd)*DepthSnowSoilLayer(LoopInd)*HeatCapacSoilSnow(LoopInd))
          endif
       elseif ( LoopInd < NumSoilLayer ) then
          MatLeft1(LoopInd) = - ThermConductSoilSnow(LoopInd-1) * DepthSnowSoilInv(LoopInd-1) / HeatCapacPerArea(LoopInd)
          MatLeft3(LoopInd) = - ThermConductSoilSnow(LoopInd  ) * DepthSnowSoilInv(LoopInd  ) / HeatCapacPerArea(LoopInd)
          MatLeft2(LoopInd) = - (MatLeft1(LoopInd) + MatLeft3 (LoopInd))
       elseif ( LoopInd == NumSoilLayer ) then
          MatLeft1(LoopInd) = - ThermConductSoilSnow(LoopInd-1) * DepthSnowSoilInv(LoopInd-1) / HeatCapacPerArea(LoopInd)
          MatLeft3(LoopInd) = 0.0
          MatLeft2(LoopInd) = - (MatLeft1(LoopInd) + MatLeft3(LoopInd))
       endif
          MatRight(LoopInd) = EnergyExcess(LoopInd) / (-HeatCapacPerArea(LoopInd))
    enddo

    ! deallocate local arrays to avoid memory leaks
    deallocate(DepthSnowSoilInv)
    deallocate(HeatCapacPerArea)
    deallocate(TempGradDepth   )
    deallocate(EnergyExcess    )

    end associate

  end subroutine SoilSnowThermalDiffusion

end module SoilSnowThermalDiffusionMod
