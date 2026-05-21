module SoilSnowTemperatureMainMod

!!! Main module to compute snow (if exists) and soil layer temperature. 
!!! Note that snow temperatures during melting season may exceed melting 
!!! point but later in SoilSnowPhaseChange subroutine the snow
!!! temperatures are reset to melting point for melting snow.

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use SoilSnowTemperatureSolverMod, only : SoilSnowTemperatureSolver
  use SoilSnowThermalDiffusionMod,  only : SoilSnowThermalDiffusion 

  implicit none

contains

  subroutine SoilSnowTemperatureMain(noahmp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: TSNOSOI
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type)     , intent(inout) :: noahmp

! local variable
    real(kind=kind_noahmp), allocatable, dimension(:) :: MatRight     ! right-hand side term of the matrix
    real(kind=kind_noahmp), allocatable, dimension(:) :: MatLeft1     ! left-hand side term
    real(kind=kind_noahmp), allocatable, dimension(:) :: MatLeft2     ! left-hand side term
    real(kind=kind_noahmp), allocatable, dimension(:) :: MatLeft3     ! left-hand side term

! --------------------------------------------------------------------
    associate(                                                                    &
              NumSoilLayer          => noahmp%config%domain%NumSoilLayer         ,& ! in,  number of soil layers
              NumSnowLayerMax       => noahmp%config%domain%NumSnowLayerMax      ,& ! in,  maximum number of snow layers
              NumSnowLayerNeg       => noahmp%config%domain%NumSnowLayerNeg      ,& ! in,  actual number of snow layers (negative)
              SoilTimeStep          => noahmp%config%domain%SoilTimeStep         ,& ! in,  noahmp soil process timestep [s]
              DepthSoilTempBottom   => noahmp%config%domain%DepthSoilTempBottom  ,& ! in,  depth [m] from soil surface for soil temp. lower boundary
              SnowDepth             => noahmp%water%state%SnowDepth              ,& ! in,  snow depth [m]
              DepthSoilTempBotToSno => noahmp%energy%state%DepthSoilTempBotToSno ,& ! out, depth [m] of soil temp. lower boundary from snow surface
              HeatFromSoilBot       => noahmp%energy%flux%HeatFromSoilBot        ,& ! out, energy influx from soil bottom during soil timestep [J/m2]
              RadSwPenetrateGrd     => noahmp%energy%flux%RadSwPenetrateGrd       & ! out, light penetrating through soil/snow water [W/m2]
             )
! ----------------------------------------------------------------------

    ! initialization
    if (.not. allocated(MatRight)) allocate(MatRight(-NumSnowLayerMax+1:NumSoilLayer))
    if (.not. allocated(MatLeft1)) allocate(MatLeft1(-NumSnowLayerMax+1:NumSoilLayer))
    if (.not. allocated(MatLeft2)) allocate(MatLeft2(-NumSnowLayerMax+1:NumSoilLayer))
    if (.not. allocated(MatLeft3)) allocate(MatLeft3(-NumSnowLayerMax+1:NumSoilLayer))
    MatRight(:) = 0.0
    MatLeft1(:) = 0.0
    MatLeft2(:) = 0.0
    MatLeft3(:) = 0.0

    ! compute solar penetration through water, needs more work
    RadSwPenetrateGrd(NumSnowLayerNeg+1:NumSoilLayer) = 0.0

    ! adjust DepthSoilTempBottom from soil surface to DepthSoilTempBotToSno from snow surface
    DepthSoilTempBotToSno = DepthSoilTempBottom - SnowDepth

    ! compute soil temperatures
    call SoilSnowThermalDiffusion(noahmp, MatLeft1, MatLeft2, MatLeft3, MatRight)
    call SoilSnowTemperatureSolver(noahmp, SoilTimeStep, MatLeft1, MatLeft2, MatLeft3, MatRight)

    ! accumulate soil bottom flux for soil timestep
    HeatFromSoilBot = HeatFromSoilBot * SoilTimeStep

    ! deallocate local arrays to avoid memory leaks
    deallocate(MatRight)
    deallocate(MatLeft1)
    deallocate(MatLeft2)
    deallocate(MatLeft3)

    end associate

  end subroutine SoilSnowTemperatureMain

end module SoilSnowTemperatureMainMod
