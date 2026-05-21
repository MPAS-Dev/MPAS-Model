module GlacierTemperatureMainMod

!!! Main module to compute snow (if exists) and glacier ice temperature. 
!!! Note that snow temperatures during melting season may exceed melting 
!!! point but later in GlacierPhaseChange subroutine the snow
!!! temperatures are reset to melting point for melting snow.

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use GlacierTemperatureSolverMod, only : GlacierTemperatureSolver
  use GlacierThermalDiffusionMod,  only : GlacierThermalDiffusion 

  implicit none

contains

  subroutine GlacierTemperatureMain(noahmp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: TSNOSOI_GLACIER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type)     , intent(inout) :: noahmp

! local variable
    real(kind=kind_noahmp), allocatable, dimension(:) :: MatRight  ! right-hand side term of the matrix
    real(kind=kind_noahmp), allocatable, dimension(:) :: MatLeft1  ! left-hand side term of the matrix
    real(kind=kind_noahmp), allocatable, dimension(:) :: MatLeft2  ! left-hand side term of the matrix
    real(kind=kind_noahmp), allocatable, dimension(:) :: MatLeft3  ! left-hand side term of the matrix

! --------------------------------------------------------------------
    associate(                                                                    &
              NumSoilLayer          => noahmp%config%domain%NumSoilLayer         ,& ! in,  number of glacier/soil layers
              NumSnowLayerMax       => noahmp%config%domain%NumSnowLayerMax      ,& ! in,  maximum number of snow layers
              NumSnowLayerNeg       => noahmp%config%domain%NumSnowLayerNeg      ,& ! in,  actual number of snow layers (negative)
              MainTimeStep          => noahmp%config%domain%MainTimeStep         ,& ! in,  main noahmp timestep [s]
              DepthSoilTempBottom   => noahmp%config%domain%DepthSoilTempBottom  ,& ! in,  depth [m] from glacier surface for lower soil temperature boundary
              SnowDepth             => noahmp%water%state%SnowDepth              ,& ! in,  snow depth [m]
              DepthSoilTempBotToSno => noahmp%energy%state%DepthSoilTempBotToSno ,& ! out, depth of lower boundary condition [m] from snow surface
              RadSwPenetrateGrd     => noahmp%energy%flux%RadSwPenetrateGrd       & ! out, light penetrating through snow/ice [W/m2]
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

    ! adjust DepthSoilTempBottom from glacier ice surface to DepthSoilTempBotToSno from snow surface
    DepthSoilTempBotToSno = DepthSoilTempBottom - SnowDepth

    ! compute soil temperatures
    call GlacierThermalDiffusion(noahmp, MatLeft1, MatLeft2, MatLeft3, MatRight)
    call GlacierTemperatureSolver(noahmp, MainTimeStep, MatLeft1, MatLeft2, MatLeft3, MatRight)

    ! deallocate local arrays to avoid memory leaks
    deallocate(MatRight)
    deallocate(MatLeft1)
    deallocate(MatLeft2)
    deallocate(MatLeft3)

    end associate

  end subroutine GlacierTemperatureMain

end module GlacierTemperatureMainMod
