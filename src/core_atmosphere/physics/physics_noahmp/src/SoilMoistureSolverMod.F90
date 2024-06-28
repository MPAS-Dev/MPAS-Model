module SoilMoistureSolverMod

!!! Compute soil moisture content using based on Richards diffusion & tri-diagonal matrix
!!! Dependent on the output from SoilWaterDiffusionRichards subroutine

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use MatrixSolverTriDiagonalMod, only : MatrixSolverTriDiagonal

  implicit none

contains

  subroutine SoilMoistureSolver(noahmp, TimeStep, MatLeft1, MatLeft2, MatLeft3, MatRight)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: SSTEP
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type)     , intent(inout) :: noahmp
    real(kind=kind_noahmp), intent(in)    :: TimeStep                               ! timestep (may not be the same as model timestep)
    real(kind=kind_noahmp), allocatable, dimension(:), intent(inout) :: MatRight    ! right-hand side term of the matrix
    real(kind=kind_noahmp), allocatable, dimension(:), intent(inout) :: MatLeft1    ! left-hand side term of the matrix
    real(kind=kind_noahmp), allocatable, dimension(:), intent(inout) :: MatLeft2    ! left-hand side term of the matrix
    real(kind=kind_noahmp), allocatable, dimension(:), intent(inout) :: MatLeft3    ! left-hand side term of the matrix

! local variable
    integer                                           :: LoopInd                    ! soil layer loop index 
    real(kind=kind_noahmp)                            :: WatDefiTmp                 ! temporary water deficiency
    real(kind=kind_noahmp), allocatable, dimension(:) :: MatRightTmp                ! temporary MatRight matrix coefficient
    real(kind=kind_noahmp), allocatable, dimension(:) :: MatLeft3Tmp                ! temporary MatLeft3 matrix coefficient

! --------------------------------------------------------------------
    associate(                                                                       &
              NumSoilLayer           => noahmp%config%domain%NumSoilLayer           ,& ! in,    number of soil layers
              DepthSoilLayer         => noahmp%config%domain%DepthSoilLayer         ,& ! in,    depth [m] of layer-bottom from soil surface
              ThicknessSnowSoilLayer => noahmp%config%domain%ThicknessSnowSoilLayer ,& ! in,    thickness of snow/soil layers [m]
              OptRunoffSubsurface    => noahmp%config%nmlist%OptRunoffSubsurface    ,& ! in,    options for drainage and subsurface runoff
              SoilMoistureSat        => noahmp%water%param%SoilMoistureSat          ,& ! in,    saturated value of soil moisture [m3/m3]
              WaterTableDepth        => noahmp%water%state%WaterTableDepth          ,& ! in,    water table depth [m]
              SoilIce                => noahmp%water%state%SoilIce                  ,& ! in,    soil ice content [m3/m3]
              SoilLiqWater           => noahmp%water%state%SoilLiqWater             ,& ! inout, soil water content [m3/m3]
              SoilMoisture           => noahmp%water%state%SoilMoisture             ,& ! inout, total soil moisture [m3/m3]
              SoilMoistureToWT       => noahmp%water%state%SoilMoistureToWT         ,& ! inout, soil moisture between bottom of soil & water table
              RechargeGwDeepWT       => noahmp%water%state%RechargeGwDeepWT         ,& ! inout, recharge to or from the water table when deep [m]
              DrainSoilBot           => noahmp%water%flux%DrainSoilBot              ,& ! inout, soil bottom drainage (m/s)
              SoilEffPorosity        => noahmp%water%state%SoilEffPorosity          ,& ! out,   soil effective porosity (m3/m3)
              SoilSaturationExcess   => noahmp%water%state%SoilSaturationExcess      & ! out,   saturation excess of the total soil [m]
             )
! ----------------------------------------------------------------------

    ! initialization
    if (.not. allocated(MatRightTmp)) allocate(MatRightTmp(1:NumSoilLayer))
    if (.not. allocated(MatLeft3Tmp)) allocate(MatLeft3Tmp(1:NumSoilLayer))
    MatRightTmp          = 0.0
    MatLeft3Tmp          = 0.0
    SoilSaturationExcess = 0.0
    SoilEffPorosity(:)   = 0.0

    ! update tri-diagonal matrix elements
    do LoopInd = 1, NumSoilLayer
       MatRight(LoopInd) =       MatRight(LoopInd) * TimeStep
       MatLeft1(LoopInd) =       MatLeft1(LoopInd) * TimeStep
       MatLeft2(LoopInd) = 1.0 + MatLeft2(LoopInd) * TimeStep
       MatLeft3(LoopInd) =       MatLeft3(LoopInd) * TimeStep
    enddo

    ! copy values for input variables before calling rosr12
    do LoopInd = 1, NumSoilLayer
       MatRightTmp(LoopInd) = MatRight(LoopInd)
       MatLeft3Tmp(LoopInd) = MatLeft3(LoopInd)
    enddo

    ! call ROSR12 to solve the tri-diagonal matrix
    call MatrixSolverTriDiagonal(MatLeft3,MatLeft1,MatLeft2,MatLeft3Tmp,MatRightTmp,MatRight,1,NumSoilLayer,0)

    do LoopInd = 1, NumSoilLayer
        SoilLiqWater(LoopInd) = SoilLiqWater(LoopInd) + MatLeft3(LoopInd)
    enddo

    !  excessive water above saturation in a layer is moved to
    !  its unsaturated layer like in a bucket

    ! for MMF scheme, there is soil moisture below NumSoilLayer, to the water table
    if ( OptRunoffSubsurface == 5 ) then
       ! update SoilMoistureToWT
       if ( WaterTableDepth < (DepthSoilLayer(NumSoilLayer)-ThicknessSnowSoilLayer(NumSoilLayer)) ) then
          ! accumulate soil drainage to update deep water table and soil moisture later
          RechargeGwDeepWT           = RechargeGwDeepWT + TimeStep * DrainSoilBot
       else
          SoilMoistureToWT           = SoilMoistureToWT + &
                                       TimeStep * DrainSoilBot / ThicknessSnowSoilLayer(NumSoilLayer)
          SoilSaturationExcess       = max((SoilMoistureToWT - SoilMoistureSat(NumSoilLayer)), 0.0) * &
                                       ThicknessSnowSoilLayer(NumSoilLayer)
          WatDefiTmp                 = max((1.0e-4 - SoilMoistureToWT), 0.0) * ThicknessSnowSoilLayer(NumSoilLayer)
          SoilMoistureToWT           = max(min(SoilMoistureToWT, SoilMoistureSat(NumSoilLayer)), 1.0e-4)
          SoilLiqWater(NumSoilLayer) = SoilLiqWater(NumSoilLayer) + &
                                       SoilSaturationExcess / ThicknessSnowSoilLayer(NumSoilLayer)
          ! reduce fluxes at the bottom boundaries accordingly
          DrainSoilBot               = DrainSoilBot - SoilSaturationExcess/TimeStep
          RechargeGwDeepWT           = RechargeGwDeepWT - WatDefiTmp
       endif
    endif

    do LoopInd = NumSoilLayer, 2, -1
       SoilEffPorosity(LoopInd) = max(1.0e-4, (SoilMoistureSat(LoopInd) - SoilIce(LoopInd)))
       SoilSaturationExcess     = max((SoilLiqWater(LoopInd)-SoilEffPorosity(LoopInd)), 0.0) * &
                                  ThicknessSnowSoilLayer(LoopInd)
       SoilLiqWater(LoopInd)    = min(SoilEffPorosity(LoopInd), SoilLiqWater(LoopInd) )
       SoilLiqWater(LoopInd-1)  = SoilLiqWater(LoopInd-1) + SoilSaturationExcess / ThicknessSnowSoilLayer(LoopInd-1)
    enddo

    SoilEffPorosity(1)   = max(1.0e-4, (SoilMoistureSat(1)-SoilIce(1)))
    SoilSaturationExcess = max((SoilLiqWater(1)-SoilEffPorosity(1)), 0.0) * ThicknessSnowSoilLayer(1)
    SoilLiqWater(1)      = min(SoilEffPorosity(1), SoilLiqWater(1))

    if ( SoilSaturationExcess > 0.0 ) then
       SoilLiqWater(2) = SoilLiqWater(2) + SoilSaturationExcess / ThicknessSnowSoilLayer(2)
       do LoopInd = 2, NumSoilLayer-1
          SoilEffPorosity(LoopInd) = max(1.0e-4, (SoilMoistureSat(LoopInd) - SoilIce(LoopInd)))
          SoilSaturationExcess     = max((SoilLiqWater(LoopInd)-SoilEffPorosity(LoopInd)), 0.0) * &
                                     ThicknessSnowSoilLayer(LoopInd)
          SoilLiqWater(LoopInd)    = min(SoilEffPorosity(LoopInd), SoilLiqWater(LoopInd))
          SoilLiqWater(LoopInd+1)  = SoilLiqWater(LoopInd+1) + SoilSaturationExcess / ThicknessSnowSoilLayer(LoopInd+1)
       enddo
       SoilEffPorosity(NumSoilLayer) = max(1.0e-4, (SoilMoistureSat(NumSoilLayer) - SoilIce(NumSoilLayer)))
       SoilSaturationExcess          = max((SoilLiqWater(NumSoilLayer)-SoilEffPorosity(NumSoilLayer)), 0.0) * &
                                       ThicknessSnowSoilLayer(NumSoilLayer)
       SoilLiqWater(NumSoilLayer)    = min(SoilEffPorosity(NumSoilLayer), SoilLiqWater(NumSoilLayer))
    endif

    SoilMoisture = SoilLiqWater + SoilIce

    ! deallocate local arrays to avoid memory leaks
    deallocate(MatRightTmp)
    deallocate(MatLeft3Tmp)

    end associate

  end subroutine SoilMoistureSolver

end module SoilMoistureSolverMod
