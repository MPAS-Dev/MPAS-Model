module SnowLayerDivideMod

!!! Snowpack layer division process
!!! Update snow ice, snow water, snow thickness, snow temperature

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use SnowLayerWaterComboMod, only: SnowLayerWaterCombo

  implicit none

contains

  subroutine SnowLayerDivide(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: DIVIDE
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                          :: LoopInd                              ! snow layer loop index
    integer                          :: NumSnowLayerTmp                      ! number of snow layer top to bottom
    real(kind=kind_noahmp)           :: SnowThickCombTmp                     ! thickness of the combined [m]
    real(kind=kind_noahmp)           :: SnowIceExtra                         ! extra snow ice to be divided compared to allowed layer thickness
    real(kind=kind_noahmp)           :: SnowLiqExtra                         ! extra snow liquid water to be divided compared to allowed layer thickness
    real(kind=kind_noahmp)           :: SnowFracExtra                        ! fraction of extra snow to be divided compared to allowed layer thickness
    real(kind=kind_noahmp)           :: SnowTempGrad                         ! temperature gradient between two snow layers
    real(kind=kind_noahmp), allocatable, dimension(:) :: SnowThickTmp        ! snow layer thickness [m]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SnowIceTmp          ! partial volume of ice [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SnowLiqTmp          ! partial volume of liquid water [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: TemperatureSnowTmp  ! node temperature [K]

! --------------------------------------------------------------------
    associate(                                                                       &
              NumSnowLayerMax        => noahmp%config%domain%NumSnowLayerMax        ,& ! in,    maximum number of snow layers
              NumSnowLayerNeg        => noahmp%config%domain%NumSnowLayerNeg        ,& ! inout, actual number of snow layers (negative)
              ThicknessSnowSoilLayer => noahmp%config%domain%ThicknessSnowSoilLayer ,& ! inout, thickness of snow/soil layers [m]
              TemperatureSoilSnow    => noahmp%energy%state%TemperatureSoilSnow     ,& ! inout, snow and soil layer temperature [K]
              SnowIce                => noahmp%water%state%SnowIce                  ,& ! inout, snow layer ice [mm]
              SnowLiqWater           => noahmp%water%state%SnowLiqWater              & ! inout, snow layer liquid water [mm]
             )
! ----------------------------------------------------------------------

    ! initialization
    if (.not. allocated(SnowIceTmp)        ) allocate(SnowIceTmp        (1:NumSnowLayerMax))
    if (.not. allocated(SnowLiqTmp)        ) allocate(SnowLiqTmp        (1:NumSnowLayerMax))
    if (.not. allocated(TemperatureSnowTmp)) allocate(TemperatureSnowTmp(1:NumSnowLayerMax))
    if (.not. allocated(SnowThickTmp)      ) allocate(SnowThickTmp      (1:NumSnowLayerMax))
    SnowIceTmp        (:) = 0.0
    SnowLiqTmp        (:) = 0.0
    TemperatureSnowTmp(:) = 0.0
    SnowThickTmp      (:) = 0.0

    do LoopInd = 1, NumSnowLayerMax
       if ( LoopInd <= abs(NumSnowLayerNeg) ) then
          SnowThickTmp(LoopInd)       = ThicknessSnowSoilLayer(LoopInd+NumSnowLayerNeg)
          SnowIceTmp(LoopInd)         = SnowIce(LoopInd+NumSnowLayerNeg)
          SnowLiqTmp(LoopInd)         = SnowLiqWater(LoopInd+NumSnowLayerNeg)
          TemperatureSnowTmp(LoopInd) = TemperatureSoilSnow(LoopInd+NumSnowLayerNeg)
       endif
    enddo

    ! start snow layer division
    NumSnowLayerTmp = abs(NumSnowLayerNeg)

    if ( NumSnowLayerTmp == 1 ) then
       ! Specify a new snow layer
       if ( SnowThickTmp(1) > 0.05 ) then
          NumSnowLayerTmp       = 2
          SnowThickTmp(1)       = SnowThickTmp(1)/2.0
          SnowIceTmp(1)         = SnowIceTmp(1)/2.0
          SnowLiqTmp(1)         = SnowLiqTmp(1)/2.0
          SnowThickTmp(2)       = SnowThickTmp(1)
          SnowIceTmp(2)         = SnowIceTmp(1)
          SnowLiqTmp(2)         = SnowLiqTmp(1)
          TemperatureSnowTmp(2) = TemperatureSnowTmp(1)
       endif
    endif

    if ( NumSnowLayerTmp > 1 ) then
       if ( SnowThickTmp(1) > 0.05 ) then     ! maximum allowed thickness (5cm) for top snow layer
          SnowThickCombTmp = SnowThickTmp(1) - 0.05
          SnowFracExtra    = SnowThickCombTmp / SnowThickTmp(1)
          SnowIceExtra     = SnowFracExtra * SnowIceTmp(1)
          SnowLiqExtra     = SnowFracExtra * SnowLiqTmp(1)
          SnowFracExtra    = 0.05 / SnowThickTmp(1)
          SnowIceTmp(1)    = SnowFracExtra*SnowIceTmp(1)
          SnowLiqTmp(1)    = SnowFracExtra*SnowLiqTmp(1)
          SnowThickTmp(1)  = 0.05

          ! update combined snow water & temperature
          call SnowLayerWaterCombo(SnowThickTmp(2), SnowLiqTmp(2), SnowIceTmp(2), TemperatureSnowTmp(2), &
                                   SnowThickCombTmp, SnowLiqExtra, SnowIceExtra, TemperatureSnowTmp(1))

          ! subdivide a new layer, maximum allowed thickness (20cm) for second snow layer
          if ( (NumSnowLayerTmp <= 2) .and. (SnowThickTmp(2) > 0.20) ) then  ! MB: change limit
         !if ( (NumSnowLayerTmp <= 2) .and. (SnowThickTmp(2) > 0.10) ) then
             NumSnowLayerTmp       = 3
             SnowTempGrad          = (TemperatureSnowTmp(1) - TemperatureSnowTmp(2)) / &
                                     ((SnowThickTmp(1)+SnowThickTmp(2)) / 2.0)
             SnowThickTmp(2)       = SnowThickTmp(2) / 2.0
             SnowIceTmp(2)         = SnowIceTmp(2) / 2.0
             SnowLiqTmp(2)         = SnowLiqTmp(2) / 2.0
             SnowThickTmp(3)       = SnowThickTmp(2)
             SnowIceTmp(3)         = SnowIceTmp(2)
             SnowLiqTmp(3)         = SnowLiqTmp(2)
             TemperatureSnowTmp(3) = TemperatureSnowTmp(2) - SnowTempGrad * SnowThickTmp(2) / 2.0
             if ( TemperatureSnowTmp(3) >= ConstFreezePoint ) then
                TemperatureSnowTmp(3) = TemperatureSnowTmp(2)
             else
                TemperatureSnowTmp(2) = TemperatureSnowTmp(2) + SnowTempGrad * SnowThickTmp(2) / 2.0
             endif
          endif
       endif ! if(SnowThickTmp(1) > 0.05)
    endif  ! if (NumSnowLayerTmp > 1)

    if ( NumSnowLayerTmp > 2 ) then
       if ( SnowThickTmp(2) > 0.2 ) then
          SnowThickCombTmp = SnowThickTmp(2) - 0.2
          SnowFracExtra    = SnowThickCombTmp / SnowThickTmp(2)
          SnowIceExtra     = SnowFracExtra * SnowIceTmp(2)
          SnowLiqExtra     = SnowFracExtra * SnowLiqTmp(2)
          SnowFracExtra    = 0.2 / SnowThickTmp(2)
          SnowIceTmp(2)    = SnowFracExtra * SnowIceTmp(2)
          SnowLiqTmp(2)    = SnowFracExtra * SnowLiqTmp(2)
          SnowThickTmp(2)  = 0.2

          ! update combined snow water & temperature
          call SnowLayerWaterCombo(SnowThickTmp(3), SnowLiqTmp(3), SnowIceTmp(3), TemperatureSnowTmp(3), &
                                   SnowThickCombTmp, SnowLiqExtra, SnowIceExtra, TemperatureSnowTmp(2))
       endif
    endif

    NumSnowLayerNeg = -NumSnowLayerTmp

    do LoopInd = NumSnowLayerNeg+1, 0
       ThicknessSnowSoilLayer(LoopInd) = SnowThickTmp(LoopInd-NumSnowLayerNeg)
       SnowIce(LoopInd)                = SnowIceTmp(LoopInd-NumSnowLayerNeg)
       SnowLiqWater(LoopInd)           = SnowLiqTmp(LoopInd-NumSnowLayerNeg)
       TemperatureSoilSnow(LoopInd)    = TemperatureSnowTmp(LoopInd-NumSnowLayerNeg)
    enddo

    ! deallocate local arrays to avoid memory leaks
    deallocate(SnowIceTmp        )
    deallocate(SnowLiqTmp        )
    deallocate(TemperatureSnowTmp)
    deallocate(SnowThickTmp      )

    end associate

  end subroutine SnowLayerDivide

end module SnowLayerDivideMod
