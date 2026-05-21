module ShallowWaterTableMmfMod

!!! Diagnoses water table depth and computes recharge when the water table is 
!!! within the resolved soil layers, according to the Miguez-Macho&Fan scheme

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine ShallowWaterTableMMF(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: SHALLOWWATERTABLE
! Original code: Miguez-Macho&Fan (Miguez-Macho et al 2007, Fan et al 2007)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                          :: LoopInd                            ! do-loop index
    integer                          :: IndAbvWatTbl                       ! layer index above water table layer
    integer                          :: IndWatTbl                          ! layer index where the water table layer is
    real(kind=kind_noahmp)           :: WatTblDepthOld                     ! old water table depth
    real(kind=kind_noahmp)           :: ThicknessUpLy                      ! upper layer thickness
    real(kind=kind_noahmp)           :: SoilMoistDeep                      ! deep layer soil moisture
    real(kind=kind_noahmp), allocatable, dimension(:) :: DepthSoilLayer0   ! temporary soil depth

! --------------------------------------------------------------------
    associate(                                                                       &
              NumSoilLayer           => noahmp%config%domain%NumSoilLayer           ,& ! in,    number of soil layers
              SoilTimeStep           => noahmp%config%domain%SoilTimeStep           ,& ! in,    noahmp soil timestep [s]
              DepthSoilLayer         => noahmp%config%domain%DepthSoilLayer         ,& ! in,    depth of soil layer-bottom [m]
              ThicknessSnowSoilLayer => noahmp%config%domain%ThicknessSnowSoilLayer ,& ! in,    thickness of snow/soil layers [m]
              SoilMoistureEqui       => noahmp%water%state%SoilMoistureEqui         ,& ! in,    equilibrium soil water  content [m3/m3]
              SoilMoistureSat        => noahmp%water%param%SoilMoistureSat          ,& ! in,    saturated value of soil moisture [m3/m3]
              SoilMatPotentialSat    => noahmp%water%param%SoilMatPotentialSat      ,& ! in,    saturated soil matric potential [m]
              SoilExpCoeffB          => noahmp%water%param%SoilExpCoeffB            ,& ! in,    soil B parameter
              SoilMoisture           => noahmp%water%state%SoilMoisture             ,& ! inout, total soil water content [m3/m3]
              WaterTableDepth        => noahmp%water%state%WaterTableDepth          ,& ! inout, water table depth [m]
              SoilMoistureToWT       => noahmp%water%state%SoilMoistureToWT         ,& ! inout, soil moisture between bottom of soil & water table
              DrainSoilBot           => noahmp%water%flux%DrainSoilBot              ,& ! inout, soil bottom drainage [m/s]
              RechargeGwShallowWT    => noahmp%water%state%RechargeGwShallowWT       & ! out,   groundwater recharge (net vertical flux across water table), positive up
             )
! ----------------------------------------------------------------------

    ! initialization
    if (.not. allocated(DepthSoilLayer0)) allocate(DepthSoilLayer0(0:NumSoilLayer))
    DepthSoilLayer0(1:NumSoilLayer) = DepthSoilLayer(1:NumSoilLayer)
    DepthSoilLayer0(0)              = 0.0

    ! find the layer where the water table is
    do LoopInd = NumSoilLayer, 1, -1
       if ( (WaterTableDepth+1.0e-6) < DepthSoilLayer0(LoopInd) ) exit
    enddo
    IndAbvWatTbl = LoopInd

    IndWatTbl    = IndAbvWatTbl + 1          ! layer where the water table is
    if ( IndWatTbl <= NumSoilLayer ) then    ! water table depth in the resolved layers
       WatTblDepthOld = WaterTableDepth
       if ( SoilMoisture(IndWatTbl) > SoilMoistureEqui(IndWatTbl) ) then
          if ( SoilMoisture(IndWatTbl) == SoilMoistureSat(IndWatTbl) ) then ! wtd went to the layer above
             WaterTableDepth     = DepthSoilLayer0(IndAbvWatTbl)
             RechargeGwShallowWT = -(WatTblDepthOld - WaterTableDepth) * &
                                   (SoilMoistureSat(IndWatTbl) - SoilMoistureEqui(IndWatTbl))
             IndAbvWatTbl        = IndAbvWatTbl-1
             IndWatTbl           = IndWatTbl-1
             if ( IndWatTbl >= 1 ) then
                if ( SoilMoisture(IndWatTbl) > SoilMoistureEqui(IndWatTbl) ) then
                   WatTblDepthOld      = WaterTableDepth
                   WaterTableDepth     = min((SoilMoisture(IndWatTbl)*ThicknessSnowSoilLayer(IndWatTbl) - &
                                              SoilMoistureEqui(IndWatTbl)*DepthSoilLayer0(IndAbvWatTbl) + &
                                              SoilMoistureSat(IndWatTbl)*DepthSoilLayer0(IndWatTbl)) /    &
                                             (SoilMoistureSat(IndWatTbl)-SoilMoistureEqui(IndWatTbl)),    &
                                             DepthSoilLayer0(IndAbvWatTbl) )
                   RechargeGwShallowWT = RechargeGwShallowWT - (WatTblDepthOld-WaterTableDepth) * &
                                         (SoilMoistureSat(IndWatTbl)-SoilMoistureEqui(IndWatTbl))
                endif
             endif
          else  ! water table depth stays in the layer
             WaterTableDepth = min((SoilMoisture(IndWatTbl)*ThicknessSnowSoilLayer(IndWatTbl) - &
                                    SoilMoistureEqui(IndWatTbl)*DepthSoilLayer0(IndAbvWatTbl) + &
                                    SoilMoistureSat(IndWatTbl)*DepthSoilLayer0(IndWatTbl) ) /   &
                                   (SoilMoistureSat(IndWatTbl)-SoilMoistureEqui(IndWatTbl)),    &
                                   DepthSoilLayer0(IndAbvWatTbl))
             RechargeGwShallowWT = -(WatTblDepthOld-WaterTableDepth) * &
                                    (SoilMoistureSat(IndWatTbl) - SoilMoistureEqui(IndWatTbl))
          endif
       else   ! water table depth has gone down to the layer below
          WaterTableDepth     = DepthSoilLayer0(IndWatTbl)
          RechargeGwShallowWT = -(WatTblDepthOld-WaterTableDepth) * &
                                 (SoilMoistureSat(IndWatTbl) - SoilMoistureEqui(IndWatTbl))
          IndWatTbl           = IndWatTbl + 1
          IndAbvWatTbl        = IndAbvWatTbl + 1
          ! water table depth crossed to the layer below. Now adjust it there
          if ( IndWatTbl <= NumSoilLayer ) then
             WatTblDepthOld = WaterTableDepth
             if ( SoilMoisture(IndWatTbl) > SoilMoistureEqui(IndWatTbl) ) then
                WaterTableDepth  = min((SoilMoisture(IndWatTbl)*ThicknessSnowSoilLayer(IndWatTbl) - &
                                        SoilMoistureEqui(IndWatTbl)*DepthSoilLayer0(IndAbvWatTbl) + &
                                        SoilMoistureSat(IndWatTbl)*DepthSoilLayer0(IndWatTbl) ) /   &
                                       (SoilMoistureSat(IndWatTbl)-SoilMoistureEqui(IndWatTbl)),    &
                                       DepthSoilLayer0(IndAbvWatTbl))
             else
                WaterTableDepth  = DepthSoilLayer0(IndWatTbl)
             endif
             RechargeGwShallowWT = RechargeGwShallowWT - (WatTblDepthOld-WaterTableDepth) *         &
                                   (SoilMoistureSat(IndWatTbl) - SoilMoistureEqui(IndWatTbl))
          else
             WatTblDepthOld      = WaterTableDepth
             ! restore smoi to equilibrium value with water from the ficticious layer below
             ! SoilMoistureToWT  = SoilMoistureToWT - (SoilMoistureEqui(NumSoilLayer)-SoilMoisture(NumSoilLayer))
             ! DrainSoilBot      = DrainSoilBot - 1000 * &
             !                     (SoilMoistureEqui(NumSoilLayer) - SoilMoisture(NumSoilLayer)) * &
             !                     ThicknessSnowSoilLayer(NumSoilLayer) / SoilTimeStep
             ! SoilMoisture(NumSoilLayer) = SoilMoistureEqui(NumSoilLayer)

             ! adjust water table depth in the ficticious layer below
             SoilMoistDeep       = SoilMoistureSat(NumSoilLayer) * (-SoilMatPotentialSat(NumSoilLayer) /          &
                                   (-SoilMatPotentialSat(NumSoilLayer) - ThicknessSnowSoilLayer(NumSoilLayer)))** &
                                   (1.0/SoilExpCoeffB(NumSoilLayer))
             WaterTableDepth     = min((SoilMoistureToWT * ThicknessSnowSoilLayer(NumSoilLayer) -                 &
                                        SoilMoistDeep * DepthSoilLayer0(NumSoilLayer) +                           &
                                        SoilMoistureSat(NumSoilLayer) * (DepthSoilLayer0(NumSoilLayer) -          &
                                        ThicknessSnowSoilLayer(NumSoilLayer))) /                                  &
                                       (SoilMoistureSat(NumSoilLayer)-SoilMoistDeep), DepthSoilLayer0(NumSoilLayer))
             RechargeGwShallowWT = RechargeGwShallowWT - (WatTblDepthOld-WaterTableDepth) *                       &
                                   (SoilMoistureSat(NumSoilLayer) - SoilMoistDeep)
          endif
       endif
    else if ( WaterTableDepth >= (DepthSoilLayer0(NumSoilLayer)-ThicknessSnowSoilLayer(NumSoilLayer)) ) then
    ! if water table depth was already below the bottom of the resolved soil crust
       WatTblDepthOld = WaterTableDepth
       SoilMoistDeep  = SoilMoistureSat(NumSoilLayer) * (-SoilMatPotentialSat(NumSoilLayer) /                     &
                        (-SoilMatPotentialSat(NumSoilLayer) - ThicknessSnowSoilLayer(NumSoilLayer)))**            &
                        (1.0/SoilExpCoeffB(NumSoilLayer))
       if ( SoilMoistureToWT > SoilMoistDeep ) then
          WaterTableDepth = min((SoilMoistureToWT * ThicknessSnowSoilLayer(NumSoilLayer) -                        &
                                 SoilMoistDeep * DepthSoilLayer0(NumSoilLayer) +                                  &
                                 SoilMoistureSat(NumSoilLayer) * (DepthSoilLayer0(NumSoilLayer) -                 &
                                 ThicknessSnowSoilLayer(NumSoilLayer))) /                                         &
                                (SoilMoistureSat(NumSoilLayer)-SoilMoistDeep), DepthSoilLayer0(NumSoilLayer))
          RechargeGwShallowWT = -(WatTblDepthOld-WaterTableDepth) * (SoilMoistureSat(NumSoilLayer)-SoilMoistDeep)
       else
          RechargeGwShallowWT = -(WatTblDepthOld - (DepthSoilLayer0(NumSoilLayer)-ThicknessSnowSoilLayer(NumSoilLayer))) * &
                                 (SoilMoistureSat(NumSoilLayer) - SoilMoistDeep)
          WatTblDepthOld      = DepthSoilLayer0(NumSoilLayer) - ThicknessSnowSoilLayer(NumSoilLayer)
          ! and now even further down
          ThicknessUpLy       = (SoilMoistDeep - SoilMoistureToWT) * ThicknessSnowSoilLayer(NumSoilLayer) /       &
                                (SoilMoistureSat(NumSoilLayer) - SoilMoistDeep)
          WaterTableDepth     = WatTblDepthOld - ThicknessUpLy
          RechargeGwShallowWT = RechargeGwShallowWT - (SoilMoistureSat(NumSoilLayer)-SoilMoistDeep) * ThicknessUpLy
          SoilMoistureToWT    = SoilMoistDeep
       endif
    endif

    if ( (IndAbvWatTbl < NumSoilLayer) .and. (IndAbvWatTbl > 0) ) then
       SoilMoistureToWT = SoilMoistureSat(IndAbvWatTbl)
    else if ( (IndAbvWatTbl < NumSoilLayer) .and. (IndAbvWatTbl <= 0) ) then
       SoilMoistureToWT = SoilMoistureSat(1)
    endif

    ! deallocate local arrays to avoid memory leaks
    deallocate(DepthSoilLayer0)

    end associate

  end subroutine ShallowWaterTableMMF

end module ShallowWaterTableMmfMod
