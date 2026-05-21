module IrrigationTriggerMod

!!! Trigger irrigation if soil moisture less than the management allowable deficit (MAD)
!!! and estimate irrigation water depth [m] using current rootzone soil moisture and field 
!!! capacity. There are two options here to trigger the irrigation scheme based on MAD
!!! OptIrrigation = 1 -> if irrigated fraction > threshold fraction
!!! OptIrrigation = 2 -> if irrigated fraction > threshold fraction and within crop season
!!! OptIrrigation = 3 -> if irrigated fraction > threshold fraction and LeafAreaIndex > threshold LeafAreaIndex

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine IrrigationTrigger(noahmp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: TRIGGER_IRRIGATION
! Original code: P. Valayamkunnath (NCAR) <prasanth@ucar.edu> (08/06/2020)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    logical                          :: FlagIrri           ! flag for irrigation activation
    integer                          :: LoopInd            ! loop index  
    real(kind=kind_noahmp)           :: SoilMoistAvail     ! available soil moisture [m] at timestep
    real(kind=kind_noahmp)           :: SoilMoistAvailMax  ! maximum available moisture [m]
    real(kind=kind_noahmp)           :: IrrigationWater    ! irrigation water amount [m]

! --------------------------------------------------------------------
    associate(                                                                       &
              DepthSoilLayer          => noahmp%config%domain%DepthSoilLayer        ,& ! in,    depth [m] of layer-bottom from soil surface
              DayJulianInYear         => noahmp%config%domain%DayJulianInYear       ,& ! in,    Julian day of the year
              OptIrrigation           => noahmp%config%nmlist%OptIrrigation         ,& ! in,    irrigation option
              OptIrrigationMethod     => noahmp%config%nmlist%OptIrrigationMethod   ,& ! in,    irrigation method option
              DatePlanting            => noahmp%biochem%param%DatePlanting          ,& ! in,    Planting day (day of year)
              DateHarvest             => noahmp%biochem%param%DateHarvest           ,& ! in,    Harvest date (day of year)
              SoilMoistureWilt        => noahmp%water%param%SoilMoistureWilt        ,& ! in,    wilting point soil moisture [m3/m3]
              SoilMoistureFieldCap    => noahmp%water%param%SoilMoistureFieldCap    ,& ! in,    reference soil moisture (field capacity) (m3/m3)
              NumSoilLayerRoot        => noahmp%water%param%NumSoilLayerRoot        ,& ! in,    number of soil layers with root present
              IrriStopDayBfHarvest    => noahmp%water%param%IrriStopDayBfHarvest    ,& ! in,    number of days before harvest date to stop irrigation
              IrriTriggerLaiMin       => noahmp%water%param%IrriTriggerLaiMin       ,& ! in,    minimum lai to trigger irrigation
              SoilWatDeficitAllow     => noahmp%water%param%SoilWatDeficitAllow     ,& ! in,    management allowable deficit (0-1)
              IrriFloodLossFrac       => noahmp%water%param%IrriFloodLossFrac       ,& ! in,    factor of flood irrigation loss
              VegFrac                 => noahmp%energy%state%VegFrac                ,& ! in,    greeness vegetation fraction
              LeafAreaIndex           => noahmp%energy%state%LeafAreaIndex          ,& ! in,    leaf area index [m2/m2]
              IrrigationFracGrid      => noahmp%water%state%IrrigationFracGrid      ,& ! in,    irrigated area fraction of a grid
              SoilLiqWater            => noahmp%water%state%SoilLiqWater            ,& ! in,    soil water content [m3/m3]
              IrrigationFracMicro     => noahmp%water%state%IrrigationFracMicro     ,& ! in,    fraction of grid under micro irrigation (0 to 1)
              IrrigationFracFlood     => noahmp%water%state%IrrigationFracFlood     ,& ! in,    fraction of grid under flood irrigation (0 to 1)
              IrrigationFracSprinkler => noahmp%water%state%IrrigationFracSprinkler ,& ! in,    sprinkler irrigation fraction (0 to 1)
              IrrigationAmtMicro      => noahmp%water%state%IrrigationAmtMicro      ,& ! inout, irrigation water amount [m] to be applied, Micro
              IrrigationAmtFlood      => noahmp%water%state%IrrigationAmtFlood      ,& ! inout, irrigation water amount [m] to be applied, Flood
              IrrigationAmtSprinkler  => noahmp%water%state%IrrigationAmtSprinkler  ,& ! inout, irrigation water amount [m] to be applied, Sprinkler
              IrrigationCntSprinkler  => noahmp%water%state%IrrigationCntSprinkler  ,& ! inout, irrigation event number, Sprinkler
              IrrigationCntMicro      => noahmp%water%state%IrrigationCntMicro      ,& ! inout, irrigation event number, Micro
              IrrigationCntFlood      => noahmp%water%state%IrrigationCntFlood       & ! inout, irrigation event number, Flood
             )
! ----------------------------------------------------------------------

    FlagIrri = .true.

    ! check if irrigation is can be activated or not
    if ( OptIrrigation == 2 ) then ! activate irrigation if within crop season
       if ( (DayJulianInYear < DatePlanting) .or. (DayJulianInYear > (DateHarvest-IrriStopDayBfHarvest)) ) &
          FlagIrri = .false.
    elseif ( OptIrrigation == 3) then ! activate if LeafAreaIndex > threshold LeafAreaIndex
       if ( LeafAreaIndex < IrriTriggerLaiMin) FlagIrri = .false.
    elseif ( (OptIrrigation > 3) .or. (OptIrrigation < 1) ) then
       FlagIrri = .false.
    endif

    if ( FlagIrri .eqv. .true. ) then
       ! estimate available water and field capacity for the root zone
       SoilMoistAvail      = 0.0
       SoilMoistAvailMax   = 0.0
       SoilMoistAvail      = (SoilLiqWater(1) - SoilMoistureWilt(1)) * (-1.0) * DepthSoilLayer(1)          ! current soil water (m) 
       SoilMoistAvailMax   = (SoilMoistureFieldCap(1) - SoilMoistureWilt(1)) * (-1.0) * DepthSoilLayer(1)  ! available water (m)
       do LoopInd = 2, NumSoilLayerRoot
         SoilMoistAvail    = SoilMoistAvail + (SoilLiqWater(LoopInd) - SoilMoistureWilt(LoopInd)) * &
                                              (DepthSoilLayer(LoopInd-1) - DepthSoilLayer(LoopInd))
         SoilMoistAvailMax = SoilMoistAvailMax + (SoilMoistureFieldCap(LoopInd) - SoilMoistureWilt(LoopInd)) * &
                                                 (DepthSoilLayer(LoopInd-1) - DepthSoilLayer(LoopInd))
       enddo

      ! check if root zone soil moisture < SoilWatDeficitAllow (calibratable)
      if ( (SoilMoistAvail/SoilMoistAvailMax) <= SoilWatDeficitAllow ) then
         ! amount of water need to be added to bring soil moisture back to 
         ! field capacity, i.e., irrigation water amount (m)
         IrrigationWater = (SoilMoistAvailMax - SoilMoistAvail) * IrrigationFracGrid * VegFrac

         ! sprinkler irrigation amount (m) based on 2D IrrigationFracSprinkler
         if ( (IrrigationAmtSprinkler == 0.0) .and. (IrrigationFracSprinkler > 0.0) .and. (OptIrrigationMethod == 0) ) then
            IrrigationAmtSprinkler = IrrigationFracSprinkler * IrrigationWater
            IrrigationCntSprinkler = IrrigationCntSprinkler + 1
         ! sprinkler irrigation amount (m) based on namelist choice
         elseif ( (IrrigationAmtSprinkler == 0.0) .and. (OptIrrigationMethod == 1) ) then
            IrrigationAmtSprinkler = IrrigationWater
            IrrigationCntSprinkler = IrrigationCntSprinkler + 1
         endif

         ! micro irrigation amount (m) based on 2D IrrigationFracMicro
         if ( (IrrigationAmtMicro == 0.0) .and. (IrrigationFracMicro > 0.0) .and. (OptIrrigationMethod == 0) ) then
            IrrigationAmtMicro = IrrigationFracMicro * IrrigationWater
            IrrigationCntMicro = IrrigationCntMicro + 1
         ! micro irrigation amount (m) based on namelist choice
         elseif ( (IrrigationAmtMicro == 0.0) .and. (OptIrrigationMethod == 2) ) then
            IrrigationAmtMicro = IrrigationWater
            IrrigationCntMicro = IrrigationCntMicro + 1
         endif

         ! flood irrigation amount (m): Assumed to saturate top two layers and 
         ! third layer to FC. As water moves from one end of the field to
         ! another, surface layers will be saturated. 
         ! flood irrigation amount (m) based on 2D IrrigationFracFlood
         if ( (IrrigationAmtFlood == 0.0) .and. (IrrigationFracFlood > 0.0) .and. (OptIrrigationMethod == 0) ) then
            IrrigationAmtFlood = IrrigationFracFlood * IrrigationWater * (1.0/(1.0 - IrriFloodLossFrac))
            IrrigationCntFlood = IrrigationCntFlood + 1
         !flood irrigation amount (m) based on namelist choice
         elseif ( (IrrigationAmtFlood == 0.0) .and. (OptIrrigationMethod == 3) ) then
            IrrigationAmtFlood = IrrigationWater * (1.0/(1.0 - IrriFloodLossFrac))
            IrrigationCntFlood = IrrigationCntFlood + 1
         endif
      else
         IrrigationWater        = 0.0
         IrrigationAmtSprinkler = 0.0
         IrrigationAmtMicro     = 0.0
         IrrigationAmtFlood     = 0.0
      endif

    endif

    end associate

  end subroutine IrrigationTrigger

end module IrrigationTriggerMod
