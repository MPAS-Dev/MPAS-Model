module BiochemCropMainMod

!!! Main Biogeochemistry module for dynamic crop (as opposed to natural vegetation)
!!! currently only include carbon processes (RE Dickinson et al.(1998) and Liu et al., 2014))
 
  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use CarbonFluxCropMod,     only : CarbonFluxCrop
  use CropGrowDegreeDayMod,  only : CropGrowDegreeDay
  use CropPhotosynthesisMod, only : CropPhotosynthesis
        
  implicit none
        
contains
     
  subroutine BiochemCropMain(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: CARBON_CROP
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Modified by Xing Liu, 2014
! Refactered code: C. He, P. Valayamkunnath & refactor team (He et al. 2023)
! -------------------------------------------------------------------------
        
    implicit none
        
    type(noahmp_type), intent(inout) :: noahmp
    
! local variables
    integer                          :: LoopInd      ! loop index
    
!-------------------------------------------------------------------------
    associate(                                                                       &
              VegType                => noahmp%config%domain%VegType                ,& ! in,    vegetation type
              DepthSoilLayer         => noahmp%config%domain%DepthSoilLayer         ,& ! in,    depth [m] of layer-bottom from soil surface
              ThicknessSnowSoilLayer => noahmp%config%domain%ThicknessSnowSoilLayer ,& ! in,    snow/soil layer thickness [m]
              IndexWaterPoint        => noahmp%config%domain%IndexWaterPoint        ,& ! in,    water point flag
              IndexIcePoint          => noahmp%config%domain%IndexIcePoint          ,& ! in,    land ice flag
              IndexBarrenPoint       => noahmp%config%domain%IndexBarrenPoint       ,& ! in,    bare soil flag
              FlagUrban              => noahmp%config%domain%FlagUrban              ,& ! in,    urban point flag
              NumSoilLayerRoot       => noahmp%water%param%NumSoilLayerRoot         ,& ! in,    number of soil layers with root present
              SoilMoistureSat        => noahmp%water%param%SoilMoistureSat          ,& ! in,    saturated value of soil moisture [m3/m3]
              SoilMoisture           => noahmp%water%state%SoilMoisture             ,& ! in,    soil moisture (ice + liq.) [m3/m3]
              SoilTranspFacAcc       => noahmp%water%state%SoilTranspFacAcc         ,& ! in,    accumulated soil water transpiration factor (0 to 1)
              LeafMass               => noahmp%biochem%state%LeafMass               ,& ! inout, leaf mass [g/m2]
              RootMass               => noahmp%biochem%state%RootMass               ,& ! inout, mass of fine roots [g/m2]
              StemMass               => noahmp%biochem%state%StemMass               ,& ! inout, stem mass [g/m2]
              WoodMass               => noahmp%biochem%state%WoodMass               ,& ! inout, mass of wood (incl. woody roots) [g/m2]
              CarbonMassDeepSoil     => noahmp%biochem%state%CarbonMassDeepSoil     ,& ! inout, stable carbon in deep soil [g/m2]
              CarbonMassShallowSoil  => noahmp%biochem%state%CarbonMassShallowSoil  ,& ! inout, short-lived carbon in shallow soil [g/m2]
              LeafAreaIndex          => noahmp%energy%state%LeafAreaIndex           ,& ! inout, leaf area index
              StemAreaIndex          => noahmp%energy%state%StemAreaIndex           ,& ! inout, stem area index
              GrossPriProduction     => noahmp%biochem%flux%GrossPriProduction      ,& ! out,   net instantaneous assimilation [g/m2/s C]
              NetPriProductionTot    => noahmp%biochem%flux%NetPriProductionTot     ,& ! out,   net primary productivity [g/m2/s C]
              NetEcoExchange         => noahmp%biochem%flux%NetEcoExchange          ,& ! out,   net ecosystem exchange [g/m2/s CO2]
              RespirationPlantTot    => noahmp%biochem%flux%RespirationPlantTot     ,& ! out,   total plant respiration [g/m2/s C]
              RespirationSoilOrg     => noahmp%biochem%flux%RespirationSoilOrg      ,& ! out,   soil organic respiration [g/m2/s C]
              CarbonMassSoilTot      => noahmp%biochem%state%CarbonMassSoilTot      ,& ! out,   total soil carbon [g/m2 C]
              CarbonMassLiveTot      => noahmp%biochem%state%CarbonMassLiveTot      ,& ! out,   total living carbon ([g/m2 C]
              GrainMass              => noahmp%biochem%state%GrainMass              ,& ! out,   mass of grain [g/m2] 
              SoilWaterRootZone      => noahmp%water%state%SoilWaterRootZone        ,& ! out,   root zone soil water
              SoilWaterStress        => noahmp%water%state%SoilWaterStress           & ! out,   water stress coeficient (1.0 for wilting)
             ) 
!------------------------------------------------------------------------

    ! initialize
    NetEcoExchange      = 0.0
    NetPriProductionTot = 0.0
    GrossPriProduction  = 0.0

    ! no biogeochemistry in non-vegetated points
    if ( (VegType == IndexWaterPoint) .or. (VegType == IndexBarrenPoint) .or. &
         (VegType == IndexIcePoint  ) .or. (FlagUrban .eqv. .true.) ) then
       LeafAreaIndex         = 0.0
       StemAreaIndex         = 0.0
       GrossPriProduction    = 0.0
       NetPriProductionTot   = 0.0
       NetEcoExchange        = 0.0
       RespirationPlantTot   = 0.0
       RespirationSoilOrg    = 0.0
       CarbonMassSoilTot     = 0.0
       CarbonMassLiveTot     = 0.0
       LeafMass              = 0.0
       RootMass              = 0.0
       StemMass              = 0.0
       WoodMass              = 0.0
       CarbonMassDeepSoil    = 0.0
       CarbonMassShallowSoil = 0.0
       GrainMass             = 0.0
       return
    endif
    
    ! start biogeochemistry process
    ! water stress
    SoilWaterStress   = 1.0 - SoilTranspFacAcc
    SoilWaterRootZone = 0.0
    do LoopInd = 1, NumSoilLayerRoot
       SoilWaterRootZone = SoilWaterRootZone + SoilMoisture(LoopInd) / SoilMoistureSat(LoopInd) * &
                                               ThicknessSnowSoilLayer(LoopInd) / (-DepthSoilLayer(NumSoilLayerRoot))
    enddo

    ! start crop carbon process
    ! Note: The following CropPhotosynthesis is not used currently. 
    ! Photosynthesis rate is directly from calculations in the energy part (similar to the treatment in CARBON subroutine)    

    !call CropPhotosynthesis(noahmp)
    call CropGrowDegreeDay(noahmp)
    call CarbonFluxCrop(noahmp)
    
    end associate
    
  end subroutine BiochemCropMain
    
end module BiochemCropMainMod
