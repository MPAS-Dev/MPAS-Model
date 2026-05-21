module BiochemVarInTransferMod

!!! Transfer input 2-D NoahmpIO Biochemistry variables to 1-D column variable
!!! 1-D variables should be first defined in /src/BiochemVarType.F90
!!! 2-D variables should be first defined in NoahmpIOVarType.F90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

  use Machine
  use NoahmpIOVarType
  use NoahmpVarType

  implicit none

contains

!=== initialize with input data or table values

  subroutine BiochemVarInTransfer(noahmp, NoahmpIO)

    implicit none

    type(noahmp_type),   intent(inout) :: noahmp
    type(NoahmpIO_type), intent(inout) :: NoahmpIO

! -------------------------------------------------------------------------
    associate(                                                   &
              I            => noahmp%config%domain%GridIndexI   ,&
              VegType      => noahmp%config%domain%VegType      ,&
              CropType     => noahmp%config%domain%CropType     ,&
              OptCropModel => noahmp%config%nmlist%OptCropModel  &
             )
! -------------------------------------------------------------------------

    ! biochem state variables
    noahmp%biochem%state%PlantGrowStage             = NoahmpIO%PGSXY   (I)   
    noahmp%biochem%state%LeafMass                   = NoahmpIO%LFMASSXY(I)
    noahmp%biochem%state%RootMass                   = NoahmpIO%RTMASSXY(I)
    noahmp%biochem%state%StemMass                   = NoahmpIO%STMASSXY(I) 
    noahmp%biochem%state%WoodMass                   = NoahmpIO%WOODXY  (I) 
    noahmp%biochem%state%CarbonMassDeepSoil         = NoahmpIO%STBLCPXY(I) 
    noahmp%biochem%state%CarbonMassShallowSoil      = NoahmpIO%FASTCPXY(I)
    noahmp%biochem%state%GrainMass                  = NoahmpIO%GRAINXY (I)  
    noahmp%biochem%state%GrowDegreeDay              = NoahmpIO%GDDXY   (I)  
    noahmp%biochem%state%NitrogenConcFoliage        = 1.0  ! for now, set to nitrogen saturation

    ! biochem parameter variables
    noahmp%biochem%param%NitrogenConcFoliageMax     = NoahmpIO%FOLNMX_TABLE (VegType)
    noahmp%biochem%param%QuantumEfficiency25C       = NoahmpIO%QE25_TABLE   (VegType)
    noahmp%biochem%param%CarboxylRateMax25C         = NoahmpIO%VCMX25_TABLE (VegType)
    noahmp%biochem%param%CarboxylRateMaxQ10         = NoahmpIO%AVCMX_TABLE  (VegType)
    noahmp%biochem%param%PhotosynPathC3             = NoahmpIO%C3PSN_TABLE  (VegType)
    noahmp%biochem%param%SlopeConductToPhotosyn     = NoahmpIO%MP_TABLE     (VegType)
    noahmp%biochem%param%RespMaintQ10               = NoahmpIO%ARM_TABLE    (VegType)
    noahmp%biochem%param%RespMaintLeaf25C           = NoahmpIO%RMF25_TABLE  (VegType)
    noahmp%biochem%param%RespMaintStem25C           = NoahmpIO%RMS25_TABLE  (VegType)
    noahmp%biochem%param%RespMaintRoot25C           = NoahmpIO%RMR25_TABLE  (VegType)
    noahmp%biochem%param%WoodToRootRatio            = NoahmpIO%WRRAT_TABLE  (VegType)
    noahmp%biochem%param%WoodPoolIndex              = NoahmpIO%WDPOOL_TABLE (VegType)
    noahmp%biochem%param%TurnoverCoeffLeafVeg       = NoahmpIO%LTOVRC_TABLE (VegType)
    noahmp%biochem%param%TemperaureLeafFreeze       = NoahmpIO%TDLEF_TABLE  (VegType)
    noahmp%biochem%param%LeafDeathWaterCoeffVeg     = NoahmpIO%DILEFW_TABLE (VegType)
    noahmp%biochem%param%LeafDeathTempCoeffVeg      = NoahmpIO%DILEFC_TABLE (VegType)
    noahmp%biochem%param%GrowthRespFrac             = NoahmpIO%FRAGR_TABLE  (VegType)
    noahmp%biochem%param%MicroRespCoeff             = NoahmpIO%MRP_TABLE    (VegType)
    noahmp%biochem%param%TemperatureMinPhotosyn     = NoahmpIO%TMIN_TABLE   (VegType)
    noahmp%biochem%param%LeafAreaPerMass1side       = NoahmpIO%SLA_TABLE    (VegType)
    noahmp%biochem%param%StemAreaIndexMin           = NoahmpIO%XSAMIN_TABLE (VegType)
    noahmp%biochem%param%WoodAllocFac               = NoahmpIO%BF_TABLE     (VegType)
    noahmp%biochem%param%WaterStressCoeff           = NoahmpIO%WSTRC_TABLE  (VegType)
    noahmp%biochem%param%LeafAreaIndexMin           = NoahmpIO%LAIMIN_TABLE (VegType)
    noahmp%biochem%param%TurnoverCoeffRootVeg       = NoahmpIO%RTOVRC_TABLE (VegType)
    noahmp%biochem%param%WoodRespCoeff              = NoahmpIO%RSWOODC_TABLE(VegType)
    ! crop model specific parameters
    if ( (OptCropModel > 0) .and. (CropType > 0) ) then
       noahmp%biochem%param%DatePlanting            = NoahmpIO%PLTDAY_TABLE   (CropType)
       noahmp%biochem%param%DateHarvest             = NoahmpIO%HSDAY_TABLE    (CropType)
       noahmp%biochem%param%NitrogenConcFoliageMax  = NoahmpIO%FOLNMXI_TABLE  (CropType)
       noahmp%biochem%param%QuantumEfficiency25C    = NoahmpIO%QE25I_TABLE    (CropType)
       noahmp%biochem%param%CarboxylRateMax25C      = NoahmpIO%VCMX25I_TABLE  (CropType)
       noahmp%biochem%param%CarboxylRateMaxQ10      = NoahmpIO%AVCMXI_TABLE   (CropType)
       noahmp%biochem%param%PhotosynPathC3          = NoahmpIO%C3PSNI_TABLE   (CropType)
       noahmp%biochem%param%SlopeConductToPhotosyn  = NoahmpIO%MPI_TABLE      (CropType)
       noahmp%biochem%param%RespMaintQ10            = NoahmpIO%Q10MR_TABLE    (CropType)
       noahmp%biochem%param%RespMaintLeaf25C        = NoahmpIO%LFMR25_TABLE   (CropType)
       noahmp%biochem%param%RespMaintStem25C        = NoahmpIO%STMR25_TABLE   (CropType)
       noahmp%biochem%param%RespMaintRoot25C        = NoahmpIO%RTMR25_TABLE   (CropType)
       noahmp%biochem%param%GrowthRespFrac          = NoahmpIO%FRA_GR_TABLE   (CropType)
       noahmp%biochem%param%TemperaureLeafFreeze    = NoahmpIO%LEFREEZ_TABLE  (CropType)
       noahmp%biochem%param%LeafAreaPerBiomass      = NoahmpIO%BIO2LAI_TABLE  (CropType)
       noahmp%biochem%param%TempBaseGrowDegDay      = NoahmpIO%GDDTBASE_TABLE (CropType)
       noahmp%biochem%param%TempMaxGrowDegDay       = NoahmpIO%GDDTCUT_TABLE  (CropType)
       noahmp%biochem%param%GrowDegDayEmerg         = NoahmpIO%GDDS1_TABLE    (CropType)
       noahmp%biochem%param%GrowDegDayInitVeg       = NoahmpIO%GDDS2_TABLE    (CropType)
       noahmp%biochem%param%GrowDegDayPostVeg       = NoahmpIO%GDDS3_TABLE    (CropType)
       noahmp%biochem%param%GrowDegDayInitReprod    = NoahmpIO%GDDS4_TABLE    (CropType)
       noahmp%biochem%param%GrowDegDayMature        = NoahmpIO%GDDS5_TABLE    (CropType)
       noahmp%biochem%param%PhotosynRadFrac         = NoahmpIO%I2PAR_TABLE    (CropType)
       noahmp%biochem%param%TempMinCarbonAssim      = NoahmpIO%TASSIM0_TABLE  (CropType)
       noahmp%biochem%param%TempMaxCarbonAssim      = NoahmpIO%TASSIM1_TABLE  (CropType)
       noahmp%biochem%param%TempMaxCarbonAssimMax   = NoahmpIO%TASSIM2_TABLE  (CropType)
       noahmp%biochem%param%CarbonAssimRefMax       = NoahmpIO%AREF_TABLE     (CropType)
       noahmp%biochem%param%LightExtCoeff           = NoahmpIO%K_TABLE        (CropType)
       noahmp%biochem%param%LightUseEfficiency      = NoahmpIO%EPSI_TABLE     (CropType)
       noahmp%biochem%param%CarbonAssimReducFac     = NoahmpIO%PSNRF_TABLE    (CropType)
       noahmp%biochem%param%RespMaintGrain25C       = NoahmpIO%GRAINMR25_TABLE(CropType)
       noahmp%biochem%param%LeafDeathTempCoeffCrop  = NoahmpIO%DILE_FC_TABLE  (CropType,:)
       noahmp%biochem%param%LeafDeathWaterCoeffCrop = NoahmpIO%DILE_FW_TABLE  (CropType,:)
       noahmp%biochem%param%CarbohydrLeafToGrain    = NoahmpIO%LFCT_TABLE     (CropType,:)
       noahmp%biochem%param%CarbohydrStemToGrain    = NoahmpIO%STCT_TABLE     (CropType,:)
       noahmp%biochem%param%CarbohydrRootToGrain    = NoahmpIO%RTCT_TABLE     (CropType,:)
       noahmp%biochem%param%CarbohydrFracToLeaf     = NoahmpIO%LFPT_TABLE     (CropType,:)
       noahmp%biochem%param%CarbohydrFracToStem     = NoahmpIO%STPT_TABLE     (CropType,:)
       noahmp%biochem%param%CarbohydrFracToRoot     = NoahmpIO%RTPT_TABLE     (CropType,:)
       noahmp%biochem%param%CarbohydrFracToGrain    = NoahmpIO%GRAINPT_TABLE  (CropType,:)
       noahmp%biochem%param%TurnoverCoeffLeafCrop   = NoahmpIO%LF_OVRC_TABLE  (CropType,:)
       noahmp%biochem%param%TurnoverCoeffStemCrop   = NoahmpIO%ST_OVRC_TABLE  (CropType,:)
       noahmp%biochem%param%TurnoverCoeffRootCrop   = NoahmpIO%RT_OVRC_TABLE  (CropType,:)

       if ( OptCropModel == 1 ) then
          noahmp%biochem%param%DatePlanting         = NoahmpIO%PLANTING(I)
          noahmp%biochem%param%DateHarvest          = NoahmpIO%HARVEST(I)
          noahmp%biochem%param%GrowDegDayEmerg      = NoahmpIO%SEASON_GDD(I) / 1770.0 * &
                                                      noahmp%biochem%param%GrowDegDayEmerg
          noahmp%biochem%param%GrowDegDayInitVeg    = NoahmpIO%SEASON_GDD(I) / 1770.0 * &
                                                      noahmp%biochem%param%GrowDegDayInitVeg
          noahmp%biochem%param%GrowDegDayPostVeg    = NoahmpIO%SEASON_GDD(I) / 1770.0 * &
                                                      noahmp%biochem%param%GrowDegDayPostVeg
          noahmp%biochem%param%GrowDegDayInitReprod = NoahmpIO%SEASON_GDD(I) / 1770.0 * &
                                                      noahmp%biochem%param%GrowDegDayInitReprod
          noahmp%biochem%param%GrowDegDayMature     = NoahmpIO%SEASON_GDD(I) / 1770.0 * &
                                                      noahmp%biochem%param%GrowDegDayMature
        endif
    endif ! activate crop parameters

    if ( noahmp%config%nmlist%OptIrrigation == 2 ) then
       noahmp%biochem%param%DatePlanting = NoahmpIO%PLANTING(I)
       noahmp%biochem%param%DateHarvest  = NoahmpIO%HARVEST (I)
    endif
    
    end associate

  end subroutine BiochemVarInTransfer

end module BiochemVarInTransferMod
