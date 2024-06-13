module ConfigVarInTransferMod

!!! Transfer input 2-D NoahmpIO Configuration variables to 1-D column variable
!!! 1-D variables should be first defined in /src/ConfigVarType.F90
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

!=== initialize with input/restart data or table values

  subroutine ConfigVarInTransfer(noahmp, NoahmpIO)

    implicit none

    type(NoahmpIO_type) , intent(inout) :: NoahmpIO
    type(noahmp_type),    intent(inout) :: noahmp

! --------------------------------------------------------------------- 
    associate(                                      &
              I               => NoahmpIO%I        ,&
              NumSnowLayerMax => NoahmpIO%NSNOW    ,&
              NumSoilLayer    => NoahmpIO%NSOIL     &
             )
! ---------------------------------------------------------------------

    ! config namelist variable
    noahmp%config%nmlist%OptDynamicVeg               = NoahmpIO%IOPT_DVEG
    noahmp%config%nmlist%OptRainSnowPartition        = NoahmpIO%IOPT_SNF
    noahmp%config%nmlist%OptSoilWaterTranspiration   = NoahmpIO%IOPT_BTR
    noahmp%config%nmlist%OptGroundResistanceEvap     = NoahmpIO%IOPT_RSF
    noahmp%config%nmlist%OptSurfaceDrag              = NoahmpIO%IOPT_SFC
    noahmp%config%nmlist%OptStomataResistance        = NoahmpIO%IOPT_CRS
    noahmp%config%nmlist%OptSnowAlbedo               = NoahmpIO%IOPT_ALB
    noahmp%config%nmlist%OptCanopyRadiationTransfer  = NoahmpIO%IOPT_RAD
    noahmp%config%nmlist%OptSnowSoilTempTime         = NoahmpIO%IOPT_STC
    noahmp%config%nmlist%OptSnowThermConduct         = NoahmpIO%IOPT_TKSNO
    noahmp%config%nmlist%OptSoilTemperatureBottom    = NoahmpIO%IOPT_TBOT
    noahmp%config%nmlist%OptSoilSupercoolWater       = NoahmpIO%IOPT_FRZ
    noahmp%config%nmlist%OptSoilPermeabilityFrozen   = NoahmpIO%IOPT_INF
    noahmp%config%nmlist%OptDynVicInfiltration       = NoahmpIO%IOPT_INFDV
    noahmp%config%nmlist%OptTileDrainage             = NoahmpIO%IOPT_TDRN
    noahmp%config%nmlist%OptIrrigation               = NoahmpIO%IOPT_IRR
    noahmp%config%nmlist%OptIrrigationMethod         = NoahmpIO%IOPT_IRRM
    noahmp%config%nmlist%OptCropModel                = NoahmpIO%IOPT_CROP
    noahmp%config%nmlist%OptSoilProperty             = NoahmpIO%IOPT_SOIL
    noahmp%config%nmlist%OptPedotransfer             = NoahmpIO%IOPT_PEDO
    noahmp%config%nmlist%OptRunoffSurface            = NoahmpIO%IOPT_RUNSRF
    noahmp%config%nmlist%OptRunoffSubsurface         = NoahmpIO%IOPT_RUNSUB
    noahmp%config%nmlist%OptGlacierTreatment         = NoahmpIO%IOPT_GLA

    ! config domain variable
    noahmp%config%domain%SurfaceType                 = 1
    noahmp%config%domain%NumSwRadBand                = 2
    noahmp%config%domain%SoilColor                   = 4
    noahmp%config%domain%NumCropGrowStage            = 8
    noahmp%config%domain%FlagSoilProcess             = NoahmpIO%calculate_soil
    noahmp%config%domain%NumSoilTimeStep             = NoahmpIO%soil_update_steps
    noahmp%config%domain%NumSnowLayerMax             = NoahmpIO%NSNOW
    noahmp%config%domain%NumSnowLayerNeg             = NoahmpIO%ISNOWXY(I)
    noahmp%config%domain%NumSoilLayer                = NoahmpIO%NSOIL
    noahmp%config%domain%GridIndexI                  = NoahmpIO%I
    noahmp%config%domain%GridIndexJ                  = NoahmpIO%J
    noahmp%config%domain%MainTimeStep                = NoahmpIO%DTBL
    noahmp%config%domain%SoilTimeStep                = NoahmpIO%DTBL * NoahmpIO%soil_update_steps
    noahmp%config%domain%GridSize                    = NoahmpIO%DX
    noahmp%config%domain%LandUseDataName             = NoahmpIO%LLANDUSE
    noahmp%config%domain%VegType                     = NoahmpIO%IVGTYP(I)
    noahmp%config%domain%CropType                    = NoahmpIO%CROPCAT(I)
    noahmp%config%domain%IndicatorIceSfc             = NoahmpIO%ICE
    noahmp%config%domain%DayJulianInYear             = NoahmpIO%JULIAN
    noahmp%config%domain%NumDayInYear                = NoahmpIO%YEARLEN
    noahmp%config%domain%Latitude                    = NoahmpIO%XLAT(I)
    noahmp%config%domain%RefHeightAboveSfc           = NoahmpIO%DZ8W(I,1)*0.5
    noahmp%config%domain%ThicknessAtmosBotLayer      = NoahmpIO%DZ8W(I,1)
    noahmp%config%domain%CosSolarZenithAngle         = NoahmpIO%COSZEN(I) 
    noahmp%config%domain%IndexWaterPoint             = NoahmpIO%ISWATER_TABLE
    noahmp%config%domain%IndexBarrenPoint            = NoahmpIO%ISBARREN_TABLE
    noahmp%config%domain%IndexIcePoint               = NoahmpIO%ISICE_TABLE
    noahmp%config%domain%IndexCropPoint              = NoahmpIO%ISCROP_TABLE
    noahmp%config%domain%IndexEBLForest              = NoahmpIO%EBLFOREST_TABLE
    noahmp%config%domain%RunoffSlopeType             = NoahmpIO%SLOPETYP
    noahmp%config%domain%DepthSoilTempBottom         = NoahmpIO%ZBOT_TABLE

    ! the following initialization cannot be done in ConfigVarInitMod
    ! because the NumSoilLayer and NumSnowLayerMax are initialized with input values in this module
    if ( .not. allocated(noahmp%config%domain%DepthSoilLayer) )          &
       allocate( noahmp%config%domain%DepthSoilLayer(1:NumSoilLayer) )
    if ( .not. allocated(noahmp%config%domain%ThicknessSoilLayer) )      &
       allocate( noahmp%config%domain%ThicknessSoilLayer(1:NumSoilLayer) )
    if ( .not. allocated(noahmp%config%domain%SoilType) )                &
       allocate( noahmp%config%domain%SoilType(1:NumSoilLayer) )
    if ( .not. allocated(noahmp%config%domain%ThicknessSnowSoilLayer) )  &
       allocate( noahmp%config%domain%ThicknessSnowSoilLayer(-NumSnowLayerMax+1:NumSoilLayer) )
    if ( .not. allocated(noahmp%config%domain%DepthSnowSoilLayer) )      &
       allocate( noahmp%config%domain%DepthSnowSoilLayer(-NumSnowLayerMax+1:NumSoilLayer) )
    
    noahmp%config%domain%SoilType              (:)   = undefined_int
    noahmp%config%domain%DepthSoilLayer        (:)   = undefined_real
    noahmp%config%domain%ThicknessSoilLayer    (:)   = undefined_real
    noahmp%config%domain%ThicknessSnowSoilLayer(:)   = undefined_real
    noahmp%config%domain%DepthSnowSoilLayer    (:)   = undefined_real

    if ( noahmp%config%nmlist%OptSoilProperty == 1 ) then
       noahmp%config%domain%SoilType(1:NumSoilLayer) = NoahmpIO%ISLTYP(I)  ! soil type same in all layers
    elseif ( noahmp%config%nmlist%OptSoilProperty == 2 ) then
       noahmp%config%domain%SoilType(1) = nint(NoahmpIO%SOILCL1(I))        ! soil type in layer1
       noahmp%config%domain%SoilType(2) = nint(NoahmpIO%SOILCL2(I))        ! soil type in layer2
       noahmp%config%domain%SoilType(3) = nint(NoahmpIO%SOILCL3(I))        ! soil type in layer3
       noahmp%config%domain%SoilType(4) = nint(NoahmpIO%SOILCL4(I))        ! soil type in layer4
    elseif ( noahmp%config%nmlist%OptSoilProperty == 3 ) then
       noahmp%config%domain%SoilType(1:NumSoilLayer) = NoahmpIO%ISLTYP(I)  ! to initialize with default
    endif 
       
    noahmp%config%domain%DepthSoilLayer(1:NumSoilLayer) = NoahmpIO%ZSOIL(1:NumSoilLayer)
    noahmp%config%domain%DepthSnowSoilLayer(-NumSnowLayerMax+1:NumSoilLayer) = &
                         NoahmpIO%ZSNSOXY(I,-NumSnowLayerMax+1:NumSoilLayer)

    ! treatment for urban point
    if ( (NoahmpIO%IVGTYP(I) == NoahmpIO%ISURBAN_TABLE) .or. (NoahmpIO%IVGTYP(I) > NoahmpIO%URBTYPE_beg) ) then
       noahmp%config%domain%FlagUrban = .true. 
       if(NoahmpIO%SF_URBAN_PHYSICS == 0 ) then
           noahmp%config%domain%VegType = NoahmpIO%ISURBAN_TABLE
       else
           noahmp%config%domain%VegType = NoahmpIO%NATURAL_TABLE  ! set urban vegetation type based on table natural
           NoahmpIO%GVFMAX(I)         = 0.96 * 100.0            ! unit: %
       endif         
    endif

    ! treatment for crop point
    noahmp%config%domain%CropType = 0
    if ( (NoahmpIO%IOPT_CROP > 0) .and. (NoahmpIO%IVGTYP(I) == NoahmpIO%ISCROP_TABLE) ) &
       noahmp%config%domain%CropType = NoahmpIO%DEFAULT_CROP_TABLE   
       
    if ( (NoahmpIO%IOPT_CROP > 0) .and. (NoahmpIO%CROPCAT(I) > 0) ) then
       noahmp%config%domain%CropType = NoahmpIO%CROPCAT(I)
       noahmp%config%domain%VegType  = NoahmpIO%ISCROP_TABLE
       NoahmpIO%VEGFRA(I)          = 0.95 * 100.0              ! unit: %
       NoahmpIO%GVFMAX(I)          = 0.95 * 100.0              ! unit: %
    endif

    ! correct inconsistent soil type
    if ( any(noahmp%config%domain%SoilType == 14) .and. (NoahmpIO%XICE(I) == 0.0) ) then
       write(*,*) "SOIL TYPE FOUND TO BE WATER AT A LAND-POINT"
       write(*,*) "RESET SOIL type to SANDY CLAY LOAM at grid = ", I
       noahmp%config%domain%SoilType = 7
    endif

    ! set warning message for inconsistent surface and subsurface runoff option
    ! for now, only the same options for surface and subsurface runoff have been tested
    if ( noahmp%config%nmlist%OptRunoffSurface /= noahmp%config%nmlist%OptRunoffSubsurface ) then
       write(*,*) "Warning: Surface and subsurface runoff options are inconsistent! They may be incompatible!"
       write(*,*) "Warning: Currently only the same options for surface and subsurface runoff are tested."
    endif

    end associate

  end subroutine ConfigVarInTransfer

end module ConfigVarInTransferMod
