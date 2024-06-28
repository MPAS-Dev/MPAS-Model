module CropGrowDegreeDayMod

!!! Compute crop growing degree days

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine CropGrowDegreeDay(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: GROWING_GDD 
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath & refactor team (He et al. 2023)
! -------------------------------------------------------------------------
        
    implicit none
       
    type(noahmp_type), intent(inout) :: noahmp

! local variables
    real(kind=kind_noahmp)           :: GrowDegDayCnt      ! gap bewtween GrowDegreeDay and GrowDegreeDay8
    real(kind=kind_noahmp)           :: TemperatureDiff    ! temperature difference for growing degree days calculation
    real(kind=kind_noahmp)           :: TemperatureAirC    ! air temperature degC

!------------------------------------------------------------------------
    associate(                                                                   &
              MainTimeStep         => noahmp%config%domain%MainTimeStep         ,& ! in,    main noahmp timestep [s]
              DayJulianInYear      => noahmp%config%domain%DayJulianInYear      ,& ! in,    Julian day of year
              TemperatureAir2m     => noahmp%energy%state%TemperatureAir2m      ,& ! in,    2-m air temperature [K]
              DatePlanting         => noahmp%biochem%param%DatePlanting         ,& ! in,    Planting day (day of year)
              DateHarvest          => noahmp%biochem%param%DateHarvest          ,& ! in,    Harvest date (day of year)
              TempBaseGrowDegDay   => noahmp%biochem%param%TempBaseGrowDegDay   ,& ! in,    Base temperature for grow degree day accumulation [C]
              TempMaxGrowDegDay    => noahmp%biochem%param%TempMaxGrowDegDay    ,& ! in,    Max temperature for grow degree day accumulation [C]
              GrowDegDayEmerg      => noahmp%biochem%param%GrowDegDayEmerg      ,& ! in,    grow degree day from seeding to emergence
              GrowDegDayInitVeg    => noahmp%biochem%param%GrowDegDayInitVeg    ,& ! in,    grow degree day from seeding to initial vegetative
              GrowDegDayPostVeg    => noahmp%biochem%param%GrowDegDayPostVeg    ,& ! in,    grow degree day from seeding to post vegetative
              GrowDegDayInitReprod => noahmp%biochem%param%GrowDegDayInitReprod ,& ! in,    grow degree day from seeding to intial reproductive
              GrowDegDayMature     => noahmp%biochem%param%GrowDegDayMature     ,& ! in,    grow degree day from seeding to physical maturity
              GrowDegreeDay        => noahmp%biochem%state%GrowDegreeDay        ,& ! inout, crop growing degree days
              IndexPlanting        => noahmp%biochem%state%IndexPlanting        ,& ! out,   Planting index index (0=off, 1=on)
              IndexHarvest         => noahmp%biochem%state%IndexHarvest         ,& ! out,   Havest index (0=on,1=off) 
              PlantGrowStage       => noahmp%biochem%state%PlantGrowStage        & ! out,   Plant growth stage (1=S1,2=S2,3=S3)
             )
!------------------------------------------------------------------------

    ! initialize
    TemperatureAirC = TemperatureAir2m - 273.15

    ! Planting and Havest index
    IndexPlanting = 1  ! planting on
    IndexHarvest  = 1  ! harvest off

    ! turn on/off the planting 
    if ( DayJulianInYear < DatePlanting ) IndexPlanting = 0   ! planting off
        
    ! turn on/off the harvesting
    if ( DayJulianInYear >= DateHarvest ) IndexHarvest  = 0   ! harvest on            

    ! Calculate the growing degree days               
    if ( TemperatureAirC < TempBaseGrowDegDay ) then
       TemperatureDiff = 0.0
    elseif ( TemperatureAirC >= TempMaxGrowDegDay ) then
       TemperatureDiff = TempMaxGrowDegDay - TempBaseGrowDegDay
    else
       TemperatureDiff = TemperatureAirC - TempBaseGrowDegDay
    endif
    GrowDegreeDay = (GrowDegreeDay + TemperatureDiff * MainTimeStep / 86400.0) * IndexPlanting * IndexHarvest
    GrowDegDayCnt = GrowDegreeDay
      
    ! Decide corn growth stage, based on Hybrid-Maize 
    ! PlantGrowStage    = 1 : Before planting
    ! PlantGrowStage    = 2 : from tassel initiation to silking
    ! PlantGrowStage    = 3 : from silking to effective grain filling
    ! PlantGrowStage    = 4 : from effective grain filling to pysiological maturity 
    ! PlantGrowStage    = 5 : GrowDegDayMax=1389
    ! PlantGrowStage    = 6 :
    ! PlantGrowStage    = 7 :
    ! PlantGrowStage    = 8 :
    ! GrowDegDayMax     = 1389
    ! GrowDegDayMax     = 1555
    ! GrowDegDayTmp     = 0.41 * GrowDegDayMax + 145.4 + 150 ! from hybrid-maize 
    ! GrowDegDayEmerg   = ((GrowDegDayTmp - 96) / 38.9 - 4) * 21
    ! GrowDegDayEmerg   = 0.77 * GrowDegDayTmp
    ! GrowDegDayPostVeg = GrowDegDayTmp + 170
    ! GrowDegDayPostVeg = 170

    ! compute plant growth stage
    PlantGrowStage = 1   ! MB: set PlantGrowStage = 1 (for initialization during growing season when no GDD)  
    if ( GrowDegDayCnt > 0.0 )                   PlantGrowStage = 2
    if ( GrowDegDayCnt >= GrowDegDayEmerg )      PlantGrowStage = 3
    if ( GrowDegDayCnt >= GrowDegDayInitVeg )    PlantGrowStage = 4 
    if ( GrowDegDayCnt >= GrowDegDayPostVeg )    PlantGrowStage = 5
    if ( GrowDegDayCnt >= GrowDegDayInitReprod ) PlantGrowStage = 6
    if ( GrowDegDayCnt >= GrowDegDayMature )     PlantGrowStage = 7
    if ( DayJulianInYear >= DateHarvest )        PlantGrowStage = 8
    if ( DayJulianInYear < DatePlanting )        PlantGrowStage = 1   

    end associate

  end subroutine CropGrowDegreeDay

end module CropGrowDegreeDayMod
