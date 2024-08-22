module RunoffSurfaceDynamicVicMod

!!! Compuate inflitration rate at soil surface and estimate surface runoff based on dynamic VIC scheme
!!! Reference: Liang, X., & Xie, Z. (2001). A new surface runoff parameterization with subgrid-scale
!!! soil heterogeneity for land surface models. Advances in Water Resources, 24(9-10), 1173-1193.

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use SoilWaterInfilPhilipMod,          only : SoilWaterInfilPhilip
  use SoilWaterInfilGreenAmptMod,       only : SoilWaterInfilGreenAmpt
  use SoilWaterInfilSmithParlangeMod,   only : SoilWaterInfilSmithParlange
  use RunoffSurfaceExcessDynamicVicMod

  implicit none

contains

  subroutine RunoffSurfaceDynamicVic(noahmp, TimeStep, InfilRateAcc)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: DYNAMIC_VIC
! Original code: Prasanth Valayamkunnath <prasanth@ucar.edu>
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

     implicit none

! in & out variabls
     type(noahmp_type)     , intent(inout) :: noahmp
     real(kind=kind_noahmp), intent(in)    :: TimeStep           ! timestep (may not be the same as model timestep)
     real(kind=kind_noahmp), intent(inout) :: InfilRateAcc       ! accumulated infiltration rate (m/s)

! local variable
     integer                               :: IndIter            ! iteration index
     integer                               :: NumIter            ! number of interation
     integer                               :: IndInfilMax        ! index to check maximum infiltration at SoilMoistureWilt
     real(kind=kind_noahmp)                :: InfilExpB          ! B parameter for infiltration scaling curve
     real(kind=kind_noahmp)                :: WaterDepthTop      ! actual water depth in top layers [m]
     real(kind=kind_noahmp)                :: WaterDepthSatTop   ! saturated water depth in top layers [m]
     real(kind=kind_noahmp)                :: WaterInSoilSfc     ! water input on soil surface [m]
     real(kind=kind_noahmp)                :: WaterDepthInit     ! initial water depth [m]
     real(kind=kind_noahmp)                :: WaterDepthMax      ! maximum water depth [m]
     real(kind=kind_noahmp)                :: InfilSfcTmp        ! surface infiltration rate [m/s]
     real(kind=kind_noahmp)                :: InfilSfcMax        ! maximum infiltration rate [m/s]
     real(kind=kind_noahmp)                :: RunoffSatExcess    ! saturation excess runoff [m/s]
     real(kind=kind_noahmp)                :: RunoffInfilExcess  ! infiltration excess runoff [m/s]
     real(kind=kind_noahmp)                :: InfilTmp           ! infiltration [m/s]
     real(kind=kind_noahmp)                :: RunoffSatExcTmp    ! temporary saturation excess runoff [m/s]
     real(kind=kind_noahmp)                :: RunoffInfExcTmp    ! temporary infiltration excess runoff [m/s]
     real(kind=kind_noahmp)                :: RunoffSatExcTmp1   ! saturation excess runoff [m/s]
     real(kind=kind_noahmp)                :: DepthYTmp          ! temporary depth Y [m]
     real(kind=kind_noahmp)                :: DepthYPrev         ! previous depth Y [m]
     real(kind=kind_noahmp)                :: DepthYInit         ! initial depth Y [m]
     real(kind=kind_noahmp)                :: TmpVar1            ! temporary variable
     real(kind=kind_noahmp)                :: Error              ! allowed error

! --------------------------------------------------------------------
     associate(                                                                     &
               NumSoilLayer          => noahmp%config%domain%NumSoilLayer          ,& ! in,  number of soil layers
               DepthSoilLayer        => noahmp%config%domain%DepthSoilLayer        ,& ! in,  depth [m] of layer-bottom from soil surface
               OptDynVicInfiltration => noahmp%config%nmlist%OptDynVicInfiltration ,& ! in,  options for infiltration in dynamic VIC runoff scheme
               SoilMoisture          => noahmp%water%state%SoilMoisture            ,& ! in,  total soil moisture [m3/m3]
               SoilSfcInflowMean     => noahmp%water%flux%SoilSfcInflowMean        ,& ! in,  mean water input on soil surface [m/s]
               SoilMoistureSat       => noahmp%water%param%SoilMoistureSat         ,& ! in,  saturated value of soil moisture [m3/m3]
               InfilHeteroDynVic     => noahmp%water%param%InfilHeteroDynVic       ,& ! in,  Dynamic VIC heterogeniety parameter for infiltration
               InfilFacDynVic        => noahmp%water%param%InfilFacDynVic          ,& ! in,  Dynamic VIC model infiltration parameter
               RunoffSurface         => noahmp%water%flux%RunoffSurface            ,& ! out, surface runoff [m/s]
               InfilRateSfc          => noahmp%water%flux%InfilRateSfc              & ! out, infiltration rate at surface [m/s]
              )
! ----------------------------------------------------------------------

     ! initialization
     WaterDepthTop     = 0.0
     WaterDepthSatTop  = 0.0
     InfilExpB         = 1.0
     WaterInSoilSfc    = 0.0
     WaterDepthMax     = 0.0
     WaterDepthInit    = 0.0
     RunoffSatExcess   = 0.0
     RunoffInfilExcess = 0.0
     InfilTmp          = 0.0
     RunoffSurface     = 0.0
     InfilRateSfc      = 0.0
     NumIter           = 20
     Error             = 1.388889E-07 * TimeStep ! 0.5 mm per hour time step
     InfilExpB         = InfilHeteroDynVic

     do IndIter = 1, NumSoilLayer-2
        WaterDepthTop    = WaterDepthTop + (SoilMoisture(IndIter) * (-1.0) * DepthSoilLayer(IndIter))              ! actual moisture in top layers, [m]
        WaterDepthSatTop = WaterDepthSatTop + (SoilMoistureSat(IndIter) * (-1.0) * DepthSoilLayer(IndIter))        ! maximum moisture in top layers, [m]  
     enddo
     if ( WaterDepthTop > WaterDepthSatTop ) WaterDepthTop = WaterDepthSatTop

     WaterInSoilSfc = SoilSfcInflowMean * TimeStep                                                                     ! precipitation depth, [m]
     WaterDepthMax  = WaterDepthSatTop * (InfilFacDynVic + 1.0)                                                    ! maximum infiltration capacity [m], Eq.14
     WaterDepthInit = WaterDepthMax * (1.0 - (1.0 - (WaterDepthTop/WaterDepthSatTop)**(1.0/(1.0+InfilFacDynVic)))) ! infiltration capacity, [m] in Eq.1
     !WaterDepthMax = CAP_minf ; WaterDepthInit = A  
     IndInfilMax = 0

     ! compute surface infiltration
     if ( OptDynVicInfiltration == 1 ) then
        call SoilWaterInfilPhilip(noahmp, TimeStep, IndInfilMax, InfilRateAcc, InfilSfcTmp)
     else if ( OptDynVicInfiltration == 2 ) then
        call SoilWaterInfilGreenAmpt(noahmp, IndInfilMax, InfilRateAcc, InfilSfcTmp)
     else if ( OptDynVicInfiltration == 3 ) then
        call SoilWaterInfilSmithParlange(noahmp, IndInfilMax, InfilRateAcc, InfilSfcTmp)
     endif

     ! I_MM = InfilSfcTmp; I_M = InfilSfcMax  
     InfilSfcMax = (InfilExpB + 1.0) * InfilSfcTmp
     if ( WaterInSoilSfc <= 0.0 ) then
        RunoffSatExcess   = 0.0
        RunoffInfilExcess = 0.0
        InfilTmp          = 0.0
        goto 2001
     else
        if ( (WaterDepthTop >= WaterDepthSatTop) .and. (WaterDepthInit >= WaterDepthMax) ) then
           WaterDepthTop     = WaterDepthSatTop
           WaterDepthInit    = WaterDepthMax
           RunoffSatExcess   = WaterInSoilSfc
           RunoffInfilExcess = 0.0
           InfilTmp          = 0.0
           goto 2001
        else
           WaterDepthInit = WaterDepthMax * (1.0-(1.0-(WaterDepthTop/WaterDepthSatTop)**(1.0/(1.0+InfilFacDynVic))))
           if ( (WaterInSoilSfc+WaterDepthInit) > WaterDepthMax ) then
              if ( (InfilSfcMax*TimeStep) >= WaterInSoilSfc) then
                 DepthYTmp       = WaterDepthMax - WaterDepthInit
                 RunoffSatExcTmp = 0.0
                 call RunoffSatExcessDynamicVic(noahmp,WaterDepthInit,WaterDepthMax,DepthYTmp,RunoffSatExcTmp)
                 TmpVar1 = WaterDepthMax - WaterDepthInit - RunoffSatExcTmp - (InfilSfcTmp*TimeStep) * &
                           (1.0-(1.0-((WaterInSoilSfc-RunoffSatExcTmp)/(InfilSfcMax*TimeStep))**(InfilExpB+1.0)))
                 if ( TmpVar1 <= 0.0 ) then
                    DepthYTmp         = WaterDepthMax - WaterDepthInit
                    InfilTmp          = WaterDepthSatTop - WaterDepthTop
                    RunoffSatExcess   = WaterInSoilSfc - InfilTmp
                    RunoffInfilExcess = 0.0
                    WaterDepthTop     = WaterDepthSatTop
                    WaterDepthInit    = WaterDepthMax
                    goto 2001
                 else
                    DepthYTmp = 0.0
                    do IndIter = 1, NumIter ! loop : iteration 1
                       DepthYPrev      = DepthYTmp
                       RunoffSatExcTmp = 0.0
                       call RunoffSatExcessDynamicVic(noahmp,WaterDepthInit,WaterDepthMax,DepthYTmp,RunoffSatExcTmp)
                       DepthYTmp       = RunoffSatExcTmp + ((InfilSfcTmp*TimeStep) * &
                                         (1.0-(1.0-((WaterInSoilSfc-RunoffSatExcTmp)/(InfilSfcMax*TimeStep))**(InfilExpB+1.0))))
                       if ( (abs(DepthYTmp-DepthYPrev) <= Error) .or. (IndIter == NumIter) ) then
                          goto 1003
                       endif
                    enddo
                 endif
              else
                 RunoffSatExcTmp = 0.0
                 call RunoffSatExcessDynamicVic(noahmp,WaterDepthInit,WaterDepthMax,DepthYTmp,RunoffSatExcTmp)
                 if ( (RunoffSatExcTmp+(InfilSfcMax*TimeStep)) <= WaterInSoilSfc ) then
                    if ( (WaterDepthMax-WaterDepthInit-RunoffSatExcTmp-(InfilSfcMax*TimeStep)) <= 0.0 ) then
                       DepthYTmp         = WaterDepthMax - WaterDepthInit
                       InfilTmp          = WaterDepthSatTop - WaterDepthTop
                       RunoffSatExcess   = WaterInSoilSfc - InfilTmp
                       RunoffInfilExcess = 0.0
                       WaterDepthTop     = WaterDepthSatTop
                       WaterDepthInit    = WaterDepthMax
                       goto 2001
                    else
                       DepthYTmp = 0.0
                       do IndIter = 1, NumIter ! loop : iteration 2
                          DepthYPrev      = DepthYTmp
                          RunoffSatExcTmp = 0.0
                          call RunoffSatExcessDynamicVic(noahmp,WaterDepthInit,WaterDepthMax,DepthYTmp,RunoffSatExcTmp)
                          DepthYTmp       = RunoffSatExcTmp + (InfilSfcTmp*TimeStep)
                          if ( (abs(DepthYTmp-DepthYPrev) <= Error) .or. (IndIter == NumIter) ) then
                             goto 1003
                          endif
                       enddo
                    endif
                 else
                    DepthYTmp = WaterInSoilSfc / 2.0
                    do IndIter = 1, NumIter ! loop : iteration 3_0
                       DepthYPrev      = DepthYTmp
                       RunoffSatExcTmp = 0.0
                       call RunoffSatExcessDynamicVic(noahmp,WaterDepthInit,WaterDepthMax,DepthYTmp,RunoffSatExcTmp)
                       DepthYTmp       = DepthYTmp - RunoffSatExcTmp - (InfilSfcTmp*TimeStep) + WaterInSoilSfc
                       if ( DepthYTmp <= 0.0 )             DepthYTmp = 0.0
                       if ( DepthYTmp >= WaterInSoilSfc  ) DepthYTmp = WaterInSoilSfc
                       if ( (abs(DepthYTmp-DepthYPrev) <= Error) .or. (IndIter == NumIter) ) then
                          DepthYInit   = DepthYTmp
                          exit
                       endif
                    enddo
                    do IndIter = 1, NumIter ! loop : iteration 3
                       DepthYPrev      = DepthYTmp
                       RunoffSatExcTmp = 0.0
                       RunoffInfExcTmp = 0.0
                       call RunoffSatExcessDynamicVic(noahmp,WaterDepthInit,WaterDepthMax,DepthYTmp,RunoffSatExcTmp)
                       call RunoffInfilExcessDynamicVic(DepthYTmp,DepthYInit,RunoffSatExcTmp,InfilSfcMax,&
                                                        InfilSfcTmp,TimeStep,WaterInSoilSfc,InfilExpB,RunoffInfExcTmp)
                       DepthYTmp       = WaterInSoilSfc - RunoffInfExcTmp
                       if ( (abs(DepthYTmp-DepthYPrev) <= Error) .or. (IndIter == NumIter) ) then
                          goto 1003
                       endif
                    enddo
1003                if ( DepthYTmp <= 0.0 ) DepthYTmp = 0.0
                    if ( DepthYTmp >= WaterInSoilSfc  ) DepthYTmp = WaterInSoilSfc
                    call RunoffSatExcessDynamicVic(noahmp,WaterDepthInit,WaterDepthMax,DepthYTmp,RunoffSatExcTmp1)
                    RunoffSatExcess   = RunoffSatExcTmp1
                    RunoffInfilExcess = WaterInSoilSfc - DepthYTmp
                    InfilTmp          = DepthYTmp - RunoffSatExcess
                    WaterDepthTop     = WaterDepthTop + InfilTmp
                    DepthYTmp         = WaterDepthInit + DepthYTmp
                    if ( WaterDepthTop <= 0.0 ) WaterDepthTop = 0.0
                    if ( WaterDepthTop >= WaterDepthSatTop ) WaterDepthTop = WaterDepthSatTop
                    WaterDepthInit = WaterDepthMax * (1.0-(1.0-(WaterDepthTop/WaterDepthSatTop)**(1.0/(1.0+InfilFacDynVic))))
                    goto 2001
                 endif
              endif
           else
              if ( (InfilSfcMax*TimeStep) >= WaterInSoilSfc) then
                 DepthYTmp = WaterInSoilSfc / 2.0
                 do IndIter = 1, NumIter           ! iteration 1
                    DepthYPrev      = DepthYTmp
                    RunoffSatExcTmp = 0.0
                    call RunoffSatExcessDynamicVic(noahmp,WaterDepthInit,WaterDepthMax,DepthYTmp,RunoffSatExcTmp)
                    DepthYTmp       = RunoffSatExcTmp + ((InfilSfcTmp*TimeStep) * &
                                      (1.0-(1.0-((WaterInSoilSfc-RunoffSatExcTmp)/(InfilSfcMax*TimeStep))**(InfilExpB+1.0))))
                    if ( (abs(DepthYTmp-DepthYPrev) <= Error) .or. (IndIter == NumIter) ) then
                       goto 1004
                    endif
                 enddo
              else
                 RunoffSatExcTmp = 0.0
                 call RunoffSatExcessDynamicVic(noahmp,WaterDepthInit,WaterDepthMax,DepthYTmp,RunoffSatExcTmp)
                 if ( (RunoffSatExcTmp+(InfilSfcMax*TimeStep)) <= WaterInSoilSfc ) then
                    DepthYTmp = WaterInSoilSfc / 2.0
                    do IndIter = 1, NumIter        ! iteration 2
                       DepthYPrev      = DepthYTmp
                       RunoffSatExcTmp = 0.0
                       call RunoffSatExcessDynamicVic(noahmp,WaterDepthInit,WaterDepthMax,DepthYTmp,RunoffSatExcTmp)
                       DepthYTmp       = RunoffSatExcTmp+(InfilSfcTmp*TimeStep)
                       if ( (abs(DepthYTmp-DepthYPrev) <= Error) .or. (IndIter == NumIter) ) then
                          goto 1004
                       endif
                    enddo
                 else
                    DepthYTmp = 0.0
                    do IndIter = 1, NumIter        ! iteration 3_0
                       DepthYPrev      = DepthYTmp
                       RunoffSatExcTmp = 0.0
                       call RunoffSatExcessDynamicVic(noahmp,WaterDepthInit,WaterDepthMax,DepthYTmp,RunoffSatExcTmp)
                       DepthYTmp       = (WaterInSoilSfc - (InfilSfcMax*TimeStep)) + DepthYTmp - RunoffSatExcTmp
                       if ( DepthYTmp <= 0.0 )             DepthYTmp = 0.0
                       if ( DepthYTmp >= WaterInSoilSfc  ) DepthYTmp = WaterInSoilSfc
                       RunoffSatExcTmp = 0.0
                       call RunoffSatExcessDynamicVic(noahmp,WaterDepthInit,WaterDepthMax,DepthYTmp,RunoffSatExcTmp)
                       if ( (abs(RunoffSatExcTmp+(InfilSfcMax*TimeStep)-WaterInSoilSfc) <= Error) .or. (IndIter == NumIter) ) then
                          DepthYInit   = DepthYTmp
                          exit
                       endif
                    enddo
                    do IndIter = 1, NumIter ! iteration 3
                       DepthYPrev      = DepthYTmp
                       RunoffSatExcTmp = 0.0
                       RunoffInfExcTmp = 0.0
                       call RunoffSatExcessDynamicVic(noahmp,WaterDepthInit,WaterDepthMax,DepthYTmp,RunoffSatExcTmp)
                       call RunoffInfilExcessDynamicVic(DepthYTmp,DepthYInit,RunoffSatExcTmp,InfilSfcMax,&
                                                        InfilSfcTmp,TimeStep,WaterInSoilSfc,InfilExpB,RunoffInfExcTmp)
                       DepthYTmp       = WaterInSoilSfc - RunoffInfExcTmp
                       if ( (abs(DepthYTmp-DepthYPrev) <= Error) .or. (IndIter == NumIter) ) then
                          goto 1004
                       endif
                    enddo
                 endif
              endif
1004          if ( DepthYTmp <= 0.0 )             DepthYTmp = 0.0
              if ( DepthYTmp >= WaterInSoilSfc  ) DepthYTmp = WaterInSoilSfc
              RunoffSatExcTmp1  = 0.0
              call RunoffSatExcessDynamicVic(noahmp,WaterDepthInit,WaterDepthMax,DepthYTmp,RunoffSatExcTmp1)
              RunoffSatExcess   = RunoffSatExcTmp1
              RunoffInfilExcess = WaterInSoilSfc - DepthYTmp
              InfilTmp          = DepthYTmp - RunoffSatExcess
              WaterDepthTop     = WaterDepthTop + InfilTmp
              if ( WaterDepthTop <= 0.0 )              WaterDepthTop = 0.0
              if ( WaterDepthTop >= WaterDepthSatTop ) WaterDepthTop = WaterDepthSatTop
              WaterDepthInit    = WaterDepthMax * (1.0-(1.0-(WaterDepthTop/WaterDepthSatTop)**(1.0/(1.0+InfilFacDynVic))))
           endif
        endif
     endif

2001 RunoffSurface = (RunoffSatExcess + RunoffInfilExcess) / TimeStep
     RunoffSurface = min(RunoffSurface, SoilSfcInflowMean)
     RunoffSurface = max(RunoffSurface, 0.0)
     InfilRateSfc  = SoilSfcInflowMean - RunoffSurface

    end associate

  end subroutine RunoffSurfaceDynamicVic

end module RunoffSurfaceDynamicVicMod
