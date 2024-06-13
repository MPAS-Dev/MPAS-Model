module RunoffSurfaceExcessDynamicVicMod

!!! Compute infiltration and saturation excess runoff for dyanmic VIC runoff scheme

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine RunoffSatExcessDynamicVic(noahmp, WaterDepthInit, WaterDepthMax, DepthYTmp, RunoffSatExcess)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: RR1 for saturation excess runoff
! Original code: Prasanth Valayamkunnath <prasanth@ucar.edu>
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

! IN & OUT variabls
    type(noahmp_type)     , intent(inout) :: noahmp
    real(kind=kind_noahmp), intent(in)    :: WaterDepthInit        ! initial water depth [m]
    real(kind=kind_noahmp), intent(in)    :: WaterDepthMax         ! maximum water depth [m]
    real(kind=kind_noahmp), intent(in)    :: DepthYTmp             ! initial depth Y [m]
    real(kind=kind_noahmp), intent(out)   :: RunoffSatExcess       ! saturation excess runoff [m/s]

! local variable
    real(kind=kind_noahmp)                :: WaterTableDepth       ! water table depth [m]
 
! ------------------------------------------------------------------
    associate(                                                     &
              InfilFacDynVic => noahmp%water%param%InfilFacDynVic  & ! in, DVIC model infiltration parameter
             )
! ------------------------------------------------------------------

    WaterTableDepth = WaterDepthInit + DepthYTmp
    if ( WaterTableDepth > WaterDepthMax ) WaterTableDepth = WaterDepthMax

    ! Saturation excess runoff , Eq 5.
    RunoffSatExcess = DepthYTmp - ((WaterDepthMax/(InfilFacDynVic+1.0)) * &
                      (((1.0 - (WaterDepthInit/WaterDepthMax))**(InfilFacDynVic+1.0)) &
                      - ((1.0 - (WaterTableDepth/WaterDepthMax))**(InfilFacDynVic+1.0))))

    if ( RunoffSatExcess < 0.0 ) RunoffSatExcess = 0.0

    end associate

  end subroutine RunoffSatExcessDynamicVic


  subroutine RunoffInfilExcessDynamicVic(DepthYTmp, DepthYInit, RunoffSatExcess, InfilRateMax, &
                                         InfilRateSfc, TimeStep, WaterInSoilSfc, InfilExpB, RunoffInfilExcess)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: RRunoffInfilExcess for infiltration excess runoff
! Original code: Prasanth Valayamkunnath <prasanth@ucar.edu>
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

! IN & OUT variabls
    real(kind=kind_noahmp), intent(in)    :: DepthYTmp              ! initial depth Y [m]
    real(kind=kind_noahmp), intent(in)    :: DepthYInit             ! initial depth Y [m]
    real(kind=kind_noahmp), intent(in)    :: RunoffSatExcess        ! saturation excess runoff [m/s]
    real(kind=kind_noahmp), intent(in)    :: InfilRateMax           ! maximum infiltration rate [m/s]
    real(kind=kind_noahmp), intent(in)    :: InfilRateSfc           ! surface infiltration rate [m/s]
    real(kind=kind_noahmp), intent(in)    :: TimeStep               ! timestep (may not be the same as model timestep)
    real(kind=kind_noahmp), intent(in)    :: WaterInSoilSfc         ! water input on soil surface [m]
    real(kind=kind_noahmp), intent(in)    :: InfilExpB              ! B parameter for infiltration scaling curve
    real(kind=kind_noahmp), intent(out)   :: RunoffInfilExcess      ! infiltration excess runoff [m/s]
! ----------------------------------------------------------------------

    if ( DepthYTmp >= DepthYInit ) then
       RunoffInfilExcess = WaterInSoilSfc - RunoffSatExcess - (InfilRateMax * TimeStep * &
                           (1.0-((1.0-(WaterInSoilSfc-RunoffSatExcess)/(InfilRateMax*TimeStep))**(InfilExpB+1.0))))
    else
       RunoffInfilExcess = WaterInSoilSfc - RunoffSatExcess - (InfilRateMax*TimeStep)
    endif

    if ( RunoffInfilExcess < 0.0) RunoffInfilExcess =0.0

  end subroutine RunoffInfilExcessDynamicVic

end module RunoffSurfaceExcessDynamicVicMod
