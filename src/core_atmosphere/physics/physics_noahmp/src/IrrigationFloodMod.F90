module IrrigationFloodMod

!!! Estimate irrigation water depth (m) based on surface flooding irrigation method
!!! Reference: chapter 4 of NRCS, Part 623 National Engineering Handbook
!!! Irrigation water is applied on the surface based on present soil moisture and
!!! infiltration rate of the soil. Flooding or overland flow is based on infiltration excess

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use IrrigationInfilPhilipMod, only : IrrigationInfilPhilip

  implicit none

contains

  subroutine IrrigationFlood(noahmp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: FLOOD_IRRIGATION
! Original code: P. Valayamkunnath (NCAR) <prasanth@ucar.edu> (08/06/2020)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    real(kind=kind_noahmp) :: InfilRateSfc     ! surface infiltration rate [m/s]

! --------------------------------------------------------------------
    associate(                                                               &
              SoilTimeStep        => noahmp%config%domain%SoilTimeStep      ,& ! in,    noahmp soil time step [s]
              NumSoilTimeStep     => noahmp%config%domain%NumSoilTimeStep   ,& ! in,    number of time step for calculating soil processes
              IrriFloodRateFac    => noahmp%water%param%IrriFloodRateFac    ,& ! in,    flood application rate factor
              IrrigationFracFlood => noahmp%water%state%IrrigationFracFlood ,& ! in,    fraction of grid under flood irrigation (0 to 1)
              IrrigationAmtFlood  => noahmp%water%state%IrrigationAmtFlood  ,& ! inout, flood irrigation water amount [m]
              SoilSfcInflowAcc    => noahmp%water%flux%SoilSfcInflowAcc     ,& ! inout, accumulated water flux into soil during soil timestep [m/s * dt_soil/dt_main]
              IrrigationRateFlood => noahmp%water%flux%IrrigationRateFlood   & ! inout, flood irrigation water rate [m/timestep]
             )
! ----------------------------------------------------------------------

    ! initialize local variables
    InfilRateSfc = 0.0

    ! estimate infiltration rate based on Philips Eq.
    call IrrigationInfilPhilip(noahmp, SoilTimeStep, InfilRateSfc)  

    ! irrigation rate of flood irrigation. It should be
    ! greater than infiltration rate to get infiltration
    ! excess runoff at the time of application
    IrrigationRateFlood = InfilRateSfc * SoilTimeStep * IrriFloodRateFac   ! Limit irrigation rate to fac*infiltration rate 
    IrrigationRateFlood = IrrigationRateFlood * IrrigationFracFlood

    if ( IrrigationRateFlood >= IrrigationAmtFlood ) then
       IrrigationRateFlood = IrrigationAmtFlood
       IrrigationAmtFlood  = 0.0
    else
       IrrigationAmtFlood  = IrrigationAmtFlood - IrrigationRateFlood
    endif

    ! update water flux going to surface soil
    SoilSfcInflowAcc = SoilSfcInflowAcc + (IrrigationRateFlood / SoilTimeStep * NumSoilTimeStep)  ! [m/s * dt_soil/dt_main]

    end associate

  end subroutine IrrigationFlood

end module IrrigationFloodMod
