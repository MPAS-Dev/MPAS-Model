module IrrigationMicroMod

!!! Estimate irrigation water depth (m) based on Micro irrigation method
!!! Reference: chapter 7 of NRCS, Part 623 National Engineering Handbook
!!! Irrigation water is applied under the canopy, within first layer 
!!! (at ~5 cm depth) considering current soil moisture

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use IrrigationInfilPhilipMod, only : IrrigationInfilPhilip

  implicit none

contains

  subroutine IrrigationMicro(noahmp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: MICRO_IRRIGATION
! Original code: P. Valayamkunnath (NCAR) <prasanth@ucar.edu> (08/06/2020)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    real(kind=kind_noahmp)           :: InfilRateSfc      ! surface infiltration rate [m/s]
    real(kind=kind_noahmp)           :: IrriRateTmp       ! temporary micro irrigation rate [m/timestep]

! --------------------------------------------------------------------
    associate(                                                               &
              SoilTimeStep        => noahmp%config%domain%SoilTimeStep      ,& ! in,    noahmp soil time step [s]
              DepthSoilLayer      => noahmp%config%domain%DepthSoilLayer    ,& ! in,    depth [m] of layer-bottom from soil surface
              IrrigationFracMicro => noahmp%water%state%IrrigationFracMicro ,& ! in,    fraction of grid under micro irrigation (0 to 1)
              IrriMicroRate       => noahmp%water%param%IrriMicroRate       ,& ! in,    micro irrigation rate [mm/hr]
              SoilLiqWater        => noahmp%water%state%SoilLiqWater        ,& ! inout, soil water content [m3/m3]
              IrrigationAmtMicro  => noahmp%water%state%IrrigationAmtMicro  ,& ! inout, micro irrigation water amount [m]
              IrrigationRateMicro => noahmp%water%flux%IrrigationRateMicro   & ! inout, micro irrigation water rate [m/timestep]
             )
! ----------------------------------------------------------------------
    
    ! initialize local variables
    InfilRateSfc     = 0.0
    IrriRateTmp = 0.0

    ! estimate infiltration rate based on Philips Eq.
    call IrrigationInfilPhilip(noahmp, SoilTimeStep, InfilRateSfc)

    ! irrigation rate of micro irrigation
    IrriRateTmp         = IrriMicroRate * (1.0/1000.0) * SoilTimeStep/ 3600.0                   ! NRCS rate/time step - calibratable
    IrrigationRateMicro = min(0.5*InfilRateSfc*SoilTimeStep, IrrigationAmtMicro, IrriRateTmp)   ! Limit irrigation rate to minimum of 0.5*infiltration rate
                                                                                                ! and to the NRCS recommended rate, (m)
    IrrigationRateMicro = IrrigationRateMicro * IrrigationFracMicro

    if ( IrrigationRateMicro >= IrrigationAmtMicro ) then
       IrrigationRateMicro = IrrigationAmtMicro
       IrrigationAmtMicro  = 0.0
    else
       IrrigationAmtMicro  = IrrigationAmtMicro - IrrigationRateMicro
    endif

    ! update soil moisture
    ! we implement drip in first layer of the Noah-MP. Change layer 1 moisture wrt to irrigation rate
    SoilLiqWater(1) = SoilLiqWater(1) + (IrrigationRateMicro / (-1.0*DepthSoilLayer(1)))

    end associate

  end subroutine IrrigationMicro

end module IrrigationMicroMod
