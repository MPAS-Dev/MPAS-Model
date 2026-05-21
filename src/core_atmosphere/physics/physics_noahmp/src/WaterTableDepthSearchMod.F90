module WaterTableDepthSearchMod

!!! Calculate/search water table depth as on WRF-Hydro/NWM

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine WaterTableDepthSearch(noahmp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: TD_FINDZWAT
! Original code: P. Valayamkunnath (NCAR)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                          :: IndSoil              ! loop index 
    integer                          :: IndSatLayer          ! check saturated layer
    real(kind=kind_noahmp)           :: WaterAvailTmp        ! temporary available water
    real(kind=kind_noahmp)           :: WaterTableDepthTmp   ! temporary water table depth [m]

! --------------------------------------------------------------------
    associate(                                                                 &
              NumSoilLayer         => noahmp%config%domain%NumSoilLayer       ,& ! in,    number of soil layers
              DepthSoilLayer       => noahmp%config%domain%DepthSoilLayer     ,& ! in,    depth [m] of layer-bottom from soil surface
              ThicknessSoilLayer   => noahmp%config%domain%ThicknessSoilLayer ,& ! in,    soil layer thickness [m]
              SoilMoistureFieldCap => noahmp%water%param%SoilMoistureFieldCap ,& ! in,    reference soil moisture (field capacity) [m3/m3]
              SoilMoistureWilt     => noahmp%water%param%SoilMoistureWilt     ,& ! in,    wilting point soil moisture [m3/m3]
              SoilMoisture         => noahmp%water%state%SoilMoisture         ,& ! inout, total soil moisture [m3/m3]
              WaterTableDepth      => noahmp%water%state%WaterTableDepth       & ! out,   water table depth [m]
             )
! ----------------------------------------------------------------------

    ! initialization
    IndSatLayer   = 0                 ! indicator for sat. layers
    WaterAvailTmp = 0.0               ! set water avail for subsfc rtng = 0.

    ! calculate/search for water table depth
    do IndSoil = NumSoilLayer, 1, -1
       if ( (SoilMoisture(IndSoil) >= SoilMoistureFieldCap(IndSoil)) .and. &
            (SoilMoistureFieldCap(IndSoil) > SoilMoistureWilt(IndSoil)) ) then
          if ( (IndSatLayer == (IndSoil+1)) .or. (IndSoil == NumSoilLayer) ) IndSatLayer = IndSoil
       endif
    enddo

    if ( IndSatLayer /= 0 ) then
       if ( IndSatLayer /= 1 ) then   ! soil column is partially sat.
          WaterTableDepthTmp = -DepthSoilLayer(IndSatLayer-1)
       else                           ! soil column is fully saturated to sfc.
          WaterTableDepthTmp = 0.0
       endif
       do IndSoil = IndSatLayer, NumSoilLayer
          WaterAvailTmp = WaterAvailTmp + &
                         (SoilMoisture(IndSoil) - SoilMoistureFieldCap(IndSoil)) * ThicknessSoilLayer(IndSoil)
       enddo
    else                              ! no saturated layers...
       WaterTableDepthTmp = -DepthSoilLayer(NumSoilLayer)
       IndSatLayer        = NumSoilLayer + 1
    endif

    WaterTableDepth = WaterTableDepthTmp

    end associate

  end subroutine WaterTableDepthSearch

end module WaterTableDepthSearchMod
