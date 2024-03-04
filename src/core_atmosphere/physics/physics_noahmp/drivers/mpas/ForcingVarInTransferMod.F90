module ForcingVarInTransferMod

!!! Transfer input 2-D NoahmpIO Forcing variables to 1-D column variable
!!! 1-D variables should be first defined in /src/ForcingVarType.F90
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

  subroutine ForcingVarInTransfer(noahmp, NoahmpIO)

    implicit none

    type(NoahmpIO_type), intent(inout) :: NoahmpIO
    type(noahmp_type),   intent(inout) :: noahmp
    
    ! local variables
    real(kind=kind_noahmp)              :: PrecipOtherRefHeight  ! other precipitation, e.g. fog [mm/s] at reference height
    real(kind=kind_noahmp)              :: PrecipTotalRefHeight  ! total precipitation [mm/s] at reference height

! ---------------------------------------------------------------
    associate(                                           &
              I      => noahmp%config%domain%GridIndexI ,&
              J      => noahmp%config%domain%GridIndexJ  &
             )
! ---------------------------------------------------------------

    noahmp%forcing%TemperatureAirRefHeight = NoahmpIO%T_PHY(I,1,J)
    noahmp%forcing%WindEastwardRefHeight   = NoahmpIO%U_PHY(I,1,J)
    noahmp%forcing%WindNorthwardRefHeight  = NoahmpIO%V_PHY(I,1,J)
    noahmp%forcing%SpecHumidityRefHeight   = NoahmpIO%QV_CURR(I,1,J)/(1.0+NoahmpIO%QV_CURR(I,1,J))  ! convert from mixing ratio to specific humidity
    noahmp%forcing%PressureAirRefHeight    = (NoahmpIO%P8W(I,1,J) + NoahmpIO%P8W(I,2,J)) * 0.5      ! air pressure at middle point of lowest atmos model layer
    noahmp%forcing%PressureAirSurface      = NoahmpIO%P8W      (I,1,J)
    noahmp%forcing%RadLwDownRefHeight      = NoahmpIO%GLW      (I,J)
    noahmp%forcing%RadSwDownRefHeight      = NoahmpIO%SWDOWN   (I,J)
    noahmp%forcing%TemperatureSoilBottom   = NoahmpIO%TMN      (I,J)

    ! treat different precipitation types
    PrecipTotalRefHeight                   = NoahmpIO%RAINBL   (I,J) / NoahmpIO%DTBL                ! convert precip unit from mm/timestep to mm/s
    noahmp%forcing%PrecipConvRefHeight     = NoahmpIO%MP_RAINC (I,J) / NoahmpIO%DTBL
    noahmp%forcing%PrecipNonConvRefHeight  = NoahmpIO%MP_RAINNC(I,J) / NoahmpIO%DTBL
    noahmp%forcing%PrecipShConvRefHeight   = NoahmpIO%MP_SHCV  (I,J) / NoahmpIO%DTBL
    noahmp%forcing%PrecipSnowRefHeight     = NoahmpIO%MP_SNOW  (I,J) / NoahmpIO%DTBL
    noahmp%forcing%PrecipGraupelRefHeight  = NoahmpIO%MP_GRAUP (I,J) / NoahmpIO%DTBL
    noahmp%forcing%PrecipHailRefHeight     = NoahmpIO%MP_HAIL  (I,J) / NoahmpIO%DTBL
    ! treat other precipitation (e.g. fog) contained in total precipitation
    PrecipOtherRefHeight                   = PrecipTotalRefHeight - noahmp%forcing%PrecipConvRefHeight - &
                                             noahmp%forcing%PrecipNonConvRefHeight - noahmp%forcing%PrecipShConvRefHeight
    PrecipOtherRefHeight                   = max(0.0, PrecipOtherRefHeight)
    noahmp%forcing%PrecipNonConvRefHeight  = noahmp%forcing%PrecipNonConvRefHeight + PrecipOtherRefHeight
    noahmp%forcing%PrecipSnowRefHeight     = noahmp%forcing%PrecipSnowRefHeight + PrecipOtherRefHeight * NoahmpIO%SR(I,J)

    end associate
 
  end subroutine ForcingVarInTransfer

end module ForcingVarInTransferMod
