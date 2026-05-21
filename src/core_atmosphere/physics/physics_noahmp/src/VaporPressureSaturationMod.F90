module VaporPressureSaturationMod

!!! Calculate saturation vapor pressure and derivative with respect to temperature
!!! using polynomials; over water when t > 0C and over ice when t <= 0C

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine VaporPressureSaturation(T, VapPresSatWat, VapPresSatIce, VapPresSatWatD, VapPresSatIceD)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: ESAT
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    real(kind=kind_noahmp), intent(in)   :: T                     ! air temperature [K]
    real(kind=kind_noahmp), intent(out)  :: VapPresSatWat         ! saturation vapor pressure over water [Pa]
    real(kind=kind_noahmp), intent(out)  :: VapPresSatIce         ! saturation vapor pressure over ice [Pa]
    real(kind=kind_noahmp), intent(out)  :: VapPresSatWatD        ! d(ESAT)/dT over water [Pa/K]
    real(kind=kind_noahmp), intent(out)  :: VapPresSatIceD        ! d(ESAT)/dT over ice [Pa/K]

! local variable
    real(kind=kind_noahmp), parameter    :: A0 = 6.107799961      ! coefficients for ESAT over water
    real(kind=kind_noahmp), parameter    :: A1 = 4.436518521e-01  ! coefficients for ESAT over water
    real(kind=kind_noahmp), parameter    :: A2 = 1.428945805e-02  ! coefficients for ESAT over water
    real(kind=kind_noahmp), parameter    :: A3 = 2.650648471e-04  ! coefficients for ESAT over water
    real(kind=kind_noahmp), parameter    :: A4 = 3.031240396e-06  ! coefficients for ESAT over water
    real(kind=kind_noahmp), parameter    :: A5 = 2.034080948e-08  ! coefficients for ESAT over water
    real(kind=kind_noahmp), parameter    :: A6 = 6.136820929e-11  ! coefficients for ESAT over water
    real(kind=kind_noahmp), parameter    :: B0 = 6.109177956      ! coefficients for ESAT over ice
    real(kind=kind_noahmp), parameter    :: B1 = 5.034698970e-01  ! coefficients for ESAT over ice
    real(kind=kind_noahmp), parameter    :: B2 = 1.886013408e-02  ! coefficients for ESAT over ice
    real(kind=kind_noahmp), parameter    :: B3 = 4.176223716e-04  ! coefficients for ESAT over ice
    real(kind=kind_noahmp), parameter    :: B4 = 5.824720280e-06  ! coefficients for ESAT over ice
    real(kind=kind_noahmp), parameter    :: B5 = 4.838803174e-08  ! coefficients for ESAT over ice
    real(kind=kind_noahmp), parameter    :: B6 = 1.838826904e-10  ! coefficients for ESAT over ice
    real(kind=kind_noahmp), parameter    :: C0 = 4.438099984e-01  ! coefficients for d(ESAT)/dT over water
    real(kind=kind_noahmp), parameter    :: C1 = 2.857002636e-02  ! coefficients for d(ESAT)/dT over water
    real(kind=kind_noahmp), parameter    :: C2 = 7.938054040e-04  ! coefficients for d(ESAT)/dT over water
    real(kind=kind_noahmp), parameter    :: C3 = 1.215215065e-05  ! coefficients for d(ESAT)/dT over water
    real(kind=kind_noahmp), parameter    :: C4 = 1.036561403e-07  ! coefficients for d(ESAT)/dT over water
    real(kind=kind_noahmp), parameter    :: C5 = 3.532421810e-10  ! coefficients for d(ESAT)/dT over water
    real(kind=kind_noahmp), parameter    :: C6 = -7.090244804e-13 ! coefficients for d(ESAT)/dT over water
    real(kind=kind_noahmp), parameter    :: D0 = 5.030305237e-01  ! coefficients for d(ESAT)/dT over ice
    real(kind=kind_noahmp), parameter    :: D1 = 3.773255020e-02  ! coefficients for d(ESAT)/dT over ice
    real(kind=kind_noahmp), parameter    :: D2 = 1.267995369e-03  ! coefficients for d(ESAT)/dT over ice
    real(kind=kind_noahmp), parameter    :: D3 = 2.477563108e-05  ! coefficients for d(ESAT)/dT over ice
    real(kind=kind_noahmp), parameter    :: D4 = 3.005693132e-07  ! coefficients for d(ESAT)/dT over ice
    real(kind=kind_noahmp), parameter    :: D5 = 2.158542548e-09  ! coefficients for d(ESAT)/dT over ice
    real(kind=kind_noahmp), parameter    :: D6 = 7.131097725e-12  ! coefficients for d(ESAT)/dT over ice

! ----------------------------------------------------------------------

  VapPresSatWat  = 100.0 * (A0 + T * (A1 + T * (A2 + T * (A3 + T * ( A4 + T * (A5 + T*A6) ) ) ) ) )
  VapPresSatIce  = 100.0 * (B0 + T * (B1 + T * (B2 + T * (B3 + T * ( B4 + T * (B5 + T*B6) ) ) ) ) )
  VapPresSatWatD = 100.0 * (C0 + T * (C1 + T * (C2 + T * (C3 + T * ( C4 + T * (C5 + T*C6) ) ) ) ) )
  VapPresSatIceD = 100.0 * (D0 + T * (D1 + T * (D2 + T * (D3 + T * ( D4 + T * (D5 + T*D6) ) ) ) ) )

  end subroutine VaporPressureSaturation

end module VaporPressureSaturationMod
