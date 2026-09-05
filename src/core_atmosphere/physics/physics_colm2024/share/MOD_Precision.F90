MODULE MOD_Precision
!-------------------------------------------------------------------------------
! !Purpose:
!       Define the precision to use for floating point and integer operations
!       throughout the model.
!-------------------------------------------------------------------------------
  integer, parameter :: r4  = selected_real_kind(5)
  integer, parameter :: r8  = selected_real_kind(12)
  integer, parameter :: r16 = selected_real_kind(24) !16 byte REAL
  integer, parameter :: i8  = selected_int_kind(13)

END MODULE MOD_Precision
