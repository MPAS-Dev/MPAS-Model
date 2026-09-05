MODULE MOD_UserDefFun

!-----------------------------------------------------------------------------------------
! !DESCRIPTION:
!
!    This MODULE contains user defined functions to replace non-standard functions.
!
!  Created by Shupeng Zhang, April 2024
!-----------------------------------------------------------------------------------------

   ! ---- PUBLIC subroutines ----

   INTERFACE isnan_ud
      MODULE procedure isnan_ud_r8
   END INTERFACE isnan_ud

CONTAINS

   ! ----------
   elemental logical FUNCTION isnan_ud_r8 (a)

   USE MOD_Precision, only: r8

   IMPLICIT NONE
   real(r8), intent(in) :: a

      isnan_ud_r8 = (a /= a)

   END FUNCTION isnan_ud_r8

   ! ----------
   integer FUNCTION findloc_ud (array, back)

   IMPLICIT NONE
   logical, intent(in) :: array(:)
   logical, intent(in), optional :: back

   ! Local Variables
   logical :: bb
   integer :: n, i, i0, i1, ii

      n = size(array)
      IF (n <= 0) THEN
         findloc_ud = 0
      ELSE

         bb = .false.
         IF (present(back)) THEN
            bb = back
         ENDIF

         IF (.not. bb) THEN
            i0 = 1; i1 = n; ii = 1
         ELSE
            i0 = n; i1 = 1; ii = -1
         ENDIF

         findloc_ud = 0
         DO i = i0, i1, ii
            IF (array(i)) THEN
               findloc_ud = i
               EXIT
            ENDIF
         ENDDO

      ENDIF

   END FUNCTION findloc_ud

END MODULE MOD_UserDefFun
