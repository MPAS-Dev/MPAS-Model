MODULE MOD_Utils

!-----------------------------------------------------------------------------------------
! !DESCRIPTION:
!
!    This MODULE CONTAINS utilities.
!
! !REVISIONS:
!    Subroutines lmder, enorm, tridia and polint are moved from other files.
!
!  Created by Shupeng Zhang, May 2023
!-----------------------------------------------------------------------------------------

   ! ---- PUBLIC subroutines ----

   PUBLIC :: normalize_longitude
   PUBLIC :: make_directory

   INTERFACE expand_list
      MODULE procedure expand_list_int32
      MODULE procedure expand_list_int64
      MODULE procedure expand_list_real8
   END INTERFACE expand_list

   PUBLIC :: append_to_list

   INTERFACE insert_into_sorted_list1
      MODULE procedure insert_into_sorted_list1_int32
      MODULE procedure insert_into_sorted_list1_int64
   END INTERFACE insert_into_sorted_list1

   PUBLIC :: insert_into_sorted_list2

   INTERFACE find_in_sorted_list1
      MODULE procedure find_in_sorted_list1_int32
      MODULE procedure find_in_sorted_list1_int64
   END INTERFACE find_in_sorted_list1

   PUBLIC :: find_in_sorted_list2

   PUBLIC :: find_nearest_south
   PUBLIC :: find_nearest_north
   PUBLIC :: find_nearest_west
   PUBLIC :: find_nearest_east

   PUBLIC :: lon_between_floor
   PUBLIC :: lon_between_ceil

   INTERFACE quicksort
      MODULE procedure quicksort_int32
      MODULE procedure quicksort_int64
      MODULE procedure quicksort_real8
   END INTERFACE quicksort

   PUBLIC :: quickselect
   PUBLIC :: median

   PUBLIC :: areaquad
   PUBLIC :: arclen

   INTERFACE unpack_inplace
      MODULE procedure unpack_inplace_int32
      MODULE procedure unpack_inplace_real8
      MODULE procedure unpack_inplace_lastdim_real8
   END INTERFACE unpack_inplace

   PUBLIC :: num_max_frequency

   PUBLIC :: lmder
   PUBLIC :: lmpar
   PUBLIC :: qrfac
   PUBLIC :: qrsolv

   PUBLIC :: enorm
   PUBLIC :: tridia
   PUBLIC :: polint

CONTAINS

   !---------------------------------
   SUBROUTINE make_directory (path)

   USE MOD_MPAS_MPI, only: CoLM_stop
   IMPLICIT NONE

   character(len=*), intent(in) :: path
   character(len=:), allocatable :: directory, command
   integer :: cmdstat, exitstat
   logical :: exists

      directory = trim(adjustl(path))
      IF (len(directory) == 0) CALL CoLM_stop('Cannot create an empty directory path.')
      IF (verify(directory, 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_./+-') /= 0) THEN
         CALL CoLM_stop('CoLM directory paths may contain only letters, digits, _, ., /, +, and -.')
      ENDIF

      inquire(file=directory, exist=exists)
      IF (exists) RETURN

      IF (directory(1:1) == '/') THEN
         command = 'mkdir -p ' // directory
      ELSE
         command = 'mkdir -p ./' // directory
      ENDIF
      CALL execute_command_line(command, wait=.true., exitstat=exitstat, cmdstat=cmdstat)
      IF (cmdstat /= 0 .or. exitstat /= 0) THEN
         CALL CoLM_stop('Failed to create CoLM directory: '//directory)
      ENDIF
      inquire(file=directory, exist=exists)
      IF (.not. exists) CALL CoLM_stop('CoLM directory creation did not produce: '//directory)

   END SUBROUTINE make_directory

   !---------------------------------
   SUBROUTINE normalize_longitude (lon)

   USE MOD_Precision
   IMPLICIT NONE

   real(r8), intent(inout) :: lon

      DO WHILE (lon >= 180.0)
         lon = lon - 360.0
      ENDDO

      DO WHILE (lon < -180.0)
         lon = lon + 360.0
      ENDDO

   END SUBROUTINE normalize_longitude

   !--------------------------------------------------
   SUBROUTINE expand_list_int32 (list, percent)

   USE MOD_Precision
   IMPLICIT NONE

   integer, allocatable, intent(inout) :: list (:)
   real(r8), intent(in) :: percent

   ! Local variables
   integer :: n0, n1
   integer, allocatable :: temp (:)

      n0 = size(list)

      allocate (temp(n0))
      temp = list

      n1 = ceiling(n0 * (1+percent))

      deallocate(list)
      allocate (list(n1))
      list(1:n0) = temp

      deallocate (temp)

   END SUBROUTINE expand_list_int32

   !--------------------------------------------------
   SUBROUTINE expand_list_int64 (list, percent)

   USE MOD_Precision
   IMPLICIT NONE

   integer*8, allocatable, intent(inout) :: list (:)
   real(r8), intent(in) :: percent

   ! Local variables
   integer :: n0, n1
   integer*8, allocatable :: temp (:)

      n0 = size(list)

      allocate (temp(n0))
      temp = list

      n1 = ceiling(n0 * (1+percent))

      deallocate(list)
      allocate (list(n1))
      list(1:n0) = temp

      deallocate (temp)

   END SUBROUTINE expand_list_int64

   !--------------------------------------------------
   SUBROUTINE expand_list_real8 (list, percent)

   USE MOD_Precision
   IMPLICIT NONE

   real(r8), allocatable, intent(inout) :: list (:)
   real(r8), intent(in) :: percent

   ! Local variables
   integer :: n0, n1
   real(r8), allocatable :: temp (:)

      n0 = size(list)

      allocate (temp(n0))
      temp = list

      n1 = ceiling(n0 * (1+percent))

      deallocate(list)
      allocate (list(n1))
      list(1:n0) = temp

      deallocate (temp)

   END SUBROUTINE expand_list_real8

   !--------------------------------------------------
   SUBROUTINE append_to_list (list1, list2)

   IMPLICIT NONE

   integer, allocatable, intent(inout) :: list1 (:)
   integer, intent(in) :: list2 (:)

   ! Local variables
   integer :: n1, n2
   integer, allocatable :: temp (:)

      IF (.not. allocated(list1)) THEN
         n1 = 0
      ELSE
         n1 = size(list1)
      ENDIF

      n2 = size(list2)

      IF (n1 > 0) THEN
         allocate (temp(n1))
         temp = list1

         deallocate(list1)
         allocate (list1(n1+n2))
         list1(1:n1) = temp

         deallocate (temp)
      ELSE
         IF (n2 > 0) allocate (list1(n2))
      ENDIF

      IF (n1 + n2 > 0) THEN
         list1(n1+1:n1+n2) = list2
      ENDIF

   END SUBROUTINE append_to_list

   !--------------------------------------------------
   SUBROUTINE insert_into_sorted_list1_int32 (x, n, list, iloc, is_new_out)

   IMPLICIT NONE

   integer, intent(in) :: x
   integer, intent(inout) :: n
   integer, intent(inout) :: list(:)
   integer, intent(out)   :: iloc
   logical, intent(out), optional :: is_new_out

   ! Local variables
   logical :: is_new
   integer :: ileft, iright

      IF (n == 0) THEN
         iloc = 1
         is_new = .true.
      ELSEIF (x <= list(1)) THEN
         iloc = 1
         is_new = (x /= list(1))
      ELSEIF (x > list(n)) THEN
         iloc = n + 1
         is_new = .true.
      ELSEIF (x == list(n)) THEN
         iloc = n
         is_new = .false.
      ELSE
         ileft  = 1
         iright = n

         DO WHILE (.true.)
            IF (iright - ileft > 1) THEN
               iloc = (ileft + iright) / 2
               IF (x > list(iloc)) THEN
                  ileft = iloc
               ELSEIF (x < list(iloc)) THEN
                  iright = iloc
               ELSE
                  is_new = .false.
                  EXIT
               ENDIF
            ELSE
               iloc = iright
               is_new = .true.
               EXIT
            ENDIF
         ENDDO
      ENDIF

      IF (is_new) THEN
         IF (iloc <= n) THEN
            list(iloc+1:n+1) = list(iloc:n)
         ENDIF

         list(iloc) = x
         n = n + 1
      ENDIF

      IF (present(is_new_out)) THEN
         is_new_out = is_new
      ENDIF

   END SUBROUTINE insert_into_sorted_list1_int32

   !--------------------------------------------------
   SUBROUTINE insert_into_sorted_list1_int64 (x, n, list, iloc, is_new_out)

   IMPLICIT NONE

   integer*8, intent(in) :: x
   integer,   intent(inout) :: n
   integer*8, intent(inout) :: list(:)
   integer,   intent(out)   :: iloc
   logical,   intent(out), optional :: is_new_out

   ! Local variables
   logical :: is_new
   integer :: ileft, iright

      IF (n == 0) THEN
         iloc = 1
         is_new = .true.
      ELSEIF (x <= list(1)) THEN
         iloc = 1
         is_new = (x /= list(1))
      ELSEIF (x > list(n)) THEN
         iloc = n + 1
         is_new = .true.
      ELSEIF (x == list(n)) THEN
         iloc = n
         is_new = .false.
      ELSE
         ileft  = 1
         iright = n

         DO WHILE (.true.)
            IF (iright - ileft > 1) THEN
               iloc = (ileft + iright) / 2
               IF (x > list(iloc)) THEN
                  ileft = iloc
               ELSEIF (x < list(iloc)) THEN
                  iright = iloc
               ELSE
                  is_new = .false.
                  EXIT
               ENDIF
            ELSE
               iloc = iright
               is_new = .true.
               EXIT
            ENDIF
         ENDDO
      ENDIF

      IF (is_new) THEN
         IF (iloc <= n) THEN
            list(iloc+1:n+1) = list(iloc:n)
         ENDIF

         list(iloc) = x
         n = n + 1
      ENDIF

      IF (present(is_new_out)) THEN
         is_new_out = is_new
      ENDIF

   END SUBROUTINE insert_into_sorted_list1_int64

   !--------------------------------------------------
   SUBROUTINE insert_into_sorted_list2 (x, y, n, xlist, ylist, iloc, is_new_out)

   IMPLICIT NONE

   integer, intent(in) :: x, y
   integer, intent(inout) :: n
   integer, intent(inout) :: xlist(:), ylist(:)
   integer, intent(out)   :: iloc
   logical, intent(out), optional :: is_new_out

   ! Local variables
   logical :: is_new
   integer :: ileft, iright

      IF (n == 0) THEN
         iloc = 1
         is_new = .true.
      ELSEIF ((y < ylist(1)) .or. ((y == ylist(1)) .and. (x <= xlist(1)))) THEN
         iloc = 1
         is_new = (x /= xlist(1)) .or. (y /= ylist(1))
      ELSEIF ((y > ylist(n)) .or. ((y == ylist(n)) .and. (x > xlist(n)))) THEN
         iloc = n + 1
         is_new = .true.
      ELSEIF ((x == xlist(n)) .and. (y == ylist(n))) THEN
         iloc = n
         is_new = .false.
      ELSE
         ileft  = 1
         iright = n

         DO WHILE (.true.)
            IF (iright - ileft > 1) THEN
               iloc = (ileft + iright) / 2
               IF ((y > ylist(iloc)) .or. ((y == ylist(iloc)) .and. (x > xlist(iloc)))) THEN
                  ileft = iloc
               ELSEIF ((y < ylist(iloc)) .or. ((y == ylist(iloc)) .and. (x < xlist(iloc)))) THEN
                  iright = iloc
               ELSE
                  is_new = .false.
                  EXIT
               ENDIF
            ELSE
               iloc = iright
               is_new = .true.
               EXIT
            ENDIF
         ENDDO
      ENDIF

      IF (is_new) THEN
         IF (iloc <= n) THEN
            xlist(iloc+1:n+1) = xlist(iloc:n)
            ylist(iloc+1:n+1) = ylist(iloc:n)
         ENDIF

         xlist(iloc) = x
         ylist(iloc) = y
         n = n + 1
      ENDIF

      IF (present(is_new_out)) THEN
         is_new_out = is_new
      ENDIF

   END SUBROUTINE insert_into_sorted_list2

   !--------------------------------------------------
   FUNCTION find_in_sorted_list1_int32 (x, n, list) result(iloc)

   IMPLICIT NONE

   integer :: iloc

   integer, intent(in) :: x
   integer, intent(in) :: n
   integer, intent(in) :: list (n)

   ! Local variables
   integer :: i, ileft, iright

      iloc = 0
      IF (n > 0) THEN
         IF ((x >= list(1)) .and. (x <= list(n))) THEN
            IF (x == list(1)) THEN
               iloc = 1
            ELSEIF (x == list(n)) THEN
               iloc = n
            ELSE
               ileft  = 1
               iright = n

               DO WHILE (iright - ileft > 1)
                  i = (ileft + iright) / 2
                  IF (x == list(i)) THEN
                     iloc = i
                     EXIT
                  ELSEIF (x > list(i)) THEN
                     ileft = i
                  ELSEIF (x < list(i)) THEN
                     iright = i
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
      ENDIF

   END FUNCTION find_in_sorted_list1_int32

   !--------------------------------------------------
   FUNCTION find_in_sorted_list1_int64 (x, n, list) result(iloc)

   IMPLICIT NONE

   integer :: iloc

   integer*8, intent(in) :: x
   integer,   intent(in) :: n
   integer*8, intent(in) :: list (n)

   ! Local variables
   integer :: i, ileft, iright

      iloc = 0
      IF (n > 0) THEN
         IF ((x >= list(1)) .and. (x <= list(n))) THEN
            IF (x == list(1)) THEN
               iloc = 1
            ELSEIF (x == list(n)) THEN
               iloc = n
            ELSE
               ileft  = 1
               iright = n

               DO WHILE (iright - ileft > 1)
                  i = (ileft + iright) / 2
                  IF (x == list(i)) THEN
                     iloc = i
                     EXIT
                  ELSEIF (x > list(i)) THEN
                     ileft = i
                  ELSEIF (x < list(i)) THEN
                     iright = i
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
      ENDIF

   END FUNCTION find_in_sorted_list1_int64

   !--------------------------------------------------
   FUNCTION find_in_sorted_list2 (x, y, n, xlist, ylist) result(iloc)

   IMPLICIT NONE

   integer :: iloc

   integer, intent(in) :: x, y
   integer, intent(in) :: n
   integer, intent(in) :: xlist(:), ylist(:)

   ! Local variables
   integer :: i, ileft, iright

      iloc = 0
      IF (n < 1) RETURN

      IF ((y < ylist(1)) .or. ((y == ylist(1)) .and. (x < xlist(1)))) THEN
         iloc = 0
      ELSEIF ((y > ylist(n)) .or. ((y == ylist(n)) .and. (x > xlist(n)))) THEN
         iloc = 0
      ELSEIF ((x == xlist(1)) .and. (y == ylist(1))) THEN
         iloc = 1
      ELSEIF ((x == xlist(n)) .and. (y == ylist(n))) THEN
         iloc = n
      ELSE
         ileft  = 1
         iright = n

         DO WHILE (.true.)
            IF (iright - ileft > 1) THEN
               i = (ileft + iright) / 2
               IF ((y == ylist(i)) .and. (x == xlist(i))) THEN
                  iloc = i
                  EXIT
               ELSEIF ((y > ylist(i)) .or. ((y == ylist(i)) .and. (x > xlist(i)))) THEN
                  ileft = i
               ELSEIF ((y < ylist(i)) .or. ((y == ylist(i)) .and. (x < xlist(i)))) THEN
                  iright = i
               ENDIF
            ELSE
               iloc = 0
               EXIT
            ENDIF
         ENDDO
      ENDIF

   END FUNCTION find_in_sorted_list2

   !-----------------------------------------------------
   FUNCTION find_nearest_south (y, n, lat) result(iloc)

   USE MOD_Precision
   IMPLICIT NONE

   integer :: iloc

   real(r8), intent(in) :: y
   integer,  intent(in) :: n
   real(r8), intent(in) :: lat (n)

   ! Local variables
   integer :: i, iright, ileft

      IF (lat(1) < lat(n))  THEN
         IF (y <= lat(1)) THEN
            iloc = 1
         ELSEIF (y >= lat(n)) THEN
            iloc = n
         ELSE
            ileft = 1;  iright = n

            DO WHILE (iright - ileft > 1)
               i = (iright + ileft) / 2
               IF (y >= lat(i)) THEN
                  ileft = i
               ELSE
                  iright = i
               ENDIF
            ENDDO

            iloc = ileft
         ENDIF
      ELSE
         IF (y >= lat(1)) THEN
            iloc = 1
         ELSEIF (y <= lat(n)) THEN
            iloc = n
         ELSE
            ileft = 1;  iright = n

            DO WHILE (iright - ileft > 1)
               i = (iright + ileft) / 2
               IF (y >= lat(i)) THEN
                  iright = i
               ELSE
                  ileft = i
               ENDIF
            ENDDO

            iloc = iright
         ENDIF
      ENDIF

   END FUNCTION find_nearest_south

   !-----------------------------------------------------
   FUNCTION find_nearest_north (y, n, lat)  result(iloc)

   USE MOD_Precision
   IMPLICIT NONE

   real(r8), intent(in) :: y
   integer,  intent(in) :: n
   real(r8), intent(in) :: lat (n)

   integer :: iloc

   ! Local variables
   integer :: i, iright, ileft

      IF (lat(1) < lat(n))  THEN
         IF (y <= lat(1)) THEN
            iloc = 1
         ELSEIF (y >= lat(n)) THEN
            iloc = n
         ELSE
            ileft = 1;  iright = n

            DO WHILE (iright - ileft > 1)
               i = (iright + ileft) / 2
               IF (y > lat(i)) THEN
                  ileft = i
               ELSE
                  iright = i
               ENDIF
            ENDDO

            iloc = iright
         ENDIF
      ELSE
         IF (y >= lat(1)) THEN
            iloc = 1
         ELSEIF (y <= lat(n)) THEN
            iloc = n
         ELSE
            ileft = 1;  iright = n

            DO WHILE (iright - ileft > 1)
               i = (iright + ileft) / 2
               IF (y > lat(i)) THEN
                  iright = i
               ELSE
                  ileft = i
               ENDIF
            ENDDO

            iloc = ileft
         ENDIF
      ENDIF

   END FUNCTION find_nearest_north

   !-----------------------------------------
   logical FUNCTION lon_between_floor (lon, west, east)

   USE MOD_Precision
   IMPLICIT NONE

   real(r8), intent(in) :: lon, west, east ! [-180, 180)

      IF (west >= east) THEN
         lon_between_floor = (lon >= west) .or. (lon < east)
      ELSE
         lon_between_floor = (lon >= west) .and. (lon < east)
      ENDIF

   END FUNCTION lon_between_floor

   !-----------------------------------------
   logical FUNCTION lon_between_ceil (lon, west, east)

   USE MOD_Precision
   IMPLICIT NONE

   real(r8), intent(in) :: lon, west, east ! [-180, 180)

      IF (west >= east) THEN
         lon_between_ceil = (lon > west) .or. (lon <= east)
      ELSE
         lon_between_ceil = (lon > west) .and. (lon <= east)
      ENDIF

   END FUNCTION lon_between_ceil

   !-----------------------------------------------------
   FUNCTION find_nearest_west (x, n, lon) result(iloc)

   USE MOD_Precision
   IMPLICIT NONE

   real(r8), intent(in) :: x
   integer,  intent(in) :: n
   real(r8), intent(in) :: lon (n)

   integer :: iloc

   ! Local variables
   integer :: i, iright, ileft

      IF (n == 1) THEN
         iloc = 1
         RETURN
      ENDIF

      IF (lon_between_floor (x, lon(n), lon(1))) THEN
         iloc = n
         RETURN
      ENDIF

      ileft = 1; iright = n
      DO WHILE (iright - ileft > 1)
         i = (iright + ileft)/2
         IF (lon_between_floor(x,lon(i),lon(iright))) THEN
            ileft = i
         ELSE
            iright = i
         ENDIF
      ENDDO

      iloc = ileft

   END FUNCTION find_nearest_west

   !-----------------------------------------------------
   FUNCTION find_nearest_east (x, n, lon) result(iloc)

   USE MOD_Precision
   IMPLICIT NONE

   real(r8), intent(in) :: x
   integer,  intent(in) :: n
   real(r8), intent(in) :: lon (n)

   integer :: iloc

   ! Local variables
   integer :: i, iright, ileft

      IF (n == 1) THEN
         iloc = 1
         RETURN
      ENDIF

      IF (lon_between_ceil (x, lon(n), lon(1))) THEN
         iloc = 1
         RETURN
      ENDIF

      ileft = 1; iright = n
      DO WHILE (iright - ileft > 1)
         i = (iright + ileft)/2
         IF (lon_between_ceil(x,lon(i),lon(iright))) THEN
            ileft = i
         ELSE
            iright = i
         ENDIF
      ENDDO

      iloc = iright

   END FUNCTION find_nearest_east


   !-----------------------------------------------------
   recursive SUBROUTINE quicksort_int32 (nA, A, order)

   USE MOD_Precision
   IMPLICIT NONE

   integer, intent(in) :: nA
   integer, intent(inout) :: A     (nA)
   integer, intent(inout) :: order (nA)

   ! Local variables
   integer :: left, right
   integer :: pivot
   integer :: marker
   integer :: itemp

      IF (nA > 1) THEN

         pivot = A (nA/2)
         left  = 0
         right = nA + 1

         DO WHILE (left < right)
            right = right - 1
            DO WHILE (A(right) > pivot)
               right = right - 1
            ENDDO

            left = left + 1
            DO WHILE (A(left) < pivot)
               left = left + 1
            ENDDO

            IF (left < right) THEN
               itemp    = A(left)
               A(left)  = A(right)
               A(right) = itemp

               itemp        = order(left)
               order(left)  = order(right)
               order(right) = itemp
            ENDIF
         ENDDO

         marker = right

         CALL quicksort_int32 (marker,    A(1:marker),    order(1:marker))
         CALL quicksort_int32 (nA-marker, A(marker+1:nA), order(marker+1:nA))

      ENDIF

   END SUBROUTINE quicksort_int32

   !-----------------------------------------------------
   recursive SUBROUTINE quicksort_int64 (nA, A, order)

   USE MOD_Precision
   IMPLICIT NONE

   integer,   intent(in) :: nA
   integer*8, intent(inout) :: A     (nA)
   integer,   intent(inout) :: order (nA)

   ! Local variables
   integer*8 :: left, right, pivot, itemp
   integer   :: marker

      IF (nA > 1) THEN

         pivot = A (nA/2)
         left  = 0
         right = nA + 1

         DO WHILE (left < right)
            right = right - 1
            DO WHILE (A(right) > pivot)
               right = right - 1
            ENDDO

            left = left + 1
            DO WHILE (A(left) < pivot)
               left = left + 1
            ENDDO

            IF (left < right) THEN
               itemp    = A(left)
               A(left)  = A(right)
               A(right) = itemp

               itemp        = order(left)
               order(left)  = order(right)
               order(right) = itemp
            ENDIF
         ENDDO

         marker = right

         CALL quicksort_int64 (marker,    A(1:marker),    order(1:marker))
         CALL quicksort_int64 (nA-marker, A(marker+1:nA), order(marker+1:nA))

      ENDIF

   END SUBROUTINE quicksort_int64

   !-----------------------------------------------------
   recursive SUBROUTINE quicksort_real8 (nA, A, order)

   USE MOD_Precision
   IMPLICIT NONE

   integer,  intent(in) :: nA
   real(r8), intent(inout) :: A     (nA)
   integer,  intent(inout) :: order (nA)

   ! Local variables
   real(r8) :: pivot, temp
   integer  :: left,  right, marker, itemp

      IF (nA > 1) THEN

         pivot = A (nA/2)
         left  = 0
         right = nA + 1

         DO WHILE (left < right)
            right = right - 1
            DO WHILE (A(right) > pivot)
               right = right - 1
            ENDDO

            left = left + 1
            DO WHILE (A(left) < pivot)
               left = left + 1
            ENDDO

            IF (left < right) THEN
               temp     = A(left)
               A(left)  = A(right)
               A(right) = temp

               itemp        = order(left)
               order(left)  = order(right)
               order(right) = itemp
            ENDIF
         ENDDO

         marker = right

         CALL quicksort_real8 (marker,    A(1:marker),    order(1:marker))
         CALL quicksort_real8 (nA-marker, A(marker+1:nA), order(marker+1:nA))

      ENDIF

   END SUBROUTINE quicksort_real8

   !-----------------------------------------------------
   recursive FUNCTION quickselect (nA, A, k) result(selected)

   USE MOD_Precision
   IMPLICIT NONE

   real(r8) :: selected

   integer , intent(in)    :: nA
   real(r8), intent(inout) :: A (nA)
   integer,  intent(in)    :: k

   ! Local variables
   integer  :: left, right
   real(r8) :: pivot
   integer  :: marker
   real(r8) :: rtemp

      IF (nA > 1) THEN

         pivot = A (nA/2)
         left  = 0
         right = nA + 1

         DO WHILE (left < right)
            right = right - 1
            DO WHILE (A(right) > pivot)
               right = right - 1
            ENDDO

            left = left + 1
            DO WHILE (A(left) < pivot)
               left = left + 1
            ENDDO

            IF (left < right) THEN
               rtemp    = A(left)
               A(left)  = A(right)
               A(right) = rtemp
            ENDIF
         ENDDO

         marker = right

         IF (k <= marker) THEN
            selected = quickselect (marker, A(1:marker), k)
         ELSE
            selected = quickselect (nA-marker, A(marker+1:nA), k-marker)
         ENDIF

      ELSE
         selected = A(1)
      ENDIF

   END FUNCTION quickselect


   ! ------------------------
   FUNCTION median(x, n, spval) result(mval)

   USE MOD_Precision
   IMPLICIT NONE

   real(r8) :: mval

   integer,  intent(in) :: n
   real(r8), intent(in) :: x(n)
   real(r8), intent(in), optional :: spval

   ! Local variables
   integer  :: nc
   real(r8), allocatable :: xtemp(:)
   logical,  allocatable :: msk  (:)
   real(r8) :: right, left

      IF (present(spval)) THEN
         allocate (msk (n))
         msk = (x /= spval)
         nc  = count(msk)
         IF (nc /= 0) THEN

            allocate (xtemp(nc))
            xtemp = pack(x, msk)

            deallocate (msk)
         ELSE

            mval = spval

            deallocate(msk)
            RETURN
         ENDIF
      ELSE
         nc = n
         allocate (xtemp(nc))
         xtemp = x
      ENDIF

      IF (mod(nc,2) == 0) THEN
         left  = quickselect(nc,xtemp,nc/2)
         right = quickselect(nc,xtemp,nc/2+1)
         mval = (left + right) / 2.0_r8
      ELSE
         mval = quickselect(nc,xtemp,nc/2+1)
      ENDIF

      deallocate (xtemp)

   END FUNCTION median


   !-----------------------------------------------------
   FUNCTION areaquad (lats, latn, lonw, lone) result(area)

   USE MOD_Precision
   IMPLICIT NONE

   real(r8) :: area ! in km^2
   real(r8), parameter :: re = 6.37122e3 ! kilometer
   real(r8), parameter :: deg2rad = 1.745329251994330e-2_r8
   real(r8), intent(in) :: lats, latn, lonw, lone

   ! Local variables
   real(r8) :: dx, dy

      IF (lone < lonw) THEN
         dx = (lone + 360 - lonw) * deg2rad
      ELSE
         dx = (lone - lonw) * deg2rad
      ENDIF

      dy = sin(latn * deg2rad) - sin(lats * deg2rad)

      area = dx * dy * re * re

   END FUNCTION areaquad

   ! --- spherical distance  ---
   FUNCTION arclen (lat1, lon1, lat2, lon2)

   USE MOD_Precision
   IMPLICIT NONE

   real(r8) :: arclen ! in km
   real(r8), intent(in) :: lat1, lon1, lat2, lon2

   real(r8), parameter :: re = 6.37122e3 ! kilometer
   real(r8) :: tmp

      tmp = sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(lon1-lon2)
      tmp = min(max(tmp, -1.), 1.)
      arclen = re * acos(tmp)

   END FUNCTION arclen

   !-----------------------------------------------------
   SUBROUTINE unpack_inplace_int32 (din, msk, dout)

   IMPLICIT NONE

   integer, intent(in) :: din (:)
   logical, intent(in) :: msk (:)
   integer, intent(inout) :: dout (:)

   ! Local variables
   integer :: n, i

      n = 0
      DO i = 1, size(msk)
         IF (msk(i)) THEN
            n = n + 1
            dout(i) = din(n)
         ENDIF
      ENDDO

   END SUBROUTINE unpack_inplace_int32

   !-----------------------------------------------------
   SUBROUTINE unpack_inplace_real8 (din, msk, dout)

   USE MOD_Precision
   IMPLICIT NONE

   real(r8), intent(in) :: din (:)
   logical,  intent(in) :: msk (:)
   real(r8), intent(inout) :: dout (:)

   ! Local variables
   integer :: n, i

      n = 0
      DO i = 1, size(msk)
         IF (msk(i)) THEN
            n = n + 1
            dout(i) = din(n)
         ENDIF
      ENDDO

   END SUBROUTINE unpack_inplace_real8

   !-----------------------------------------------------
   SUBROUTINE unpack_inplace_lastdim_real8 (din, msk, dout)

   USE MOD_Precision
   IMPLICIT NONE

   real(r8), intent(in) :: din (:,:)
   logical,  intent(in) :: msk (:)
   real(r8), intent(inout) :: dout (:,:)

   ! Local variables
   integer :: n, i

      n = 0
      DO i = 1, size(msk)
         IF (msk(i)) THEN
            n = n + 1
            dout(:,i) = din(:,n)
         ENDIF
      ENDDO

   END SUBROUTINE unpack_inplace_lastdim_real8

   !---------------------------------------------------
   integer FUNCTION num_max_frequency (data_in)

   IMPLICIT NONE

   integer, intent(in) :: data_in(:)

   ! Local Variables
   integer, allocatable :: data_(:), cnts(:)
   integer :: ndata, i, n, iloc
   logical :: is_new

      ndata = size(data_in)
      allocate (data_(ndata))
      allocate (cnts (ndata))

      n = 0
      cnts(:) = 0
      DO i = 1, ndata
         CALL insert_into_sorted_list1 (data_in(i), n, data_, iloc, is_new)
         IF (is_new) THEN
            IF (iloc < n) cnts(iloc+1:ndata) = cnts(iloc:ndata-1)
            cnts(iloc) = 1
         ELSE
            cnts(iloc) = cnts(iloc) + 1
         ENDIF
      ENDDO

      num_max_frequency = data_(maxloc(cnts,dim=1))

      deallocate(data_)
      deallocate(cnts )

   END FUNCTION num_max_frequency

   !----------------------------------------------------
   SUBROUTINE lmder ( fcn, m, n, x, fvec, fjac, ldfjac, ftol, xtol, gtol, maxfev, &
     diag, mode, factor, nprint, info, nfev, njev, ipvt, qtf, xdat, npoint, ydat, &
     ydatks, nptf, phi, k_s, isiter, L_vgm)

   !*******************************************************************************
   !
   !! LMDER minimizes M functions in N variables by the Levenberg-Marquardt method
   !  implemented for fitting the SW retention & hydraulic conductivity parameters
   !  in the Campbell/van Genuchten models.
   !
   !  Discussion:
   !
   !    LMDER minimizes the sum of the squares of M nonlinear functions in
   !    N variables by a modification of the Levenberg-Marquardt algorithm.
   !    The user must provide a subroutine which calculates the functions
   !    and the jacobian.
   !
   !  Licensing:
   !
   !    This code may freely be copied, modified, and used for any purpose.
   !
   !  Modified:
   !
   !    06 April 2010
   !
   !  Author:
   !
   !    Original FORTRAN77 version by Jorge More, Burton Garbow, Kenneth Hillstrom.
   !    FORTRAN90 version by John Burkardt.
   !    Modified by Nan Wei, 2019/01
   !
   !  Reference:
   !
   !    Jorge More, Burton Garbow, Kenneth Hillstrom,
   !    User Guide for MINPACK-1,
   !    Technical Report ANL-80-74,
   !    Argonne National Laboratory, 1980.
   !
   !  Parameters:
   !
   !    Input, external FCN, the name of the user-supplied subroutine which
   !    calculates the functions and the jacobian.  FCN should have the form:
   !      subroutine fcn ( m, n, x, fvec, fjac, ldfjac, iflag, xdat, npoint, ydat, ydatks, nptf, phi, k_s, isiter, L_vgm)
   !      integer ( kind = 4 ) ldfjac
   !      integer ( kind = 4 ) n
   !      real ( kind = 8 ) fjac(ldfjac,n)
   !      real ( kind = 8 ) fvec(m)
   !      integer ( kind = 4 ) iflag
   !      real ( kind = 8 ) x(n)
   !      xdat, npoint, ydat, ydatks, nptf, phi, k_s and isiter are transfered as the inputs of the fitting functions.
   !      L_vgm are only used for vanGenuchten_Mualem soil model input.
   !
   !    If IFLAG = 0 on input, then FCN is only being called to allow the user
   !    to print out the current iterate.
   !    If IFLAG = 1 on input, FCN should calculate the functions at X and
   !    return this vector in FVEC.
   !    If IFLAG = 2 on input, FCN should calculate the jacobian at X and
   !    return this matrix in FJAC.
   !    To terminate the algorithm, FCN may set IFLAG negative on return.
   !
   !    Input, integer ( kind = 4 ) M, is the number of functions.
   !
   !    Input, integer ( kind = 4 ) N, is the number of variables.
   !    N must not exceed M.
   !
   !    Input/output, real ( kind = 8 ) X(N).  On input, X must contain an initial
   !    estimate of the solution vector.  On output X contains the final
   !    estimate of the solution vector.
   !
   !    Output, real ( kind = 8 ) FVEC(M), the functions evaluated at the output X.
   !
   !    Output, real ( kind = 8 ) FJAC(LDFJAC,N), an M by N array.  The upper
   !    N by N submatrix of FJAC contains an upper triangular matrix R with
   !    diagonal elements of nonincreasing magnitude such that
   !      P' * ( JAC' * JAC ) * P = R' * R,
   !    where P is a permutation matrix and JAC is the final calculated jacobian.
   !    Column J of P is column IPVT(J) of the identity matrix.  The lower
   !    trapezoidal part of FJAC contains information generated during
   !    the computation of R.
   !
   !    Input, integer ( kind = 4 ) LDFJAC, the leading dimension of FJAC.
   !    LDFJAC must be at least M.
   !
   !    Input, real ( kind = 8 ) FTOL.  Termination occurs when both the actual
   !    and predicted relative reductions in the sum of squares are at most FTOL.
   !    Therefore, FTOL measures the relative error desired in the sum of
   !    squares.  FTOL should be nonnegative.
   !
   !    Input, real ( kind = 8 ) XTOL.  Termination occurs when the relative error
   !    between two consecutive iterates is at most XTOL.  XTOL should be
   !    nonnegative.
   !
   !    Input, real ( kind = 8 ) GTOL.  Termination occurs when the cosine of the
   !    angle between FVEC and any column of the jacobian is at most GTOL in
   !    absolute value.  Therefore, GTOL measures the orthogonality desired
   !    between the function vector and the columns of the jacobian.  GTOL should
   !    be nonnegative.
   !
   !    Input, integer ( kind = 4 ) MAXFEV.  Termination occurs when the number of
   !    calls to FCN with IFLAG = 1 is at least MAXFEV by the end of an iteration.
   !
   !    Input/output, real ( kind = 8 ) DIAG(N).  If MODE = 1, then DIAG is set
   !    internally.  If MODE = 2, then DIAG must contain positive entries that
   !    serve as multiplicative scale factors for the variables.
   !
   !    Input, integer ( kind = 4 ) MODE, scaling option.
   !    1, variables will be scaled internally.
   !    2, scaling is specified by the input DIAG vector.
   !
   !    Input, real ( kind = 8 ) FACTOR, determines the initial step bound.  This
   !    bound is set to the product of FACTOR and the euclidean norm of DIAG*X if
   !    nonzero, or else to FACTOR itself.  In most cases, FACTOR should lie
   !    in the interval (0.1, 100) with 100 the recommended value.
   !
   !    Input, integer ( kind = 4 ) NPRINT, enables controlled printing of iterates
   !    if it is positive.  In this case, FCN is called with IFLAG = 0 at the
   !    beginning of the first iteration and every NPRINT iterations thereafter
   !    and immediately prior to return, with X and FVEC available
   !    for printing.  If NPRINT is not positive, no special calls
   !    of FCN with IFLAG = 0 are made.
   !
   !    Output, integer ( kind = 4 ) INFO, error flag.  If the user has terminated
   !    execution, INFO is set to the (negative) value of IFLAG. See description
   !    of FCN.  Otherwise, INFO is set as follows:
   !    0, improper input parameters.
   !    1, both actual and predicted relative reductions in the sum of
   !       squares are at most FTOL.
   !    2, relative error between two consecutive iterates is at most XTOL.
   !    3, conditions for INFO = 1 and INFO = 2 both hold.
   !    4, the cosine of the angle between FVEC and any column of the jacobian
   !       is at most GTOL in absolute value.
   !    5, number of calls to FCN with IFLAG = 1 has reached MAXFEV.
   !    6, FTOL is too small.  No further reduction in the sum of squares
   !       is possible.
   !    7, XTOL is too small.  No further improvement in the approximate
   !       solution X is possible.
   !    8, GTOL is too small.  FVEC is orthogonal to the columns of the
   !       jacobian to machine precision.
   !
   !    Output, integer ( kind = 4 ) NFEV, the number of calls to FCN with
   !    IFLAG = 1.
   !
   !    Output, integer ( kind = 4 ) NJEV, the number of calls to FCN with
   !    IFLAG = 2.
   !
   !    Output, integer ( kind = 4 ) IPVT(N), defines a permutation matrix P
   !    such that JAC*P = Q*R, where JAC is the final calculated jacobian, Q is
   !    orthogonal (not stored), and R is upper triangular with diagonal
   !    elements of nonincreasing magnitude.  Column J of P is column
   !    IPVT(J) of the identity matrix.
   !
   !    Output, real ( kind = 8 ) QTF(N), contains the first N elements of Q'*FVEC.
   !
   IMPLICIT NONE

   integer ( kind = 4 ) ldfjac
   integer ( kind = 4 ) m
   integer ( kind = 4 ) n

   real ( kind = 8 ) actred
   real ( kind = 8 ) delta
   real ( kind = 8 ) diag(n)
   real ( kind = 8 ) dirder
   real ( kind = 8 ) epsmch
   real ( kind = 8 ) factor
   external fcn
   real ( kind = 8 ) fjac(ldfjac,n)
   real ( kind = 8 ) fnorm
   real ( kind = 8 ) fnorm1
   real ( kind = 8 ) ftol
   real ( kind = 8 ) fvec(m)
   real ( kind = 8 ) gnorm
   real ( kind = 8 ) gtol
   integer ( kind = 4 ) i
   integer ( kind = 4 ) iflag
   integer ( kind = 4 ) info
   integer ( kind = 4 ) ipvt(n)
   integer ( kind = 4 ) iter
   integer ( kind = 4 ) j
   integer ( kind = 4 ) l
   integer ( kind = 4 ) maxfev
   integer ( kind = 4 ) mode
   integer ( kind = 4 ) nfev
   integer ( kind = 4 ) njev
   integer ( kind = 4 ) nprint
   real ( kind = 8 ) par
   logical pivot
   real ( kind = 8 ) pnorm
   real ( kind = 8 ) prered
   real ( kind = 8 ) qtf(n)
   real ( kind = 8 ) ratio
   real ( kind = 8 ) sum2
   real ( kind = 8 ) temp
   real ( kind = 8 ) temp1
   real ( kind = 8 ) temp2
   real ( kind = 8 ) wa1(n)
   real ( kind = 8 ) wa2(n)
   real ( kind = 8 ) wa3(n)
   real ( kind = 8 ) wa4(m)
   real ( kind = 8 ) xnorm
   real ( kind = 8 ) x(n)
   real ( kind = 8 ) xtol
   real ( kind = 8 ) phi,k_s
   integer ( kind = 4 ) isiter
   integer ( kind = 4 ) npoint
   integer ( kind = 4 ) nptf
   real ( kind = 8 ) xdat(npoint)
   real ( kind = 8 ) ydat  (nptf,npoint)
   real ( kind = 8 ) ydatks(nptf,npoint)
   real ( kind = 8 ), optional :: L_vgm

      epsmch = epsilon ( epsmch )

      info = 0
      iflag = 0
      nfev = 0
      njev = 0
   !
   !  Check the input parameters for errors.
   !
      IF ( n <= 0 ) THEN
         go to 300
      ENDIF

      IF ( m < n ) THEN
         go to 300
      ENDIF

      IF ( ldfjac < m &
       .or. ftol < 0.0D+00 .or. xtol < 0.0D+00 .or. gtol < 0.0D+00 &
        .or. maxfev <= 0 .or. factor <= 0.0D+00 ) THEN
         go to 300
      ENDIF

      IF ( mode == 2 ) THEN
         DO j = 1, n
            IF ( diag(j) <= 0.0D+00 ) THEN
               go to 300
            ENDIF
         ENDDO
      ENDIF
   !
   !  Evaluate the function at the starting point and calculate its norm.
   !
      iflag = 1
      IF (present(L_vgm)) THEN
         CALL fcn ( m, n, x, fvec, fjac, ldfjac, iflag, xdat, npoint, ydat, ydatks, nptf, phi, k_s, isiter, L_vgm)
      ELSE
         CALL fcn ( m, n, x, fvec, fjac, ldfjac, iflag, xdat, npoint, ydat, ydatks, nptf, phi, k_s, isiter )
      ENDIF
      nfev = 1
      IF ( iflag < 0 ) THEN
         go to 300
      ENDIF

      fnorm = enorm ( m, fvec )
   !
   !  Initialize Levenberg-Marquardt parameter and iteration counter.
   !
      par = 0.0D+00
      iter = 1
   !
   !  Beginning of the outer loop.
   !
      DO
   !
   !  Calculate the jacobian matrix.
   !
         iflag = 2
         IF (present(L_vgm)) THEN
            CALL fcn ( m, n, x, fvec, fjac, ldfjac, iflag, xdat, npoint, ydat, ydatks, nptf, phi, k_s, isiter, L_vgm)
         ELSE
            CALL fcn ( m, n, x, fvec, fjac, ldfjac, iflag, xdat, npoint, ydat, ydatks, nptf, phi, k_s, isiter )
         ENDIF

         njev = njev + 1

         IF ( iflag < 0 ) THEN
            go to 300
         ENDIF
   !
   !     IF requested, call FCN to enable printing of iterates.
   !
         IF ( 0 < nprint ) THEN
            iflag = 0
            IF ( mod ( iter - 1, nprint ) == 0 ) THEN
               IF (present(L_vgm)) THEN
                  CALL fcn ( m, n, x, fvec, fjac, ldfjac, iflag, xdat, npoint, ydat, ydatks, nptf, phi, k_s, isiter, L_vgm)
               ELSE
                  CALL fcn ( m, n, x, fvec, fjac, ldfjac, iflag, xdat, npoint, ydat, ydatks, nptf, phi, k_s, isiter )
               ENDIF
            ENDIF
            IF ( iflag < 0 ) THEN
               go to 300
            ENDIF
         ENDIF
   !
   !     Compute the QR factorization of the jacobian.
   !
         pivot = .true.
         CALL qrfac ( m, n, fjac, ldfjac, pivot, ipvt, n, wa1, wa2 )

   !     On the first iteration and if mode is 1, scale according
   !     to the norms of the columns of the initial jacobian.
   !
         IF ( iter == 1 ) THEN

            IF ( mode /= 2 ) THEN
               diag(1:n) = wa2(1:n)
               DO j = 1, n
                  IF ( wa2(j) == 0.0D+00 ) THEN
                     diag(j) = 1.0D+00
                  ENDIF
               ENDDO
            ENDIF
   !
   !        On the first iteration, calculate the norm of the scaled X
   !        and initialize the step bound DELTA.
   !
            wa3(1:n) = diag(1:n) * x(1:n)

            xnorm = enorm ( n, wa3 )

            IF ( xnorm == 0.0D+00 ) THEN
               delta = factor
            ELSE
               delta = factor * xnorm
            ENDIF

         ENDIF
   !
   !     Form Q'*FVEC and store the first N components in QTF.
   !
         wa4(1:m) = fvec(1:m)

         DO j = 1, n

            IF ( fjac(j,j) /= 0.0D+00 ) THEN
               sum2 = dot_product ( wa4(j:m), fjac(j:m,j) )
               temp = - sum2 / fjac(j,j)
               wa4(j:m) = wa4(j:m) + fjac(j:m,j) * temp
            ENDIF

            fjac(j,j) = wa1(j)
            qtf(j) = wa4(j)

         ENDDO
   !
   !     Compute the norm of the scaled gradient.
   !
         gnorm = 0.0D+00

         IF ( fnorm /= 0.0D+00 ) THEN

            DO j = 1, n
               l = ipvt(j)
               IF ( wa2(l) /= 0.0D+00 ) THEN
                  sum2 = dot_product ( qtf(1:j), fjac(1:j,j) ) / fnorm
                  gnorm = max ( gnorm, abs ( sum2 / wa2(l) ) )
               ENDIF
            ENDDO

         ENDIF
   !
   !     Test for convergence of the gradient norm.
   !
         IF ( gnorm <= gtol ) THEN
            info = 4
            go to 300
         ENDIF
   !
   !     Rescale if necessary.
   !
         IF ( mode /= 2 ) THEN
            DO j = 1, n
               diag(j) = max ( diag(j), wa2(j) )
            ENDDO
         ENDIF
   !
   !     Beginning of the inner loop.
   !
         DO
   !
   !     Determine the Levenberg-Marquardt parameter.

            CALL lmpar ( n, fjac, ldfjac, ipvt, diag, qtf, delta, par, wa1, wa2 )

   !        Store the direction p and x + p. calculate the norm of p.
   !
            wa1(1:n) = - wa1(1:n)
            wa2(1:n) = x(1:n) + wa1(1:n)
            wa3(1:n) = diag(1:n) * wa1(1:n)

            pnorm = enorm ( n, wa3 )
   !
   !        On the first iteration, adjust the initial step bound.
   !
            IF ( iter == 1 ) THEN
               delta = min ( delta, pnorm )
            ENDIF
   !
   !        Evaluate the function at x + p and calculate its norm.
   !
            iflag = 1
            IF (present(L_vgm)) THEN
               CALL fcn ( m, n, wa2, wa4, fjac, ldfjac, iflag, xdat, npoint, ydat, ydatks, nptf, phi, k_s, isiter, L_vgm)
            ELSE
               CALL fcn ( m, n, wa2, wa4, fjac, ldfjac, iflag, xdat, npoint, ydat, ydatks, nptf, phi, k_s, isiter )
            ENDIF

            nfev = nfev + 1

            IF ( iflag < 0 ) THEN
               go to 300
            ENDIF

            fnorm1 = enorm ( m, wa4 )
   !
   !        Compute the scaled actual reduction.
   !
            IF ( 0.1D+00 * fnorm1 < fnorm ) THEN
               actred = 1.0D+00 - ( fnorm1 / fnorm ) ** 2
            ELSE
               actred = - 1.0D+00
            ENDIF
   !
   !        Compute the scaled predicted reduction and
   !        the scaled directional derivative.
   !
            DO j = 1, n
               wa3(j) = 0.0D+00
               l = ipvt(j)
               temp = wa1(l)
               wa3(1:j) = wa3(1:j) + fjac(1:j,j) * temp
            ENDDO

            temp1 = enorm ( n, wa3 ) / fnorm
            temp2 = ( sqrt ( par ) * pnorm ) / fnorm
            prered = temp1 ** 2 + temp2 ** 2 / 0.5D+00
            dirder = - ( temp1 ** 2 + temp2 ** 2 )
   !
   !        Compute the ratio of the actual to the predicted reduction.
   !
            IF ( prered /= 0.0D+00 ) THEN
               ratio = actred / prered
            ELSE
               ratio = 0.0D+00
            ENDIF
   !
   !        Update the step bound.
   !
            IF ( ratio <= 0.25D+00 ) THEN

               IF ( 0.0D+00 <= actred ) THEN
                  temp = 0.5D+00
               ENDIF

               IF ( actred < 0.0D+00 ) THEN
                  temp = 0.5D+00 * dirder / ( dirder + 0.5D+00 * actred )
               ENDIF

               IF ( 0.1D+00 * fnorm1 >= fnorm .or. temp < 0.1D+00 ) THEN
                  temp = 0.1D+00
               ENDIF

               delta = temp * min ( delta, pnorm / 0.1D+00 )
               par = par / temp

            ELSE

               IF ( par == 0.0D+00 .or. ratio >= 0.75D+00 ) THEN
                  delta = 2.0D+00 * pnorm
                  par = 0.5D+00 * par
               ENDIF

            ENDIF
   !
   !        Successful iteration.
   !
   !        Update X, FVEC, and their norms.
   !
            IF ( 0.0001D+00 <= ratio ) THEN
               x(1:n) = wa2(1:n)
               wa2(1:n) = diag(1:n) * x(1:n)
               fvec(1:m) = wa4(1:m)
               xnorm = enorm ( n, wa2 )
               fnorm = fnorm1
               iter = iter + 1
            ENDIF
   !
   !        Tests for convergence.
   !
            IF ( abs ( actred) <= ftol .and. &
                 prered <= ftol .and. &
                 0.5D+00 * ratio <= 1.0D+00 ) THEN
               info = 1
            ENDIF

            IF ( delta <= xtol * xnorm ) THEN
               info = 2
            ENDIF

            IF ( abs ( actred) <= ftol .and. prered <= ftol &
                .and. 0.5D+00 * ratio <= 1.0D+00 .and. info == 2 ) THEN
               info = 3
            ENDIF

            IF ( info /= 0 ) THEN
               go to 300
            ENDIF
   !
   !        Tests for termination and stringent tolerances.
   !
            IF ( nfev >= maxfev ) THEN
               info = 5
            ENDIF

            IF ( abs ( actred ) <= epsmch .and. prered <= epsmch &
               .and. 0.5D+00 * ratio <= 1.0D+00 ) THEN
               info = 6
            ENDIF

            IF ( delta <= epsmch * xnorm ) THEN
               info = 7
            ENDIF

            IF ( gnorm <= epsmch ) THEN
               info = 8
            ENDIF

            IF ( info /= 0 ) THEN
               go to 300
            ENDIF
   !
   !        End of the inner loop. repeat IF iteration unsuccessful.
   !
            IF ( 0.0001D+00 <= ratio ) THEN
               EXIT
            ENDIF

         ENDDO
   !
   !  End of the outer loop.
   !
      ENDDO

      300 continue
   !
   !  Termination, either normal or user imposed.
   !
      IF ( iflag < 0 ) THEN
         info = iflag
      ENDIF

      iflag = 0

      IF ( 0 < nprint ) THEN
         IF (present(L_vgm)) THEN
            CALL fcn ( m, n, x, fvec, fjac, ldfjac, iflag, xdat, npoint, ydat, ydatks, nptf, phi, k_s, isiter, L_vgm)
         ELSE
            CALL fcn ( m, n, x, fvec, fjac, ldfjac, iflag, xdat, npoint, ydat, ydatks, nptf, phi, k_s, isiter )
         ENDIF
      ENDIF

      RETURN
   END SUBROUTINE lmder

   SUBROUTINE lmpar ( n, r, ldr, ipvt, diag, qtb, delta, par, x, sdiag )

   !*****************************************************************************80
   !
   !! LMPAR computes a parameter for the Levenberg-Marquardt method.
   !
   !  Discussion:
   !
   !    Given an M by N matrix A, an N by N nonsingular diagonal
   !    matrix D, an M-vector B, and a positive number DELTA,
   !    the problem is to determine a value for the parameter
   !    PAR such that IF X solves the system
   !
   !      A*X = B,
   !      sqrt ( PAR ) * D * X = 0,
   !
   !    in the least squares sense, and DXNORM is the euclidean
   !    norm of D*X, THEN either PAR is zero and
   !
   !      ( DXNORM - DELTA ) <= 0.1 * DELTA,
   !
   !    or PAR is positive and
   !
   !      abs ( DXNORM - DELTA) <= 0.1 * DELTA.
   !
   !    This FUNCTION completes the solution of the problem
   !    IF it is provided with the necessary information from the
   !    QR factorization, with column pivoting, of A.  That is, IF
   !    A*P = Q*R, WHERE P is a permutation matrix, Q has orthogonal
   !    columns, and R is an upper triangular matrix with diagonal
   !    elements of nonincreasing magnitude, THEN LMPAR expects
   !    the full upper triangle of R, the permutation matrix P,
   !    and the first N components of Q'*B.  On output
   !    LMPAR also provides an upper triangular matrix S such that
   !
   !      P' * ( A' * A + PAR * D * D ) * P = S'* S.
   !
   !    S is employed within LMPAR and may be of separate interest.
   !
   !    Only a few iterations are generally needed for convergence
   !    of the algorithm.
   !
   !    IF, however, the limit of 10 iterations is reached, THEN the output
   !    PAR will contain the best value obtained so far.
   !
   !  Licensing:
   !
   !    This code may freely be copied, modified, and used for any purpose.
   !
   !  Modified:
   !
   !    24 January 2014
   !
   !  Author:
   !
   !    Original FORTRAN77 version by Jorge More, Burton Garbow, Kenneth Hillstrom.
   !    FORTRAN90 version by John Burkardt.
   !
   !  Reference:
   !
   !    Jorge More, Burton Garbow, Kenneth Hillstrom,
   !    User Guide for MINPACK-1,
   !    Technical Report ANL-80-74,
   !    Argonne National Laboratory, 1980.
   !
   !  Parameters:
   !
   !    Input, integer ( kind = 4 ) N, the order of R.
   !
   !    Input/output, real ( kind = 8 ) R(LDR,N),the N by N matrix.  The full
   !    upper triangle must contain the full upper triangle of the matrix R.
   !    On output the full upper triangle is unaltered, and the strict lower
   !    triangle CONTAINS the strict upper triangle (transposed) of the upper
   !    triangular matrix S.
   !
   !    Input, integer ( kind = 4 ) LDR, the leading dimension of R.  LDR must be
   !    no less than N.
   !
   !    Input, integer ( kind = 4 ) IPVT(N), defines the permutation matrix P
   !    such that A*P = Q*R.  Column J of P is column IPVT(J) of the
   !    identity matrix.
   !
   !    Input, real ( kind = 8 ) DIAG(N), the diagonal elements of the matrix D.
   !
   !    Input, real ( kind = 8 ) QTB(N), the first N elements of the vector Q'*B.
   !
   !    Input, real ( kind = 8 ) DELTA, an upper bound on the euclidean norm
   !    of D*X.  DELTA should be positive.
   !
   !    Input/output, real ( kind = 8 ) PAR.  On input an initial estimate of the
   !    Levenberg-Marquardt parameter.  On output the final estimate.
   !    PAR should be nonnegative.
   !
   !    Output, real ( kind = 8 ) X(N), the least squares solution of the system
   !    A*X = B, sqrt(PAR)*D*X = 0, for the output value of PAR.
   !
   !    Output, real ( kind = 8 ) SDIAG(N), the diagonal elements of the upper
   !    triangular matrix S.
   !
   IMPLICIT NONE

   integer ( kind = 4 ) ldr
   integer ( kind = 4 ) n

   real ( kind = 8 ) delta
   real ( kind = 8 ) diag(n)
   real ( kind = 8 ) dwarf
   real ( kind = 8 ) dxnorm
   real ( kind = 8 ) gnorm
   real ( kind = 8 ) fp
   integer ( kind = 4 ) i
   integer ( kind = 4 ) ipvt(n)
   integer ( kind = 4 ) iter
   integer ( kind = 4 ) j
   integer ( kind = 4 ) k
   integer ( kind = 4 ) l
   integer ( kind = 4 ) nsing
   real ( kind = 8 ) par
   real ( kind = 8 ) parc
   real ( kind = 8 ) parl
   real ( kind = 8 ) paru
   real ( kind = 8 ) qnorm
   real ( kind = 8 ) qtb(n)
   real ( kind = 8 ) r(ldr,n)
   real ( kind = 8 ) sdiag(n)
   real ( kind = 8 ) sum2
   real ( kind = 8 ) temp
   real ( kind = 8 ) wa1(n)
   real ( kind = 8 ) wa2(n)
   real ( kind = 8 ) x(n)

      !
      !  DWARF is the smallest positive magnitude.
      !
      dwarf = tiny ( dwarf )
      !
      !  Compute and store in X the Gauss-Newton direction.
      !
      !  IF the jacobian is rank-deficient, obtain a least squares solution.
      !
      nsing = n

      DO j = 1, n
         wa1(j) = qtb(j)
         IF ( r(j,j) == 0.0D+00 .and. nsing == n ) THEN
            nsing = j - 1
         ENDIF
         IF ( nsing < n ) THEN
            wa1(j) = 0.0D+00
         ENDIF
      ENDDO

      DO k = 1, nsing
         j = nsing - k + 1
         wa1(j) = wa1(j) / r(j,j)
         temp = wa1(j)
         wa1(1:j-1) = wa1(1:j-1) - r(1:j-1,j) * temp
      ENDDO

      DO j = 1, n
         l = ipvt(j)
         x(l) = wa1(j)
      ENDDO
      !
      !  Initialize the iteration counter.
      !  Evaluate the FUNCTION at the origin, and test
      !  for acceptance of the Gauss-Newton direction.
      !
      iter = 0
      wa2(1:n) = diag(1:n) * x(1:n)
      dxnorm = enorm ( n, wa2 )
      fp = dxnorm - delta

      IF ( fp <= 0.1D+00 * delta ) THEN
         IF ( iter == 0 ) THEN
            par = 0.0D+00
         ENDIF
         RETURN
      ENDIF
      !
      !  IF the jacobian is not rank deficient, the Newton
      !  step provides a lower bound, PARL, for the zero of
      !  the FUNCTION.
      !
      !  Otherwise set this bound to zero.
      !
      parl = 0.0D+00

      IF ( n <= nsing ) THEN

         DO j = 1, n
            l = ipvt(j)
            wa1(j) = diag(l) * ( wa2(l) / dxnorm )
         ENDDO

         DO j = 1, n
            sum2 = dot_product ( wa1(1:j-1), r(1:j-1,j) )
            wa1(j) = ( wa1(j) - sum2 ) / r(j,j)
         ENDDO

         temp = enorm ( n, wa1 )
         parl = ( ( fp / delta ) / temp ) / temp

      ENDIF
      !
      !  Calculate an upper bound, PARU, for the zero of the FUNCTION.
      !
      DO j = 1, n
         sum2 = dot_product ( qtb(1:j), r(1:j,j) )
         l = ipvt(j)
         wa1(j) = sum2 / diag(l)
      ENDDO

      gnorm = enorm ( n, wa1 )
      paru = gnorm / delta

      IF ( paru == 0.0D+00 ) THEN
         paru = dwarf / min ( delta, 0.1D+00 )
      ENDIF
      !
      !  IF the input PAR lies outside of the interval (PARL, PARU),
      !  set PAR to the closer endpoint.
      !
      par = max ( par, parl )
      par = min ( par, paru )
      IF ( par == 0.0D+00 ) THEN
         par = gnorm / dxnorm
      ENDIF
      !
      !  Beginning of an iteration.
      !
      DO

         iter = iter + 1
         !
         !  Evaluate the FUNCTION at the current value of PAR.
         !
         IF ( par == 0.0D+00 ) THEN
            par = max ( dwarf, 0.001D+00 * paru )
         ENDIF

         wa1(1:n) = sqrt ( par ) * diag(1:n)

         CALL qrsolv ( n, r, ldr, ipvt, wa1, qtb, x, sdiag )

         wa2(1:n) = diag(1:n) * x(1:n)
         dxnorm = enorm ( n, wa2 )
         temp = fp
         fp = dxnorm - delta
         !
         !  IF the FUNCTION is small enough, accept the current value of PAR.
         !
         IF ( abs ( fp ) <= 0.1D+00 * delta ) THEN
            EXIT
         ENDIF
         !
         !  Test for the exceptional cases WHERE PARL
         !  is zero or the number of iterations has reached 10.
         !
         IF ( parl == 0.0D+00 .and. fp <= temp .and. temp < 0.0D+00 ) THEN
            EXIT
         ELSEIF ( iter == 10 ) THEN
            EXIT
         ENDIF
         !
         !  Compute the Newton correction.
         !
         DO j = 1, n
            l = ipvt(j)
            wa1(j) = diag(l) * ( wa2(l) / dxnorm )
         ENDDO

         DO j = 1, n
            wa1(j) = wa1(j) / sdiag(j)
            temp = wa1(j)
            wa1(j+1:n) = wa1(j+1:n) - r(j+1:n,j) * temp
         ENDDO

         temp = enorm ( n, wa1 )
         parc = ( ( fp / delta ) / temp ) / temp
         !
         !  Depending on the sign of the FUNCTION, update PARL or PARU.
         !
         IF ( 0.0D+00 < fp ) THEN
            parl = max ( parl, par )
         ELSEIF ( fp < 0.0D+00 ) THEN
            paru = min ( paru, par )
         ENDIF
         !
         !  Compute an improved estimate for PAR.
         !
         par = max ( parl, par + parc )
         !
         !  END of an iteration.
         !
      ENDDO
      !
      !  Termination.
      !
      IF ( iter == 0 ) THEN
         par = 0.0D+00
      ENDIF

      RETURN

   END SUBROUTINE lmpar

   SUBROUTINE qrfac ( m, n, a, lda, pivot, ipvt, lipvt, rdiag, acnorm )

   !*****************************************************************************80
   !
   !! QRFAC computes a QR factorization using Householder transformations.
   !
   !  Discussion:
   !
   !    This FUNCTION uses Householder transformations with optional column
   !    pivoting to compute a QR factorization of the
   !    M by N matrix A.  That is, QRFAC determines an orthogonal
   !    matrix Q, a permutation matrix P, and an upper trapezoidal
   !    matrix R with diagonal elements of nonincreasing magnitude,
   !    such that A*P = Q*R.
   !
   !    The Householder transformation for column K, K = 1,2,...,min(M,N),
   !    is of the form
   !
   !      I - ( 1 / U(K) ) * U * U'
   !
   !    WHERE U has zeros in the first K-1 positions.
   !
   !    The form of this transformation and the method of pivoting first
   !    appeared in the corresponding LINPACK routine.
   !
   !  Licensing:
   !
   !    This code may freely be copied, modified, and used for any purpose.
   !
   !  Modified:
   !
   !    06 April 2010
   !
   !  Author:
   !
   !    Original FORTRAN77 version by Jorge More, Burton Garbow, Kenneth Hillstrom.
   !    FORTRAN90 version by John Burkardt.
   !
   !  Reference:
   !
   !    Jorge More, Burton Garbow, Kenneth Hillstrom,
   !    User Guide for MINPACK-1,
   !    Technical Report ANL-80-74,
   !    Argonne National Laboratory, 1980.
   !
   !  Parameters:
   !
   !    Input, integer ( kind = 4 ) M, the number of rows of A.
   !
   !    Input, integer ( kind = 4 ) N, the number of columns of A.
   !
   !    Input/output, real ( kind = 8 ) A(LDA,N), the M by N array.
   !    On input, A CONTAINS the matrix for which the QR factorization is to
   !    be computed.  On output, the strict upper trapezoidal part of A CONTAINS
   !    the strict upper trapezoidal part of R, and the lower trapezoidal
   !    part of A CONTAINS a factored form of Q, the non-trivial elements of
   !    the U vectors described above.
   !
   !    Input, integer ( kind = 4 ) LDA, the leading dimension of A, which must
   !    be no less than M.
   !
   !    Input, logical PIVOT, is TRUE IF column pivoting is to be carried out.
   !
   !    Output, integer ( kind = 4 ) IPVT(LIPVT), defines the permutation matrix P
   !    such that A*P = Q*R.  Column J of P is column IPVT(J) of the identity
   !    matrix.  IF PIVOT is false, IPVT is not referenced.
   !
   !    Input, integer ( kind = 4 ) LIPVT, the dimension of IPVT, which should
   !    be N IF pivoting is used.
   !
   !    Output, real ( kind = 8 ) RDIAG(N), CONTAINS the diagonal elements of R.
   !
   !    Output, real ( kind = 8 ) ACNORM(N), the norms of the corresponding
   !    columns of the input matrix A.  IF this information is not needed,
   !    THEN ACNORM can coincide with RDIAG.
   !
   IMPLICIT NONE

   integer ( kind = 4 ) lda
   integer ( kind = 4 ) lipvt
   integer ( kind = 4 ) m
   integer ( kind = 4 ) n

   real ( kind = 8 ) a(lda,n)
   real ( kind = 8 ) acnorm(n)
   real ( kind = 8 ) ajnorm
   real ( kind = 8 ) epsmch
   integer ( kind = 4 ) i
   integer ( kind = 4 ) i4_temp
   integer ( kind = 4 ) ipvt(lipvt)
   integer ( kind = 4 ) j
   integer ( kind = 4 ) k
   integer ( kind = 4 ) kmax
   integer ( kind = 4 ) minmn
   logical pivot
   real ( kind = 8 ) r8_temp(m)
   real ( kind = 8 ) rdiag(n)
   real ( kind = 8 ) temp
   real ( kind = 8 ) wa(n)

      epsmch = epsilon ( epsmch )
      !
      !  Compute the initial column norms and initialize several arrays.
      !
      DO j = 1, n
         acnorm(j) = enorm ( m, a(1:m,j) )
      ENDDO

      rdiag(1:n) = acnorm(1:n)
      wa(1:n) = acnorm(1:n)

      IF ( pivot ) THEN
         DO j = 1, n
            ipvt(j) = j
         ENDDO
      ENDIF
      !
      !  Reduce A to R with Householder transformations.
      !
      minmn = min ( m, n )

      DO j = 1, minmn
         !
         !  Bring the column of largest norm into the pivot position.
         !
         IF ( pivot ) THEN

            kmax = j

            DO k = j, n
               IF ( rdiag(kmax) < rdiag(k) ) THEN
                  kmax = k
               ENDIF
            ENDDO

            IF ( kmax /= j ) THEN

               r8_temp(1:m) = a(1:m,j)
               a(1:m,j)     = a(1:m,kmax)
               a(1:m,kmax)  = r8_temp(1:m)

               rdiag(kmax) = rdiag(j)
               wa(kmax) = wa(j)

               i4_temp    = ipvt(j)
               ipvt(j)    = ipvt(kmax)
               ipvt(kmax) = i4_temp

            ENDIF

         ENDIF
         !
         !  Compute the Householder transformation to reduce the
         !  J-th column of A to a multiple of the J-th unit vector.
         !
         ajnorm = enorm ( m-j+1, a(j,j) )

         IF ( ajnorm /= 0.0D+00 ) THEN

            IF ( a(j,j) < 0.0D+00 ) THEN
               ajnorm = -ajnorm
            ENDIF

            a(j:m,j) = a(j:m,j) / ajnorm
            a(j,j) = a(j,j) + 1.0D+00
            !
            !  Apply the transformation to the remaining columns and update the norms.
            !
            DO k = j + 1, n

               temp = dot_product ( a(j:m,j), a(j:m,k) ) / a(j,j)

               a(j:m,k) = a(j:m,k) - temp * a(j:m,j)

               IF ( pivot .and. rdiag(k) /= 0.0D+00 ) THEN

                  temp = a(j,k) / rdiag(k)
                  rdiag(k) = rdiag(k) * sqrt ( max ( 0.0D+00, 1.0D+00-temp ** 2 ) )

                  IF ( 0.05D+00 * ( rdiag(k) / wa(k) ) ** 2 <= epsmch ) THEN
                     rdiag(k) = enorm ( m-j, a(j+1,k) )
                     wa(k) = rdiag(k)
                  ENDIF

               ENDIF

            ENDDO

         ENDIF

         rdiag(j) = - ajnorm

      ENDDO

      RETURN

   END SUBROUTINE qrfac

   SUBROUTINE qrsolv ( n, r, ldr, ipvt, diag, qtb, x, sdiag )

   !*****************************************************************************80
   !
   !! QRSOLV solves a rectangular linear system A*x=b in the least squares sense.
   !
   !  Discussion:
   !
   !    Given an M by N matrix A, an N by N diagonal matrix D,
   !    and an M-vector B, the problem is to determine an X which
   !    solves the system
   !
   !      A*X = B
   !      D*X = 0
   !
   !    in the least squares sense.
   !
   !    This FUNCTION completes the solution of the problem
   !    IF it is provided with the necessary information from the
   !    QR factorization, with column pivoting, of A.  That is, IF
   !    A*P = Q*R, WHERE P is a permutation matrix, Q has orthogonal
   !    columns, and R is an upper triangular matrix with diagonal
   !    elements of nonincreasing magnitude, THEN QRSOLV expects
   !    the full upper triangle of R, the permutation matrix p,
   !    and the first N components of Q'*B.
   !
   !    The system is THEN equivalent to
   !
   !      R*Z = Q'*B
   !      P'*D*P*Z = 0
   !
   !    WHERE X = P*Z.  IF this system does not have full rank,
   !    THEN a least squares solution is obtained.  On output QRSOLV
   !    also provides an upper triangular matrix S such that
   !
   !      P'*(A'*A + D*D)*P = S'*S.
   !
   !    S is computed within QRSOLV and may be of separate interest.
   !
   !  Licensing:
   !
   !    This code may freely be copied, modified, and used for any purpose.
   !
   !  Modified:
   !
   !    06 April 2010
   !
   !  Author:
   !
   !    Original FORTRAN77 version by Jorge More, Burton Garbow, Kenneth Hillstrom.
   !    FORTRAN90 version by John Burkardt.
   !
   !  Reference:
   !
   !    Jorge More, Burton Garbow, Kenneth Hillstrom,
   !    User Guide for MINPACK-1,
   !    Technical Report ANL-80-74,
   !    Argonne National Laboratory, 1980.
   !
   !  Parameters:
   !
   !    Input, integer ( kind = 4 ) N, the order of R.
   !
   !    Input/output, real ( kind = 8 ) R(LDR,N), the N by N matrix.
   !    On input the full upper triangle must contain the full upper triangle
   !    of the matrix R.  On output the full upper triangle is unaltered, and
   !    the strict lower triangle CONTAINS the strict upper triangle
   !    (transposed) of the upper triangular matrix S.
   !
   !    Input, integer ( kind = 4 ) LDR, the leading dimension of R, which must be
   !    at least N.
   !
   !    Input, integer ( kind = 4 ) IPVT(N), defines the permutation matrix P such
   !    that A*P = Q*R.  Column J of P is column IPVT(J) of the identity matrix.
   !
   !    Input, real ( kind = 8 ) DIAG(N), the diagonal elements of the matrix D.
   !
   !    Input, real ( kind = 8 ) QTB(N), the first N elements of the vector Q'*B.
   !
   !    Output, real ( kind = 8 ) X(N), the least squares solution.
   !
   !    Output, real ( kind = 8 ) SDIAG(N), the diagonal elements of the upper
   !    triangular matrix S.
   !
   IMPLICIT NONE

   integer ( kind = 4 ) ldr
   integer ( kind = 4 ) n

   real ( kind = 8 ) c
   real ( kind = 8 ) cotan
   real ( kind = 8 ) diag(n)
   integer ( kind = 4 ) i
   integer ( kind = 4 ) ipvt(n)
   integer ( kind = 4 ) j
   integer ( kind = 4 ) k
   integer ( kind = 4 ) l
   integer ( kind = 4 ) nsing
   real ( kind = 8 ) qtb(n)
   real ( kind = 8 ) qtbpj
   real ( kind = 8 ) r(ldr,n)
   real ( kind = 8 ) s
   real ( kind = 8 ) sdiag(n)
   real ( kind = 8 ) sum2
   real ( kind = 8 ) t
   real ( kind = 8 ) temp
   real ( kind = 8 ) wa(n)
   real ( kind = 8 ) x(n)

      !
      !  Copy R and Q'*B to preserve input and initialize S.
      !
      !  In particular, SAVE the diagonal elements of R in X.
      !
      DO j = 1, n
         r(j:n,j) = r(j,j:n)
         x(j) = r(j,j)
      ENDDO

      wa(1:n) = qtb(1:n)
      !
      !  Eliminate the diagonal matrix D using a Givens rotation.
      !
      DO j = 1, n
         !
         !  Prepare the row of D to be eliminated, locating the
         !  diagonal element using P from the QR factorization.
         !
         l = ipvt(j)

         IF ( diag(l) /= 0.0D+00 ) THEN

            sdiag(j:n) = 0.0D+00
            sdiag(j) = diag(l)
            !
            !  The transformations to eliminate the row of D
            !  modify only a single element of Q'*B
            !  beyond the first N, which is initially zero.
            !
            qtbpj = 0.0D+00

            DO k = j, n
               !
               !  Determine a Givens rotation which eliminates the
               !  appropriate element in the current row of D.
               !
               IF ( sdiag(k) /= 0.0D+00 ) THEN

                  IF ( abs ( r(k,k) ) < abs ( sdiag(k) ) ) THEN
                     cotan = r(k,k) / sdiag(k)
                     s = 0.5D+00 / sqrt ( 0.25D+00 + 0.25D+00 * cotan ** 2 )
                     c = s * cotan
                  ELSE
                     t = sdiag(k) / r(k,k)
                     c = 0.5D+00 / sqrt ( 0.25D+00 + 0.25D+00 * t ** 2 )
                     s = c * t
                  ENDIF
                  !
                  !  Compute the modified diagonal element of R and
                  !  the modified element of (Q'*B,0).
                  !
                  r(k,k) = c * r(k,k) + s * sdiag(k)
                  temp = c * wa(k) + s * qtbpj
                  qtbpj = - s * wa(k) + c * qtbpj
                  wa(k) = temp
                  !
                  !  Accumulate the tranformation in the row of S.
                  !
                  DO i = k + 1, n
                     temp = c * r(i,k) + s * sdiag(i)
                     sdiag(i) = - s * r(i,k) + c * sdiag(i)
                     r(i,k) = temp
                  ENDDO

               ENDIF

            ENDDO

         ENDIF
         !
         !  Store the diagonal element of S and restore
         !  the corresponding diagonal element of R.
         !
         sdiag(j) = r(j,j)
         r(j,j) = x(j)

      ENDDO
      !
      !  Solve the triangular system for Z.  IF the system is
      !  singular, THEN obtain a least squares solution.
      !
      nsing = n

      DO j = 1, n

         IF ( sdiag(j) == 0.0D+00 .and. nsing == n ) THEN
            nsing = j - 1
         ENDIF

         IF ( nsing < n ) THEN
            wa(j) = 0.0D+00
         ENDIF

      ENDDO

      DO j = nsing, 1, -1
         sum2 = dot_product ( wa(j+1:nsing), r(j+1:nsing,j) )
         wa(j) = ( wa(j) - sum2 ) / sdiag(j)
      ENDDO
      !
      !  Permute the components of Z back to components of X.
      !
      DO j = 1, n
         l = ipvt(j)
         x(l) = wa(j)
      ENDDO

      RETURN

   END SUBROUTINE qrsolv

   FUNCTION enorm ( n, x )

   !*****************************************************************************80
   !
   !! ENORM computes the Euclidean norm of a vector.
   !
   !  Discussion:
   !
   !    This is an extremely simplified version of the original ENORM
   !    routine, which has been renamed to "ENORM2".
   !
   !  Licensing:
   !
   !    This code may freely be copied, modified, and used for any purpose.
   !
   !  Modified:
   !
   !    06 April 2010
   !
   !  Author:
   !
   !    Original FORTRAN77 version by Jorge More, Burton Garbow, Kenneth Hillstrom.
   !    FORTRAN90 version by John Burkardt.
   !
   !  Reference:
   !
   !    Jorge More, Burton Garbow, Kenneth Hillstrom,
   !    User Guide for MINPACK-1,
   !    Technical Report ANL-80-74,
   !    Argonne National Laboratory, 1980.
   !
   !  Parameters:
   !
   !    Input, integer ( kind = 4 ) N, is the length of the vector.
   !
   !    Input, real ( kind = 8 ) X(N), the vector whose norm is desired.
   !
   !    Output, real ( kind = 8 ) ENORM, the Euclidean norm of the vector.
   !
   IMPLICIT NONE

   integer ( kind = 4 ) n
   real ( kind = 8 ) x(n)
   real ( kind = 8 ) enorm

      enorm = sqrt ( sum ( x(1:n) ** 2 ))

      RETURN

   END FUNCTION enorm

   SUBROUTINE tridia (n, a, b, c, r, u)

   USE MOD_Precision
   IMPLICIT NONE
   integer,  intent(in) :: n       !length of diagonal element vector
   real(r8), intent(in) :: a(1:n)  !subdiagonal elements
   real(r8), intent(in) :: b(1:n)  !diagonal elements
   real(r8), intent(in) :: c(1:n)  !superdiagonal elements
   real(r8), intent(in) :: r(1:n)  !right hand side
   real(r8), intent(out) :: u(1:n) !solution vector

   integer j
   real(r8) gam(1:n),bet

      bet = b(1)
      u(1) = r(1) / bet
      DO j = 2, n
            gam(j) = c(j-1) / bet
            bet = b(j) - a(j) * gam(j)
            u(j) = (r(j) - a(j)*u(j-1)) / bet
      ENDDO
      DO j = n-1, 1, -1
            u(j) = u(j) - gam(j+1) * u(j+1)
      ENDDO

   END SUBROUTINE tridia

   ! -----------------------------------------------------------------
   SUBROUTINE polint(xa,ya,n,x,y)

   ! Given arrays xa and ya, each of length n, and gi
   ! value y, and an error estimate dy. IF P (x) is the p
   ! P (xa(i)) = ya(i), i = 1, . . . , n, THEN the returned value
   ! (from: "Numerical Recipes")

   USE MOD_Precision
   IMPLICIT NONE
   integer n,NMAX
   real(r8) dy,x,y,xa(n),ya(n)
   parameter (NMAX=10)      !Largest anticipated val
   integer i,m,ns
   real(r8) den,dif,dift,ho,hp,w,c(NMAX),d(NMAX)

      ns=1
      dif=abs(x-xa(1))

      DO i=1,n       !Here we find the index ns of the closest table entry,
         dift=abs(x-xa(i))
         IF(dift.lt.dif) THEN
            ns=i
            dif=dift
         ENDIF
         c(i)=ya(i)  !and initialize the tableau of c's and d's.
         d(i)=ya(i)
      ENDDO

      y=ya(ns)       !This is the initial approximation to y.
      ns=ns-1

      DO m=1,n-1  !For each column of the tableau,
         DO i=1,n-m   !we loop over the current c's and d's and update them.
            ho=xa(i)-x
            hp=xa(i+m)-x
            w=c(i+1)-d(i)
            den=ho-hp
            IF(den.eq.0.) print*, 'failure in polint'  !two input xa's are identical.
            den=w/den
            d(i)=hp*den                                !here the c's and d's are updated.
            c(i)=ho*den
         ENDDO
         IF(2*ns.lt.n-m)THEN  !After each column in the tableau is completed, we decide
            dy=c(ns+1)        !which correction, c or d, we want to add to our accumulating
         ELSE                 !value of y, i.e., which path to take through
            dy=d(ns)          !the tableau-forking up or down. We DO this in such a
            ns=ns-1           !way as to take the most "straight line" route through the
         ENDIF                !tableau to its apex, updating ns accordingly to keep track
         y=y+dy               !of WHERE we are. This route keeps the partial approximations
      ENDDO                   !centered (insofar as possible) on the target x. T he
      !last dy added is thus the error indication.

   END SUBROUTINE polint

END MODULE MOD_Utils
