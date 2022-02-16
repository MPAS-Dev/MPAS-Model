module data_types

   integer, parameter :: LESS = -1, EQUAL = 0, GREATER = 1

   type geo_point
      real :: lat, lon
   end type geo_point

   type send_list_ptr
      integer :: nodeID
      integer :: nNodeList
      integer, pointer, dimension(:) :: nodeList
      type (send_list_ptr), pointer :: next
   end type send_list_ptr

   type recv_list_ptr
      integer :: nodeID
      integer :: nNodeList
      integer, pointer, dimension(:) :: nodeList
      type (recv_list_ptr), pointer :: next
   end type recv_list_ptr

   type adjacency_list
      integer :: nNodes
      integer :: nNeighbors
      integer, pointer, dimension(:) :: neighbor, start, len
   end type adjacency_list

   type binary_tree
      integer :: node1, node2
      integer :: vertex1, vertex2
      real :: lat1, lon1, lat2, lon2
      type (binary_tree), pointer :: left, right, parent
   end type binary_tree

   contains


   integer function cmp_points(a, b)
 
      implicit none

      type (geo_point), intent(in) :: a, b

      if (a%lat > b%lat) then
         cmp_points = GREATER
      else if (a%lat == b%lat) then
         if (a%lon > b%lon) then
            cmp_points = GREATER
         else if (a%lon == b%lon) then
            cmp_points = EQUAL
         else
            cmp_points = LESS
         end if 
      else
         cmp_points = LESS
      end if

   end function cmp_points


   subroutine swap_points(a, b)

      implicit none 

      type (geo_point), intent(inout) :: a, b

      type (geo_point) :: temp

      temp = a 
      a = b
      b = temp

   end subroutine swap_points


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! SUBROUTINE CONVERT_ADJACENCY_LIST
   !
   ! Convert adjacency list from format provided by STRIPACK to format used in
   !   our code.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine convert_adjacency_list(n, lend, nvc, list, lptr, alist)

      implicit none

      integer, intent(in) :: n, nvc
      integer, dimension(n), intent(in) :: lend
      integer, dimension(nvc), intent(in) :: lptr
      integer, dimension(nvc), intent(in) :: list
      type (adjacency_list), intent(inout) :: alist

      integer :: i, j, k, len, ipos

      len = 0

      ! Count total number of nodes
      do i=1,n

         ! Scan neighbors of i
         k = lend(i)
         k = lptr(lend(i))
         len = len + 1

         do while (k /= lend(i))
            k = lptr(k)
            len = len + 1
         end do

      end do

      alist % nNodes = n
      alist % nNeighbors = len
      allocate(alist % neighbor(len))
      allocate(alist % start(n))
      allocate(alist % len(n))

      ipos = 0
      do i=1,n

         ! Scan neighbors of i
         k = lend(i)
         k = lptr(lend(i))
         ipos = ipos + 1

         alist % start(i) = ipos
         alist % neighbor(ipos) = list(k)
         alist % len(i) = 1

         do while (k /= lend(i))
            k = lptr(k)
            ipos = ipos + 1

            alist % neighbor(ipos) = list(k)
            alist % len(i) = alist % len(i) + 1
         end do
      end do

   end subroutine convert_adjacency_list


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! SUBROUTINE CONVERT_CORNER_LIST
   !
   ! Convert VC list from format provided by STRIPACK to format used in
   !   our code.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine convert_corner_list(n, lend, nvc, listc, lptr, clist)

      implicit none

      integer, intent(in) :: n, nvc
      integer, dimension(n), intent(in) :: lend
      integer, dimension(nvc), intent(in) :: lptr
      integer, dimension(nvc), intent(in) :: listc
      type (adjacency_list), intent(inout) :: clist

      integer :: i, j, k, len, ipos

      len = 0

      ! Count total number of nodes
      do i=1,n

         ! Scan neighbors of i
         k = lend(i)
         k = lptr(lend(i))
         len = len + 1

         do while (k /= lend(i))
            k = lptr(k)
            len = len + 1
         end do

      end do

      clist % nNodes = n
      clist % nNeighbors = len
      allocate(clist % neighbor(len))
      allocate(clist % start(n))
      allocate(clist % len(n))

      ipos = 0
      do i=1,n

         ! Scan neighbors of i
         k = lend(i)
         k = lptr(lend(i))
         ipos = ipos + 1

         clist % start(i) = ipos
         clist % neighbor(ipos) = listc(k)
         clist % len(i) = 1

         do while (k /= lend(i))
            k = lptr(k)
            ipos = ipos + 1

            clist % neighbor(ipos) = listc(k)
            clist % len(i) = clist % len(i) + 1
         end do
      end do

   end subroutine convert_corner_list

end module data_types
