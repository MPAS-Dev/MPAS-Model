module mpas_kd_tree
    
   !***********************************************************************
   !
   !  module mpas_kd_tree
   !
   !> \brief   MPAS KD-Tree module
   !> \author  Miles A. Curry 
   !> \date    03/04/19
   !> \details
   !>
   !>
   !> TODO: Fail immediatly edge cases  
   !>       1. Different number of dimensions
   !>       2. No points
   !>       3. No dimensions
   !>
   !> TODO: Add nn-search - Search for n nearest neighbors instead of only 1
   !>
   !
   !-----------------------------------------------------------------------
   implicit none

   private

   public :: kdnode

   ! Public Subroutines
   public :: mpas_kd_insert
   public :: mpas_kd_construct
   public :: mpas_kd_search
   public :: mpas_kd_free

   type kdnode
      type (kdnode), pointer :: left => null()
      type (kdnode), pointer :: right => null()

      integer :: split_dim
      real, dimension(:), pointer :: point
      
      integer :: cell
      logical :: is_halo
   end type kdnode

   contains

   !***********************************************************************
   !
   !  routine mpas_kd_insert
   !
   !> \brief   MPAS KD-Tree insert routine
   !> \author  Miles A. Curry 
   !> \date    03/04/19
   !> \details
   !> This routine adds a point, `val(:)` to an existing KD-Tree or it creates 
   !> a new kdtree if kdtree is not associated.
   !>
   !> The `dim` variable is an optional dummy argument and should not be set 
   !> to zero, but preferably not used when starting with the root of a kdtree
   !> i.e.:
   !> 
   !> call mpas_kdtree(kdtree, val)
   !>
   !> TODO: Create a wrapper?
   !> TODO: Does insert work?
   !
   !-----------------------------------------------------------------------
   recursive subroutine mpas_kd_insert(kdtree, val, dim)

      implicit none

      ! Input Variables
      type(kdnode), intent(inout), pointer :: kdtree
      real, dimension(:), intent(in) :: val
      integer, optional, value :: dim

      integer :: d 

      if (.NOT. present(dim)) then
         d = 0
      else
         d = dim
      endif

      d = modulo(d, size(val)) + 1

      if ( .NOT. associated(kdtree)) then
         allocate(kdtree)
         allocate(kdtree % point(size(val)))
         kdtree % left => null()
         kdtree % right => null()
         kdtree % split_dim = d
         kdtree % point(:) = val(:)
         return
      endif

      if (val(kdtree % split_dim ) > kdtree % point(kdtree % split_dim)) then
         call mpas_kd_insert(kdtree % right, val, kdtree % split_dim)
      else
         call mpas_kd_insert(kdtree % left, val, kdtree % split_dim)
      endif

   end subroutine mpas_kd_insert

   !***********************************************************************
   !
   !  recusrive routine mpas_kd_construct_internal
   !
   !> \brief   Create a KD-Tree from a set of k-Dimensional points
   !> \author  Miles A. Curry 
   !> \date    03/04/19
   !> \details
   !> Recursive function for mpas_kd_construct. See mpas_kd_construct for
   !> more information.
   !>
   !
   !-----------------------------------------------------------------------
   recursive function mpas_kd_construct_internal(points, ndims, npoints, dim) result(tree)

      implicit none

      ! Input Varaibles
      !real, dimension(:,:) :: points
      type (kdnode), dimension(:), target :: points
      integer, intent(in) :: ndims
      integer, value :: npoints
      integer, value :: dim

      ! Return Value
      type (kdnode), pointer :: tree

      ! Local Variables
      integer :: median

      if (npoints < 1) then
         tree => null()
         return
      endif

      ! Sort the points at the split dimension
      dim = mod(dim, ndims) + 1
      call quickSort(points, dim, 1, npoints, ndims)

      median = (1 + npoints) / 2 

      points(median) % split_dim = dim
      tree => points(median)

      ! Build the right and left sub-trees but do not include the 
      ! node that was just allocated (i.e. points(:, median))
      points(median)%left => mpas_kd_construct_internal(points(1:median-1), ndims, median - 1, points(median) % split_dim)
      points(median)%right => mpas_kd_construct_internal(points(median+1:npoints), ndims, npoints - median, points(median) % split_dim)

   end function mpas_kd_construct_internal

   !***********************************************************************
   !
   !  routine mpas_kd_construct
   !
   !> \brief   Create a KD-Tree from a set of k-Dimensional points
   !> \author  Miles A. Curry 
   !> \date    03/04/19
   !> \details
   !> This routine creates a balanced KD-Tree from a set of K-dimensional 
   !> points via quicksort and it returns a pointer to the root node of that
   !> tree. The points dummy argument, should be an array with the dimensions
   !> defined as: `points(k, n)` with k being the number of dimensions, and n
   !> being the number of points.    
   !>
   !> tree => mpas_kd_construct(points)
   !>
   !
   !-----------------------------------------------------------------------
   function mpas_kd_construct(points, ndims) result(tree)

      implicit none

      ! Input Varaibles
      !real, dimension(:,:) :: points
      type (kdnode), dimension(:) :: points
      integer, intent(in) :: ndims

      ! Return Value
      type (kdnode), pointer :: tree

      ! Local Varaibles
      integer :: npoints

      npoints = size(points)
      
      if(npoints < 1) then
         ! No points were passed in, return null
         write(0,*) "ERROR: mpas_kd_tree - No points were passed in to construct!"
         tree => null()
         return
      endif

      tree => mpas_kd_construct_internal(points(:), ndims, npoints, 0)

   end function mpas_kd_construct

   !***********************************************************************
   !
   !  recursive routine mpas_kd_search_internal
   !
   !> \brief   Find the nearest neighbor within a KD-Tree
   !> \author  Miles A. Curry 
   !> \date    03/04/19
   !> \details
   !> Recursive subroutine for mpas_kd_search. See mpas_kd_search for more
   !> information.
   !
   !-----------------------------------------------------------------------
   recursive subroutine mpas_kd_search_internal(kdtree, query, res, distance)

      implicit none

      ! Input Variables
      type(kdnode), pointer, intent(in) :: kdtree
      real, dimension(:), intent(in) :: query
      type(kdnode), pointer, intent(inout) :: res
      !real, dimension(:), intent(inout) :: res
      real, intent(inout) :: distance

      ! Local Values
      real :: current_distance

      current_distance = sum((kdtree % point(:) - query(:))**2)
      if (current_distance < distance) then
         distance = current_distance
         res => kdtree
      endif

      !
      ! To find the nearest point, we first attempt to find the point in the same manner
      ! as a single deminsion BST.
      !
      ! However, because we are looking for the nearest neighbor, then there might be
      ! a possibility that the nearest neighbor is on the otherside of the tree.
      !
      ! Thus, to determine if we need to search the opposite child we just searched, we
      ! will compare the distance of the current minimum distance, and the root node
      ! that we branched off of.
      !
      ! If the distance to the root node, is less then the current minimum distance,
      ! then the nearist neighbor might be in opposite child.
      !

      ! TODO: Double precision calculations

      if (query(kdtree % split_dim) > kdtree % point(kdtree % split_dim)) then
         if (associated(kdtree % right)) then ! Search right
            call mpas_kd_search_internal(kdtree % right, query, res, distance)
         endif
         if ((kdtree % point(kdtree % split_dim) - query(kdtree % split_dim))**2 <= distance .AND. associated(kdtree % left)) then 
            call mpas_kd_search_internal(kdtree % left, query, res, distance)
         endif
      else if (query(kdtree % split_dim) < kdtree % point(kdtree % split_dim)) then 
         if (associated(kdtree % left)) then ! Search left
            call mpas_kd_search_internal(kdtree % left, query, res, distance)
         endif
         if ((kdtree % point(kdtree % split_dim) - query(kdtree % split_dim))**2 <= distance .AND. associated(kdtree % right)) then
            call mpas_kd_search_internal(kdtree % right, query, res, distance)
         endif
      else ! Nearest point could be in either left or right subtree, so search both
         if(associated(kdtree % right)) call mpas_kd_search_internal(kdtree % right, query, res, distance)
         if(associated(kdtree % left)) call mpas_kd_search_internal(kdtree % left, query, res, distance)
      endif

   end subroutine mpas_kd_search_internal

   !***********************************************************************
   !
   !  routine mpas_kd_search
   !
   !> \brief   Find the nearest neighbor within a KD-Tree
   !> \author  Miles A. Curry 
   !> \date    03/04/19
   !> \details
   !> Find `point` within `kdtree` and return the nearest neighbor (or the point)
   !> within `result` return the distance in `min_d`.
   !>
   !
   !-----------------------------------------------------------------------
   subroutine mpas_kd_search(kdtree, query, res, distance)

      implicit none
      type(kdnode), pointer, intent(in) :: kdtree
      real, dimension(:), intent(in) :: query
      type(kdnode), pointer, intent(inout) :: res
      real, intent(out), optional :: distance

      real :: dis

      if (size(kdtree % point) /= size(query)) then
         write(0,*) "ERROR: Searching a ", size(kdtree % point), "dimensional kdtree for a point that only"
         write(0,*) "ERROR: ", size(query), " dimensions. Please supply a point of equal"
         write(0,*) "ERROR: dimensions!"
         return
      endif

      dis = huge(dis)
      call mpas_kd_search_internal(kdtree, query, res, dis)

      if(present(distance)) then
         distance = dis
      endif

   end subroutine mpas_kd_search

   !***********************************************************************
   !
   !  routine mpas_kd_free
   !
   !> \brief   Free all nodes within a tree. 
   !> \author  Miles A. Curry
   !> \date    03/04/19
   !> \details
   !> Recursivly deallocate all nodes within `kdtree` including `kdtree` itself.
   !>
   !
   !-----------------------------------------------------------------------
   recursive subroutine mpas_kd_free(kdtree)

      implicit none
      type(kdnode), pointer :: kdtree

      write(0,*) "in tree"

      if (.not. associated(kdtree)) then
         return
      endif

      if (associated(kdtree % left)) then
         call mpas_kd_free(kdtree % left)
      endif

      if (associated(kdtree % right)) then
         call mpas_kd_free(kdtree % right)
      endif

      deallocate(kdtree % point)
      deallocate(kdtree)

   end subroutine mpas_kd_free


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Sorts
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   !***********************************************************************
   !
   !  routine mpas_kd_quicksort
   !
   !> \brief   Sort an array along a dimension
   !> \author  Miles A. Curry
   !> \date    03/04/19
   !> \details
   !> Sort points starting from arrayStart, to arrayEnd along the given dimension 
   !> `dim`. If two points are swapped, the entire K-Coordinate point are swapped.
   !>
   !> TODO: Change the name of this function to mpas_kd_quicksort
   !
   !-----------------------------------------------------------------------
   recursive subroutine quickSort(array, dim, arrayStart, arrayEnd, ndims)

      implicit none

      ! Input Variables
      !real, dimension(:,:) :: array
      type (kdnode), dimension(:) :: array
      integer, intent(in), value :: dim
      integer, intent(in), value :: arrayStart, arrayEnd
      integer, intent(in) :: ndims

      ! Local Variables
      type (kdnode) :: temp
      real, dimension(ndims) :: pivot_value

      integer :: l, r, pivot, s

      if ((arrayEnd - arrayStart) < 1) then
         return
      endif

      ! Create the left, right, and start pointers
      l = arrayStart
      r = arrayEnd - 1
      s = l

      pivot = (l+r)/2
      pivot_value = array(pivot) % point

      ! Move the pivot to the far right
      temp = array(pivot)
      array(pivot) = array(arrayEnd)
      array(arrayEnd) = temp

      do while ( .TRUE. )
         ! Advance the left pointer until it is a value less then our pivot_value(dim)
         do while ( .TRUE. )
            if (array(l) % point(dim) < pivot_value(dim)) then
               l = l + 1
            else
               exit
            endif
         enddo

         ! Advance the right pointer until it is a value more then our pivot_value(dim)
         do while ( .TRUE. )
            if ( r <= 0 ) then
               exit
            endif

            if(array(r) % point(dim) >= pivot_value(dim)) then
               r = r - 1
            else
               exit
            endif
         enddo

         if ( l >= r ) then 
            exit
         else ! Swap elements about the pivot
            temp = array(l)
            array(l) = array(r)
            array(r) = temp
         endif
      enddo

      ! Move the pivot to l ended up
      temp = array(l)
      array(l) = array(arrayEnd)
      array(arrayEnd) = temp

      !Quick Sort on the lower partition
      call quickSort(array(:), dim, s, l-1, ndims)

      !Quick sort on the upper partition
      call quickSort(array(:), dim, l+1, arrayEnd, ndims)

   end subroutine quicksort

end module mpas_kd_tree
