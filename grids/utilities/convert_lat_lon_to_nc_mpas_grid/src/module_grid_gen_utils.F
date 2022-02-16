module grid_gen_utils

   use sphere_utilities


   contains
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! SUBROUTINE GENERATE_BASE_GRID
   !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine generate_base_grid(points, neighbors, triangles)
   
      use data_types
      use grid_params
      use grid_constants
   
      implicit none
   
      type (geo_point), dimension(nicos_vertices), intent(out) :: points
      integer, dimension(nicos_neighbors,nicos_vertices), intent(out) :: neighbors
      integer, dimension(3,20), intent(out) :: triangles
   
      real dl,dtri
      integer im1, ii, ip1, i
   
   !--  set point positions
   
      dl = 2.*pii/5.
      dtri = acos(cos(0.5*dl)/(1.+cos(0.5*dl)))
   
      points(1)%lat = pii/2.  ! latitude of north pole point
      points(1)%lon = 0.      ! longitude of north pole point
   
      !
      ! Set points just south of the north pole
      !
      do i=2,6
        points(i)%lat = pii/2. - dtri       ! latitude, same for all 5 point
        points(i)%lon = 0. + dl*float(i-2)  ! longitude
      enddo
   
      points(12)%lat = -pii/2.  ! latitude of south pole point
      points(12)%lon = 0.      ! longitude of south pole point
   
      do i=7,11  ! the set points just north of the south pole
        points(i)%lat = -pii/2. + dtri  ! latitude, same for all 5 point
        points(i)%lon = 0. + dl*(0.5+float(i-7))  ! longitude. shifted from north
      enddo
   
   !  construct neighbors
   
      do i=1,5
        neighbors(i,1) = i+1  ! north pole neighbors, points 2 through 6
        neighbors(i,12) = i+6 ! south pole neighbors, points 7 through 11
        neighbors(1,i+1) = 1 ! add north pole point
        neighbors(1,i+6) = 12
      enddo
   
      do i=2,6
        im1 = i-1
        ip1 = i+1
        if(im1 .lt. 2) im1 = 6
        if(ip1 .gt. 6) ip1 = 2
        neighbors(2,i) = im1
        neighbors(3,i) = ip1
   
        im1 = i+4
        ii  = i+5
        if(im1 .lt. 7) im1 = 11
        neighbors(4,i) = im1
        neighbors(5,i) = ii
   
      enddo
   
      do i=7,11
        im1 = i-1
        ip1 = i+1
        if(im1 .lt. 7) im1 = 11
        if(ip1 .gt. 11) ip1 = 7
        neighbors(2,i) = im1
        neighbors(3,i) = ip1
   
        im1 = i-5
        ii  = i-4
        if(ii .gt. 6) ii = 2
        neighbors(4,i) = im1
        neighbors(5,i) = ii
      enddo
   
   ! set the triangle points
   
      do i=1,5  !  triangles with north pole points
        triangles(1,i) = 1
        triangles(2,i) = i+1
        triangles(3,i) = i+2
        if(triangles(2,i) .gt. 6) triangles(2,i) = triangles(2,i) - 5
        if(triangles(3,i) .gt. 6) triangles(3,i) = triangles(3,i) - 5
      enddo
   
      do i=16,20  ! triangles with south pole points
        triangles(1,i) = 12
        triangles(2,i) = i-9
        triangles(3,i) = i-8
        if(triangles(2,i) .gt. 11) triangles(2,i) = triangles(2,i) - 5
        if(triangles(3,i) .gt. 11) triangles(3,i) = triangles(3,i) - 5
      enddo
   
      do i=6,10  ! upward pointing equator triangles 
        triangles(1,i) = i-4
        triangles(2,i) = i+5
        triangles(3,i) = i+1
        if(triangles(2,i) .gt. 11) triangles(2,i) = triangles(2,i) - 5
        if(triangles(3,i) .gt. 11) triangles(3,i) = triangles(3,i) - 5
      enddo
   
      do i=11,15  ! downward pointing equator triangles 
        triangles(1,i) = i-4
        triangles(2,i) = i-9
        triangles(3,i) = i-8
        if(triangles(2,i) .gt. 6) triangles(2,i) = triangles(2,i) - 5
        if(triangles(3,i) .gt. 6) triangles(3,i) = triangles(3,i) - 5
      enddo
   
      write(6,*) ' '
      write(6,*) ' triangle nodes '
      do i=1,20
        write(6,*) i, triangles(1,i),triangles(2,i),triangles(3,i)
      enddo
   
   end subroutine generate_base_grid 
   
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! SUBROUTINE PROCESS_TRIANGLE
   !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine process_triangle(triangle, scalar, nmax, n, np)
   
      use data_types
   
      implicit none
   
      integer, intent(in) :: nmax, np
      integer, intent(inout) :: n
      type (geo_point), dimension(3), intent(in) :: triangle
      type (geo_point), dimension(nmax), intent(inout) :: scalar
   
      integer :: i, j, k
      type (geo_point), dimension(np) :: ab, ac, pline
   
      call great_circle_points(triangle(1), triangle(2), ab, np)
      call great_circle_points(triangle(1), triangle(3), ac, np)
   
      do i=1,np
         call add_point(scalar, nmax, n, ab(i))
         call add_point(scalar, nmax, n, ac(i))
      end do
       
      do i=3,np-1
         call great_circle_points(ab(i), ac(i), pline, i)
         do j=2,i-1
            n = n + 1
            scalar(n) = pline(j)
!!! No need to check for points on the interior of the triangle
!!!            call add_point(scalar, nmax, n, pline(j))
         end do 
      end do

      i=np
      call great_circle_points(ab(i), ac(i), pline, i)
      do j=2,i-1
         call add_point(scalar, nmax, n, pline(j))
      end do 
   
   end subroutine process_triangle
   
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! SUBROUTINE ADD_POINT
   !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine add_point(array, nmax, n, point)
   
      use data_types
   
      implicit none
   
      integer, intent(in) :: nmax
      integer, intent(inout) :: n
      type (geo_point), intent(in) :: point
      type (geo_point), dimension(nmax), intent(inout) :: array
   
      integer :: i
      logical :: already_have
   
      already_have = .false.
   
      do i=1,n
         if (abs(point%lat - array(i)%lat) < 0.00001 .and. &
             abs(point%lon - array(i)%lon) < 0.00001) then
            already_have = .true.
            exit
         end if
      end do
   
      if (.not. already_have) then
         n = n + 1
         array(n) = point
      end if
   
   end subroutine add_point
   
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! SUBROUTINE COMPUTE_H_AREA
   !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine compute_h_area(corners, centers, areas, n)
   
      use data_types
   
      implicit none
   
      integer, intent(in) :: n
      type (geo_point), dimension(6,2*n-1,n), intent(inout) :: corners
      type (geo_point), dimension(2*n-1,n), intent(inout) :: centers
      real, dimension(2*n-1,n), intent(inout) :: areas
   
      integer :: i, j, it, itp1
      real :: hex_area
   
      do j=1,n
      do i=1,2*n-1
        hex_area = 0.
        do it=1,6   !  6 triangles in the hexagon 
          itp1 = it+1
          if(itp1 > 6) itp1 = 1
            hex_area = hex_area + triangle_area( centers(i,j),      &
                                                 corners(  it,i,j), &
                                                 corners(itp1,i,j), 1.)
        enddo
        areas(i,j) = hex_area
      enddo
      enddo
   
   end subroutine compute_h_area
   
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! SUBROUTINE COMPUTE_EDGE_LENGTHS
   !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine compute_edge_lengths(corners, edge_lengths, n)
   
      use data_types
   
      implicit none
   
      integer, intent(in) :: n
      type (geo_point), dimension(6,2*n-1,n), intent(inout) :: corners
      real, dimension(6,2*n-1,n), intent(inout) :: edge_lengths
   
      integer :: i, j, it, itp1
   
      do j=1,n
      do i=1,2*n-1
        do it=1,6
          itp1 = it+1
          if(itp1 > 6) itp1 = 1
          edge_lengths(it,i,j) = sphere_distance( corners(  it,i,j), &
                                                  corners(itp1,i,j), 1.)
        end do
      end do
      end do
   
   end subroutine compute_edge_lengths
   
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! SUBROUTINE COMPUTE_DX
   !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine compute_dx( centers, dx, n )
   
      use data_types
   
      implicit none
   
      integer, intent(in) :: n
      type (geo_point), dimension(0:2*n,0:n+1), intent(inout) :: centers
      real, dimension(3,2*n-1,n), intent(inout) :: dx
   
      integer :: i, j
   
      do j=1,n
      do i=1,2*n-1 
        dx(1,i,j) = sphere_distance( centers(i  ,j  ), &
                                     centers(i-1,j  ), 1. )
        dx(2,i,j) = sphere_distance( centers(i  ,j  ), &
                                     centers(i-1,j-1), 1. )
        dx(3,i,j) = sphere_distance( centers(i  ,j  ), &
                                     centers(i  ,j-1), 1. )
      enddo
      enddo
   
   end subroutine compute_dx
   
end module grid_gen_utils
