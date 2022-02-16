      module sphere_utilities

      contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     FUNCTION TRIANGLE_AREA
!     
!     Given the (latitude, longitude) coordinates of the corners of a triangle, 
!     plus the radius of the sphere, compute the area of the triangle.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      real function triangle_area(p1, p2, p3, radius)

      use data_types

      implicit none

      type (geo_point), intent(in) :: p1, p2, p3
      real, intent(in) :: radius

      real :: a, b, c, s, e, pii, tanqe

      pii = 2.*asin(1.0)

      a = sphere_distance(p1,p2,radius)
      b = sphere_distance(p2,p3,radius)
      c = sphere_distance(p3,p1,radius)
      s = 0.5*(a+b+c)

      tanqe = sqrt(tan(0.5*s)*tan(0.5*(s-a))*tan(0.5*(s-b))*tan(0.5*(s-c)))
      e = 4.*atan(tanqe)
      triangle_area = radius*radius*e

      end function triangle_area

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     FUNCTION OBTUSE
!     
!     Given the (latitude, longitude) coordinates of the corners of a triangle,
!     determine if the triangle is obtuse
!     
!     obtuse.ne.0 then the triangle is obtuse
!     value of 1,2,3 means that angle associated with p1,p2,p3 is > 90
!     obtuse = 0 then the triangle is not obtuse
!     
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      integer function obtuse(p1, p2, p3)

      use data_types

      implicit none

      type (geo_point), intent(in) :: p1, p2, p3

      real :: x1(3), x2(3), x3(3), dot, r(3), s(3), rmag, smag

      obtuse = 0

      call convert_lx(x1(1), x1(2), x1(3), 1.0, p1)
      call convert_lx(x2(1), x2(2), x2(3), 1.0, p2)
      call convert_lx(x3(1), x3(2), x3(3), 1.0, p3)

! test angle formed by x3,x1,x2
      r(:) = x3(:) - x1(:)
      s(:) = x2(:) - x1(:)
      rmag = sqrt(r(1)**2+r(2)**2+r(3)**2)
      smag = sqrt(s(1)**2+s(2)**2+s(3)**2)
      r(:) = r(:) / rmag
      s(:) = s(:) / smag
      dot = r(1)*s(1) + r(2)*s(2) + r(3)*s(3)
      if(dot.lt.0) obtuse = 1

! test angle formed by x1,x2,x3
      r(:) = x1(:) - x2(:)
      s(:) = x3(:) - x2(:)
      rmag = sqrt(r(1)**2+r(2)**2+r(3)**2)
      smag = sqrt(s(1)**2+s(2)**2+s(3)**2)
      r(:) = r(:) / rmag
      s(:) = s(:) / smag
      dot = r(1)*s(1) + r(2)*s(2) + r(3)*s(3)
      if(dot.lt.0) obtuse = 2

! test angle formed by x2,x3,x1
      r(:) = x2(:) - x3(:)
      s(:) = x1(:) - x3(:)
      rmag = sqrt(r(1)**2+r(2)**2+r(3)**2)
      smag = sqrt(s(1)**2+s(2)**2+s(3)**2)
      r(:) = r(:) / rmag
      s(:) = s(:) / smag
      dot = r(1)*s(1) + r(2)*s(2) + r(3)*s(3)
      if(dot.lt.0) obtuse = 3

      end function obtuse


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     FUNCTION SPHERE_DISTANCE
!     
!     Given two (latitude, longitude) coordinates on the surface of a sphere,
!     plus the radius of the sphere, compute the great circle distance between
!     the points.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      real function sphere_distance(p1, p2, radius)

      use data_types

      implicit none

      type (geo_point), intent(in) :: p1, p2
      real, intent(in) :: radius

      real :: arg1

      arg1 = sqrt( sin(0.5*(p2%lat-p1%lat))**2 +  &
      cos(p1%lat)*cos(p2%lat)*sin(0.5*(p2%lon-p1%lon))**2 )
      sphere_distance = 2.*radius*asin(arg1)

      end function sphere_distance


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     FUNCTION PLANE_DISTANCE
!     
!     Given two (latitude, longitude) coordinates on the surface of a sphere,
!     plus the radius of the sphere, compute the secant distance between
!     the points.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      real function plane_distance(p1, p2, radius)

      use data_types

      implicit none

      type (geo_point), intent(in) :: p1, p2   
      real, intent(in) :: radius

      real :: x1, x2, y1, y2, z1, z2

      z1 = sin(p1%lat)
      z2 = sin(p2%lat)
      x1 = cos(p1%lon)*cos(p1%lat)
      x2 = cos(p2%lon)*cos(p2%lat)
      y1 = sin(p1%lon)*cos(p1%lat)
      y2 = sin(p2%lon)*cos(p2%lat)

      plane_distance = radius*sqrt((z1-z2)**2+(x1-x2)**2+(y1-y2)**2)

      end function plane_distance


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     FUNCTION ARC_ANGLE
!     
!     Given two (latitude, longitude) coordinates on the surface of a sphere,
!     compute the angle between the points as measured from the origin of the
!     sphere.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      real function arc_angle(p1, p2)

      use data_types

      implicit none

      type (geo_point), intent(in) :: p1, p2

      real :: arg1

      arg1 = sqrt( sin(0.5*(p2%lat-p1%lat))**2 +  &
      cos(p1%lat)*cos(p2%lat)*sin(0.5*(p2%lon-p1%lon))**2 )
      arc_angle = 2.*asin(arg1)

      end function arc_angle


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     FUNCTION GREAT_CIRCLE_POINTS
!     
!     Return n points equally spaced along the great circle arc between (lat1,lon1) 
!     and (lat2,lon2).  These points include the end points of the arc.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine great_circle_points(p1, p2, pl, n)

      use data_types

      implicit none

      integer, intent(in) :: n
      type (geo_point), intent(in) :: p1, p2
      type (geo_point), dimension(n), intent(inout) :: pl

      real :: x1, x2, y1, y2, z1, z2
      real :: dx, dl
      real :: x, y, z
      integer :: i
      real :: dtheta, dinc, dt

      real :: pii, rtod

      pii = 2.*asin(1.0)
      rtod = 180./pii

!     write(6,*) ' in gcp ',rtod*lat1,rtod*lon1,rtod*lat2,rtod*lon2

      if (n < 2) then
         write(6,*) ' n less than 2 in great_circle_points '
         stop
      end if

      if (n == 2) then
         pl(1) = p1
         pl(2) = p2
      end if

      dtheta = arc_angle(p1, p2)
      dinc = dtheta/float(n-1)

      call convert_lx(x1,y1,z1,1.,p1)
      call convert_lx(x2,y2,z2,1.,p2)

!     set the end points

      pl(1) = p1
      pl(n) = p2

!     write(6,*) ' x1,y1,z1 ',x1,y1,z1 
!     write(6,*) ' x2,y2,z2 ',x2,y2,z2 

!     compute the interior points.  see notes for derivation

      do i=2,n-1
         dt = float(i-1)*dinc

         if (dt <= 0.5*dtheta) then
            dx = 1.-tan(0.5*dtheta-dt)/tan(0.5*dtheta)
!     write(6,*) ' case 1 ',dx
            x = x1+0.5*dx*(x2-x1)
            y = y1+0.5*dx*(y2-y1)
            z = z1+0.5*dx*(z2-z1)
         else
            dt = dtheta-dt
            dx = 1.-tan(0.5*dtheta-dt)/tan(0.5*dtheta)
!     write(6,*) ' case 2 ',dx
            x = x2+0.5*dx*(x1-x2)
            y = y2+0.5*dx*(y1-y2)
            z = z2+0.5*dx*(z1-z2)
         end if

!     write(6,*) ' x,y,z ',x,y,z

         call convert_xl(x,y,z,pl(i)) 
      enddo

      end subroutine great_circle_points


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     SUBROUTINE DIVIDE_TRIANGLE
!     
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     subroutine divide_triangle( p1, p2, p3, pnew)
!     
!     use data_types
!     
!     implicit none
!     
!     type (geo_point), intent(in) :: p1, p2, p3
!     type (geo_point), dimension(6), intent(inout) :: pnew
!     
!     real :: t_area, area_total, radius
!     type (geo_point), dimension(3) :: pts
!     type (geo_point) :: c
!     
!     radius = 1.
!     pnew(1) = p1
!     pnew(4) = p2
!     pnew(6) = p3
!     
!     call great_circle_points(p1,p2,pts,3)
!     pnew(2) = pts(2)
!     
!     call great_circle_points(p1,p3,pts,3)
!     pnew(3) = pts(2)
!     
!     call great_circle_points(p2,p3,pts,3)
!     pnew(5) = pts(2)
!     
!     
!     write(6,*) ' '
!     write(6,*) ' original triangle '
!     write(6,*) p1%lat, p1%lon
!     write(6,*) p2%lat, p2%lon
!     write(6,*) p3%lat, p3%lon
!     
!     t_area = triangle_area(p1,p2,p3,radius)
!     write(6,*) ' area ',t_area
!     call compute_voronoi_corner(p1,p2,p3,c)
!     write(6,*) ' voronoi corner ',c%lat,c%lon
!     
!     area_total = 0.
!     
!     write(6,*) ' '
!     write(6,*) ' new triangles '
!     
!     write(6,*) ' triangle 1 '
!     write(6,*) pnew(1)%lat,pnew(1)%lon
!     write(6,*) pnew(1)%lat,pnew(2)%lon
!     write(6,*) pnew(1)%lat,pnew(3)%lon
!     t_area = triangle_area( pnew(1),pnew(2),pnew(3),radius)
!     area_total = area_total + t_area
!     write(6,*) ' area ',t_area
!     
!     write(6,*) ' triangle 2 '
!     write(6,*) pnew(2)%lat,pnew(2)%lon
!     write(6,*) pnew(4)%lat,pnew(4)%lon
!     write(6,*) pnew(5)%lat,pnew(5)%lon
!     t_area = triangle_area( pnew(2),pnew(4),pnew(5),radius)
!     area_total = area_total + t_area
!     write(6,*) ' area ',t_area
!     
!     write(6,*) ' triangle 3 '
!     write(6,*) pnew(2)%lat,pnew(2)%lon
!     write(6,*) pnew(5)%lat,pnew(5)%lon
!     write(6,*) pnew(3)%lat,pnew(3)%lon
!     t_area = triangle_area( pnew(2),pnew(5),pnew(3),radius)
!     area_total = area_total + t_area
!     write(6,*) ' area ',t_area
!     
!     write(6,*) ' triangle 4 '
!     write(6,*) pnew(3)%lat,pnew(3)%lon
!     write(6,*) pnew(5)%lat,pnew(5)%lon
!     write(6,*) pnew(6)%lat,pnew(6)%lon
!     t_area = triangle_area( pnew(3),pnew(5),pnew(6),radius)
!     area_total = area_total + t_area
!     write(6,*) ' area ',t_area
!     write(6,*) ' total area is ',area_total
!     
!     end subroutine divide_triangle


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     SUBROUTINE COMPUTE_VORONOI_CORNER
!     
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     subroutine compute_voronoi_corner( p0, p1, p2, vc )
!     
!     use data_types
!     
!     implicit none
!     
!     type (geo_point), intent(in) :: p0, p1, p2
!     type (geo_point), intent(out) :: vc
!     
!     real :: x0, y0, z0, x1, y1, z1, x2, y2, z2, xc, yc, zc, cabs
!     real :: a1, a2, a3, b1, b2, b3
!     real :: dot0
!     
!     z0 = sin(p0%lat)
!     z1 = sin(p1%lat)
!     z2 = sin(p2%lat)
!     
!     x0 = cos(p0%lon)*cos(p0%lat)
!     x1 = cos(p1%lon)*cos(p1%lat)
!     x2 = cos(p2%lon)*cos(p2%lat)
!     
!     y0 = sin(p0%lon)*cos(p0%lat)
!     y1 = sin(p1%lon)*cos(p1%lat)
!     y2 = sin(p2%lon)*cos(p2%lat)
!     
!     a1 = x2-x0
!     a2 = y2-y0
!     a3 = z2-z0
!     
!     b1 = x1-x0
!     b2 = y1-y0
!     b3 = z1-z0
!     
!     
!     xc = a2*b3-a3*b2
!     yc = a3*b1-a1*b3
!     zc = a1*b2-a2*b1
!     cabs = sqrt(xc*xc+yc*yc+zc*zc)
!     
!     !   write(6,*) ' cabs = ',cabs
!     !   write(6,*) ' xc, yc, zc = ',xc,yc,zc
!     !   write(6,*) ' x0, y0, z0 = ',x0,y0,z0
!     !   write(6,*) ' x1, y1, z1 = ',x1,y1,z1
!     !   write(6,*) ' x2, y2, z2 = ',x2,y2,z2
!     dot0 = x0*xc+y0*yc+z0*zc
!     !   write(6,*) ' dot is ',dot0
!     
!     if( dot0 < 0.) then ! flip p1 with p2
!     
!     z2 = sin(p1%lat)
!     z1 = sin(p2%lat)
!     
!     x2 = cos(p1%lon)*cos(p1%lat)
!     x1 = cos(p2%lon)*cos(p2%lat)
!     
!     y2 = sin(p1%lon)*cos(p1%lat)
!     y1 = sin(p2%lon)*cos(p2%lat)
!     
!     a1 = x2-x0
!     a2 = y2-y0
!     a3 = z2-z0
!     
!     b1 = x1-x0
!     b2 = y1-y0
!     b3 = z1-z0
!     
!     
!     xc = a2*b3-a3*b2
!     yc = a3*b1-a1*b3
!     zc = a1*b2-a2*b1
!     cabs = sqrt(xc*xc+yc*yc+zc*zc)
!     
!     !   write(6,*) ' flipping '
!     !   write(6,*) ' cabs = ',cabs
!     !   write(6,*) ' xc, yc, zc = ',xc,yc,zc
!     dot0 = x0*xc+y0*yc+z0*zc
!     !   write(6,*) ' dot is ',dot0
!     
!     end if
!     
!     
!     xc = xc/cabs
!     yc = yc/cabs
!     zc = zc/cabs
!     
!     call convert_xl(xc,yc,zc,vc)
!     
!     end subroutine compute_voronoi_corner


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     SUBROUTINE CONVERT_LX
!     
!     Convert (lat,lon) to an (x, y, z) location on a sphere with specified radius.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine convert_lx(x, y, z, radius, latlon)

      use data_types

      implicit none

      real, intent(in) :: radius
      type (geo_point), intent(in) :: latlon
      real, intent(out) :: x, y, z

      z = radius * sin(latlon%lat)
      x = radius * cos(latlon%lon) * cos(latlon%lat)
      y = radius * sin(latlon%lon) * cos(latlon%lat)

      end subroutine convert_lx


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     SUBROUTINE CONVERT_XL
!     
!     Convert (x, y, z) to a (lat, lon) location on a sphere with 
!     radius sqrt(x^2 + y^2 + z^2).
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine convert_xl(x, y, z, latlon)

      use data_types

      implicit none

      real, intent(in) :: x, y, z
      type (geo_point), intent(out) :: latlon

      real :: dl, clat, pii, rtod
      real :: eps
      parameter (eps=1.e-10)

      pii = 2.*asin(1.0)
      rtod=180./pii
      dl = sqrt(x*x + y*y + z*z)

      latlon%lat = asin(z/dl)

!     check for being close to either pole

      if (abs(x) > eps) then

         if (abs(y) > eps) then

            latlon%lon = atan(abs(y/x))

            if ((x <= 0.) .and. (y >= 0.)) then
               latlon%lon = pii-latlon%lon
            else if ((x <= 0.) .and. (y < 0.)) then
               latlon%lon = latlon%lon+pii
            else if ((x >= 0.) .and. (y <= 0.)) then
               latlon%lon = 2*pii-latlon%lon
            end if

         else                   ! we're either on longitude 0 or 180

            if (x > 0) then
               latlon%lon = 0.
            else
               latlon%lon = pii
            end if

         end if

      else if (abs(y) > eps) then  

         if (y > 0) then 
            latlon%lon = pii/2.
         else
            latlon%lon = 3.*pii/2.
         end if

      else                      ! we are at a pole

         latlon%lon = 0.

      end if

      end subroutine convert_xl


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     SUBROUTINE GC_INTERSECT
!     
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine gc_intersect(p0, p1, p2, p3, pc)

      use data_types

      implicit none

      type (geo_point), intent(in) :: p0, p1, p2, p3
      type (geo_point), intent(out) :: pc

      real :: x0, y0, z0, x1, y1, z1, x2, y2, z2, x3, y3, z3
      real :: n1, n2, n3, m1, m2, m3
      real :: xc, yc, zc, dot
      real, parameter :: radius=1.0

      call convert_lx(x0,y0,z0,radius,p0)
      call convert_lx(x1,y1,z1,radius,p1)
      call convert_lx(x2,y2,z2,radius,p2)
      call convert_lx(x3,y3,z3,radius,p3)

      n1 =  (y0 * z1 - y1 * z0)
      n2 = -(x0 * z1 - x1 * z0)
      n3 =  (x0 * y1 - x1 * y0)

      m1 =  (y2 * z3 - y3 * z2)
      m2 = -(x2 * z3 - x3 * z2)
      m3 =  (x2 * y3 - x3 * y2)

      xc =  (n2 * m3 - n3 * m2)
      yc = -(n1 * m3 - n3 * m1)
      zc =  (n1 * m2 - n2 * m1)

      dot = x0*xc + y0*yc + z0*zc

      if (dot < 0.0) then
         xc = -xc
         yc = -yc
         zc = -zc
      end if

      call convert_xl(xc,yc,zc,pc)

      end subroutine gc_intersect


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     FUNCTION POS_ANG
!     
!     Normalize an angle, given in radians, to lie in the interval [0,2*PI].
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      real function pos_ang(angle)

      implicit none

      real, intent(in) :: angle

      real :: pii

      pii = 2.*asin(1.0)
      pos_ang = angle

      if(angle > 2.*pii) then
         pos_ang = angle - 2.*pii
      else if(angle < 0.) then
         pos_ang = angle + 2.*pii
      end if

      end function pos_ang


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     FUNCTION MERIDIAN_ANGLE
!     
!     Find the angle between the meridian that intersects point (lat1,lon1)
!     and the great circle passing through points (lat1,lon1) (lat2,lon2).
!     (lat1,lon1) is the vertex of the angle. 
!     
!     Convention: zero points north, 90 points west, -90 point east, 
!     points south 180, -180 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      real function meridian_angle(p1, p2)

      use data_types

      implicit none

      type (geo_point), intent(in) :: p1, p2

      type (geo_point) :: np

      real :: pii, da, db, dc
      type (geo_point) :: p3
      real :: cosa
      real :: eps
      parameter (eps = 1.e-04)
      real :: ax, ay, az
      real :: bx, by, bz
      real :: cx, cy, cz

      np = p1
      np%lat = np%lat + 0.05

      call convert_lx(ax, ay, az, 1.0, p1)
      call convert_lx(bx, by, bz, 1.0, np)
      call convert_lx(cx, cy, cz, 1.0, p2)

      meridian_angle = plane_angle(ax, ay, az, bx, by, bz, cx, cy, cz, ax, ay, az)
      return

      if (p1%lon == p2%lon) then

         meridian_angle = 0.0

      else

         pii = 2.*asin(1.0)
         dc = arc_angle(p1,p2)

         p3%lon = p1%lon
         if (p1%lat + dc <= pii/2.0) then
            p3%lat = p1%lat+dc
         else
            p3%lat = p1%lat-dc
         end if
         db = arc_angle(p1,p3)
         da = arc_angle(p2,p3)

!     see spherical trig section on online wolfram pages - eq(11) ->

         cosa = max(-1.,min(1.,(cos(da)-cos(db)*cos(dc))/(sin(db)*sin(dc))))
         meridian_angle = acos(cosa)


         if (((p2%lon > p1%lon) .and. (p2%lon - p1%lon <= pii)) .or. &
         ((p2%lon < p1%lon) .and. (p1%lon - p2%lon >= pii))) then
         meridian_angle = -abs(meridian_angle)
      else
         meridian_angle = abs(meridian_angle)
      end if
      
      end if

      end function meridian_angle


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     SUBROUTINE CENTER_OF_MASS
!     
!     Find centriod of the triangle whose corners are at (lat1,lon1), (lat2,lon2),
!     and (lat3,lon3).
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine center_of_mass(p1, p2, p3, pc)

      use data_types

      implicit none

      type (geo_point), intent(in) :: p1, p2, p3
      type (geo_point), intent(out) :: pc

      real :: x1, x2, x3, xc
      real :: y1, y2, y3, yc
      real :: z1, z2, z3, zc

      call convert_lx(x1,y1,z1,1.,p1)
      call convert_lx(x2,y2,z2,1.,p2)
      call convert_lx(x3,y3,z3,1.,p3)

      xc = (x1+x2+x3)/3.
      yc = (y1+y2+y3)/3.
      zc = (z1+z2+z3)/3.

      call convert_xl(xc,yc,zc,pc)

      end subroutine center_of_mass


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     SUBROUTINE DIVIDE_TRIANGLE
!     
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine divide_triangle(p1, p2, p3, n, p)

      use data_types

      implicit none

      type (geo_point), intent(in) :: p1, p2, p3
      integer, intent(in) :: n
      type (geo_point), dimension(3,n), intent(out) :: p

      integer :: i, j, k
      integer :: glevel         ! Level of decomposition
      type (geo_point), allocatable, dimension(:) :: p1p2, p1p3
      type (geo_point), allocatable, dimension(:,:) :: line

      glevel = nint(log(real(n)) / log(4.0)) ! Each subdivision gives four times the number of 
!   triangles, so log4(n) gives the level decomposition

      glevel = (2 ** glevel) + 1
      allocate(line(glevel, glevel))
      allocate(p1p2(glevel))
      allocate(p1p3(glevel))

      call great_circle_points(p1, p2, p1p2, glevel)
      call great_circle_points(p1, p3, p1p3, glevel)
      
      line(1,1) = p1
      line(1,2) = p1p2(2)
      line(2,2) = p1p3(2)

      do i = 3,glevel
         call great_circle_points(p1p2(i), p1p3(i), line(:,i), i)
!     do j=1,i
!     write(0,*) j,i,' P ',line(j,i)%lat*180./3.14159, line(j,i)%lon*180./3.14159
!     end do
      end do
      
      k = 1
      do i = 1,glevel-1
         do j = 1,i
            p(1,k) = line(j,i)
            p(2,k) = line(j,i+1)
            p(3,k) = line(j+1,i+1)
!     write(0,*) j,i, ' - ',p(1,k)%lat*180./3.14159,p(1,k)%lon*180./3.14159
!     write(0,*) j,i+1, ' - ',p(2,k)%lat*180./3.14159,p(2,k)%lon*180./3.14159
!     write(0,*) j+1,i+1, ' - ',p(3,k)%lat*180./3.14159,p(3,k)%lon*180./3.14159
            k = k + 1
         end do
      end do

!     write(0,*) '-----------'
      do i = glevel,3,-1
         do j = 2,i-1
            p(1,k) = line(j,i)
            p(2,k) = line(j,i-1)
            p(3,k) = line(j-1,i-1)
!     write(0,*) j,i, ' - ',p(1,k)%lat*180./3.14159,p(1,k)%lon*180./3.14159
!     write(0,*) j,i-1, ' - ',p(2,k)%lat*180./3.14159,p(2,k)%lon*180./3.14159
!     write(0,*) j-1,i-1, ' - ',p(3,k)%lat*180./3.14159,p(3,k)%lon*180./3.14159
            k = k + 1
         end do
      end do

      deallocate(line)
      deallocate(p1p2)
      deallocate(p1p3)

      end subroutine divide_triangle 


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     SUBROUTINE POINT_TO_PLANE
!     
!     Find projection (xp, yp, zp) of a point (Qx,Qy,Qz) onto the plane defined by
!     the equation ax+by+cz+d=0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine point_to_plane(a, b, c, d, Qx, Qy, Qz, xp, yp, zp)

      implicit none

      real, intent(in) :: a, b, c, d ! The coefficients in the equation of the plane
      real, intent(in) :: Qx, Qy, Qz ! The coordinates of the point Q to be projected to the plane
      real, intent(out) :: xp, yp, zp ! The coordinates of the point projected in the plane

      real :: Px, Py, Pz        ! A point P in the plane  ax + by + cz + d = 0
      real :: PQx, PQy, PQz     ! Components of the vector from P to Q
      real :: PQn               ! The dot product of PQ and the vector normal to the plane
      real :: m2                ! The magnitude and squared magnitude of the vector n normal to the plane

      m2 = (a**2.0 + b**2.0 + c**2.0)

      Px = -d*a/m2
      Py = -d*b/m2
      Pz = -d*c/m2

      PQx = Qx - Px
      PQy = Qy - Py
      PQz = Qz - Pz

      PQn = PQx * a + PQy * b + PQz * c

!                 . Q
!         n ^    /
!           |  /
!           |/
! ----------.-------------------
!           P

      xp = Qx - PQn * a / m2
      yp = Qy - PQn * b / m2
      zp = Qz - PQn * c / m2

      end subroutine point_to_plane


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     SUBROUTINE POINT_TO_SPHERE
!     
!     Find projection (xp, yp, zp) of a point (Qx,Qy,Qz) in the plane defined by
!     the equation ax+by+cz+d=0 onto the surface of the sphere with radius r 
!     centered at the origin.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine point_to_sphere(a, b, c, d, r, Qx, Qy, Qz, xp, yp, zp)

      implicit none

      real, intent(in) :: a, b, c, d ! The coefficients in the equation of the plane
      real, intent(in) :: r     ! The radius of the sphere
      real, intent(in) :: Qx, Qy, Qz ! The coordinates of the point Q to be projected to the sphere
      real, intent(out) :: xp, yp, zp ! The coordinates of the point projected to the sphere
      
      real :: aa, bb, cc        ! Coefficients of quadratic equation 
      real :: disc, t1, t2

! Solve for the interesection of the line (Qx - at, Qy - bt, Qz - ct) and the
!   sphere x^2 + y^2 + z^2 - r^2 = 0
      aa = a**2.0 + b**2.0 + c**2.0
      bb = -2.0*(Qx*a + Qy*b + Qz*c)
      cc = Qx**2.0 + Qy**2.0 + Qz**2.0 - r**2.0

      disc = bb**2.0 - 4.0*aa*cc

      if (disc < 0.0) then      ! Point has no projection on the surface of the sphere
         xp = 0.0
         yp = 0.0
         zp = 0.0
      else if (disc == 0.0) then ! Point has exactly one projection (line through point and
         t1 = -bb / (2.0*aa)
         xp = Qx - a*t1         !   and normal to plane is tangent to sphere
         yp = Qy - b*t1
         zp = Qz - c*t1
      else                      ! Point has two projections; choose the one that is closest
         t1 = (-bb + sqrt(disc)) / (2.0*aa)
         t2 = (-bb - sqrt(disc)) / (2.0*aa)
         if (abs(t1) <= abs(t2)) then
            xp = Qx - a*t1
            yp = Qy - b*t1
            zp = Qz - c*t1
         else
            xp = Qx - a*t2
            yp = Qy - b*t2
            zp = Qz - c*t2
         end if
      end if

      end subroutine point_to_sphere


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     SUBROUTINE ROTATE_ABOUT_VECTOR
!     
!     Rotates the point (x,y,z) through an angle theta about the vector 
!     originating at (a, b, c) and having direction (u, v, w).
!     
!     Reference: http://inside.mines.edu/~gmurray/ArbitraryAxisRotation/ArbitraryAxisRotation.html
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine rotate_about_vector(x, y, z, theta, a, b, c, u, v, w, xp, yp, zp)

      implicit none

      real, intent(in) :: x, y, z, theta, a, b, c, u, v, w
      real, intent(out) :: xp, yp, zp

      real :: vw2, uw2, uv2
      real :: m

      vw2 = v**2.0 + w**2.0
      uw2 = u**2.0 + w**2.0
      uv2 = u**2.0 + v**2.0
      m = sqrt(u**2.0 + v**2.0 + w**2.0)

      xp = (a*vw2 + u*(-b*v-c*w+u*x+v*y+w*z) + ((x-a)*vw2+u*(b*v+c*w-v*y-w*z))*cos(theta) + m*(-c*v+b*w-w*y+v*z)*sin(theta))/m**2.0
      yp = (b*uw2 + v*(-a*u-c*w+u*x+v*y+w*z) + ((y-b)*uw2+v*(a*u+c*w-u*x-w*z))*cos(theta) + m*( c*u-a*w+w*x-u*z)*sin(theta))/m**2.0
      zp = (c*uv2 + w*(-a*u-b*v+u*x+v*y+w*z) + ((z-c)*uv2+w*(a*u+b*v-u*x-v*y))*cos(theta) + m*(-b*u+a*v-v*x+u*y)*sin(theta))/m**2.0

      end subroutine rotate_about_vector


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     FUNCTION PLANE_ANGLE
!     
!     Computes the angle between vectors AB and AC, given points A, B, and C, and 
!     a vector (u,v,w) normal to the plane.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      real function plane_angle(ax, ay, az, bx, by, bz, cx, cy, cz, u, v, w)

      implicit none

      real, intent(in) :: ax, ay, az, bx, by, bz, cx, cy, cz, u, v, w

      real :: ABx, ABy, ABz     ! The components of the vector AB
      real :: mAB               ! The magnitude of AB
      real :: ACx, ACy, ACz     ! The components of the vector AC
      real :: mAC               ! The magnitude of AC

      real :: Dx                ! The i-components of the cross product AB x AC
      real :: Dy                ! The j-components of the cross product AB x AC
      real :: Dz                ! The k-components of the cross product AB x AC

      real :: cos_angle

      ABx = bx - ax
      ABy = by - ay
      ABz = bz - az
      mAB = sqrt(ABx**2.0 + ABy**2.0 + ABz**2.0)

      ACx = cx - ax
      ACy = cy - ay
      ACz = cz - az
      mAC = sqrt(ACx**2.0 + ACy**2.0 + ACz**2.0)


      Dx =   (ABy * ACz) - (ABz * ACy)
      Dy = -((ABx * ACz) - (ABz * ACx))
      Dz =   (ABx * ACy) - (ABy * ACx)
      
      cos_angle = (ABx*ACx + ABy*ACy + ABz*ACz) / (mAB * mAC)

      if (cos_angle < -1.0) then
         cos_angle = -1.0
      else if (cos_angle > 1.0) then
         cos_angle = 1.0
      end if

      if ((Dx*u + Dy*v + Dz*w) >= 0.0) then
         plane_angle = acos(cos_angle)
      else
         plane_angle = -acos(cos_angle)
      end if

      end function plane_angle

      end module sphere_utilities
