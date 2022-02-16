!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! PROGRAM CONVERT_GRID_TO_HCM
!
! Developer: Pedro S. Peixoto - Oct 2015
!
!
! Reads a .nc grid and overwrites the edge position to be relative to
! the midpoint of the Voronoi cell edges instead of the Triangle edges midpoints.
!
! Created based on grid_rotate.f90 from MPAS repository
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program convert_grid_to_hcm

  implicit none
  include 'netcdf.inc'
  
  integer, parameter :: RKIND = 8
  real(kind=RKIND) :: pii, omega
  character(100) :: filename
  character(100) :: newFilename


  ! filename and newFilename are provided as command-line argument arguments 
  call getarg(1,filename)
  call getarg(2,newFilename)

  pii = 2.*asin(1.0)
  omega = 2.0*pii / 86400.0

  call main()

contains


   subroutine main()

      implicit none

      integer :: ncid, dimid, varid, ierr

      integer :: nCells, nVertices, nEdges
      integer :: i

      real(kind=RKIND), dimension(:), allocatable :: xCell
      real(kind=RKIND), dimension(:), allocatable :: yCell
      real(kind=RKIND), dimension(:), allocatable :: zCell
      real(kind=RKIND), dimension(:), allocatable :: latCell
      real(kind=RKIND), dimension(:), allocatable :: lonCell

      real(kind=RKIND), dimension(:), allocatable :: xVertex
      real(kind=RKIND), dimension(:), allocatable :: yVertex
      real(kind=RKIND), dimension(:), allocatable :: zVertex
      real(kind=RKIND), dimension(:), allocatable :: latVertex
      real(kind=RKIND), dimension(:), allocatable :: lonVertex

      real(kind=RKIND), dimension(:), allocatable :: xEdge
      real(kind=RKIND), dimension(:), allocatable :: yEdge
      real(kind=RKIND), dimension(:), allocatable :: zEdge
      real(kind=RKIND), dimension(:), allocatable :: latEdge
      real(kind=RKIND), dimension(:), allocatable :: lonEdge
      real(kind=RKIND), dimension(:), allocatable :: angleEdge
      real(kind=RKIND), dimension(:), allocatable :: fEdge
      real(kind=RKIND), dimension(:), allocatable :: fVertex


      integer, dimension(:,:), allocatable :: verticesOnEdge
      integer, dimension(2) :: onEdgeStart                   
      integer, dimension(2) :: onEdgeCount

      real (kind=RKIND) :: original_latitude_radians, original_longitude_radians, new_latitude_radians, new_longitude_radians
      real (kind=RKIND) :: thetaLat, thetaLon, thetaBirdsEye
      real (kind=RKIND) :: x0LongitudeAtEquator, y0LongitudeAtEquator, z0LongitudeAtEquator
      real (kind=RKIND) :: uCrossProduct, vCrossProduct, wCrossProduct
      real (kind=RKIND) :: xNew, yNew, zNew

      real (kind=RKIND) :: v
      real (kind=RKIND) :: ax, ay, az
      real (kind=RKIND) :: bx, by, bz
      real (kind=RKIND) :: cx, cy, cz

	  character (len=3) :: HCm="HCm"
      character(220) :: copyCmd

      !call read_namelist(original_latitude_degrees, original_longitude_degrees, new_latitude_degrees, new_longitude_degrees, birdseye_rotation_counter_clockwise_degrees)

      if(trim(filename) == "") then
         write(0,*) "Error: no source file was specified"
         return
      end if

      if(trim(newFilename) == "") then
         write(0,*) "Error: no distination file was specified"
         return
      end if

      copyCmd = "cp " // trim(filename) // " " // trim(newFilename)

      call system(copyCmd)

      ierr = nf_open(newFilename, NF_WRITE, ncid)
      if(ierr /= 0) then
         write(0,*) "Error: could not find file: ", filename
         return
      end if

      ierr = nf_inq_dimid(ncid, "nCells", dimid)
      ierr = nf_inq_dimlen(ncid, dimid, nCells)

      ierr = nf_inq_dimid(ncid, "nVertices", dimid)
      ierr = nf_inq_dimlen(ncid, dimid, nVertices)

      ierr = nf_inq_dimid(ncid, "nEdges", dimid)
      ierr = nf_inq_dimlen(ncid, dimid, nEdges)

      allocate(xCell(1:nCells))
      allocate(yCell(1:nCells))
      allocate(zCell(1:nCells))
      allocate(latCell(1:nCells))
      allocate(lonCell(1:nCells))

      allocate(xVertex(1:nVertices))
      allocate(yVertex(1:nVertices))
      allocate(zVertex(1:nVertices))
      allocate(latVertex(1:nVertices))
      allocate(lonVertex(1:nVertices))

      allocate(xEdge(1:nEdges))
      allocate(yEdge(1:nEdges))
      allocate(zEdge(1:nEdges))
      allocate(latEdge(1:nEdges))
      allocate(lonEdge(1:nEdges))
      allocate(angleEdge(1:nEdges))
      allocate(fEdge(1:nEdges))
      allocate(fVertex(1:nVertices))
      allocate(verticesOnEdge(1:2, 1:nEdges))

      data onEdgeStart /1, 1/
      onEdgeCount(1) = 2
      onEdgeCount(2) = nEdges

      ierr = nf_inq_varid(ncid, "xCell", varid)
      ierr = nf_get_vara_double(ncid, varid, 1, nCells, xCell)

      ierr = nf_inq_varid(ncid, "yCell", varid)
      ierr = nf_get_vara_double(ncid, varid, 1, nCells, yCell)

      ierr = nf_inq_varid(ncid, "zCell", varid)
      ierr = nf_get_vara_double(ncid, varid, 1, nCells, zCell)

      ierr = nf_inq_varid(ncid, "latCell", varid)
      ierr = nf_get_vara_double(ncid, varid, 1, nCells, latCell)

      ierr = nf_inq_varid(ncid, "lonCell", varid)
      ierr = nf_get_vara_double(ncid, varid, 1, nCells, lonCell)
 

      ierr = nf_inq_varid(ncid, "xVertex", varid)
      ierr = nf_get_vara_double(ncid, varid, 1, nVertices, xVertex)

      ierr = nf_inq_varid(ncid, "yVertex", varid)
      ierr = nf_get_vara_double(ncid, varid, 1, nVertices, yVertex)

      ierr = nf_inq_varid(ncid, "zVertex", varid)
      ierr = nf_get_vara_double(ncid, varid, 1, nVertices, zVertex)

      ierr = nf_inq_varid(ncid, "latVertex", varid)
      ierr = nf_get_vara_double(ncid, varid, 1, nVertices, latVertex)

      ierr = nf_inq_varid(ncid, "lonVertex", varid)
      ierr = nf_get_vara_double(ncid, varid, 1, nVertices, lonVertex)


      ierr = nf_inq_varid(ncid, "xEdge", varid)
      ierr = nf_get_vara_double(ncid, varid, 1, nEdges, xEdge)

      ierr = nf_inq_varid(ncid, "yEdge", varid)
      ierr = nf_get_vara_double(ncid, varid, 1, nEdges, yEdge)

      ierr = nf_inq_varid(ncid, "zEdge", varid)
      ierr = nf_get_vara_double(ncid, varid, 1, nEdges, zEdge)

      ierr = nf_inq_varid(ncid, "latEdge", varid)
      ierr = nf_get_vara_double(ncid, varid, 1, nEdges, latEdge)

      ierr = nf_inq_varid(ncid, "lonEdge", varid)
      ierr = nf_get_vara_double(ncid, varid, 1, nEdges, lonEdge)

      ierr = nf_inq_varid(ncid, "angleEdge", varid)
      ierr = nf_get_vara_double(ncid, varid, 1, nEdges, angleEdge)

      ierr = nf_inq_varid(ncid, "fEdge", varid)
      if (ierr == NF_NOERR) then
         ierr = nf_get_vara_double(ncid, varid, 1, nEdges, fEdge)
      end if

      ierr = nf_inq_varid(ncid, "fVertex", varid)
      if (ierr == NF_NOERR) then
         ierr = nf_get_vara_double(ncid, varid, 1, nVertices, fVertex)
      end if

      ierr = nf_inq_varid(ncid, "verticesOnEdge", varid)
      ierr = nf_get_vara_int(ncid, varid, onEdgeStart, onEdgeCount, verticesOnEdge)


      do i=1,nEdges
         !call executeRotation(xEdge(i), yEdge(i), zEdge(i), thetaLat, thetaLon, thetaBirdsEye, uCrossProduct, vCrossProduct, wCrossProduct, xNew, yNew, zNew)

         !First vertex coordinates
		 ax = xVertex(verticesOnEdge(1,i))
         ay = yVertex(verticesOnEdge(1,i))
         az = zVertex(verticesOnEdge(1,i))

         !Second vertex coordinates
         cx = xVertex(verticesOnEdge(2,i))
         cy = yVertex(verticesOnEdge(2,i))
         cz = zVertex(verticesOnEdge(2,i))

         !Voronoi edge midpoint - cartesian line
         ! temporary use bx, by, bz
         bx = (ax+cx)/2.0
         by = (ay+cy)/2.0
         bz = (az+cz)/2.0

         ! Normalize to sphere
         v = sqrt(bx**2.0 + by**2.0 + bz**2.0)
         xEdge(i) = bx / v
         yEdge(i) = by / v
         zEdge(i) = bz / v

         !Convert to lat lon
         call convert_xl(xEdge(i), yEdge(i), zEdge(i), latEdge(i), lonEdge(i))

         !compute new angle edge
         bx = -cos(lonEdge(i))*sin(latEdge(i)) + ax
         by = -sin(lonEdge(i))*sin(latEdge(i)) + ay
         bz =  cos(latEdge(i))                 + az
         
         v = sqrt(bx**2.0 + by**2.0 + bz**2.0)
         bx = bx / v
         by = by / v
         bz = bz / v

		 !Compute angle between AB and AC lines
         angleEdge(i) = sphere_angle(ax, ay, az, bx, by, bz, cx, cy, cz)

      end do

      fEdge=2*omega*sin(latEdge)

      ierr = nf_inq_varid(ncid, "xEdge", varid)
      ierr = nf_put_vara_double(ncid, varid, 1, nEdges, xEdge)

      ierr = nf_inq_varid(ncid, "yEdge", varid)
      ierr = nf_put_vara_double(ncid, varid, 1, nEdges, yEdge)

      ierr = nf_inq_varid(ncid, "zEdge", varid)
      ierr = nf_put_vara_double(ncid, varid, 1, nEdges, zEdge)

      ierr = nf_inq_varid(ncid, "latEdge", varid)
      ierr = nf_put_vara_double(ncid, varid, 1, nEdges, latEdge)

      ierr = nf_inq_varid(ncid, "lonEdge", varid)
      ierr = nf_put_vara_double(ncid, varid, 1, nEdges, lonEdge)

      ierr = nf_inq_varid(ncid, "angleEdge", varid)
      ierr = nf_put_vara_double(ncid, varid, 1, nEdges, angleEdge)

      ierr = nf_inq_varid(ncid, "fEdge", varid)
      if (ierr == NF_NOERR) then
         ierr = nf_put_vara_double(ncid, varid, 1, nEdges, fEdge)
      end if

      ierr=nf_redef(ncid)

      ierr = nf_put_att_text(ncid, NF_GLOBAL, 'Staggering', 3, HCm)
      !ierr = nf_put_att_int(ncid, NF_GLOBAL, 'HCmEdges', NF_INT, 1, 1)

      ierr = nf_close(ncid)

   end subroutine main



   real function degreesToRadians(degAngle)

      implicit none

      real(kind=RKIND) :: degAngle
      degreesToRadians = degAngle * 2 * pii / 360 
   end function degreesToRadians


   subroutine read_namelist(config_original_latitude_degrees, config_original_longitude_degrees, config_new_latitude_degrees, config_new_longitude_degrees, config_birdseye_rotation_counter_clockwise_degrees)

      implicit none

      integer :: funit

      real(kind=RKIND), intent(out) :: config_original_latitude_degrees
      real(kind=RKIND), intent(out) :: config_original_longitude_degrees
      real(kind=RKIND), intent(out) :: config_new_latitude_degrees
      real(kind=RKIND), intent(out) :: config_new_longitude_degrees
      real(kind=RKIND), intent(out) :: config_birdseye_rotation_counter_clockwise_degrees

      namelist /input/ config_original_latitude_degrees, &
                       config_original_longitude_degrees, &
                       config_new_latitude_degrees, &
                       config_new_longitude_degrees, &
                       config_birdseye_rotation_counter_clockwise_degrees

      config_original_latitude_degrees = 0
      config_original_longitude_degrees = 0
      config_new_latitude_degrees = 0
      config_new_longitude_degrees = 0
      config_birdseye_rotation_counter_clockwise_degrees = 0

      funit = 21

      open(funit,file='namelist.input',status='old',form='formatted')
      read(funit,input)
      close(funit)

   end subroutine read_namelist







   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! SUBROUTINE CONVERT_XL
   !
   ! Convert (x, y, z) to a (lat, lon) location on a sphere with
   !    radius sqrt(x^2 + y^2 + z^2).
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine convert_xl(x, y, z, lat, lon)
   
      implicit none
   
      real (kind=RKIND), intent(in) :: x, y, z
      real (kind=RKIND), intent(out) :: lat, lon
   
      real (kind=RKIND) :: dl, clat
      real (kind=RKIND) :: eps
      parameter (eps=1.e-10)
   
      dl = sqrt(x*x + y*y + z*z)
      lat = asin(z/dl)
   
   !  check for being close to either pole
   
      if (abs(x) > eps) then
   
         if (abs(y) > eps) then
   
            lon = atan(abs(y/x))
   
            if ((x <= 0.) .and. (y >= 0.)) then
               lon = pii-lon
            else if ((x <= 0.) .and. (y < 0.)) then
               lon = lon+pii
            else if ((x >= 0.) .and. (y <= 0.)) then
               lon = 2*pii-lon
            end if
   
         else ! we're either on longitude 0 or 180
   
            if (x > 0) then
               lon = 0.
            else
               lon = pii
            end if
   
         end if
   
      else if (abs(y) > eps) then
   
         if (y > 0) then
            lon = pii/2.
         else
            lon = 3.*pii/2.
         end if
   
      else  ! we are at a pole
   
         lon = 0.
   
      end if

   end subroutine convert_xl


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! SUBROUTINE CONVERT_LX
   !
   ! Convert (lat,lon) to an (x, y, z) location on a sphere with specified radius.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine convert_lx(x, y, z, radius, lat, lon)

      implicit none

      real (kind=RKIND), intent(in) :: radius, lat, lon
      real (kind=RKIND), intent(out) :: x, y, z

      z = radius * sin(lat)
      x = radius * cos(lon) * cos(lat)
      y = radius * sin(lon) * cos(lat)

   end subroutine convert_lx


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! FUNCTION SPHERE_ANGLE
   !
   ! Computes the angle between arcs AB and AC, given points A, B, and C
   ! Equation numbers w.r.t. http://mathworld.wolfram.com/SphericalTrigonometry.html
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   real function sphere_angle(ax, ay, az, bx, by, bz, cx, cy, cz)

      implicit none

      real (kind=RKIND), intent(in) :: ax, ay, az, bx, by, bz, cx, cy, cz

      real (kind=RKIND) :: a, b, c          ! Side lengths of spherical triangle ABC

      real (kind=RKIND) :: ABx, ABy, ABz    ! The components of the vector AB
      real (kind=RKIND) :: mAB              ! The magnitude of AB
      real (kind=RKIND) :: ACx, ACy, ACz    ! The components of the vector AC
      real (kind=RKIND) :: mAC              ! The magnitude of AC

      real (kind=RKIND) :: Dx               ! The i-components of the cross product AB x AC
      real (kind=RKIND) :: Dy               ! The j-components of the cross product AB x AC
      real (kind=RKIND) :: Dz               ! The k-components of the cross product AB x AC

      real (kind=RKIND) :: s                ! Semiperimeter of the triangle
      real (kind=RKIND) :: sin_angle

      a = acos(max(min(bx*cx + by*cy + bz*cz,1.0_RKIND),-1.0_RKIND))      ! Eqn. (3)
      b = acos(max(min(ax*cx + ay*cy + az*cz,1.0_RKIND),-1.0_RKIND))      ! Eqn. (2)
      c = acos(max(min(ax*bx + ay*by + az*bz,1.0_RKIND),-1.0_RKIND))      ! Eqn. (1)

      ABx = bx - ax
      ABy = by - ay
      ABz = bz - az

      ACx = cx - ax
      ACy = cy - ay
      ACz = cz - az

      Dx =   (ABy * ACz) - (ABz * ACy)
      Dy = -((ABx * ACz) - (ABz * ACx))
      Dz =   (ABx * ACy) - (ABy * ACx)

      s = 0.5*(a + b + c)
!      sin_angle = sqrt((sin(s-b)*sin(s-c))/(sin(b)*sin(c)))   ! Eqn. (28)
      sin_angle = sqrt(min(1.0_RKIND,max(0.0_RKIND,(sin(s-b)*sin(s-c))/(sin(b)*sin(c)))))   ! Eqn. (28)

      if ((Dx*ax + Dy*ay + Dz*az) >= 0.0) then
         sphere_angle =  2.0 * asin(max(min(sin_angle,1.0_RKIND),-1.0_RKIND))
      else
         sphere_angle = -2.0 * asin(max(min(sin_angle,1.0_RKIND),-1.0_RKIND))
      end if

   end function sphere_angle
   



   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! SUBROUTINE CROSS_PRODUCT
   !
   ! Computes C = A x B
   !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine cross_product(Ax, Ay, Az, &
                            Bx, By, Bz, &
                            Cx, Cy, Cz)

      real (kind=RKIND), intent(in)  :: Ax, Ay, Az
      real (kind=RKIND), intent(in)  :: Bx, By, Bz
      real (kind=RKIND), intent(out) :: Cx, Cy, Cz

      Cx = (Ay * Bz) - (Az * By)
      Cy = (Az * Bx) - (Ax * Bz)
      Cz = (Ax * By) - (Ay * Bx)

   end subroutine cross_product                                 

end program convert_grid_to_hcm
