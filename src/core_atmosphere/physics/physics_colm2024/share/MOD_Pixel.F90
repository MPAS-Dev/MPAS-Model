#include <define.h>

MODULE MOD_Pixel

!------------------------------------------------------------------------------------
! !DESCRIPTION:
!
!    Pixels are rasterized points defined by fine-resolution data.
!
!    CoLM USE multiple grids to construct pixels. Grids are assimilated into pixel
!    coordinate one by one. One grid is assimilated by adding grid lines not present
!    in pixel coordinate. In other words, pixel coordinate is the union of all grids.
!
!    Pixels are used to carry out land surface tessellation. The grids used to
!    construct pixels are associated with surface data such as land cover types, soil
!    parameters, plant function types, leaf area index and forest height.
!    By using pixels, these variables are downscaled to fine resolution.
!
!    In pixel data type, region boundaries and each pixel boundaries are defined.
!    Subroutines to assimilate grid and map pixel to grid are defined as methods.
!
!  Created by Shupeng Zhang, May 2023
!------------------------------------------------------------------------------------

   USE MOD_Precision
   IMPLICIT NONE

   ! ---- data types ----
   type :: pixel_type

      real(r8) :: edges  ! southern edge (degrees)
      real(r8) :: edgen  ! northern edge (degrees)
      real(r8) :: edgew  ! western  edge (degrees)
      real(r8) :: edgee  ! eastern  edge (degrees)

      integer :: nlon, nlat
      real(r8), allocatable :: lat_s (:)
      real(r8), allocatable :: lat_n (:)
      real(r8), allocatable :: lon_w (:)
      real(r8), allocatable :: lon_e (:)

   CONTAINS
      procedure, PUBLIC :: set_edges   => pixel_set_edges

      procedure, PRIVATE :: assimilate_latlon => pixel_assimilate_latlon
      procedure, PUBLIC  :: assimilate_gblock => pixel_assimilate_gblock
      procedure, PUBLIC  :: assimilate_grid   => pixel_assimilate_grid

      procedure, PUBLIC :: map_to_grid => pixel_map_to_grid

      procedure, PUBLIC :: load_from_file => pixel_load_from_file

      final :: pixel_free_mem

   END type pixel_type

   ! ---- Instance ----
   type(pixel_type) :: pixel

CONTAINS

   ! --------------------------------
   SUBROUTINE pixel_set_edges (this, &
         edges_in, edgen_in, edgew_in, edgee_in)

   USE MOD_Precision
   USE MOD_MPAS_MPI
   USE MOD_Utils
   IMPLICIT NONE

   class(pixel_type) :: this

   real(r8), intent(in) :: edges_in, edgen_in
   real(r8), intent(in) :: edgew_in, edgee_in

      this%nlon = 1
      this%nlat = 1

      this%edges = edges_in
      this%edgen = edgen_in
      this%edgew = edgew_in
      this%edgee = edgee_in

      CALL normalize_longitude (this%edgew)
      CALL normalize_longitude (this%edgee)

      allocate (this%lat_s (1))
      allocate (this%lat_n (1))
      allocate (this%lon_w (1))
      allocate (this%lon_e (1))

      this%lat_s(1) = this%edges
      this%lat_n(1) = this%edgen
      this%lon_w(1) = this%edgew
      this%lon_e(1) = this%edgee

      IF (mpas_is_root) THEN
         write(*,'(A)') '----- Region information -----'
         write(*,'(A,F10.4,A,F10.4,A,F10.4,A,F10.4,A)') ' (south,north,west,east) = (', &
            this%edges, ',', this%edgen, ',', this%edgew, ',', this%edgee, ')'
      ENDIF

   END SUBROUTINE pixel_set_edges

   ! --------------------------------
   SUBROUTINE pixel_assimilate_latlon (this, &
         nlat, lat_s, lat_n, nlon, lon_w, lon_e)

   USE MOD_Precision
   USE MOD_Utils
   IMPLICIT NONE
   class(pixel_type) :: this

   integer,  intent(in) :: nlat
   real(r8), intent(in) :: lat_s(nlat), lat_n(nlat)
   integer,  intent(in) :: nlon
   real(r8), intent(in) :: lon_w(nlon), lon_e(nlon)

   ! Local variables
   real(r8) :: south, north, west, east

   integer :: ny, yinc
   integer :: iy1, iy2, ys2, yn2
   real(r8), allocatable :: ytmp(:)

   integer :: nx, nlonc
   integer :: ix1, ix2, xw2
   real(r8), allocatable :: xtmp(:), loncirc(:)

      IF (lat_s(1) <= lat_s(nlat)) THEN
         yinc = 1
         south = lat_s(1)
         north = lat_n(nlat)
      ELSE
         yinc = -1
         south = lat_s(nlat)
         north = lat_n(1)
      ENDIF

      allocate (ytmp (this%nlat+nlat+2))

      ny = 0
      DO iy1 = 1, this%nlat

         ny = ny + 1
         ytmp(ny) = this%lat_s(iy1)

         IF ((this%lat_s(iy1) < north) .and. (this%lat_n(iy1) > south)) THEN
            ys2 = find_nearest_south (this%lat_s(iy1), nlat, lat_s)
            yn2 = find_nearest_north (this%lat_n(iy1), nlat, lat_n)
            DO iy2 = ys2, yn2, yinc
               IF (lat_s(iy2) > this%lat_s(iy1)) THEN
                  ny = ny + 1
                  ytmp(ny) = lat_s(iy2)
               ENDIF
            ENDDO
            IF (lat_n(yn2) < this%lat_n(iy1)) THEN
               ny = ny + 1
               ytmp(ny) = lat_n(yn2)
            ENDIF
         ENDIF
      ENDDO

      ny = ny + 1
      ytmp(ny) = this%lat_n(this%nlat)

      deallocate (this%lat_s)
      deallocate (this%lat_n)

      this%nlat = ny - 1
      allocate (this%lat_s (this%nlat))
      allocate (this%lat_n (this%nlat))

      this%lat_s = ytmp(1:ny-1)
      this%lat_n = ytmp(2:ny)

      deallocate (ytmp)

      west = lon_w(1)
      east = lon_e(nlon)

      IF (west == east) THEN
         nlonc = nlon
         allocate (loncirc (nlonc))
         loncirc = lon_w
      ELSE
         nlonc = nlon + 1
         allocate (loncirc (nlonc))
         loncirc(1:nlon) = lon_w
         loncirc(nlon+1) = east
      ENDIF

      allocate (xtmp (this%nlon+nlon+2))
      nx = 0
      DO ix1 = 1, this%nlon

         nx = nx + 1
         xtmp(nx) = this%lon_w(ix1)

         xw2 = find_nearest_west (this%lon_w(ix1), nlonc, loncirc)
         ix2 = mod(xw2,nlonc) + 1
         DO WHILE (.true.)
            IF (lon_between_floor(loncirc(ix2), this%lon_w(ix1), this%lon_e(ix1))) THEN
               IF (loncirc(ix2) /= this%lon_w(ix1)) THEN
                  nx = nx + 1
                  xtmp(nx) = loncirc(ix2)
               ENDIF

               IF (ix2 /= xw2) THEN
                  ix2 = mod(ix2,nlonc) + 1
               ELSE
                  EXIT
               ENDIF
            ELSE
               EXIT
            ENDIF
         ENDDO

      ENDDO

      nx = nx + 1
      xtmp(nx) = this%lon_e(this%nlon)

      deallocate (this%lon_w)
      deallocate (this%lon_e)

      this%nlon = nx - 1
      allocate (this%lon_w (this%nlon))
      allocate (this%lon_e (this%nlon))

      this%lon_w = xtmp(1:nx-1)
      this%lon_e = xtmp(2:nx)

      deallocate (xtmp)

   END SUBROUTINE pixel_assimilate_latlon

   ! --------------------------------
   SUBROUTINE pixel_assimilate_gblock (this)

   USE MOD_Block, only: gblock
   IMPLICIT NONE
   class(pixel_type) :: this

      CALL this%assimilate_latlon ( &
         gblock%nyblk, gblock%lat_s, gblock%lat_n, &
         gblock%nxblk, gblock%lon_w, gblock%lon_e)

   END SUBROUTINE pixel_assimilate_gblock

   ! --------------------------------
   SUBROUTINE pixel_assimilate_grid (this, grid)

   USE MOD_Grid
   IMPLICIT NONE
   class(pixel_type) :: this

   type(grid_type), intent(in) :: grid

      CALL this%assimilate_latlon ( &
         grid%nlat, grid%lat_s, grid%lat_n, &
         grid%nlon, grid%lon_w, grid%lon_e)

   END SUBROUTINE pixel_assimilate_grid

   ! --------------------------------
   SUBROUTINE pixel_map_to_grid (this, grd)

   USE MOD_Grid
   USE MOD_Utils
   IMPLICIT NONE
   class(pixel_type) :: this

   type(grid_type), intent(inout) :: grd

   ! Local variables
   integer :: iy1, iy2, ix1, ix2
   real(r8) :: south, north, west, east

      IF (allocated (grd%xgrd))  deallocate (grd%xgrd)
      IF (allocated (grd%ygrd))  deallocate (grd%ygrd)

      allocate (grd%ygrd (this%nlat))

      IF (grd%yinc == 1) THEN
         south = grd%lat_s(1)
         north = grd%lat_n(grd%nlat)
      ELSE
         south = grd%lat_s(grd%nlat)
         north = grd%lat_n(1)
      ENDIF

      iy1 = 1
      DO WHILE (.true.)
         IF ((this%lat_s(iy1) < north) .and. (this%lat_n(iy1) > south)) THEN
            iy2 = find_nearest_south (this%lat_s(iy1), grd%nlat, grd%lat_s)
            DO WHILE (this%lat_n(iy1) <= grd%lat_n(iy2))
               grd%ygrd(iy1) = iy2
               iy1 = iy1 + 1
               IF (iy1 > this%nlat) EXIT
            ENDDO
         ELSE
            write(*,*) 'Warning: grid in latitude does not cover simulation region completely.', &
               south, north, this%lat_s(iy1), this%lat_n(iy1)
            grd%ygrd(iy1) = -1
            iy1 = iy1 + 1
         ENDIF
         IF (iy1 > this%nlat) EXIT
      ENDDO

      allocate (grd%xgrd (this%nlon))

      west = grd%lon_w(1)
      east = grd%lon_e(grd%nlon)

      ix1 = 1
      DO WHILE (.true.)
         IF (    lon_between_floor(this%lon_w(ix1), west, east) &
            .or. lon_between_ceil (this%lon_e(ix1), west, east) ) THEN

            ix2 = find_nearest_west (this%lon_w(ix1), grd%nlon, grd%lon_w)
            DO WHILE (lon_between_ceil(this%lon_e(ix1), grd%lon_w(ix2), grd%lon_e(ix2)))
               grd%xgrd(ix1) = ix2
               ix1 = ix1 + 1
               IF (ix1 > this%nlon) EXIT
            ENDDO

         ELSE
            write(*,*) 'Warning: grid in longitude does not cover simulation region completely.', &
               west, east, this%lon_w(ix1), this%lon_e(ix1)
            grd%xgrd(ix1) = -1
            ix1 = ix1 + 1
         ENDIF
         IF (ix1 > this%nlon) EXIT
      ENDDO

   END SUBROUTINE pixel_map_to_grid

   ! --------------------------------
   SUBROUTINE pixel_load_from_file (this, dir_landdata)

   USE MOD_NetCDFSerial
   USE MOD_MPAS_MPI, only: CoLM_stop
   USE, INTRINSIC :: ieee_arithmetic, only: ieee_is_finite
   IMPLICIT NONE

   class(pixel_type) :: this

   character(len=*), intent(in) :: dir_landdata
   ! Local variables
   character(len=256) :: filename

      filename = trim(dir_landdata) // '/pixel.nc'

      CALL ncio_read_bcast_serial (filename, 'edges', this%edges)
      CALL ncio_read_bcast_serial (filename, 'edgen', this%edgen)
      CALL ncio_read_bcast_serial (filename, 'edgew', this%edgew)
      CALL ncio_read_bcast_serial (filename, 'edgee', this%edgee)

      CALL ncio_read_bcast_serial (filename, 'lat_s', this%lat_s)
      CALL ncio_read_bcast_serial (filename, 'lat_n', this%lat_n)
      CALL ncio_read_bcast_serial (filename, 'lon_w', this%lon_w)
      CALL ncio_read_bcast_serial (filename, 'lon_e', this%lon_e)

      this%nlon = size(this%lon_w)
      this%nlat = size(this%lat_s)

      IF (this%nlon < 1 .or. this%nlat < 1 .or. size(this%lon_e) /= this%nlon .or. &
          size(this%lat_n) /= this%nlat) THEN
         CALL CoLM_stop('CoLM pixel.nc contains inconsistent pixel-boundary dimensions.')
      ENDIF
      IF (.not. all(ieee_is_finite((/this%edges, this%edgen, this%edgew, this%edgee/))) .or. &
          .not. all(ieee_is_finite(this%lat_s)) .or. .not. all(ieee_is_finite(this%lat_n)) .or. &
          .not. all(ieee_is_finite(this%lon_w)) .or. .not. all(ieee_is_finite(this%lon_e))) THEN
         CALL CoLM_stop('CoLM pixel.nc contains non-finite pixel boundaries.')
      ENDIF
      IF (any(this%lat_s < -90._r8) .or. any(this%lat_n > 90._r8) .or. &
          any(this%lat_n <= this%lat_s)) THEN
         CALL CoLM_stop('CoLM pixel.nc contains invalid latitude boundaries.')
      ENDIF
      IF (this%nlat > 1) THEN
         IF (any(this%lat_s(2:) <= this%lat_s(:this%nlat-1))) &
            CALL CoLM_stop('CoLM pixel.nc latitude pixels are not ordered south to north.')
      ENDIF

   END SUBROUTINE pixel_load_from_file

   ! --------------------------------
   SUBROUTINE pixel_free_mem (this)

   IMPLICIT NONE
   type (pixel_type) :: this

      IF (allocated(this%lat_s))  deallocate(this%lat_s)
      IF (allocated(this%lat_n))  deallocate(this%lat_n)
      IF (allocated(this%lon_w))  deallocate(this%lon_w)
      IF (allocated(this%lon_e))  deallocate(this%lon_e)

   END SUBROUTINE pixel_free_mem

END MODULE MOD_Pixel
