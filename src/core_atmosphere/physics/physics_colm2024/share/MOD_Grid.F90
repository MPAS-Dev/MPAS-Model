#include <define.h>

MODULE MOD_Grid

!-------------------------------------------------------------------------------
! !DESCRIPTION:
!
!    Definition of latitude-longitude grids and data types related to grids.
!
!    Latitude-longitude grid can be defined by
!    1. "name"   : frequently used grids is predefined in this MODULE;
!    2. "ndims"  : how many longitude and latitude grids are used globally;
!    3. "res"    : longitude and latitude resolutions in radian
!    4. "center" : longitude and latitude grid centers, and the border lines
!                  are defined by center lines of grid centers; the region
!                  boundaries is optional.
!    5. "file"   : read grid informations from a file, the variables are
!                  'lat_s', 'lat_n', 'lon_w', 'lon_e'
!    6. "copy"   : copy grid informations from an existing grid
!
!    Grid centers in radian can be calculated by using "set_rlon" and "set_rlat"
!
!    Two additional data types are defined:
!    1. "grid_list_type"   : list of grid boxes;
!    2. "grid_concat_type" : used to concatenate grids distributed in blocks.
!
!  Created by Shupeng Zhang, May 2023
!-------------------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_UserDefFun
   IMPLICIT NONE

   ! ---- data types ----
   type :: grid_type

      integer :: nlat
      integer :: nlon

      ! Latitude direction. (yinc = 1) means south to north.
      integer :: yinc

      ! Coordinates.
      real(r8), allocatable :: lat_s (:)
      real(r8), allocatable :: lat_n (:)
      real(r8), allocatable :: lon_w (:)
      real(r8), allocatable :: lon_e (:)

      ! Blocks.
      integer, allocatable :: xdsp(:), ydsp(:)
      integer, allocatable :: xcnt(:), ycnt(:)
      integer, allocatable :: xblk(:), yblk(:)
      integer, allocatable :: xloc(:), yloc(:)

      ! Mapping to pixels.
      integer, allocatable :: xgrd(:), ygrd(:)

      ! Grid info.
      real(r8), allocatable :: rlon(:)
      real(r8), allocatable :: rlat(:)

   CONTAINS
      procedure, PUBLIC :: define_by_name   => grid_define_by_name
      procedure, PUBLIC :: define_by_ndims  => grid_define_by_ndims
      procedure, PUBLIC :: define_by_res    => grid_define_by_res
      procedure, PUBLIC :: define_by_center => grid_define_by_center
      procedure, PUBLIC :: define_from_file => grid_define_from_file
      procedure, PUBLIC :: define_by_copy   => grid_define_by_copy

      procedure, PUBLIC :: set_rlon => grid_set_rlon
      procedure, PUBLIC :: set_rlat => grid_set_rlat

      procedure, PRIVATE :: init => grid_init
      procedure, PRIVATE :: normalize  => grid_normalize
      procedure, PRIVATE :: set_blocks => grid_set_blocks

      final :: grid_free_mem

   END type grid_type

   ! ---- data types ----
   type :: grid_list_type
      integer :: ng
      integer, allocatable :: ilat(:)
      integer, allocatable :: ilon(:)
   END type grid_list_type

   type :: segment_type
      integer :: blk
      integer :: cnt
      integer :: bdsp
      integer :: gdsp
   END type segment_type

   type :: grid_info_type
      integer :: nlat, nlon
      real(r8), allocatable :: lat_s(:)
      real(r8), allocatable :: lat_n(:)
      real(r8), allocatable :: lon_w(:)
      real(r8), allocatable :: lon_e(:)
      real(r8), allocatable :: lon_c(:) !grid center
      real(r8), allocatable :: lat_c(:) !grid center
   END type grid_info_type

   type :: grid_concat_type
      integer :: ndatablk
      integer :: nxseg, nyseg
      type(segment_type), allocatable :: xsegs(:), ysegs(:)
      type(grid_info_type) :: ginfo
   CONTAINS
      procedure, PUBLIC :: set => set_grid_concat
      final :: grid_concat_free_mem
   END type grid_concat_type

CONTAINS

   ! --------------------------------
   SUBROUTINE grid_init (this, nlon, nlat)

   IMPLICIT NONE
   class (grid_type) :: this

   integer, intent(in) :: nlon
   integer, intent(in) :: nlat

      this%nlat = nlat
      this%nlon = nlon

      IF (allocated(this%lat_s)) deallocate(this%lat_s)
      IF (allocated(this%lat_n)) deallocate(this%lat_n)
      IF (allocated(this%lon_w)) deallocate(this%lon_w)
      IF (allocated(this%lon_e)) deallocate(this%lon_e)
      allocate (this%lat_s (nlat))
      allocate (this%lat_n (nlat))
      allocate (this%lon_w (nlon))
      allocate (this%lon_e (nlon))

   END SUBROUTINE grid_init

   ! --------------------------------
   SUBROUTINE grid_define_by_name (this, gridname)

   IMPLICIT NONE
   class (grid_type) :: this

   character(len=*), intent(in) :: gridname

   ! Local variables
   integer  :: nlat, nlon, ilat, ilon
   real(r8) :: del_lat, del_lon

      IF (trim(gridname) == 'merit_90m') THEN

         nlat = 180*60*20
         nlon = 360*60*20

         this%nlat = nlat
         this%nlon = nlon

         CALL this%init (this%nlon, this%nlat)

         del_lat = 180.0 / nlat
         DO ilat = 1, this%nlat
            this%lat_s(ilat) = 90.0 - del_lat * ilat - del_lat/2.0
            this%lat_n(ilat) = 90.0 - del_lat * (ilat-1) - del_lat/2.0
         ENDDO

         del_lon = 360.0 / nlon
         DO ilon = 1, this%nlon
            this%lon_w(ilon) = -180.0 + del_lon * (ilon-1) - del_lon/2.0
            this%lon_e(ilon) = -180.0 + del_lon * ilon - del_lon/2.0
         ENDDO

         CALL this%normalize  ()
         CALL this%set_blocks ()

      ENDIF

      IF (trim(gridname) == 'colm_5km') THEN

         CALL this%define_by_ndims (8640,4320)

      ENDIF

      IF (trim(gridname) == 'colm_1km') THEN

         CALL this%define_by_ndims (43200,21600)

      ENDIF


      IF (trim(gridname) == 'colm_500m') THEN

         CALL this%define_by_ndims (86400,43200)

      ENDIF

      IF (trim(gridname) == 'colm_100m') THEN

         CALL this%define_by_ndims (432000,216000)

      ENDIF

      IF (trim(gridname) == 'nitrif_2deg') THEN

         CALL this%define_by_ndims (144,96)

      ENDIF

   END SUBROUTINE grid_define_by_name

   !-----------------------------------------------------
   SUBROUTINE grid_define_by_ndims (this, lon_points, lat_points)

   IMPLICIT NONE
   class (grid_type) :: this

   integer, intent(in) :: lon_points
   integer, intent(in) :: lat_points

   ! Local variables
   integer  :: ilat, ilon
   real(r8) :: del_lat, del_lon

      this%nlat = lat_points
      this%nlon = lon_points

      CALL this%init (this%nlon, this%nlat)

      del_lat = 180.0 / lat_points
      DO ilat = 1, this%nlat
         this%lat_s(ilat) = 90.0 - del_lat * ilat
         this%lat_n(ilat) = 90.0 - del_lat * (ilat-1)
      ENDDO

      del_lon = 360.0 / lon_points
      DO ilon = 1, this%nlon
         this%lon_w(ilon) = -180.0 + del_lon * (ilon-1)
         this%lon_e(ilon) = -180.0 + del_lon * ilon
      ENDDO

      this%lon_e(this%nlon) = -180.0

      CALL this%normalize  ()
      CALL this%set_blocks ()

   END SUBROUTINE grid_define_by_ndims

   !-----------------------------------------------------
   SUBROUTINE grid_define_by_res (this, lon_res, lat_res)

   IMPLICIT NONE
   class (grid_type) :: this

   real(r8), intent(in) :: lon_res, lat_res

   ! Local variables
   integer  :: lon_points, lat_points

      lon_points = nint(360.0/lon_res)
      lat_points = nint(180.0/lat_res)

      CALL this%define_by_ndims (lon_points, lat_points)

   END SUBROUTINE grid_define_by_res

   !---------------------------------------------
   SUBROUTINE grid_define_by_center (this, lat_in, lon_in, &
         south, north, west, east)

   USE MOD_Precision
   USE MOD_Utils
   IMPLICIT NONE
   class (grid_type) :: this

   real(r8), intent(in) :: lat_in(:), lon_in(:)
   real(r8), intent(in), optional :: south, north, west, east

   ! Local variables
   integer :: ilat, ilon, ilone, ilonw
   real(r8), allocatable :: lon_in_n(:)

      this%nlat = size(lat_in)
      this%nlon = size(lon_in)

      CALL this%init (this%nlon, this%nlat)

      IF (lat_in(1) > lat_in(this%nlat)) THEN
         this%yinc = -1
      ELSE
         this%yinc = 1
      ENDIF

      DO ilat = 1, this%nlat
         IF (this%yinc == 1) THEN
            IF (ilat < this%nlat) THEN
               this%lat_n(ilat) = (lat_in(ilat) + lat_in(ilat+1)) * 0.5
            ELSE
               IF (present(north)) THEN
                  this%lat_n(ilat) = north
               ELSE
                  this%lat_n(ilat) = 90.0
               ENDIF
            ENDIF

            IF (ilat > 1) THEN
               this%lat_s(ilat) = (lat_in(ilat-1) + lat_in(ilat)) * 0.5
            ELSE
               IF (present(south)) THEN
                  this%lat_s(ilat) = south
               ELSE
                  this%lat_s(ilat) = -90.0
               ENDIF
            ENDIF
         ELSEIF (this%yinc == -1) THEN
            IF (ilat > 1) THEN
               this%lat_n(ilat) = (lat_in(ilat-1) + lat_in(ilat)) * 0.5
            ELSE
               IF (present(north)) THEN
                  this%lat_n(ilat) = north
               ELSE
                  this%lat_n(ilat) = 90.0
               ENDIF
            ENDIF

            IF (ilat < this%nlat) THEN
               this%lat_s(ilat) = (lat_in(ilat) + lat_in(ilat+1)) * 0.5
            ELSE
               IF (present(south)) THEN
                  this%lat_s(ilat) = south
               ELSE
                  this%lat_s(ilat) = -90.0
               ENDIF
            ENDIF
         ENDIF
      ENDDO

      allocate (lon_in_n (size(lon_in)))

      lon_in_n = lon_in
      DO ilon = 1, size(lon_in_n)
         CALL normalize_longitude (lon_in_n(ilon))
      ENDDO

      DO ilon = 1, this%nlon
         ilone = mod(ilon,this%nlon) + 1
         IF (lon_in_n(ilon) > lon_in_n(ilone)) THEN
            this%lon_e(ilon) = (lon_in_n(ilon) + lon_in_n(ilone) + 360.0) * 0.5
         ELSE
            this%lon_e(ilon) = (lon_in_n(ilon) + lon_in_n(ilone)) * 0.5
         ENDIF

         IF ((ilon == this%nlon) .and. (present(east))) THEN
            this%lon_e(this%nlon) = east
         ENDIF

         ilonw = ilon - 1
         IF (ilonw == 0) ilonw = this%nlon
         IF (lon_in_n(ilonw) > lon_in_n(ilon)) THEN
            this%lon_w(ilon) = (lon_in_n(ilonw) + lon_in_n(ilon) + 360.0) * 0.5
         ELSE
            this%lon_w(ilon) = (lon_in_n(ilonw) + lon_in_n(ilon)) * 0.5
         ENDIF

         IF ((ilon == 1) .and. (present(west))) THEN
            this%lon_w(1) = west
         ENDIF
      ENDDO

      deallocate (lon_in_n)

      CALL this%normalize  ()
      CALL this%set_blocks ()

   END SUBROUTINE grid_define_by_center

   !-----------------------------------------------------
   SUBROUTINE grid_define_from_file (this, filename, latname, lonname)

   USE MOD_NetCDFSerial
   IMPLICIT NONE
   class (grid_type) :: this

   character(len=*), intent(in) :: filename
   character(len=*), intent(in), optional :: latname, lonname

   ! Local Variables
   real(r8), allocatable :: lon_in(:)
   real(r8), allocatable :: lat_in(:)

      IF (.not. (present(latname) .and. present(lonname))) THEN

         CALL ncio_read_bcast_serial (filename, 'lat_s', this%lat_s)
         CALL ncio_read_bcast_serial (filename, 'lat_n', this%lat_n)
         CALL ncio_read_bcast_serial (filename, 'lon_w', this%lon_w)
         CALL ncio_read_bcast_serial (filename, 'lon_e', this%lon_e)

         this%nlat = size(this%lat_s)
         this%nlon = size(this%lon_w)

         CALL this%normalize  ()
         CALL this%set_blocks ()

      ELSE

         CALL ncio_read_bcast_serial (filename, latname, lat_in)
         CALL ncio_read_bcast_serial (filename, lonname, lon_in)
         CALL this%define_by_center (lat_in, lon_in)

         deallocate (lat_in, lon_in)
      ENDIF

   END SUBROUTINE grid_define_from_file

   !-----------------------------------------------------
   RECURSIVE SUBROUTINE grid_define_by_copy (this, grid_in)

   USE MOD_NetCDFSerial
   IMPLICIT NONE
   class(grid_type), intent(inout) :: this
   type(grid_type), intent(in) :: grid_in

      CALL this%init (grid_in%nlon, grid_in%nlat)

      this%lat_s = grid_in%lat_s
      this%lat_n = grid_in%lat_n
      this%lon_w = grid_in%lon_w
      this%lon_e = grid_in%lon_e

      CALL this%normalize  ()
      CALL this%set_blocks ()

   END SUBROUTINE grid_define_by_copy

   !-----------------------------------------------------
   SUBROUTINE grid_normalize (this)

   USE MOD_Utils
   IMPLICIT NONE
   class(grid_type) :: this

   ! Local variable
   integer :: ilon, ilat

      DO ilon = 1, this%nlon
         CALL normalize_longitude (this%lon_w(ilon))
         CALL normalize_longitude (this%lon_e(ilon))
      ENDDO

      DO ilat = 1, this%nlat
         this%lat_s(ilat) = max(-90.0, min(90.0, this%lat_s(ilat)))
         this%lat_n(ilat) = max(-90.0, min(90.0, this%lat_n(ilat)))
      ENDDO

      IF (this%lat_s(1) <= this%lat_s(this%nlat)) THEN
         this%yinc = 1
      ELSE
         this%yinc = -1
      ENDIF

      ! align grid
      DO ilon = 1, this%nlon-1
         IF (lon_between_ceil(this%lon_e(ilon), this%lon_w(ilon+1), this%lon_e(ilon+1))) THEN
            this%lon_e(ilon) = this%lon_w(ilon+1)
         ELSE
            this%lon_w(ilon+1) = this%lon_e(ilon)
         ENDIF
      ENDDO

      IF (this%nlon > 1) THEN
         ilon = this%nlon
         IF (lon_between_ceil(this%lon_e(ilon), this%lon_w(1), this%lon_e(1))) THEN
            this%lon_e(ilon) = this%lon_w(1)
         ENDIF
      ENDIF

      DO ilat = 1, this%nlat-1
         IF (this%yinc == 1) THEN
            this%lat_n(ilat)   = max(this%lat_n(ilat),this%lat_s(ilat+1))
            this%lat_s(ilat+1) = this%lat_n(ilat)
         ELSEIF (this%yinc == -1) THEN
            this%lat_s(ilat)   = min(this%lat_s(ilat),this%lat_n(ilat+1))
            this%lat_n(ilat+1) = this%lat_s(ilat)
         ENDIF
      ENDDO

   END SUBROUTINE grid_normalize

   !-----------------------------------------------------
   SUBROUTINE grid_set_blocks (this)

   USE MOD_Namelist
   USE MOD_Block
   USE MOD_Utils
   IMPLICIT NONE

   class (grid_type) :: this

   ! Local variables
   integer  :: ilat, ilon, iblk, jblk, ilon_e
   real(r8) :: edges, edgen, edgew, edgee

      IF (allocated(this%xcnt)) deallocate(this%xcnt)
      IF (allocated(this%xdsp)) deallocate(this%xdsp)
      IF (allocated(this%ycnt)) deallocate(this%ycnt)
      IF (allocated(this%ydsp)) deallocate(this%ydsp)
      allocate (this%xcnt (gblock%nxblk))
      allocate (this%xdsp (gblock%nxblk))
      allocate (this%ycnt (gblock%nyblk))
      allocate (this%ydsp (gblock%nyblk))

      IF (allocated(this%xblk)) deallocate(this%xblk)
      IF (allocated(this%yblk)) deallocate(this%yblk)
      allocate (this%xblk (this%nlon))
      allocate (this%yblk (this%nlat))

      IF (allocated(this%xloc)) deallocate(this%xloc)
      IF (allocated(this%yloc)) deallocate(this%yloc)
      allocate (this%xloc (this%nlon))
      allocate (this%yloc (this%nlat))

      edges = DEF_domain%edges
      edgen = DEF_domain%edgen
      edgew = DEF_domain%edgew
      edgee = DEF_domain%edgee

      CALL normalize_longitude (edgew)
      CALL normalize_longitude (edgee)

      IF (this%yinc == 1) THEN

         this%ycnt(:) = 0
         this%yblk(:) = 0

         IF (edges < this%lat_s(1)) THEN
            jblk = find_nearest_south (this%lat_s(1), gblock%nyblk, gblock%lat_s)
            ilat = 1
         ELSE
            jblk = find_nearest_south (edges, gblock%nyblk, gblock%lat_s)
            ilat = find_nearest_south (edges, this%nlat, this%lat_s)
         ENDIF

         this%ydsp(jblk) = ilat - 1

         DO WHILE (ilat <= this%nlat)
            IF (this%lat_s(ilat) < edgen) THEN
               IF (this%lat_s(ilat) < gblock%lat_n(jblk)) THEN

                  this%ycnt(jblk) = this%ycnt(jblk) + 1

                  this%yblk(ilat) = jblk
                  this%yloc(ilat) = this%ycnt(jblk)

                  ilat = ilat + 1
               ELSE
                  jblk = jblk + 1
                  IF (jblk <= gblock%nyblk) THEN
                     this%ydsp(jblk) = ilat - 1
                  ELSE
                     EXIT
                  ENDIF
               ENDIF
            ELSE
               EXIT
            ENDIF
         ENDDO

      ELSE

         this%ycnt(:) = 0
         this%yblk(:) = 0

         IF (edgen > this%lat_n(1)) THEN
            jblk = find_nearest_north (this%lat_n(1), gblock%nyblk, gblock%lat_n)
            ilat = 1
         ELSE
            jblk = find_nearest_north (edgen, gblock%nyblk, gblock%lat_n)
            ilat = find_nearest_north (edgen, this%nlat, this%lat_n)
         ENDIF

         this%ydsp(jblk) = ilat - 1

         DO WHILE (ilat <= this%nlat)
            IF (this%lat_n(ilat) > edges) THEN
               IF (this%lat_n(ilat) > gblock%lat_s(jblk)) THEN

                  this%ycnt(jblk) = this%ycnt(jblk) + 1

                  this%yblk(ilat) = jblk
                  this%yloc(ilat) = this%ycnt(jblk)

                  ilat = ilat + 1
               ELSE
                  jblk = jblk - 1
                  IF (jblk >= 1) THEN
                     this%ydsp(jblk) = ilat - 1
                  ELSE
                     EXIT
                  ENDIF
               ENDIF
            ELSE
               EXIT
            ENDIF
         ENDDO

      ENDIF


      this%xcnt(:) = 0
      this%xblk(:) = 0

      IF ((this%lon_w(1) /= this%lon_e(this%nlon)) &
         .and. (lon_between_floor(edgew, this%lon_e(this%nlon), this%lon_w(1)))) THEN
         iblk = find_nearest_west (this%lon_w(1), gblock%nxblk, gblock%lon_w)
         ilon = 1
      ELSE
         iblk = find_nearest_west (edgew, gblock%nxblk, gblock%lon_w)
         ilon = find_nearest_west (edgew, this%nlon, this%lon_w)
      ENDIF

      this%xdsp(iblk) = ilon - 1
      this%xcnt(iblk) = 1
      this%xblk(ilon) = iblk
      this%xloc(ilon) = 1

      ilon_e = ilon - 1
      IF (ilon_e == 0) ilon_e = this%nlon
      ilon = mod(ilon,this%nlon) + 1
      DO WHILE (.true.)
         IF (lon_between_floor(this%lon_w(ilon), edgew, edgee)) THEN
            IF (lon_between_floor(this%lon_w(ilon), gblock%lon_w(iblk), gblock%lon_e(iblk))) THEN

               this%xcnt(iblk) = this%xcnt(iblk) + 1

               this%xblk(ilon) = iblk
               this%xloc(ilon) = this%xcnt(iblk)

               IF (ilon /= ilon_e) THEN
                  ilon = mod(ilon,this%nlon) + 1
               ELSE
                  EXIT
               ENDIF
            ELSE
               iblk = mod(iblk,gblock%nxblk) + 1
               IF (this%xcnt(iblk) == 0) THEN
                  this%xdsp(iblk) = ilon - 1
               ELSE
                  ilon_e = this%xdsp(iblk) + this%xcnt(iblk)
                  IF (ilon_e > this%nlon) ilon_e = ilon_e - this%nlon

                  this%xdsp(iblk) = ilon - 1
                  this%xcnt(iblk) = 0
                  DO WHILE (.true.)
                     this%xcnt(iblk) = this%xcnt(iblk) + 1
                     this%xblk(ilon) = iblk
                     this%xloc(ilon) = this%xcnt(iblk)

                     IF (ilon /= ilon_e) THEN
                        ilon = mod(ilon,this%nlon) + 1
                     ELSE
                        EXIT
                     ENDIF
                  ENDDO

                  EXIT
               ENDIF
            ENDIF
         ELSE
            EXIT
         ENDIF
      ENDDO

   END SUBROUTINE grid_set_blocks

   !-----------
   SUBROUTINE grid_set_rlon (this)

   USE MOD_Precision
   USE MOD_Utils
   USE MOD_Vars_Global, only: pi
   IMPLICIT NONE

   class (grid_type) :: this

   ! Local variables
   integer  :: ix
   real(r8) :: lon

      IF (.not. allocated(this%rlon)) THEN
         allocate (this%rlon(this%nlon))
      ENDIF

      DO ix = 1, this%nlon
         IF (this%lon_w(ix) <= this%lon_e(ix)) THEN
            lon = (this%lon_w(ix) + this%lon_e(ix)) * 0.5
         ELSE
            lon = (this%lon_w(ix) + this%lon_e(ix)) * 0.5 + 180.0
         ENDIF

         CALL normalize_longitude (lon)

         this%rlon(ix) = lon / 180.0_r8 * pi
      ENDDO

   END SUBROUTINE grid_set_rlon

   !-----------
   SUBROUTINE grid_set_rlat (this)

   USE MOD_Precision
   USE MOD_Utils
   USE MOD_Vars_Global, only: pi
   IMPLICIT NONE

   class (grid_type) :: this

   ! Local variables
   integer :: iy

      IF (.not. allocated(this%rlat)) THEN
         allocate (this%rlat(this%nlat))
      ENDIF

      DO iy = 1, this%nlat
         this%rlat(iy) = &
            (this%lat_s(iy) + this%lat_n(iy)) * 0.5 / 180.0_r8 * pi
      ENDDO

   END SUBROUTINE grid_set_rlat

   !---------
   SUBROUTINE grid_free_mem (this)

   IMPLICIT NONE
   type (grid_type) :: this

      IF (allocated (this%lat_s))  deallocate (this%lat_s)
      IF (allocated (this%lat_n))  deallocate (this%lat_n)
      IF (allocated (this%lon_w))  deallocate (this%lon_w)
      IF (allocated (this%lon_e))  deallocate (this%lon_e)

      IF (allocated (this%xdsp))   deallocate (this%xdsp)
      IF (allocated (this%ydsp))   deallocate (this%ydsp)

      IF (allocated (this%xcnt))   deallocate (this%xcnt)
      IF (allocated (this%ycnt))   deallocate (this%ycnt)

      IF (allocated (this%xblk))   deallocate (this%xblk)
      IF (allocated (this%yblk))   deallocate (this%yblk)

      IF (allocated (this%xloc))   deallocate (this%xloc)
      IF (allocated (this%yloc))   deallocate (this%yloc)

      IF (allocated (this%xgrd))   deallocate (this%xgrd)
      IF (allocated (this%ygrd))   deallocate (this%ygrd)

      IF (allocated (this%rlon))   deallocate (this%rlon)
      IF (allocated (this%rlat))   deallocate (this%rlat)

   END SUBROUTINE grid_free_mem

   !----------
   SUBROUTINE set_grid_concat (this, grid)

   USE MOD_Block
   USE MOD_Utils
   IMPLICIT NONE

   class(grid_concat_type) :: this
   type(grid_type), intent(in) :: grid

   ! Local variables
   integer :: ilat_l, ilat_u, ilat, ilatloc, jblk, iyseg
   integer :: ilon_w, ilon_e, ilon, ilonloc, iblk, ixseg

      ilat_l = findloc_ud(grid%yblk /= 0)
      ilat_u = findloc_ud(grid%yblk /= 0, back=.true.)

      this%ginfo%nlat = ilat_u - ilat_l + 1
      IF (allocated(this%ginfo%lat_s)) deallocate(this%ginfo%lat_s)
      IF (allocated(this%ginfo%lat_n)) deallocate(this%ginfo%lat_n)
      IF (allocated(this%ginfo%lat_c)) deallocate(this%ginfo%lat_c)
      allocate (this%ginfo%lat_s (this%ginfo%nlat))
      allocate (this%ginfo%lat_n (this%ginfo%nlat))
      allocate (this%ginfo%lat_c (this%ginfo%nlat))

      this%nyseg = 0
      jblk  = 0
      ilatloc = 0
      DO ilat = ilat_l, ilat_u
         IF (grid%yblk(ilat) /= jblk) THEN
            this%nyseg = this%nyseg + 1
            jblk  = grid%yblk(ilat)
         ENDIF

         ilatloc = ilatloc + 1
         this%ginfo%lat_s(ilatloc) = grid%lat_s(ilat)
         this%ginfo%lat_n(ilatloc) = grid%lat_n(ilat)
         this%ginfo%lat_c(ilatloc) = (grid%lat_s(ilat)+grid%lat_n(ilat)) * 0.5
      ENDDO

      IF (allocated(this%ysegs)) deallocate(this%ysegs)
      allocate (this%ysegs (this%nyseg))

      iyseg = 0
      jblk  = 0
      DO ilat = ilat_l, ilat_u
         IF (grid%yblk(ilat) /= jblk) THEN
            iyseg = iyseg + 1
            jblk  = grid%yblk(ilat)
            this%ysegs(iyseg)%blk  = jblk
            this%ysegs(iyseg)%bdsp = grid%yloc(ilat) - 1
            this%ysegs(iyseg)%gdsp = ilat - ilat_l
            this%ysegs(iyseg)%cnt  = 1
         ELSE
            this%ysegs(iyseg)%cnt  = this%ysegs(iyseg)%cnt + 1
         ENDIF
      ENDDO

      IF (all(grid%xblk > 0)) THEN
         ilon_w = 1
         ilon_e = grid%nlon
      ELSE
         ilon_w = findloc_ud(grid%xblk /= 0)
         DO WHILE (.true.)
            ilon = ilon_w - 1
            IF (ilon == 0) ilon = grid%nlon

            IF (grid%xblk(ilon) /= 0) THEN
               ilon_w = ilon
            ELSE
               EXIT
            ENDIF
         ENDDO

         ilon_e = ilon_w
         DO WHILE (.true.)
            ilon = mod(ilon_e,grid%nlon) + 1

            IF (grid%xblk(ilon) /= 0) THEN
               ilon_e = ilon
            ELSE
               EXIT
            ENDIF
         ENDDO
      ENDIF

      this%ginfo%nlon = ilon_e - ilon_w + 1
      IF (this%ginfo%nlon <= 0) THEN
         this%ginfo%nlon = this%ginfo%nlon + grid%nlon
      ENDIF

      IF (allocated(this%ginfo%lon_w)) deallocate(this%ginfo%lon_w)
      IF (allocated(this%ginfo%lon_e)) deallocate(this%ginfo%lon_e)
      IF (allocated(this%ginfo%lon_c)) deallocate(this%ginfo%lon_c)
      allocate (this%ginfo%lon_w (this%ginfo%nlon))
      allocate (this%ginfo%lon_e (this%ginfo%nlon))
      allocate (this%ginfo%lon_c (this%ginfo%nlon))

      this%nxseg = 0
      ilon = ilon_w - 1
      iblk = 0
      ilonloc = 0
      DO WHILE (.true.)
         ilon = mod(ilon,grid%nlon) + 1
         IF ((grid%xblk(ilon) /= iblk) .or. (grid%xloc(ilon) == 1)) THEN
            this%nxseg = this%nxseg + 1
            iblk = grid%xblk(ilon)
         ENDIF

         ilonloc = ilonloc + 1
         this%ginfo%lon_w(ilonloc) = grid%lon_w(ilon)
         this%ginfo%lon_e(ilonloc) = grid%lon_e(ilon)

         this%ginfo%lon_c(ilonloc) = (grid%lon_w(ilon) + grid%lon_e(ilon)) * 0.5
         IF (grid%lon_w(ilon) > grid%lon_e(ilon)) THEN
            this%ginfo%lon_c(ilonloc) = this%ginfo%lon_c(ilonloc) + 180.0
            CALL normalize_longitude (this%ginfo%lon_c(ilonloc))
         ENDIF

         IF (ilon == ilon_e) EXIT
      ENDDO

      DO ilon = 2, this%ginfo%nlon
         IF ((this%ginfo%lon_c(ilon) < this%ginfo%lon_c(ilon-1)) &
            .and. (this%ginfo%lon_c(ilon) < 0)) THEN
            this%ginfo%lon_c(ilon) = this%ginfo%lon_c(ilon) + 360.0
         ENDIF
      ENDDO

      IF (allocated(this%xsegs)) deallocate(this%xsegs)
      allocate (this%xsegs (this%nxseg))

      ixseg = 0
      iblk = 0
      ilon = ilon_w - 1
      ilonloc = 0
      DO WHILE (.true.)
         ilon = mod(ilon,grid%nlon) + 1
         ilonloc = ilonloc + 1
         IF ((grid%xblk(ilon) /= iblk) .or. (grid%xloc(ilon) == 1)) THEN
            ixseg = ixseg + 1
            iblk = grid%xblk(ilon)
            this%xsegs(ixseg)%blk  = iblk
            this%xsegs(ixseg)%bdsp = grid%xloc(ilon) - 1
            this%xsegs(ixseg)%gdsp = ilonloc - 1
            this%xsegs(ixseg)%cnt = 1
         ELSE
            this%xsegs(ixseg)%cnt = this%xsegs(ixseg)%cnt + 1
         ENDIF

         IF (ilon == ilon_e) EXIT
      ENDDO

      this%ndatablk = 0

      DO iyseg = 1, this%nyseg
         DO ixseg = 1, this%nxseg
            iblk = this%xsegs(ixseg)%blk
            jblk = this%ysegs(iyseg)%blk
            IF (gblock%owner_rank(iblk,jblk) >= 0) THEN
               this%ndatablk = this%ndatablk + 1
            ENDIF
         ENDDO
      ENDDO

   END SUBROUTINE set_grid_concat

   !-------
   SUBROUTINE grid_concat_free_mem (this)

   IMPLICIT NONE

   type(grid_concat_type) :: this

      IF (allocated(this%xsegs)) deallocate(this%xsegs)
      IF (allocated(this%ysegs)) deallocate(this%ysegs)

      IF (allocated(this%ginfo%lat_s)) deallocate(this%ginfo%lat_s)
      IF (allocated(this%ginfo%lat_n)) deallocate(this%ginfo%lat_n)
      IF (allocated(this%ginfo%lat_c)) deallocate(this%ginfo%lat_c)
      IF (allocated(this%ginfo%lon_w)) deallocate(this%ginfo%lon_w)
      IF (allocated(this%ginfo%lon_e)) deallocate(this%ginfo%lon_e)
      IF (allocated(this%ginfo%lon_c)) deallocate(this%ginfo%lon_c)

   END SUBROUTINE grid_concat_free_mem

END MODULE MOD_Grid
