#include <define.h>

MODULE MOD_NetCDFPoint

!----------------------------------------------------------------------------------
! !DESCRIPTION:
!
!    High-level Subroutines to read and write variables in files with netCDF format.
!    Read data for single point run.
!
!  Created by Shupeng Zhang, March 2025
!----------------------------------------------------------------------------------

   USE netcdf
   USE MOD_Precision
   USE MOD_Grid
   USE MOD_Utils
   USE MOD_NetCDFSerial, only : nccheck
   IMPLICIT NONE

   ! PUBLIC subroutines
   PUBLIC :: read_point_var_2d_int32
   PUBLIC :: read_point_var_2d_real8
   PUBLIC :: read_point_var_3d_real8
   PUBLIC :: read_point_var_3d_first_real8
   PUBLIC :: read_point_var_2d_time_real8
   PUBLIC :: read_point_var_3d_time_real8
   PUBLIC :: read_point_5x5_var_2d_int32
   PUBLIC :: read_point_5x5_var_2d_real8
   PUBLIC :: read_point_5x5_var_3d_real8
   PUBLIC :: read_point_5x5_var_2d_time_real8
   PUBLIC :: read_point_5x5_var_3d_time_real8

CONTAINS

   ! ------
   SUBROUTINE read_point_var_2d_int32 (grid, filename, varname, site_lon, site_lat, rdata)

   IMPLICIT NONE

   type(grid_type),  intent(in) :: grid
   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: varname
   real(r8), intent(in) :: site_lon
   real(r8), intent(in) :: site_lat

   integer, intent(out) :: rdata

   ! local variables
   integer :: ilat, ilon
   integer :: ncid, varid
   integer :: rcache(1)

      ilon = find_nearest_west  (site_lon, grid%nlon, grid%lon_w)
      ilat = find_nearest_south (site_lat, grid%nlat, grid%lat_s)

      CALL nccheck (nf90_open      (trim(filename), NF90_NOWRITE, ncid), trace=trim(filename)//' cannot open')
      CALL nccheck (nf90_inq_varid (ncid, trim(varname), varid), trace=trim(varname)//' in file '//trim(filename))
      CALL nccheck (nf90_get_var   (ncid, varid, rcache, (/ilon,ilat/), (/1,1/)) )
      CALL nccheck (nf90_close     (ncid) )

      rdata = rcache(1)

   END SUBROUTINE read_point_var_2d_int32

   ! ------
   SUBROUTINE read_point_var_2d_real8 (grid, filename, varname, site_lon, site_lat, rdata)

   IMPLICIT NONE

   type(grid_type),  intent(in) :: grid
   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: varname
   real(r8), intent(in) :: site_lon
   real(r8), intent(in) :: site_lat

   real(r8), intent(out) :: rdata

   ! local variables
   integer  :: ilat, ilon
   integer  :: ncid, varid
   real(r8) :: rcache(1)

      ilon = find_nearest_west  (site_lon, grid%nlon, grid%lon_w)
      ilat = find_nearest_south (site_lat, grid%nlat, grid%lat_s)

      CALL nccheck (nf90_open      (trim(filename), NF90_NOWRITE, ncid), trace=trim(filename)//' cannot open')
      CALL nccheck (nf90_inq_varid (ncid, trim(varname), varid), trace=trim(varname)//' in file '//trim(filename))
      CALL nccheck (nf90_get_var   (ncid, varid, rcache, (/ilon,ilat/), (/1,1/)) )
      CALL nccheck (nf90_close     (ncid) )

      rdata = rcache(1)

   END SUBROUTINE read_point_var_2d_real8

   ! ------
   SUBROUTINE read_point_var_3d_real8 (grid, filename, varname, site_lon, site_lat, nlastdim, rdata)

   IMPLICIT NONE

   type(grid_type),  intent(in) :: grid
   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: varname
   real(r8), intent(in) :: site_lon
   real(r8), intent(in) :: site_lat
   integer,  intent(in) :: nlastdim

   real(r8), allocatable, intent(out) :: rdata(:)

   ! local variables
   integer :: ilat, ilon
   integer :: ncid, varid

      ilon = find_nearest_west  (site_lon, grid%nlon, grid%lon_w)
      ilat = find_nearest_south (site_lat, grid%nlat, grid%lat_s)

      allocate (rdata (nlastdim))

      CALL nccheck (nf90_open      (trim(filename), NF90_NOWRITE, ncid), trace=trim(filename)//' cannot open')
      CALL nccheck (nf90_inq_varid (ncid, trim(varname), varid), trace=trim(varname)//' in file '//trim(filename))
      CALL nccheck (nf90_get_var   (ncid, varid, rdata, (/ilon,ilat,1/), (/1,1,nlastdim/)) )
      CALL nccheck (nf90_close     (ncid) )

   END SUBROUTINE read_point_var_3d_real8

   ! ------
   SUBROUTINE read_point_var_3d_first_real8 (grid, filename, varname, site_lon, site_lat, nfirstdim, rdata)

   IMPLICIT NONE

   type(grid_type),  intent(in) :: grid
   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: varname
   real(r8), intent(in) :: site_lon
   real(r8), intent(in) :: site_lat
   integer,  intent(in) :: nfirstdim

   real(r8), allocatable, intent(out) :: rdata(:)

   ! local variables
   integer :: ilat, ilon
   integer :: ncid, varid

      ilon = find_nearest_west  (site_lon, grid%nlon, grid%lon_w)
      ilat = find_nearest_south (site_lat, grid%nlat, grid%lat_s)

      allocate (rdata (nfirstdim))

      CALL nccheck (nf90_open      (trim(filename), NF90_NOWRITE, ncid), trace=trim(filename)//' cannot open')
      CALL nccheck (nf90_inq_varid (ncid, trim(varname), varid), trace=trim(varname)//' in file '//trim(filename))
      CALL nccheck (nf90_get_var   (ncid, varid, rdata, (/1,ilon,ilat/), (/nfirstdim,1,1/)) )
      CALL nccheck (nf90_close     (ncid) )

   END SUBROUTINE read_point_var_3d_first_real8

   ! ------
   SUBROUTINE read_point_var_2d_time_real8 (grid, filename, varname, site_lon, site_lat, itime, rdata)

   IMPLICIT NONE

   type(grid_type),  intent(in) :: grid
   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: varname
   real(r8), intent(in) :: site_lon
   real(r8), intent(in) :: site_lat
   integer,  intent(in) :: itime

   real(r8), intent(out) :: rdata

   ! local variables
   integer  :: ilat, ilon
   integer  :: ncid, varid
   real(r8) :: rcache(1)

      ilon = find_nearest_west  (site_lon, grid%nlon, grid%lon_w)
      ilat = find_nearest_south (site_lat, grid%nlat, grid%lat_s)

      CALL nccheck (nf90_open      (trim(filename), NF90_NOWRITE, ncid), trace=trim(filename)//' cannot open')
      CALL nccheck (nf90_inq_varid (ncid, trim(varname), varid), trace=trim(varname)//' in file '//trim(filename))
      CALL nccheck (nf90_get_var   (ncid, varid, rcache, (/ilon,ilat,itime/), (/1,1,1/)) )
      CALL nccheck (nf90_close     (ncid) )

      rdata = rcache(1)

   END SUBROUTINE read_point_var_2d_time_real8

   ! ------
   SUBROUTINE read_point_var_3d_time_real8 (grid, filename, varname, site_lon, site_lat, nlastdim, itime, rdata)

   IMPLICIT NONE

   type(grid_type),  intent(in) :: grid
   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: varname
   real(r8), intent(in) :: site_lon
   real(r8), intent(in) :: site_lat
   integer,  intent(in) :: nlastdim
   integer,  intent(in) :: itime

   real(r8), allocatable, intent(out) :: rdata(:)

   ! local variables
   integer :: ilat, ilon
   integer :: ncid, varid

      ilon = find_nearest_west  (site_lon, grid%nlon, grid%lon_w)
      ilat = find_nearest_south (site_lat, grid%nlat, grid%lat_s)

      allocate (rdata (nlastdim))

      CALL nccheck (nf90_open      (trim(filename), NF90_NOWRITE, ncid), trace=trim(filename)//' cannot open')
      CALL nccheck (nf90_inq_varid (ncid, trim(varname), varid), trace=trim(varname)//' in file '//trim(filename))
      CALL nccheck (nf90_get_var   (ncid, varid, rdata, (/ilon,ilat,1,itime/), (/1,1,nlastdim,1/)) )
      CALL nccheck (nf90_close     (ncid) )

   END SUBROUTINE read_point_var_3d_time_real8

   ! -----
   SUBROUTINE get_5x5_filename (grid, dir_5x5, sfx, site_lon, site_lat, file_5x5, start2)

   IMPLICIT NONE

   type (grid_type),  intent(in) :: grid
   character (len=*), intent(in) :: dir_5x5
   character (len=*), intent(in) :: sfx
   real(r8), intent(in) :: site_lon
   real(r8), intent(in) :: site_lat

   character (len=*), intent(out) :: file_5x5
   integer, intent(out) :: start2(2)

   ! local variables
   integer :: ilon, ilat, nxbox, nybox, ibox, jbox
   character (len=4) :: str

      ilon = find_nearest_west  (site_lon, grid%nlon, grid%lon_w)
      ilat = find_nearest_south (site_lat, grid%nlat, grid%lat_s)

      nxbox = grid%nlon / 360 * 5
      nybox = grid%nlat / 180 * 5

      ibox = (ilon-1)/nxbox + 1
      jbox = (ilat-1)/nybox + 1
      start2(1) = ilon - (ibox-1)*nxbox
      start2(2) = ilat - (jbox-1)*nybox

      file_5x5 = trim(dir_5x5) // '/RG'
      write(str, '(I4)') (19-jbox)*5
      file_5x5 = trim(file_5x5) // '_' // trim(adjustl(str))
      write(str, '(I4)') (ibox-37)*5
      file_5x5 = trim(file_5x5) // '_' // trim(adjustl(str))
      write(str, '(I4)') (18-jbox)*5
      file_5x5 = trim(file_5x5) // '_' // trim(adjustl(str))
      write(str, '(I4)') (ibox-36)*5
      file_5x5 = trim(file_5x5) // '_' // trim(adjustl(str))
      file_5x5 = trim(file_5x5) // '.' // trim(sfx) // '.nc'

   END SUBROUTINE get_5x5_filename

   ! -----
   SUBROUTINE read_point_5x5_var_2d_int32 (grid, dir_5x5, sfx, varname, site_lon, site_lat, rdata)

   IMPLICIT NONE

   type (grid_type),  intent(in) :: grid
   character (len=*), intent(in) :: dir_5x5
   character (len=*), intent(in) :: sfx
   character (len=*), intent(in) :: varname

   real(r8), intent(in) :: site_lon
   real(r8), intent(in) :: site_lat

   integer,  intent(out) :: rdata

   ! Local variables
   character(len=256) :: filename
   integer :: ncid, varid, start2(2)
   integer :: rcache(1)

      CALL get_5x5_filename (grid, dir_5x5, sfx, site_lon, site_lat, filename, start2)

      CALL nccheck (nf90_open      (trim(filename), NF90_NOWRITE, ncid), trace=trim(filename)//' cannot open')
      CALL nccheck (nf90_inq_varid (ncid, trim(varname), varid), trace=trim(varname)//' in file '//trim(filename))
      CALL nccheck (nf90_get_var   (ncid, varid, rcache, start2, (/1,1/)) )
      CALL nccheck (nf90_close     (ncid) )

      rdata = rcache(1)

   END SUBROUTINE read_point_5x5_var_2d_int32

   ! -----
   SUBROUTINE read_point_5x5_var_2d_real8 (grid, dir_5x5, sfx, varname, site_lon, site_lat, rdata)

   IMPLICIT NONE

   type (grid_type),  intent(in) :: grid
   character (len=*), intent(in) :: dir_5x5
   character (len=*), intent(in) :: sfx
   character (len=*), intent(in) :: varname

   real(r8), intent(in) :: site_lon
   real(r8), intent(in) :: site_lat

   real(r8), intent(out) :: rdata

   ! Local variables
   character(len=256) :: filename
   integer  :: ncid, varid, start2(2)
   real(r8) :: rcache(1)

      CALL get_5x5_filename (grid, dir_5x5, sfx, site_lon, site_lat, filename, start2)

      CALL nccheck (nf90_open      (trim(filename), NF90_NOWRITE, ncid), trace=trim(filename)//' cannot open')
      CALL nccheck (nf90_inq_varid (ncid, trim(varname), varid), trace=trim(varname)//' in file '//trim(filename))
      CALL nccheck (nf90_get_var   (ncid, varid, rcache, start2, (/1,1/)) )
      CALL nccheck (nf90_close     (ncid) )

      rdata = rcache(1)

   END SUBROUTINE read_point_5x5_var_2d_real8

   ! -----
   SUBROUTINE read_point_5x5_var_3d_real8 (grid, dir_5x5, sfx, varname, site_lon, site_lat, nlastdim, rdata)

   IMPLICIT NONE

   type (grid_type),  intent(in) :: grid
   character (len=*), intent(in) :: dir_5x5
   character (len=*), intent(in) :: sfx
   character (len=*), intent(in) :: varname

   real(r8), intent(in) :: site_lon
   real(r8), intent(in) :: site_lat
   integer,  intent(in) :: nlastdim

   real(r8), allocatable, intent(out) :: rdata(:)

   ! Local variables
   character(len=256) :: filename
   integer :: ncid, varid, start3(3)

      CALL get_5x5_filename (grid, dir_5x5, sfx, site_lon, site_lat, filename, start3(1:2))

      allocate (rdata (nlastdim))

      start3(3) = 1

      CALL nccheck (nf90_open      (trim(filename), NF90_NOWRITE, ncid), trace=trim(filename)//' cannot open')
      CALL nccheck (nf90_inq_varid (ncid, trim(varname), varid), trace=trim(varname)//' in file '//trim(filename))
      CALL nccheck (nf90_get_var   (ncid, varid, rdata, start3, (/1,1,nlastdim/)) )
      CALL nccheck (nf90_close     (ncid) )

   END SUBROUTINE read_point_5x5_var_3d_real8

   ! -----
   SUBROUTINE read_point_5x5_var_2d_time_real8 (grid, dir_5x5, sfx, varname, site_lon, site_lat, itime, rdata)

   IMPLICIT NONE

   type (grid_type),  intent(in) :: grid
   character (len=*), intent(in) :: dir_5x5
   character (len=*), intent(in) :: sfx
   character (len=*), intent(in) :: varname

   real(r8), intent(in) :: site_lon
   real(r8), intent(in) :: site_lat
   integer,  intent(in) :: itime

   real(r8), intent(out) :: rdata

   ! Local variables
   character(len=256) :: filename
   integer  :: ncid, varid, start3(3)
   real(r8) :: rcache(1)

      CALL get_5x5_filename (grid, dir_5x5, sfx, site_lon, site_lat, filename, start3(1:2))

      start3(3) = itime

      CALL nccheck (nf90_open      (trim(filename), NF90_NOWRITE, ncid), trace=trim(filename)//' cannot open')
      CALL nccheck (nf90_inq_varid (ncid, trim(varname), varid), trace=trim(varname)//' in file '//trim(filename))
      CALL nccheck (nf90_get_var   (ncid, varid, rcache, start3, (/1,1,1/)) )
      CALL nccheck (nf90_close     (ncid) )

      rdata = rcache(1)

   END SUBROUTINE read_point_5x5_var_2d_time_real8

   ! -----
   SUBROUTINE read_point_5x5_var_3d_time_real8 (grid, dir_5x5, sfx, varname, &
         site_lon, site_lat, nlastdim, itime, rdata)

   IMPLICIT NONE

   type (grid_type),  intent(in) :: grid
   character (len=*), intent(in) :: dir_5x5
   character (len=*), intent(in) :: sfx
   character (len=*), intent(in) :: varname

   real(r8), intent(in) :: site_lon
   real(r8), intent(in) :: site_lat
   integer,  intent(in) :: nlastdim
   integer,  intent(in) :: itime

   real(r8), allocatable, intent(out) :: rdata(:)

   ! Local variables
   character(len=256) :: filename
   integer :: ncid, varid, start4(4)

      CALL get_5x5_filename (grid, dir_5x5, sfx, site_lon, site_lat, filename, start4(1:2))

      allocate (rdata (nlastdim))

      start4(3) = 1
      start4(4) = itime

      CALL nccheck (nf90_open      (trim(filename), NF90_NOWRITE, ncid), trace=trim(filename)//' cannot open')
      CALL nccheck (nf90_inq_varid (ncid, trim(varname), varid), trace=trim(varname)//' in file '//trim(filename))
      CALL nccheck (nf90_get_var   (ncid, varid, rdata, start4, (/1,1,nlastdim,1/)) )
      CALL nccheck (nf90_close     (ncid) )

   END SUBROUTINE read_point_5x5_var_3d_time_real8

END MODULE MOD_NetCDFPoint
