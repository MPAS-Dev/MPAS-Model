! This module calculates the parameters required for the subgrid-
! scale orographic gravity-wave drag (GWDO) scheme on the MPAS
! mesh.  These parameters are for the small-scale GWD (Tsiringakis et al.,
! 2017) and the turbulent orographic form drag (TOFD) (Beljaars, 2004)
! schemes of the GSL drag suite.  30 second (~1km) global topography
! is used.  The topographic data comes from the 'fix' file
! HGT.Beljaars_filtered.lat-lon.30s_res.nc.
! The output fields are:
! - stddev      standard deviation of subgrid-scale topograpy
! - convexity   convexity (kurtosis) of subgrid-scale topography
! - ol{1,2,3,4} orographic effective lengths of subgrid-scale topography
!   for 4 orientations: 1-westerly, 2-southerly, 3-southwesterly, 4-northwesterly
! - oa{1,2,3,4} orographic asymmetries of subgrid-scale topography
!   for 4 orientations: 1-westerly, 2-southerly, 3-southwesterly, 4-northwesterly
!
! Based on code by Michael Duda provided by NCAR/MMM
!
module mpas_gsl_oro_data_sm_scale

use iso_c_binding, only : c_char, c_int, c_float, c_ptr, c_loc

use mpas_derived_types
use mpas_framework
use mpas_kind_types
use mpas_log, only : mpas_log_write
use mpas_stream_manager
use mpas_c_interfacing, only : mpas_f_to_c_string

implicit none

public :: calc_gsl_oro_data_sm_scale

private

   interface
      subroutine read_geogrid(fname, rarray, nx, ny, nz, isigned, endian, &
                              wordsize, status) bind(C)
         use iso_c_binding, only : c_char, c_int, c_float, c_ptr
         character (c_char), dimension(*), intent(in) :: fname
         type (c_ptr), value :: rarray
         integer (c_int), intent(in), value :: nx
         integer (c_int), intent(in), value :: ny
         integer (c_int), intent(in), value :: nz
         integer (c_int), intent(in), value :: isigned
         integer (c_int), intent(in), value :: endian
         integer (c_int), intent(in), value :: wordsize
         integer (c_int), intent(inout) :: status
      end subroutine read_geogrid
   end interface

real (kind=RKIND), parameter :: Pi = 2.0_RKIND * asin(1.0_RKIND)

integer, parameter :: topo_x = 43200  ! x-dimension of fine grid (30-arc-second topog array)
integer, parameter :: topo_y = 21600  ! y-dimension of fine grid (30-arc-second topog array)

real (kind=RKIND), allocatable :: lat1d_fine(:) !< latitude of fine grid pts (radians)
real (kind=RKIND), allocatable :: lon1d_fine(:) !< longitude of fine grid pts (radians)

real (kind=RKIND), allocatable :: lon_MPAS(:)   ! "adjusted" longitude

real (kind=RKIND), parameter :: p5 = 0.5_RKIND !< one half


contains

subroutine calc_gsl_oro_data_sm_scale(nCells,lat_MPAS,lon_MPAS_raw,area_MPAS, &
                                 Re,std_dev,convexity,OA1,OA2,OA3,OA4,        &
                                 OL1,OL2,OL3,OL4,domain,duplicate_oro_data)

implicit none

type (domain_type), intent(inout) :: domain

integer, intent(in)  :: nCells
real (kind=RKIND), dimension(:), intent(in) :: lat_MPAS, lon_MPAS_raw  ! radians
real (kind=RKIND), dimension(:), intent(in) :: area_MPAS  ! approx area of MPAS grid cell (m^2)
real (kind=RKIND), intent(in) :: Re
real (kind=RKIND), dimension(:), intent(out) :: std_dev,convexity,OA1,OA2,OA3,OA4, &
                                                OL1,OL2,OL3,OL4
logical, dimension(:), intent(out) :: duplicate_oro_data  ! flag for 'small' grid cell size

integer (c_int) :: istatus
integer :: ix, iy
integer (c_int) :: isigned, endian, wordsize, nx, ny, nz
real (c_float) :: scalefactor
real (c_float), dimension(:,:,:), pointer, contiguous :: tile
type (c_ptr) :: tile_ptr
character(len=StrKIND) :: filename
character(kind=c_char), dimension(StrKIND+1) :: c_filename


real (kind=RKIND) :: DX        ! grid size in km

integer :: i,j,ii,jj
integer :: iErr

integer, parameter :: tile_x = 8640       ! x-dimension of each tile of global 30-arc-second topography
integer, parameter :: tile_y = 4320       ! y-dimension of each tile of global 30-arc-second topography
integer, parameter :: tile_bdr = 0        ! number of layers of border/halo points surrounding each tile

integer :: nfinepoints   ! number of fine grid points in each coarse grid cell

real (kind=RKIND) :: sum2, sum4, var


real (kind=RKIND), allocatable :: zs(:,:)
           
logical :: zs_accum

real (kind=RKIND) :: zs_mean

real (kind=RKIND), parameter :: max_convexity = 10._RKIND  ! max value for convexity

integer :: nu, nd, nw, nt
real (kind=RKIND) :: ratio


real (kind=R4KIND), allocatable :: HGT_M_fine(:,:)
real (kind=RKIND) :: dlta_lat, dlta_lon

character(len=StrKIND), pointer :: config_geog_data_path
character(len=StrKIND) :: geog_sub_path
character(len=StrKIND+1) :: geog_data_path     ! same as config_geog_data_path, but guaranteed to have a trailing slash

integer :: i_blk, j_blk
integer :: ii_loc, jj_loc, ii_m, jj_m
integer, dimension(3) :: s_ii, e_ii, s_jj, e_jj
real (kind=RKIND), dimension(3) :: lat_blk, lon_blk



call mpas_log_write('Creating oro_data_ss fields')

call mpas_pool_get_config(domain % configs, 'config_geog_data_path', config_geog_data_path)

write(geog_data_path, '(a)') config_geog_data_path
i = len_trim(geog_data_path)
if (geog_data_path(i:i) /= '/') then
   geog_data_path(i+1:i+1) = '/'
end if
geog_sub_path = 'topo_ugwp_30s/'

!
! Retrieve 30s topo data from WPS_GEOG
!
isigned  = 1
endian   = 0
wordsize = 4
scalefactor = 0.1
nx = tile_x + 2*tile_bdr
ny = tile_y + 2*tile_bdr
nz = 1

allocate(HGT_M_fine(topo_x,topo_y))
allocate(tile(tile_x+2*tile_bdr,tile_y+2*tile_bdr,1))
tile_ptr = c_loc(tile)

do iy=1,topo_y,tile_y
do ix=1,topo_x,tile_x
   write(filename,'(a,i5.5,a1,i5.5,a1,i5.5,a1,i5.5)') trim(geog_data_path)//trim(geog_sub_path), &
                       ix, '-', (ix+tile_x-1), '.', iy, '-', (iy+tile_y-1)
   call mpas_f_to_c_string(filename, c_filename)
   call read_geogrid(c_filename, tile_ptr, nx, ny, nz, isigned, endian,  &
                  wordsize, istatus) 
   tile(:,:,:) = tile(:,:,:) * scalefactor
   if (istatus /= 0) then
      call mpas_log_write('Error reading topography tile '//trim(filename), messageType=MPAS_LOG_ERR)
      iErr = 1
      return
   end if
   HGT_M_fine(ix:(ix+tile_x-1),iy:(iy+tile_y-1)) = tile((tile_bdr+1):(tile_x+tile_bdr),(tile_bdr+1):(tile_y+tile_bdr),1)
end do
end do

deallocate(tile)


! Calculate fine grid lat/lon in radians
allocate (lat1d_fine(topo_y))
allocate (lon1d_fine(topo_x))
do j = 1,topo_y
   lat1d_fine(j) = ( -90._RKIND + (180._RKIND/topo_y)*(j-p5) )*Pi/180._RKIND
end do
do i = 1,topo_x
   lon1d_fine(i) = (-180._RKIND + (360._RKIND/topo_x)*(i-p5) )*Pi/180._RKIND
end do


! Reassign MPAS longitude to vary from -Pi to Pi to match lon1d_fine range
! Transfer data from lon_MPAS_raw to lon_MPAS
allocate (lon_MPAS(nCells))
do i = 1,nCells
   if ( lon_MPAS_raw(i).gt.Pi ) then
      lon_MPAS(i) = lon_MPAS_raw(i) - 2*Pi
   else
      lon_MPAS(i) = lon_MPAS_raw(i)
   end if
end do

! Initialize GWD statistics fields
std_dev(:) = 0._RKIND
convexity(:) = 0._RKIND
OA1(:) = 0._RKIND
OA2(:) = 0._RKIND
OA3(:) = 0._RKIND
OA4(:) = 0._RKIND
OL1(:) = 0._RKIND
OL2(:) = 0._RKIND
OL3(:) = 0._RKIND
OL4(:) = 0._RKIND

! Determine whether grid size is less than 7.5km -- the limit for large-scale stats.
duplicate_oro_data(:) = .false.
do i = 1,nCells
   dX = sqrt(area_MPAS(i))   ! grid size in meters
   if ( dX .lt. 7500._RKIND ) duplicate_oro_data(i) = .true.
end do


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This is a loop over all the MPAS (coarse) grid cells
! The subgrid-scale topographic variables needed for the large-scale
! orographic gravity wave drag schemes are calculated by the following steps:
! 1) Sample the fine-scale (30sec) topography contained within each
!    coarse grid cell.
! 2) Calculate the orographic statistics: stddev,convexity,oa1,...oa4,
!    ol1,...,ol4
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


do i = 1,nCells

      ! Calculate approximate side-lengths of square lat-long "coarse" grid
      ! cell centered on MPAS cell (units = radians)
      dlta_lat = sqrt(area_MPAS(i))/Re
      dlta_lon = sqrt(area_MPAS(i))/(Re*COS(lat_MPAS(i)))

      ! Determine lat/lon of 9 lat-lon block centers
      ! Note:  lat_blk(2)/lon_blk(2) = lat_MPAS(i)/lon_MPAS(i)
      ! Note:  abs(lon_blk) may exceed Pi
      do i_blk = 1,3
         lon_blk(i_blk) = lon_MPAS(i) + (i_blk-2)*dlta_lon
      end do
      ! Note:  abs(lat_blk) may exceed Pi/2 (90 degrees)
      do j_blk = 1,3
         lat_blk(j_blk) = lat_MPAS(i) + (j_blk-2)*dlta_lat
      end do

      ! Find starting and ending fine-grid i,j indices for each
      ! of the 9 "coarse-grid" blocks
      ! Note:  Index value of -999 is returned if latitude of grid points
      !        exceed 90 degrees north or south
      do i_blk = 1,3
         s_ii(i_blk) = nearest_i_east(lon_blk(i_blk)-p5*dlta_lon)
         e_ii(i_blk) = nearest_i_west(lon_blk(i_blk)+p5*dlta_lon)
      end do
      do j_blk = 1,3
         s_jj(j_blk) = nearest_j_north(lat_blk(j_blk)-p5*dlta_lat)
         e_jj(j_blk) = nearest_j_south(lat_blk(j_blk)+p5*dlta_lat)
      end do

      ! Calculate lat/lon relevant to each "coarse grid" block
      do i_blk = 1,3

         ! "Shave" blocks on north or south due to proximity to poles
         ! if necessary
         j_blk = 1  ! southern row
         ! Check for "shaved" block due to proximity to south pole
         if ( (s_jj(j_blk).eq.-999).and.(e_jj(j_blk).ne.-999) ) then
            s_jj(j_blk) = 1   ! southern boundary of shaved block
            ! Reassign latitude of block center
            lat_blk(j_blk) = p5*(lat1d_fine(1)+lat1d_fine(e_jj(j_blk)))
         end if

         j_blk = 2  ! center row
         ! Check for "shaved" block due to proximity to south or north pole
         ! Note:  We're assuming e_jj(2) and s_jj(2) can't both be -999
         if ( s_jj(j_blk).eq.-999 ) then
            s_jj(j_blk) = 1  ! block shaved on the south
            ! Reassign latitude of block center
            lat_blk(j_blk) = p5*(lat1d_fine(1)+lat1d_fine(e_jj(j_blk)))
         end if
         if ( e_jj(j_blk).eq.-999 ) then
            e_jj(j_blk) = topo_y  ! block shaved on the north
            ! Reassign latitude of block center
            lat_blk(j_blk) = p5*(lat1d_fine(s_jj(j_blk))+lat1d_fine(topo_y))
         end if

         j_blk = 3  ! northern row
         ! Check for "shaved" block due to proximity to north pole
         if ( (e_jj(j_blk).eq.-999).and.(s_jj(j_blk).ne.-999) ) then
            e_jj(j_blk) = topo_y  ! northern boundary of shaved block
            ! Reassign latitude of block center 
            lat_blk(j_blk) = p5*(lat1d_fine(s_jj(j_blk))+lat1d_fine(topo_y))
         end if

      end do

      ! Calculate number of fine-grid points within center coarse block (2,2)
      ! Check if center block straddles date line
      if ( s_ii(2).gt.e_ii(2) ) then
         ii_m = topo_x - s_ii(2) + 1 + e_ii(2)
      else
         ii_m = e_ii(2) - s_ii(2) + 1
      end if
      jj_m = e_jj(2) - s_jj(2) + 1


      ! Assign values to "zs", which is the fine-grid surface topography field
      ! that we will calculate statistics on, i.e, stddev, convexity, etc.
      allocate (zs(ii_m,jj_m))

      do jj = s_jj(2), e_jj(2)
         jj_loc = jj - s_jj(2) + 1  ! local j-index (1 ... jj_m)
         ! Check if block straddles the date line
         if ( s_ii(2).gt.e_ii(2) ) then
            do ii = s_ii(2), topo_x  ! west of the date line
               ii_loc = ii - s_ii(2) + 1   ! local i-index ( 1 ... ii_m)
               zs(ii_loc,jj_loc) = HGT_M_fine(ii,jj)
            end do
            do ii = 1, e_ii(2)   ! east of the date line
               ii_loc = ii_loc + 1  ! local i-index ( 1 ... ii_m )
               zs(ii_loc,jj_loc) = HGT_M_fine(ii,jj)
            end do
         else   ! no crossing of the date line
            do ii = s_ii(2), e_ii(2)
               ii_loc = ii - s_ii(2) + 1   ! local i-index ( 1 ... ii_m)
               zs(ii_loc,jj_loc) = HGT_M_fine(ii,jj)
            end do
         end if
      end do

      !
      ! Finally, we can now calculate the topographic statistics fields needed
      ! for the gravity wave drag scheme
      !

      ! Make sure statistics are zero if there is no terrain in the grid cell
      ! Note:  This is a proxy for a landmask
      zs_accum = .false.
      do jj = 1,jj_m
         do ii = 1,ii_m
            if ( abs(zs(ii,jj)).gt.1.E-1 ) zs_accum = .true.
         end do
      end do
      if ( .not.zs_accum ) then   ! no terrain in the grid cell
         std_dev(i) = 0._RKIND
         convexity(i) = 0._RKIND
         OA1(i) = 0._RKIND
         OA2(i) = 0._RKIND
         OA3(i) = 0._RKIND
         OA4(i) = 0._RKIND
         OL1(i) = 0._RKIND
         OL2(i) = 0._RKIND
         OL3(i) = 0._RKIND
         OL4(i) = 0._RKIND
         deallocate(zs)
         cycle   ! move on to next (coarse) grid cell 
      end if

      !
      ! Calculate standard deviation of subgrid-scale terrain height
      !

      ! Calculate mean height
      sum2 = 0._RKIND
      nfinepoints = ii_m*jj_m
      do jj = 1,jj_m
         do ii = 1,ii_m
            sum2 = sum2 + zs(ii,jj)
         end do
      end do
      zs_mean = sum2 / real(nfinepoints,RKIND)

      ! Calculate standard deviation
      sum2 = 0._RKIND
      do jj = 1,jj_m
         do ii = 1,ii_m
            sum2 = sum2 + ( zs(ii,jj) - zs_mean )**2
         end do
      end do
      std_dev(i) = sqrt( sum2/real(nfinepoints,RKIND) )

      !
      ! Calculate convexity of sub-grid-scale terrain
      !

      sum2 = 0._RKIND
      sum4 = 0._RKIND
      do jj = 1,jj_m
         do ii = 1,ii_m
            sum2 = sum2 + ( zs(ii,jj) - zs_mean )**2
            sum4 = sum4 + ( zs(ii,jj) - zs_mean )**4
         end do
      end do

      var = sum2 / real(nfinepoints,RKIND)
      if ( abs(var) < 1.0E-05_RKIND ) then
         convexity(i) = 0._RKIND
      else
         convexity(i) = min( sum4 / ( var**2 *                    &
                        real(nfinepoints,RKIND) ), max_convexity )
      end if

      !
      ! Calculate orographic asymmetries
      !

      ! OA1 -- orographic asymmetry in West direction
      nu = 0
      nd = 0
      do jj = 1,jj_m
         if(mod(ii_m,2).eq.0.) then
           do ii = 1,ii_m/2   ! left half of box
              if ( zs(ii,jj) > zs_mean ) nu = nu + 1
           end do
         else
           do ii = 1,ii_m/2+1   ! left half of box
              if ( zs(ii,jj) > zs_mean ) nu = nu + 1
           end do
         endif
         do ii = ii_m/2 + 1, ii_m  ! right half of box
            if ( zs(ii,jj) > zs_mean ) nd = nd + 1
         end do
      end do
      if ( nu + nd > 0 ) then
         OA1(i) = real((nu - nd),RKIND) /     &
                                   real((nu + nd),RKIND)
      else
         OA1(i) = 0._RKIND
      end if

      ! OA2 -- orographic asymmetry in South direction
      nu = 0
      nd = 0
      if(mod(jj_m,2).eq.0.) then
        do jj = 1,jj_m/2   ! bottom half of box
           do ii = 1,ii_m
              if ( zs(ii,jj) > zs_mean ) nu = nu + 1
           end do
        end do
      else
        do jj = 1,jj_m/2+1   ! bottom half of box
           do ii = 1,ii_m
              if ( zs(ii,jj) > zs_mean ) nu = nu + 1
           end do
        end do
      endif
      do jj = jj_m/2 + 1,jj_m   ! top half of box
         do ii = 1, ii_m
            if ( zs(ii,jj) > zs_mean ) nd = nd + 1
         end do
      end do
      if ( nu + nd > 0 ) then
         OA2(i) = real((nu - nd),RKIND) /     &
                                   real((nu + nd),RKIND)
      else
         OA2(i) = 0._RKIND
      end if

      ! OA3 -- orographic asymmetry in South-West direction
      nu = 0
      nd = 0
      ratio = real(jj_m,RKIND)/real(ii_m,RKIND)
      do jj = 1,jj_m
         do ii = 1,ii_m
            if ( nint(real(ii,RKIND)*ratio) <= (jj_m - jj + 1) ) then
               ! south-west half of box
               if ( zs(ii,jj) > zs_mean ) nu = nu + 1
            endif
            if ( nint(real(ii,RKIND)*ratio) >= (jj_m - jj + 1) ) then
               ! north-east half of box
               if ( zs(ii,jj) > zs_mean ) nd = nd + 1
            end if
         end do
      end do
      if ( nu + nd > 0 ) then
         OA3(i) = real((nu - nd),RKIND) /     &
                                   real((nu + nd),RKIND)
      else
         OA3(i) = 0._RKIND
      end if

      ! OA4 -- orographic asymmetry in North-West direction
      nu = 0
      nd = 0
      ratio = real(jj_m,RKIND)/real(ii_m,RKIND)
      do jj = 1,jj_m
         do ii = 1,ii_m
            if ( nint(real(ii,RKIND)*ratio) <= jj ) then
               ! north-west half of box
               if ( zs(ii,jj) > zs_mean ) nu = nu + 1
            end if
            if ( nint(real(ii,RKIND)*ratio) >= jj ) then
                ! south-east half of box
               if ( zs(ii,jj) > zs_mean ) nd = nd + 1
            end if
         end do
      end do
      if ( nu + nd > 0 ) then
         OA4(i) = real((nu - nd),RKIND) /     &
                                   real((nu + nd),RKIND)
      else
         OA4(i) = 0._RKIND
      end if


      !
      ! Calculate orographic effective lengths
      !

      ! OL1 -- orographic effective length for Westerly flow
      nw = 0
      nt = 0
      do jj = max(jj_m/4,1), 3*jj_m/4
         ! within central east-west band of box
         do ii = 1, ii_m
            if ( zs(ii,jj) > zs_mean ) nw = nw + 1
            nt = nt + 1
         end do
      end do
      if ( nt /= 0 ) then
         OL1(i) = real(nw,RKIND) / real(nt,RKIND)
      else
         OL1(i) = 0._RKIND
      end if

      ! OL2 -- orographic effective length for Southerly flow
      nw = 0
      nt = 0
      do jj = 1, jj_m
         do ii = max(ii_m/4,1), 3*ii_m/4
            ! within central north-south band of box
            if ( zs(ii,jj) > zs_mean ) nw = nw + 1
            nt = nt + 1
         end do
      end do
      if ( nt /= 0 ) then
         OL2(i) = real(nw,RKIND) / real(nt,RKIND)
      else
         OL2(i) = 0._RKIND
      end if

      ! OL3 -- orographic effective length for South-Westerly flow
      nw = 0
      nt = 0
      do jj = 1, jj_m/2
         do ii = 1, ii_m/2
            if ( zs(ii,jj) > zs_mean ) nw = nw + 1
            nt = nt + 1
         end do
      end do
      do jj = jj_m/2+1, jj_m
         do ii = ii_m/2+1, ii_m
            if ( zs(ii,jj) > zs_mean ) nw = nw + 1
            nt = nt + 1
         end do
      end do
      if ( nt /= 0 ) then
         OL3(i) = real(nw,RKIND) / real(nt,RKIND)
      else
         OL3(i) = 0._RKIND
      end if

      ! OL4 -- orographic effective length for North-Westerly flow
      nw = 0
      nt = 0
      do jj = jj_m/2+1, jj_m
         do ii = 1, ii_m/2
            if ( zs(ii,jj) > zs_mean ) nw = nw + 1
            nt = nt + 1
         end do
      end do
      do jj = 1, jj_m/2
         do ii = ii_m/2+1, ii_m
            if ( zs(ii,jj) > zs_mean ) nw = nw + 1
            nt = nt + 1
         end do
      end do
      if ( nt /= 0 ) then
         OL4(i) = real(nw,RKIND) / real(nt,RKIND)
      else
         OL4(i) = 0._RKIND
      end if

      deallocate (zs)

end do   ! i = 1,nCells


! Deallocate arrays
deallocate(lat1d_fine)
deallocate(lon1d_fine)
deallocate(lon_MPAS)
deallocate(HGT_M_fine)


end subroutine calc_gsl_oro_data_sm_scale



!> Finds nearest fine-grid i index to the east of a given longitude
!!
!! @param[in] lon_in longitude (radians)
!! @return nearest_i_east Nearest grid point i-index east of selected point
!! @author Michael Toy, NOAA/GSL
function nearest_i_east(lon_in)
! Calculates nearest fine-grid i index to the east of (or on) a given longitude
implicit none

integer :: nearest_i_east
real (kind=RKIND), intent(in) :: lon_in
real (kind=RKIND) :: lon
integer :: i

lon = lon_in
! Make sure longitude is between -Pi and Pi
do while ( (lon.lt.(-Pi)).or.(lon.gt.Pi) )
   if ( lon.lt.(-Pi) ) lon = lon + 2*Pi
   if ( lon.gt.Pi ) lon = lon - 2*Pi
end do

if ( lon.gt.lon1d_fine(topo_x) ) then
   nearest_i_east = 1
else
   i = 1
   do while ( lon1d_fine(i).lt.lon )
      i = i + 1
   end do
   nearest_i_east = i
end if

end function nearest_i_east

!> Finds nearest fine-grid i index to the west of a given longitude
!!
!! @param[in] lon_in longitude (radians)
!! @return nearest_i_west Nearest grid point i-index west of selected point
!! @author Michael Toy, NOAA/GSL
function nearest_i_west(lon_in)
! Calculates nearest fine-grid i index to the west of a given longitude
implicit none

integer :: nearest_i_west
real (kind=RKIND), intent(in) :: lon_in
real (kind=RKIND) :: lon
integer :: i

lon = lon_in
! Make sure longitude is between -Pi and Pi
do while ( (lon.lt.(-Pi)).or.(lon.gt.Pi) )
   if ( lon.lt.(-Pi) ) lon = lon + 2*Pi
   if ( lon.gt.Pi ) lon = lon - 2*Pi
end do

if ( (lon.lt.lon1d_fine(1)).or.(lon.ge.lon1d_fine(topo_x)) ) then
   nearest_i_west = topo_x
else
   i = 1
   do while ( lon1d_fine(i).le.lon )
      i = i + 1
   end do
   nearest_i_west = i - 1
end if

end function nearest_i_west

!> Calculates nearest fine-grid j index to the north of a given latitude
!!
!! @param[in] lat_in Latitude (radians)
!! @return nearest_j_north Nearest fine-grid j index to the north of a given latitude
!! @author Michael Toy, NOAA/GSL
function nearest_j_north(lat_in)
! Calculates nearest fine-grid j index to the north of a given latitude
! Note:  If the abs(latitude) is greater than Pi/2 (90 degrees) then
!        the value -999 is returned
implicit none

integer :: nearest_j_north
real (kind=RKIND), intent(in) :: lat_in
real (kind=RKIND) :: lat
integer :: j

lat = lat_in
if ( abs(lat_in).gt.p5*Pi ) then
   nearest_j_north = -999
else
   j = 1
   do while ( (lat1d_fine(j).lt.lat).and.(j.lt.topo_y) )
      j = j + 1
   end do
   nearest_j_north = j
end if

end function nearest_j_north

!> Calculates nearest fine-grid j index to the south of a given latitude
!!
!! @param[in] lat_in Latitude (radians)
!! @return nearest_j_south Nearest fine-grid j index to the south of a given latitude
!! @author Michael Toy, NOAA/GSL
function nearest_j_south(lat_in)
! Calculates nearest fine-grid j index to the south of a given latitude
! Note:  If the abs(latitude) is greater than Pi/2 (90 degrees) then
!        the value -999 is returned
implicit none

integer :: nearest_j_south
real (kind=RKIND), intent(in) :: lat_in
real (kind=RKIND) :: lat
integer :: j

lat = lat_in
if ( abs(lat_in).gt.p5*Pi ) then
   nearest_j_south = -999
elseif ( lat_in.le.lat1d_fine(1) ) then
   nearest_j_south = 1
else
   j = 2
   do while ( (lat1d_fine(j).le.lat).and.(j.le.topo_y) )
      j = j + 1
   end do
   nearest_j_south = j - 1
end if

end function nearest_j_south

!> Interpolates (or extrapolates) linear function y = y(x)
!! 
!! @param[in] x Input "x" value
!! @param[in] x1 Known point 1
!! @param[in] x2 Known point 2
!! @param[in] y1 Known y(x1)
!! @param[in] y2 Known y(x2)
!! @return interp_1d Interpolated y value at x
!! @author Michael Toy, NOAA/GSL
function interp_1d(x,x1,x2,y1,y2)
! Interpolates (or extrapolates) linear function y = y(x)
! to x given y1 = y(x1) and y2 = y(x2)
implicit none

real (kind=RKIND) :: interp_1d
real (kind=RKIND), intent(in) :: x,x1,x2,y1,y2
real (kind=RKIND) :: slope

! Formula for a line: y = y1 + slope*(x - x1)
slope = (y2-y1)/(x2-x1)
interp_1d = y1 + slope*(x-x1)

end function interp_1d



end module mpas_gsl_oro_data_sm_scale
