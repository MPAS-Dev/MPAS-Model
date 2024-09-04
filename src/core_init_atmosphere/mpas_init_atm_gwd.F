! Copyright (c) 2016,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
module mpas_init_atm_gwd

   use iso_c_binding, only : c_char, c_int, c_float, c_ptr, c_loc

   use mpas_derived_types, only : MPAS_LOG_ERR
   use mpas_framework
   use mpas_timekeeping
   use mpas_log, only : mpas_log_write
   use mpas_c_interfacing, only : mpas_f_to_c_string

   public :: compute_gwd_fields

   private

   type :: mpas_gwd_tile_type

      real (kind=R4KIND), dimension(:), pointer :: array
      integer :: tile_start_x, tile_start_y
   ! linked list next pointer
      type (mpas_gwd_tile_type), pointer :: next => null()

   end type mpas_gwd_tile_type

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

   integer, parameter :: I1KIND = selected_int_kind(2)

   real (kind=RKIND), parameter :: Re = 6371229.0_RKIND        ! Earth radius in MPAS-Atmosphere
   real (kind=RKIND), parameter :: Pi = 2.0_RKIND * asin(1.0_RKIND)
   real (kind=RKIND), parameter :: rad2deg = 180.0_RKIND / Pi

   integer, parameter :: topo_x = 43200            ! x-dimension of global 30-arc-second topography array
   integer, parameter :: topo_y = 21600            ! y-dimension of global 30-arc-second topography array
   integer, parameter :: tile_x = 1200       ! x-dimension of each tile of global 30-arc-second topography
   integer, parameter :: tile_y = 1200       ! y-dimension of each tile of global 30-arc-second topography

   real (kind=RKIND), parameter :: pts_per_degree = real(topo_x,RKIND) / 360.0_RKIND

   ! The following are set at the beginning of the compute_gwd_fields routine depending
   ! on the source of topography data to be used
   real (kind=RKIND) :: start_lat
   real (kind=RKIND) :: start_lon

   integer :: topo_shift    ! special handling

   ! Nominal delta-x (in meters) for sub-grid topography cells
   real (kind=RKIND), parameter ::  sg_delta = 2.0 * Pi * Re / (360.0_RKIND * real(pts_per_degree,RKIND))

   real (kind=R4KIND), dimension(:), pointer :: topo ! Global 30-arc-second topography
   real (kind=RKIND), dimension(:,:), pointer :: box   ! Subset of topography covering a grid cell
   real (kind=RKIND), dimension(:,:), pointer :: dxm   ! Size (meters) in zonal direction of a grid cell
   real (kind=RKIND) :: box_mean                       ! Mean value of topography in box
   !integer :: nx, ny                                   ! Dimensions of box covering grid cell
   integer (kind=I1KIND), dimension(:), pointer :: landuse         ! Global 30-arc-second landuse
   integer (kind=I1KIND), dimension(:,:), pointer :: box_landuse     ! Subset of landuse covering a grid cell

   ! NB: At present, only the USGS GLCC land cover dataset is supported, so we can assume 16 == water 
   !     See the read_global_30s_landuse function 
   integer (kind=I1KIND), parameter :: WATER = 16

   integer (kind=I1KIND), dimension(:), pointer :: hlanduse ! Dominant land mask (0 or 1)
   real (kind=RKIND) :: hc   ! critical height

   integer, dimension(:), pointer :: local_tile_x, local_tile_x_land ! 
   integer, dimension(:), pointer :: local_tile_y ! 
   integer, dimension(:), pointer :: local_box_x ! 
   integer, dimension(:), pointer :: local_box_y ! 
   integer:: max_local_tiles, cum_local_tiles

   contains


   !***********************************************************************
   !
   !  function compute_gwd_fields
   !
   !> \brief   Main routine for computing GWDO fields on an MPAS mesh
   !> \author  Michael Duda
   !> \date    29 May 2015
   !> \details 
   !>  This is the main routine for computing GWDO statistics on an MPAS mesh.
   !>  Currently computed fields are:
   !>  var2d
   !>  con
   !>  ol{1,2,3,4}
   !>  oa{1,2,3,4}
   !
   !-----------------------------------------------------------------------
   function compute_gwd_fields(domain) result(iErr)
   
      use mpas_derived_types
      use mpas_kind_types
      use mpas_timer
      use mpas_stream_manager
   
      implicit none
   
      type (domain_type), intent(inout) :: domain
      integer :: iErr

      type (mpas_pool_type), pointer :: mesh, state
      integer :: iCell, i
      real (kind=RKIND) :: dc
      real (kind=RKIND), pointer :: config_gwd_cell_scaling
      integer, pointer :: nCells
      integer, pointer :: nEdges
      integer, dimension(:), pointer :: nEdgesOnCell
      integer, dimension(:,:), pointer :: edgesOnCell
      logical :: onUnitSphere
      real (kind=RKIND), pointer :: sphere_radius
      real (kind=RKIND), dimension(:), pointer :: latCell, lonCell, dcEdge
      real (kind=RKIND), dimension(:), pointer :: var2d, con, oa1, oa2, oa3, oa4, ol1, ol2, ol3, ol4
      real (kind=RKIND), dimension(:), pointer :: elvmax, htheta, hgamma, hsigma
      character(len=StrKIND), pointer :: config_geog_data_path
      character(len=StrKIND), pointer :: config_topo_data
      character(len=StrKIND) :: geog_sub_path
      character(len=StrKIND+1) :: geog_data_path      ! same as config_geog_data_path, but guaranteed to have a trailing slash

      TYPE(mpas_gwd_tile_type), POINTER :: tilesHead

      ! Variables for smoothing variance
      integer, dimension(:,:), pointer:: cellsOnCell
      integer (kind=I1KIND) :: sum_landuse
      real (kind=RKIND) :: sum_var
      integer :: nx, ny
      character(len=StrKIND):: message


      call mpas_pool_get_subpool(domain % blocklist % structs, 'mesh', mesh)
      call mpas_pool_get_subpool(domain % blocklist % structs, 'state', state)

      call mpas_pool_get_config(mesh, 'sphere_radius', sphere_radius)
      call mpas_pool_get_config(domain % configs, 'config_geog_data_path', config_geog_data_path)
      call mpas_pool_get_config(domain % configs, 'config_topo_data', config_topo_data)
      call mpas_pool_get_config(domain % configs, 'config_gwd_cell_scaling', config_gwd_cell_scaling)

      write(geog_data_path, '(a)') config_geog_data_path
      i = len_trim(geog_data_path)
      if (geog_data_path(i:i) /= '/') then
         geog_data_path(i+1:i+1) = '/'
      end if

      select case(trim(config_topo_data))
         case('GTOPO30')
            call mpas_log_write('--- Using GTOPO30 terrain dataset for GWDO static fields')
            geog_sub_path = 'topo_30s/'
            start_lat =  -90.0_RKIND
            start_lon = -180.0_RKIND
         case('GMTED2010')
            call mpas_log_write('--- Using GMTED2010 terrain dataset for GWDO static fields')
            geog_sub_path = 'topo_gmted2010_30s/'

            ! NB: the GMTED2010 data on disk actually has start_lon = 0.0, but the read_global_30s_topo()
            !     routine will shift the dataset when writing to the topo array so that the start_lon seen
            !     by the rest of this code is -180.0.
            start_lat =  -90.0_RKIND
            start_lon = -180.0_RKIND
         case('default')
            call mpas_log_write('*****************************************************************', messageType=MPAS_LOG_ERR)
            call mpas_log_write('Invalid topography dataset '''//trim(config_topo_data) &
                                          //''' selected for config_topo_data',                      messageType=MPAS_LOG_ERR)
            call mpas_log_write('   Possible options are: ''GTOPO30'', ''GMTED2010''',               messageType=MPAS_LOG_ERR)
            call mpas_log_write('*****************************************************************', messageType=MPAS_LOG_ERR)
            call mpas_log_write('Please correct the namelist.', messageType=MPAS_LOG_CRIT)
      end select
      call mpas_log_write('')

      !
      ! Retrieve pointers to arrays holding the latitudes and longitudes of
      ! cells, and arrays that will hold the computed GWDO statistics
      !
      call mpas_pool_get_dimension(mesh, 'nCells', nCells)
      call mpas_pool_get_dimension(mesh, 'nEdges', nEdges)
      call mpas_pool_get_array(mesh, 'latCell', latCell)
      call mpas_pool_get_array(mesh, 'lonCell', lonCell)
      call mpas_pool_get_array(mesh, 'nEdgesOnCell', nEdgesOnCell)
      call mpas_pool_get_array(mesh, 'edgesOnCell', edgesOnCell)
      call mpas_pool_get_array(mesh, 'cellsOnCell', cellsOnCell)
      call mpas_pool_get_array(mesh, 'dcEdge', dcEdge)
      call mpas_pool_get_array(mesh, 'var2d', var2d)
      call mpas_pool_get_array(mesh, 'con', con)
      call mpas_pool_get_array(mesh, 'ol1', ol1)
      call mpas_pool_get_array(mesh, 'ol2', ol2)
      call mpas_pool_get_array(mesh, 'ol3', ol3)
      call mpas_pool_get_array(mesh, 'ol4', ol4)
      call mpas_pool_get_array(mesh, 'oa1', oa1)
      call mpas_pool_get_array(mesh, 'oa2', oa2)
      call mpas_pool_get_array(mesh, 'oa3', oa3)
      call mpas_pool_get_array(mesh, 'oa4', oa4)
!      call mpas_pool_get_array(mesh, 'elvmax', elvmax)
!      call mpas_pool_get_array(mesh, 'theta', htheta)
!      call mpas_pool_get_array(mesh, 'gamma', hgamma)
!      call mpas_pool_get_array(mesh, 'sigma', hsigma)

      nx = topo_x
      ny = topo_y

      allocate(local_box_x(nx*ny))
      allocate(local_box_y(nx*ny))

      allocate(hlanduse(nCells+1))    ! +1, since we access hlanduse(cellsOnCell(i,iCell)) later on for iCell=1,nCells
       

      !
      ! It is possible that this code is called before the mesh fields have been scaled
      ! up to "Earth-sized". Because we need "Earth" distances to cut out bounding
      ! boxes from topography, we try here to detect whether we are on an unscaled
      ! unit sphere or not: if the maximum dcEdge value is less than 1.0, assume this
      ! is the case.
      !
      if (maxval(dcEdge(1:nEdges)) < 1.0_RKIND) then
         call mpas_log_write('Computing GWD statistics on a unit sphere')
         onUnitSphere = .true.
      else
         onUnitSphere = .false.
      end if

      if (config_gwd_cell_scaling /= 1.0) then
         call mpas_log_write('Using effective cell diameters scaled by a factor of $r', realArgs=(/config_gwd_cell_scaling/))
         call mpas_log_write('in the computation of GWD static fields.')
      end if

      tilesHead => null()

      max_local_tiles = (topo_x/tile_x) * (topo_y/tile_y)
      allocate(local_tile_x(max_local_tiles))
      allocate(local_tile_x_land(max_local_tiles))
      allocate(local_tile_y(max_local_tiles))

      topo_shift = topo_x / 2
      !
      ! Main loop to compute each of the GWDO fields for every horizontal
      ! grid cell in the mesh.
      !
      cum_local_tiles=0

      call mpas_log_write(' End of determine_complete_tile_set. We need ($i / $i) tiles ', intArgs=(/cum_local_tiles, max_local_tiles/))
      
      do iCell=1,nCells

         !
         ! First, get an estimate of the mean diameter (in meters) of the grid 
         ! cell by averaging the distances to each of the neighboring cells
         !
         dc = 0.0
         do i=1,nEdgesOnCell(iCell)
            dc = dc + dcEdge(edgesOnCell(i,iCell))
         end do
         dc = dc / real(nEdgesOnCell(iCell),RKIND)
         if (onUnitSphere) then
            dc = dc * sphere_radius
         end if
         dc = dc * config_gwd_cell_scaling

         call get_box_size_from_lat_lon(latCell(iCell), lonCell(iCell), dc, nx, ny)

         !
         ! Cut out a rectangular piece of the global 30-arc-second topography
         ! data that is centered at the lat/lon of the current cell being
         ! processed and that is just large enough to cover the cell. The
         ! rectangular array of topography data is stored in the module
         ! variable 'box', and the dimensions of this array are given by the
         ! module variables 'nx' and 'ny'. The get_box() routine also
         ! computes the mean elevation in the array and stores that value in
         ! the module variable 'box_mean'.
         !
         call get_box_points(latCell(iCell),lonCell(iCell), nx, ny, geog_data_path, geog_sub_path, tilesHead)
         !call get_box(nx, ny, geog_data_path, geog_sub_path)

         !
         ! With a box of 30-arc-second data for the current grid cell, call
         ! subroutines to compute each sub-grid orography statistic
         !
         var2d(iCell) = get_var(nx, ny)
         !con(iCell) = get_con(nx, ny)
         oa1(iCell) = get_oa1(nx, ny)
         oa2(iCell) = get_oa2(nx, ny)
         oa3(iCell) = get_oa3(nx, ny)
         oa4(iCell) = get_oa4(nx, ny)

         ! Critical height, to be used in OL computation
         ! See Appendix of Kim, Y-J, 1996: Representation of Sub-Grid Scale Orographic Effects
         ! in a General Circulation Model. J. Climate, 9, 2698-2717.
         hc = 1116.2_RKIND - 0.878_RKIND * var2d(iCell)

         ol1(iCell) = get_ol1(nx, ny)
         ol2(iCell) = get_ol2(nx, ny)
         ol3(iCell) = get_ol3(nx, ny)
         ol4(iCell) = get_ol4(nx, ny)

         !hlanduse(iCell) = get_dom_landmask(nx, ny)  ! get dominant land mask in cell

         ! elvmax_1 = get_elvmax(nx,ny)
         ! htheta_1 = get_htheta(nx,ny)
         ! hgamma_1 = get_hgamma(nx,ny)
         ! hsigma_1 = get_hsigma(nx,ny)
         
      end do


      ! Smooth variance at isolated points
      do iCell = 1,nCells
         sum_landuse = 0_I1KIND
         sum_var     = 0.0_RKIND
         do i=1,nEdgesOnCell(iCell)
             sum_landuse = sum_landuse + hlanduse(cellsOnCell(i,iCell)) 
             sum_var     = sum_var     + var2d(cellsOnCell(i,iCell))
         end do

         if (sum_landuse == int(nEdgesOnCell(iCell),kind=I1KIND) .and. hlanduse(iCell) == 0_I1KIND) then
             call mpas_log_write('smoothing out a water point, iCell = $i', intArgs=(/iCell/))
             var2d(iCell) = sum_var / real(nEdgesOnCell(iCell),kind=RKIND)
         else if (sum_landuse == 0_I1KIND .and. hlanduse(iCell) == 1_I1KIND) then
             call mpas_log_write('smoothing out a land point, iCell = $i', intArgs=(/iCell/))
             var2d(iCell) = sum_var / real(nEdgesOnCell(iCell),kind=RKIND)
         end if
      end do


      !deallocate(topo)
      !deallocate(landuse)
      deallocate(hlanduse)

      iErr = 0

   end function compute_gwd_fields

   subroutine get_box_size_from_lat_lon(lat, lon, dx, nx, ny)
      
      implicit none

      real (kind=RKIND), intent(in) :: lat
      real (kind=RKIND), intent(in) :: lon
      real (kind=RKIND), intent(in) :: dx

      integer, intent(out) :: nx
      integer, intent(out) :: ny

      !
      ! Get number of points to extract in the zonal direction
      !
      if (cos(lat) > (2.0 * pts_per_degree * dx * 180.0) / (real(topo_x,RKIND) * Pi * Re)) then
         nx = ceiling((180.0 * dx * pts_per_degree) / (Pi * Re * cos(lat)))
      else
         nx = topo_x / 2
      end if

      !
      ! Get number of points to extract in the meridional direction
      !
      ny = ceiling((180.0 * dx * pts_per_degree) / (Pi * Re))


   end subroutine get_box_size_from_lat_lon



   function unique_tile(ix, jx, len) result (is_unique)
      
      implicit none

      integer, intent(in) :: ix
      integer, intent(in) :: jx
      integer, intent(in) :: len

      integer :: i
      logical :: is_unique

      do i = 1, len
         if(local_tile_x(i)==ix .and. local_tile_y(i)==jx) then 
            is_unique = .false.
            return
         end if
      end do

      is_unique = .true.

   end function unique_tile

   function locate_tile_in_set(tile_index_i, tile_index_j, is_topo) result (tile_index)
      
      implicit none

      integer, intent(in) :: tile_index_i
      integer, intent(in) :: tile_index_j
      logical, intent(in) :: is_topo

      integer :: itile, tile_index


      do itile = 1, cum_local_tiles
         if(local_tile_x(itile)==tile_index_i .and. local_tile_y(itile)==tile_index_j) then 
            tile_index = itile
            return
         end if
      end do

      tile_index = -1

   end function locate_tile_in_set


   function locate_tile_in_list(tile_index_i, tile_index_j, is_topo) result (tile_index)
      
      implicit none
      type(mpas_gwd_tile_type), pointer :: tilesHead
      type(mpas_gwd_tile_type), pointer :: thisTile
      integer, intent(in) :: tile_index_i
      integer, intent(in) :: tile_index_j
      logical, intent(in) :: is_topo

      integer :: itile, tile_index

      ! loop over tiles
      thisTile => tilesHead

      do while (associated(thisTile))

         if(thisTile%tile_start_x==tile_index_i .and. thisTile%tile_start_y==tile_index_j) then 
            tile_index = itile
            return
         end if

         thisTile => thisTile % next

      enddo   ! associated(thisTile)

   end function locate_tile_in_list

   function determine_complete_tile_set(nx, ny) result(iErr)

      implicit none
      integer :: iErr
      integer, intent(in) :: nx, ny

      integer :: ix, jx, ii, l,i, iCell, itile, jtile

      ii = cum_local_tiles
      do l=1,nx*ny
         
         if (local_box_x(l) > topo_shift) then
            itile = floor( real(local_box_x(l) - topo_shift - 1) / real(tile_x)) * tile_x + 1
         else
            itile = floor( real(local_box_x(l) + topo_shift - 1) / real(tile_x)) * tile_x + 1
         end if
         jtile = floor( real(local_box_y(l) - 1) / real(tile_y)) * tile_y + 1
         
         if(unique_tile(itile, jtile, ii)) then
            ii = ii + 1
            local_tile_x(ii) = itile
            local_tile_x_land(ii) = floor( real(local_box_x(l) - 1) / real(tile_x)) * tile_x + 1
            local_tile_y(ii) = jtile
            !call mpas_log_write('---- Adding tile to local tile list ($i, land_x:$i, $i)', intArgs=(/local_tile_x(ii),local_tile_x_land(ii), local_tile_y(ii)/))
            !call mpas_log_write('-------- local box_x : $i local_box_x(l) - topo_shift: $i', intArgs=(/local_box_x(l),local_box_x(l) - topo_shift/))
         end if
      end do

      cum_local_tiles = ii

      !
   end function determine_complete_tile_set


   function get_tile_from_box_point(tilesHead, box_x, box_y, nx, ny, path, sub_path) result(thisTile)

      implicit none
      integer :: iErr
      integer, intent(in) :: box_x, box_y, nx, ny
      type(mpas_gwd_tile_type), pointer, intent(in) :: tilesHead
      type(mpas_gwd_tile_type), pointer :: thisTile
      character(len=*), intent(in) :: path
      character(len=*), intent(in) :: sub_path

      integer :: ix, jx, ii, l,i, iCell, tile_start_x, tile_start_y
      
         
      ! if (box_x > topo_shift) then
      !    tile_start_x = floor( real(box_x - topo_shift - 1) / real(tile_x)) * tile_x + 1
      ! else
      !    tile_start_x = floor( real(box_x + topo_shift - 1) / real(tile_x)) * tile_x + 1
      ! end if
      tile_start_x = floor( real(box_x - 1) / real(tile_x)) * tile_x + 1
      tile_start_y = floor( real(box_y - 1) / real(tile_y)) * tile_y + 1


      ! loop over tiles
      thisTile => tilesHead

      do while (associated(thisTile))

         if(thisTile%tile_start_x==tile_start_x .and. thisTile%tile_start_y==tile_start_y) then 
            exit
         end if

         thisTile => thisTile % next

      enddo   ! associated(thisTile)

      if (.not. associated(thisTile)) then 
         thisTile => add_tile(tilesHead, tile_start_x, tile_start_y, path, sub_path)
      end if

      !
   end function get_tile_from_box_point


   function add_tile(tilesHead, tile_start_x, tile_start_y, path, sub_path) result(newTile)

      implicit none
      integer :: iErr
      integer, intent(in) :: tile_start_x, tile_start_y
      type(mpas_gwd_tile_type), pointer :: tilesHead
      type(mpas_gwd_tile_type), pointer :: newTile
      character(len=*), intent(in) :: path
      character(len=*), intent(in) :: sub_path

      allocate(newTile)
      allocate(newTile%array(topo_x*topo_y))
      newTile%tile_start_x = tile_start_x
      newTile%tile_start_y = tile_start_y
      newTile%next => tilesHead

      iErr = read_30s_topo_tile(path, sub_path, newTile%array, newTile%tile_start_x, newTile%tile_start_y)
      tilesHead => newTile

      cum_local_tiles = cum_local_tiles + 1

      call mpas_log_write('In add_tile for tile_start:($i,$i), cum_local_tiles:$i ', intArgs=(/tile_start_x, tile_start_y,cum_local_tiles /))

   end function add_tile

   function read_30s_topo_tile(path, sub_path, topo, tile_start_x, tile_start_y) result(iErr)

      implicit none

      character(len=*), intent(in) :: path
      character(len=*), intent(in) :: sub_path
      REAL(kind=R4KIND), DIMENSION(:), POINTER, intent(in) :: topo
      integer, intent(in) :: tile_start_x
      integer, intent(in) :: tile_start_y
      integer :: iErr

      integer, parameter :: tile_x = 1200       ! x-dimension of each tile of global 30-arc-second topography
      integer, parameter :: tile_y = 1200       ! y-dimension of each tile of global 30-arc-second topography
      integer, parameter :: tile_bdr = 3        ! number of layers of border/halo points surrounding each tile

      integer (c_int) :: istatus
      integer :: ix, iy, ix_shift, il
      integer (c_int) :: isigned, endian, wordsize, nx, ny, nz
      real (c_float) :: scalefactor
      real (c_float), dimension(:,:,:), pointer, contiguous :: tile
      type (c_ptr) :: tile_ptr
      character(len=StrKIND) :: filename
      character(len=StrKIND):: message
      character(kind=c_char), dimension(StrKIND+1) :: c_filename

      allocate(tile(tile_x+2*tile_bdr,tile_y+2*tile_bdr,1))
      tile_ptr = c_loc(tile)

      isigned  = 1
      endian   = 0
      wordsize = 2
      scalefactor = 1.0
      nx = tile_x + 2*tile_bdr
      ny = tile_y + 2*tile_bdr
      nz = 1

      topo_shift = 0

      !allocate(topo(tile_x*tile_y))
      
      !
      ! For GMTED2010 data, the dataset starts at 0.0 longitude, but we need to shift the starting location
      ! in the topo array to -180.0, so we introduce an offset in the x-coordinate of topo_x/2
      !
      if (trim(sub_path) == 'topo_gmted2010_30s/') then
         topo_shift = topo_x / 2
      end if

      write(filename,'(a,i5.5,a1,i5.5,a1,i5.5,a1,i5.5)') trim(path)//trim(sub_path), tile_start_x, '-', (tile_start_x+tile_x-1), '.', &
      tile_start_y, '-', (tile_start_y+tile_y-1)
      call mpas_f_to_c_string(filename, c_filename)
      call read_geogrid(c_filename, tile_ptr, nx, ny, nz, isigned, endian, &
                        wordsize, istatus)
      tile(:,:,:) = tile(:,:,:) * scalefactor
      if (istatus /= 0) then
         call mpas_log_write('Error reading topography tile '//trim(filename), messageType=MPAS_LOG_ERR)
         iErr = 1
         return
      end if
      
      ix_shift = (il-1)*tile_x*tile_y + 1
      !topo(1:(tile_x*tile_y-1)) = reshape(tile((tile_bdr+1):(tile_x+tile_bdr),(tile_bdr+1):(tile_y+tile_bdr),1),(/tile_x*tile_y/))   
      topo = reshape(tile((tile_bdr+1):(tile_x+tile_bdr),(tile_bdr+1):(tile_y+tile_bdr),1),(/tile_x*tile_y/))

      deallocate(tile)

      iErr = 0

   end function read_30s_topo_tile



   !***********************************************************************
   !
   !  function read_global_30s_topo
   !
   !> \brief   Reads global 30-arc-second topography into 'topo' module variable
   !> \author  Michael Duda
   !> \date    28 August 2017
   !> \details 
   !>  This subroutine reads the global 30-arc-second topography from the subdirectory 
   !>  identified by the 'sub_path' argument within the 'path' provided as the first argument.
   !
   !-----------------------------------------------------------------------
   function read_global_30s_topo(path, sub_path) result(iErr)

      implicit none

      character(len=*), intent(in) :: path
      character(len=*), intent(in) :: sub_path

      integer :: iErr

      integer, parameter :: tile_x = 1200       ! x-dimension of each tile of global 30-arc-second topography
      integer, parameter :: tile_y = 1200       ! y-dimension of each tile of global 30-arc-second topography
      integer, parameter :: tile_bdr = 3        ! number of layers of border/halo points surrounding each tile

      integer (c_int) :: istatus
      integer :: ix, iy, ix_shift, il
      integer (c_int) :: isigned, endian, wordsize, nx, ny, nz
      real (c_float) :: scalefactor
      real (c_float), dimension(:,:,:), pointer, contiguous :: tile
      type (c_ptr) :: tile_ptr
      character(len=StrKIND) :: filename
      character(len=StrKIND):: message
      character(kind=c_char), dimension(StrKIND+1) :: c_filename

      allocate(tile(tile_x+2*tile_bdr,tile_y+2*tile_bdr,1))
      tile_ptr = c_loc(tile)

      isigned  = 1
      endian   = 0
      wordsize = 2
      scalefactor = 1.0
      nx = tile_x + 2*tile_bdr
      ny = tile_y + 2*tile_bdr
      nz = 1

      topo_shift = 0

      allocate(topo(tile_x*tile_y*cum_local_tiles))
      
      !
      ! For GMTED2010 data, the dataset starts at 0.0 longitude, but we need to shift the starting location
      ! in the topo array to -180.0, so we introduce an offset in the x-coordinate of topo_x/2
      !
      if (trim(sub_path) == 'topo_gmted2010_30s/') then
         topo_shift = topo_x / 2
      end if

      do il=1,cum_local_tiles
         ix = local_tile_x(il)
         iy = local_tile_y(il)

         write(filename,'(a,i5.5,a1,i5.5,a1,i5.5,a1,i5.5)') trim(path)//trim(sub_path), ix, '-', (ix+tile_x-1), '.', &
                                                                                      iy, '-', (iy+tile_y-1)
         call mpas_f_to_c_string(filename, c_filename)
         call read_geogrid(c_filename, tile_ptr, nx, ny, nz, isigned, endian, &
                           wordsize, istatus)
         tile(:,:,:) = tile(:,:,:) * scalefactor
         if (istatus /= 0) then
            call mpas_log_write('Error reading topography tile '//trim(filename), messageType=MPAS_LOG_ERR)
            iErr = 1
            return
         end if
         
         ix_shift = (il-1)*tile_x*tile_y + 1
         topo(ix_shift:(ix_shift+tile_x*tile_y-1)) = reshape(tile((tile_bdr+1):(tile_x+tile_bdr),(tile_bdr+1):(tile_y+tile_bdr),1),(/tile_x*tile_y/))

      end do

      deallocate(tile)

      iErr = 0

   end function read_global_30s_topo


   !***********************************************************************
   !
   !  function read_global_30s_landuse
   !
   !> \brief   Reads global 30-arc-second landuse into 'landuse' module variable
   !> \author  Michael Duda
   !> \date    14 March 2017
   !> \details 
   !>  This subroutine reads the global 30-arc-second USGS landuse from
   !>   the subdirectory 'landuse_30s' of the path provided as an argument.
   !
   !-----------------------------------------------------------------------
   function read_global_30s_landuse(path) result(iErr)

      implicit none

      character(len=*), intent(in) :: path

      integer :: iErr

      integer, parameter :: tile_x = 1200       ! x-dimension of each tile of global 30-arc-second landuse
      integer, parameter :: tile_y = 1200       ! y-dimension of each tile of global 30-arc-second landuse

      integer (c_int) :: istatus
      integer :: ix, iy, ix_shift, il
      integer (c_int) :: isigned, endian, wordsize, nx, ny, nz
      real (c_float) :: scalefactor
      real (c_float), dimension(:,:,:), pointer, contiguous :: tile
      type (c_ptr) :: tile_ptr
      character(len=StrKIND) :: filename
      character(kind=c_char), dimension(StrKIND+1) :: c_filename
      character(len=StrKIND):: message

      allocate(tile(tile_x,tile_y,1))
      tile_ptr = c_loc(tile)

      isigned  = 1
      endian   = 0
      wordsize = 1
      scalefactor = 1.0
      nx = tile_x
      ny = tile_y
      nz = 1

      allocate(landuse(tile_x*tile_y*cum_local_tiles))

      do il=1,cum_local_tiles
         ix = local_tile_x_land(il)
         iy = local_tile_y(il)

         write(filename,'(a,i5.5,a1,i5.5,a1,i5.5,a1,i5.5)') trim(path)//'/landuse_30s/', ix, '-', (ix+tile_x-1), '.', &
                                                                                      iy, '-', (iy+tile_y-1)
         call mpas_f_to_c_string(filename, c_filename)
         call read_geogrid(c_filename, tile_ptr, nx, ny, nz, isigned, endian, &
                           wordsize, istatus)
         tile(:,:,:) = tile(:,:,:) * scalefactor
         if (istatus /= 0) then
            call mpas_log_write('Error reading landuse tile '//trim(filename))
            iErr = 1
            return
         end if
         
         ix_shift = (il-1)*tile_x*tile_y + 1
         landuse(ix_shift:(ix_shift+tile_x*tile_y-1)) = reshape(int(tile(1:tile_x,1:tile_y,1), kind=I1KIND),(/tile_x*tile_y/))
         
      end do

      deallocate(tile)

      iErr = 0

   end function read_global_30s_landuse


   !***********************************************************************
   !
   !  function get_dom_landmask
   !
   !> \brief   Returns the dominant land mask in a cell
   !> \author  May Wong
   !> \date    
   !> \details 1 = land, 0 = water
   !
   !-----------------------------------------------------------------------
   integer (kind=I1KIND) function get_dom_landmask(nx, ny)

      implicit none
      integer, intent(in) :: nx, ny

      integer :: i, j      
      real (kind=RKIND) :: xland
      xland = 0.0_RKIND

      ! Get dominant land/water mask in the box
      do j=1,ny
      do i=1,nx
         if (box_landuse(i,j) /= WATER) then
            xland = xland + 1.0_RKIND
         end if
      end do
      end do
      xland = xland / real(nx*ny,kind=RKIND)

      if (xland >= 0.5_RKIND) then
         get_dom_landmask = 1_I1KIND
      else
         get_dom_landmask = 0_I1KIND
      end if 

   end function get_dom_landmask 



   subroutine get_box_points(lat, lon, nx, ny, path, sub_path, tilesHead)

      implicit none
      character(len=*), intent(in) :: path
      character(len=*), intent(in) :: sub_path
      real (kind=RKIND), intent(in) :: lat, lon

      integer, intent(in) :: nx, ny
      TYPE(mpas_gwd_tile_type), POINTER, intent(in) :: tilesHead
      TYPE(mpas_gwd_tile_type), POINTER :: thisTile

      integer :: i, j, ii, jj, ic, jc, l, tile_offset
      real (kind=RKIND) :: sg_lat


      if (associated(box)) deallocate(box)
      allocate(box(nx,ny))
      
      if (associated(box_landuse)) deallocate(box_landuse)
      allocate(box_landuse(nx,ny))

      if (associated(dxm)) deallocate(dxm)
      allocate(dxm(nx,ny))
      !
      !
      ! Find coordinates in global topography array of the box center
      !
      ic = nint((lon*rad2deg - start_lon) * pts_per_degree) + 1
      jc = nint((lat*rad2deg - start_lat) * pts_per_degree) + 1

      if (ic <= 0) ic = ic + topo_x
      if (ic > topo_x) ic = ic - topo_x

      !
      ! Extract sub-array (box) from global array; must properly account for 
      ! the periodicity in the longitude coordinate, as well as the poles
      !
      box_mean = 0.0
      l = 0
      do j=1,ny
      do i=1,nx

         ii = i - nx/2 + ic
         jj = j - ny/2 + jc

         if (jj <= 0) then
            jj = -jj + 1
            ii = ii + topo_y
         end if
         if (jj > topo_y) then
            jj = topo_y - (jj - topo_y - 1)
            ii = ii + topo_y
         end if
         do while (ii <= 0)
            ii = ii + topo_x
         end do
         do while (ii > topo_x)
            ii = ii - topo_x
         end do
         l = l + 1
      

         if (ii - topo_shift > 0 ) then 
            ii = ii - topo_shift
         else
            ii = ii + topo_shift
         end if

         thisTile => get_tile_from_box_point(tilesHead, ii, jj, nx, ny, path, sub_path)

         ! ibox = (j-1)*nx + i        

         

         ! tile_index_i = floor( real(ix - 1) / real(tile_x)) * tile_x + 1
         ! tile_index_j = floor( real(jx - 1) / real(tile_y)) * tile_y + 1

         ! itile = locate_tile_in_set(tile_index_i, tile_index_j, .true.)

         !call mpas_log_write('In box_new for (i,j):($i,$i), (ix,jx):($i,$i), tile_index_i,tile_index_j:($i,$i), itile:$i ', intArgs=(/i,j,ix,jx,tile_index_i,tile_index_j,itile/))
         
         ! tile_base = (itile-1)*tile_x*tile_y + 1
         tile_offset = (ii - thisTile%tile_start_x) + (jj - thisTile%tile_start_y) * tile_x + 1
         if (tile_offset < 0) then 
             call mpas_log_write('In box_new for (i,j):($i,$i), tile_offset:$i tile_start_end $i, $i', intArgs=(/ii,jj,tile_offset, thisTile%tile_start_x, thisTile%tile_start_y/))
             call mpas_log_write('In box_new for (i,j):($i,$i), lat:$r, lon:$r', intArgs=(/ii,jj/), realArgs=(/lat*rad2deg, lon*rad2deg /))
         end if
         box(i,j) = thisTile%array(tile_offset)
         !box_landuse(i,j) = landuse(tile_offset)
         sg_lat = (start_lat + (real(jj-1,RKIND) + 0.5) / pts_per_degree) / rad2deg  ! Add 0.5 for cell center
         dxm(i,j) = sg_delta * cos(sg_lat)
         box_mean = box_mean + box(i,j)
 
      end do
      end do

      box_mean = box_mean / real(nx*ny, RKIND)

   end subroutine get_box_points



   !***********************************************************************
   !
   !  subroutine get_box
   !
   !> \brief   Cuts out a rectangular box of data centered at a given (lat,lon)
   !> \author  Michael Duda
   !> \date    29 May 2015
   !> \details 
   !>  This subroutine extracts a rectangular sub-array of the 30-arc-second
   !>  global topography dataset, stored in the module variable 'topo'; the
   !>  sub-array will be centered at the (lat,lon) specified in the argument
   !>  list, and will have a width and height large enough to span 'dx' meters.
   !>  The extracted sub-array is stored in the module variable 'box', and the
   !>  dimensions of this sub-array are stored in the module variables 'nx' and
   !>  'ny'.
   !>  Since the mean value of the terrain in a grid cell is needed by many of
   !>  the GWDO statistics computations, this mean value is also computed by
   !>  this subroutine and stored in the module variable 'box_mean'.
   !
   !-----------------------------------------------------------------------
   subroutine get_box(nx, ny)

      implicit none

      integer, intent(in) :: nx, ny
      integer :: itile, ix_shift, ix, ix_land, ix_topo, jx, ibox, tile_index_i, tile_index_j
      integer :: tile_base, tile_offset, i, j
      real (kind=RKIND) :: sg_lat
      character(len=StrKIND):: message
      

      if (associated(box)) deallocate(box)
      allocate(box(nx,ny))
      
      if (associated(box_landuse)) deallocate(box_landuse)
      allocate(box_landuse(nx,ny))

      if (associated(dxm)) deallocate(dxm)
      allocate(dxm(nx,ny))

      !
      ! Extract sub-array (box) from global array; must properly account for 
      ! the periodicity in the longitude coordinate, as well as the poles
      !
      box_mean = 0.0
      ibox = 1
      do j=1,ny
         do i=1,nx

            ibox = (j-1)*nx + i
            ! pixels on the global topo grid 
            ix = local_box_x(ibox) !+ topo_shift
            ix_topo = local_box_x(ibox)
            jx = local_box_y(ibox)

            if (ix - topo_shift > 0 ) then 
               ix = ix - topo_shift
            else
               ix = ix + topo_shift
            end if

            tile_index_i = floor( real(ix - 1) / real(tile_x)) * tile_x + 1
            tile_index_j = floor( real(jx - 1) / real(tile_y)) * tile_y + 1

            itile = locate_tile_in_set(tile_index_i, tile_index_j, .true.)

            !call mpas_log_write('In box_new for (i,j):($i,$i), (ix,jx):($i,$i), tile_index_i,tile_index_j:($i,$i), itile:$i ', intArgs=(/i,j,ix,jx,tile_index_i,tile_index_j,itile/))

            tile_base = (itile-1)*tile_x*tile_y + 1
            tile_offset = (ix - local_tile_x(itile)) + (jx - local_tile_y(itile)) * tile_x
            
            box(i,j) = topo(tile_base + tile_offset)
            !box_landuse(i,j) = landuse(tile_base + tile_offset)
            sg_lat = (start_lat + (real(jx-1,RKIND) + 0.5) / pts_per_degree) / rad2deg  ! Add 0.5 for cell center
            dxm(i,j) = sg_delta * cos(sg_lat)
            box_mean = box_mean + box(i,j)

            ibox = ibox + 1 
         end do
      end do


      !
      ! Compute mean topography in the extracted box
      !
      box_mean = box_mean / real(nx*ny, RKIND)

   end subroutine get_box


   !***********************************************************************
   !
   !  function get_var
   !
   !> \brief   Computes standard deviation of sub-grid-scale terrain
   !> \author  Michael Duda
   !> \date    29 May 2015
   !> \details 
   !
   !-----------------------------------------------------------------------
   real (kind=RKIND) function get_var(nx, ny)

      implicit none

      integer :: i, j
      integer, intent(in) :: nx, ny
      real (kind=RKIND) :: s2

      s2 = 0.0

      do j=1,ny
         do i=1,nx
            s2 = s2 + (box(i,j) - box_mean)**2
         end do
      end do

      get_var = sqrt(s2 / real(nx*ny,RKIND))

   end function get_var


   !***********************************************************************
   !
   !  function get_con
   !
   !> \brief   Computes orographic convexity of sub-grid-scale terrain
   !> \author  Michael Duda
   !> \date    29 May 2015
   !> \details 
   !
   !-----------------------------------------------------------------------
   real (kind=RKIND) function get_con(nx, ny)

      implicit none

      integer :: i, j
      integer, intent(in) :: nx, ny
      real (kind=RKIND) :: s2, s4, var, xland, mean_land, mean_water, oro

      s2 = 0.0
      s4 = 0.0
      mean_land = 0.0
      mean_water = 0.0
      xland = 0.0

      !
      ! Compute grid-box mean
      !
      do j=1,ny
         do i=1,nx
            if (box_landuse(i,j) /= WATER) then
               xland = xland + 1.0
               mean_land = mean_land + box(i,j)
            else
               mean_water = mean_water + box(i,j)
            end if
         end do
      end do
      if (xland > 0.0) then
         mean_land = mean_land / xland
      end if
      if (xland < real(nx*ny,kind=RKIND)) then
         mean_water = mean_water / (real(nx*ny,kind=RKIND) - xland)
      end if
      xland = xland / real(nx*ny,kind=RKIND)
 
      if (xland >= 0.5_RKIND) then
         oro = mean_land
      else
         oro = mean_water
      end if

      do j=1,ny
         do i=1,nx
            s2 = s2 + (box(i,j) - box_mean)**2
            s4 = s4 + (box(i,j) - oro)**4
         end do
      end do

      var = s2 / real(nx*ny,RKIND)

      if (sqrt(var) < 1.0) then
         get_con = 0.0
      else
         get_con = s4 / (var**2 * real(nx*ny,RKIND))
      end if

      !
      ! Zero-ing all convexity statistics over dominantly water points.
      !
      if (xland < 0.5_RKIND) then
         get_con = 0.0
      end if

   end function get_con


   !***********************************************************************
   !
   !  function get_oa1
   !
   !> \brief   Computes orographic asymmetry in the West direction
   !> \author  Michael Duda
   !> \date    29 May 2015
   !> \details 
   !>  This function computes the sub-grid orographic asymmetry following 
   !>  the comment from N. Wood in the footnote of Kim and Doyle (QRJMS, 2005).
   !
   !-----------------------------------------------------------------------
   real (kind=RKIND) function get_oa1(nx, ny)

      implicit none

      integer :: i, j
      integer, intent(in) :: nx, ny
      integer :: nu, nd

      nu = 0
      nd = 0
      do j=1,ny
         do i=1,nx/2
            if (box(i,j) > box_mean) nu = nu + 1
         end do
         do i=nx/2+1,nx
            if (box(i,j) > box_mean) nd = nd + 1
         end do
      end do

      if (nu + nd > 0) then
         get_oa1 = real((nu - nd),RKIND) / real((nu + nd),RKIND)
      else
         get_oa1 = 0.0
      end if

   end function get_oa1


   !***********************************************************************
   !
   !  function get_oa2
   !
   !> \brief   Computes orographic asymmetry in the South direction
   !> \author  Michael Duda
   !> \date    29 May 2015
   !> \details 
   !>  This function computes the sub-grid orographic asymmetry following 
   !>  the comment from N. Wood in the footnote of Kim and Doyle (QRJMS, 2005).
   !
   !-----------------------------------------------------------------------
   real (kind=RKIND) function get_oa2(nx, ny)

      implicit none
      integer, intent(in) :: nx, ny

      integer :: i, j
      integer :: nu, nd

      nu = 0
      nd = 0
      do j=1,ny/2
         do i=1,nx
            if (box(i,j) > box_mean) nu = nu + 1
         end do
      end do
      do j=ny/2+1,ny
         do i=1,nx
            if (box(i,j) > box_mean) nd = nd + 1
         end do
      end do

      if (nu + nd > 0) then
         get_oa2 = real((nu - nd),RKIND) / real((nu + nd),RKIND)
      else
         get_oa2 = 0.0
      end if

   end function get_oa2


   !***********************************************************************
   !
   !  function get_oa3
   !
   !> \brief   Computes orographic asymmetry in the South-West direction
   !> \author  Michael Duda
   !> \date    29 May 2015
   !> \details 
   !>  This function computes the sub-grid orographic asymmetry following 
   !>  the comment from N. Wood in the footnote of Kim and Doyle (QRJMS, 2005).
   !
   !-----------------------------------------------------------------------
   real (kind=RKIND) function get_oa3(nx, ny)

      implicit none
      integer, intent(in) :: nx, ny

      integer :: i, j
      integer :: nu, nd
      real (kind=RKIND) :: ratio

      nu = 0
      nd = 0
      ratio = real(ny,RKIND)/real(nx,RKIND)
      do j=1,ny
         do i=1,nx
            if (nint(real(i,RKIND) * ratio) < (ny - j)) then
               if (box(i,j) > box_mean) nu = nu + 1
            else
               if (box(i,j) > box_mean) nd = nd + 1
            end if
         end do
      end do

      if (nu + nd > 0) then
         get_oa3 = real((nu - nd),RKIND) / real((nu + nd),RKIND)
      else
         get_oa3 = 0.0
      end if

   end function get_oa3


   !***********************************************************************
   !
   !  function get_oa4
   !
   !> \brief   Computes orographic asymmetry in the North-West direction
   !> \author  Michael Duda
   !> \date    29 May 2015
   !> \details 
   !>  This function computes the sub-grid orographic asymmetry following 
   !>  the comment from N. Wood in the footnote of Kim and Doyle (QRJMS, 2005).
   !
   !-----------------------------------------------------------------------
   real (kind=RKIND) function get_oa4(nx, ny)

      implicit none
      integer, intent(in) :: nx, ny

      integer :: i, j
      integer :: nu, nd
      real (kind=RKIND) :: ratio

      nu = 0
      nd = 0
      ratio = real(ny,RKIND)/real(nx,RKIND)
      do j=1,ny
         do i=1,nx
            if (nint(real(i,RKIND) * ratio) < j) then
               if (box(i,j) > box_mean) nu = nu + 1
            else
               if (box(i,j) > box_mean) nd = nd + 1
            end if
         end do
      end do

      if (nu + nd > 0) then
         get_oa4 = real((nu - nd),RKIND) / real((nu + nd),RKIND)
      else
         get_oa4 = 0.0
      end if

   end function get_oa4


   !***********************************************************************
   !
   !  function get_ol1
   !
   !> \brief   Computes orographic effective length for Westerly flow
   !> \author  Michael Duda
   !> \date    29 May 2015
   !> \details 
   !
   !-----------------------------------------------------------------------
   real (kind=RKIND) function get_ol1(nx, ny)

      implicit none
      integer, intent(in) :: nx, ny

      integer :: i, j
      integer :: nw
      integer :: nt

      nw = 0
      nt = 0

      do j=ny/4,3*ny/4
         do i=1,nx
            if (box(i,j) > hc) nw = nw + 1
            nt = nt + 1
         end do
      end do

      get_ol1 = real(nw,RKIND) / real(nt,RKIND)

   end function get_ol1


   !***********************************************************************
   !
   !  function get_ol2
   !
   !> \brief   Computes orographic effective length for Southerly flow
   !> \author  Michael Duda
   !> \date    29 May 2015
   !> \details 
   !
   !-----------------------------------------------------------------------
   real (kind=RKIND) function get_ol2(nx, ny)

      implicit none
      integer, intent(in) :: nx, ny

      integer :: i, j
      integer :: nw
      integer :: nt

      nw = 0
      nt = 0

      do j=1,ny
         do i=nx/4,3*nx/4
            if (box(i,j) > hc) nw = nw + 1
            nt = nt + 1
         end do
      end do

      get_ol2 = real(nw,RKIND) / real(nt,RKIND)

   end function get_ol2


   !***********************************************************************
   !
   !  function get_ol3
   !
   !> \brief   Computes orographic effective length for South-Westerly flow
   !> \author  Michael Duda
   !> \date    29 May 2015
   !> \details 
   !
   !-----------------------------------------------------------------------
   real (kind=RKIND) function get_ol3(nx, ny)

      implicit none
      integer, intent(in) :: nx, ny

      integer :: i, j
      integer :: nw
      integer :: nt

      nw = 0
      nt = 0

      do j=1,ny/2
         do i=1,nx/2
            if (box(i,j) > hc) nw = nw + 1
            nt = nt + 1
         end do
      end do
      do j=ny/2+1,ny
         do i=nx/2+1,nx
            if (box(i,j) > hc) nw = nw + 1
            nt = nt + 1
         end do
      end do

      get_ol3 = real(nw,RKIND) / real(nt,RKIND)

   end function get_ol3


   !***********************************************************************
   !
   !  function get_ol4
   !
   !> \brief   Computes orographic effective length for North-Westerly flow
   !> \author  Michael Duda
   !> \date    29 May 2015
   !> \details 
   !
   !-----------------------------------------------------------------------
   real (kind=RKIND) function get_ol4(nx, ny)

      implicit none
      integer, intent(in) :: nx, ny

      integer :: i, j
      integer :: nw
      integer :: nt

      nw = 0
      nt = 0

      do j=ny/2+1,ny
         do i=1,nx/2
            if (box(i,j) > hc) nw = nw + 1
            nt = nt + 1
         end do
      end do
      do j=1,ny/2
         do i=nx/2+1,nx
            if (box(i,j) > hc) nw = nw + 1
            nt = nt + 1
         end do
      end do

      get_ol4 = real(nw,RKIND) / real(nt,RKIND)

   end function get_ol4


   !***********************************************************************
   !
   !  function get_elvmax
   !
   !> \brief   Computes maximum subgrid orography height
   !> \author  Michael Duda
   !> \date    20 December 2015
   !> \details 
   !
   !-----------------------------------------------------------------------
   real (kind=RKIND) function get_elvmax(nx, ny)

      implicit none
      integer, intent(in) :: nx, ny

      integer :: i, j

      get_elvmax = box(1,1)

      do j=1,ny
         do i=1,nx
            if (box(i,j) > get_elvmax) then
               get_elvmax = box(i,j)
            end if
         end do
      end do

   end function get_elvmax


   !***********************************************************************
   !
   !  function get_htheta
   !
   !> \brief   Computes angle of principle axis of the gradient correlation tensor
   !> \author  Michael Duda
   !> \date    20 December 2015
   !> \details Computation following Lott and Miller (QJRMS 1997)
   !
   !-----------------------------------------------------------------------
   real (kind=RKIND) function get_htheta(nx, ny)

      implicit none
      integer, intent(in) :: nx, ny

      integer :: i, j
      real (kind=RKIND) :: dx, dy
      real (kind=RKIND) :: xfp, yfp
      real (kind=RKIND) :: hx2, hy2, hxy
      real (kind=RKIND) :: hk, hl

      hx2 = 0.0
      hy2 = 0.0
      hxy = 0.0

      do j=2,ny-1
      do i=2,nx-1
         dx = dxm(i,j)
         dy = sg_delta
         xfp = (box(i+1,j) - box(i-1,j)) / (2.0 * dx)
         yfp = (box(i,j+1) - box(i,j-1)) / (2.0 * dy)
         hx2 = hx2 + xfp * xfp
         hy2 = hy2 + yfp * yfp
         hxy = hxy + xfp * yfp
      end do
      end do

      hx2 = hx2 / real((nx-2)*(ny-2),RKIND)
      hy2 = hy2 / real((nx-2)*(ny-2),RKIND)
      hxy = hxy / real((nx-2)*(ny-2),RKIND)

      hk = 0.5 * (hx2 + hy2)
      hl = 0.5 * (hx2 - hy2)

      get_htheta = 0.5 * atan2(hxy, hl)

   end function get_htheta


   !***********************************************************************
   !
   !  function get_hgamma
   !
   !> \brief   Computes anisotropy of subgrid orography
   !> \author  Michael Duda
   !> \date    20 December 2015
   !> \details Computation following Lott and Miller (QJRMS 1997)
   !
   !-----------------------------------------------------------------------
   real (kind=RKIND) function get_hgamma(nx, ny)

      implicit none
      integer, intent(in) :: nx, ny

      integer :: i, j
      real (kind=RKIND) :: dx, dy
      real (kind=RKIND) :: xfp, yfp
      real (kind=RKIND) :: hx2, hy2, hxy
      real (kind=RKIND) :: hk, hl, hlp

      hx2 = 0.0
      hy2 = 0.0
      hxy = 0.0

      do j=2,ny-1
      do i=2,nx-1
         dx = dxm(i,j)
         dy = sg_delta
         xfp = (box(i+1,j) - box(i-1,j)) / (2.0 * dx)
         yfp = (box(i,j+1) - box(i,j-1)) / (2.0 * dy)
         hx2 = hx2 + xfp * xfp
         hy2 = hy2 + yfp * yfp
         hxy = hxy + xfp * yfp
      end do
      end do

      hx2 = hx2 / real((nx-2)*(ny-2),RKIND)
      hy2 = hy2 / real((nx-2)*(ny-2),RKIND)
      hxy = hxy / real((nx-2)*(ny-2),RKIND)

      hk = 0.5 * (hx2 + hy2)
      hl = 0.5 * (hx2 - hy2)
      hlp = sqrt(hl*hl + hxy*hxy)

      if ((hk + hlp) > 0.0 .and. (hk - hlp) >= 0.0) then
         get_hgamma = sqrt((hk - hlp) / (hk + hlp))
      else
         get_hgamma = 0.0
      end if

   end function get_hgamma


   !***********************************************************************
   !
   !  function get_hsigma
   !
   !> \brief   Computes mean slope of subgrid orography
   !> \author  Michael Duda
   !> \date    20 December 2015
   !> \details Computation following Lott and Miller (QJRMS 1997)
   !
   !-----------------------------------------------------------------------
   real (kind=RKIND) function get_hsigma(nx, ny)

      implicit none
      integer, intent(in) :: nx, ny

      integer :: i, j
      real (kind=RKIND) :: dx, dy
      real (kind=RKIND) :: xfp, yfp
      real (kind=RKIND) :: hx2, hy2, hxy
      real (kind=RKIND) :: hk, hl, hlp

      hx2 = 0.0
      hy2 = 0.0
      hxy = 0.0

      do j=2,ny-1
      do i=2,nx-1
         dx = dxm(i,j)
         dy = sg_delta
         xfp = (box(i+1,j) - box(i-1,j)) / (2.0 * dx)
         yfp = (box(i,j+1) - box(i,j-1)) / (2.0 * dy)
         hx2 = hx2 + xfp * xfp
         hy2 = hy2 + yfp * yfp
         hxy = hxy + xfp * yfp
      end do
      end do

      hx2 = hx2 / real((nx-2)*(ny-2),RKIND)
      hy2 = hy2 / real((nx-2)*(ny-2),RKIND)
      hxy = hxy / real((nx-2)*(ny-2),RKIND)

      hk = 0.5 * (hx2 + hy2)
      hl = 0.5 * (hx2 - hy2)
      hlp = sqrt(hl*hl + hxy*hxy)

      get_hsigma = sqrt(hk + hlp)

   end function get_hsigma

end module mpas_init_atm_gwd
