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

   integer, parameter :: I1KIND = selected_int_kind(2)

   ! A derived type to hold contents of a tile (both topo and landuse)
   type :: mpas_gwd_tile_type

      real (kind=R4KIND), dimension(:,:), pointer :: topo_array => null()
      integer (kind=I1KIND), dimension(:,:), pointer :: landuse_array => null()
      ! coordinates of the tile to be read. 
      ! NB: tile_start_x can be used as is to read landuse tiles, but need an 
      ! adjustment to account for the shifting of topo array start_lon to -180.0.
      integer :: tile_start_x = -1, tile_start_y = -1
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

   real (kind=RKIND), parameter :: Re = 6371229.0_RKIND        ! Earth radius in MPAS-Atmosphere
   real (kind=RKIND), parameter :: Pi = 2.0_RKIND * asin(1.0_RKIND)
   real (kind=RKIND), parameter :: rad2deg = 180.0_RKIND / Pi

   integer, parameter :: topo_x = 43200            ! x-dimension of global 30-arc-second topography array
   integer, parameter :: topo_y = 21600            ! y-dimension of global 30-arc-second topography array
   integer, parameter :: tile_x = 1200             ! x-dimension of each tile of global 30-arc-second topography
   integer, parameter :: tile_y = 1200             ! y-dimension of each tile of global 30-arc-second topography

   real (kind=RKIND), parameter :: pts_per_degree = real(topo_x,RKIND) / 360.0_RKIND

   ! The following are set at the beginning of the compute_gwd_fields routine depending
   ! on the source of topography data to be used
   real (kind=RKIND) :: start_lat
   real (kind=RKIND) :: start_lon

   ! To introduce an offset in the x-coordinate in the case of GMTED2010 topo data, as the dataset starts at 0.0 longitude
   integer :: topo_shift = 0

   ! Nominal delta-x (in meters) for sub-grid topography cells
   real (kind=RKIND), parameter ::  sg_delta = 2.0 * Pi * Re / (360.0_RKIND * real(pts_per_degree,RKIND))

   ! NB: At present, only the USGS GLCC land cover dataset is supported, so we can assume 16 == water 
   !     See the read_global_30s_landuse function 
   integer (kind=I1KIND), parameter :: WATER = 16

   integer (kind=I1KIND), dimension(:), pointer :: hlanduse ! Dominant land mask (0 or 1)
   real (kind=RKIND) :: hc   ! critical height

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

      type(mpas_gwd_tile_type), pointer :: tilesHead  ! Pointer to linked list of tiles

      ! Variables for smoothing variance
      integer, dimension(:,:), pointer:: cellsOnCell
      integer (kind=I1KIND) :: sum_landuse
      real (kind=RKIND) :: sum_var

      real (kind=RKIND), dimension(:,:), pointer :: box ! Subset of topography covering a grid cell
      real (kind=RKIND), dimension(:,:), pointer :: dxm ! Size (meters) in zonal direction of a grid cell
      real (kind=RKIND) :: box_mean                     ! Mean value of topography in box
      integer (kind=I1KIND), dimension(:,:), pointer :: box_landuse ! Subset of landuse covering a grid cell
      integer :: nx, ny

      box => null()
      dxm => null()
      box_landuse => null()
      tilesHead => null()

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
            ! so we introduce an offset in the x-coordinate of topo_x/2
            topo_shift = topo_x / 2
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
      ! call mpas_pool_get_array(mesh, 'elvmax', elvmax)
      ! call mpas_pool_get_array(mesh, 'theta', htheta)
      ! call mpas_pool_get_array(mesh, 'gamma', hgamma)
      ! call mpas_pool_get_array(mesh, 'sigma', hsigma)

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

      !
      ! Main loop to compute each of the GWDO fields for every horizontal
      ! grid cell in the mesh.
      !
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

         !
         ! Cut out a rectangular piece of the global 30-arc-second topography
         ! data that is centered at the lat/lon (in radians) of the current cell being
         ! processed and that is just large enough to cover the cell. The
         ! rectangular array of topography data is stored in the local
         ! variable 'box', and the dimensions of this array are obtained from 
         ! the routine get_box_size_from_lat() and stored in the
         ! local variables 'nx' and 'ny'. The get_box() routine also
         ! computes the mean elevation in the array and stores that value in
         ! the local variable 'box_mean'. 'tilesHead' points to the head of the linked
         ! list of tiles, which is used by get_box() and its internal subroutines to search
         ! for tile data and add new tiles to the head of this list as necessary. 
         !
         call get_box_size_from_lat(latCell(iCell), dc, nx, ny)
 
         call get_box(latCell(iCell)*rad2deg,lonCell(iCell)*rad2deg, nx, ny, &
                      geog_data_path, geog_sub_path, tilesHead, box, box_landuse, dxm, box_mean)

         !
         ! With a box of 30-arc-second data for the current grid cell, call
         ! subroutines to compute each sub-grid orography statistic
         !
         var2d(iCell) = get_var(box, box_mean, nx, ny)
         con(iCell) = get_con(box, box_landuse, box_mean, nx, ny)
         oa1(iCell) = get_oa1(box, box_mean, nx, ny)
         oa2(iCell) = get_oa2(box, box_mean, nx, ny)
         oa3(iCell) = get_oa3(box, box_mean, nx, ny)
         oa4(iCell) = get_oa4(box, box_mean, nx, ny)

         ! Critical height, to be used in OL computation
         ! See Appendix of Kim, Y-J, 1996: Representation of Sub-Grid Scale Orographic Effects
         ! in a General Circulation Model. J. Climate, 9, 2698-2717.
         hc = 1116.2_RKIND - 0.878_RKIND * var2d(iCell)

         ol1(iCell) = get_ol1(box, nx, ny)
         ol2(iCell) = get_ol2(box, nx, ny)
         ol3(iCell) = get_ol3(box, nx, ny)
         ol4(iCell) = get_ol4(box, nx, ny)

         hlanduse(iCell) = get_dom_landmask(box_landuse, nx, ny)  ! get dominant land mask in cell

         ! elvmax(iCell) = get_elvmax(box, nx, ny)
         ! htheta(iCell) = get_htheta(box, dxm, nx, ny)
         ! hgamma(iCell) = get_hgamma(box, dxm, nx, ny)
         ! hsigma(iCell) = get_hsigma(box, dxm, nx, ny)
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

      if (associated(box)) deallocate(box)
      if (associated(box_landuse)) deallocate(box_landuse)
      if (associated(dxm)) deallocate(dxm)
      deallocate(hlanduse)

      iErr = free_tile_list(tilesHead)

   end function compute_gwd_fields


   !***********************************************************************
   !
   !  subroutine get_box_size_from_lat
   !
   !> \brief   Routine to obtain box size given the mean diameter (meters), lat, lon (radians)
   !> \author  Abishek Gopal
   !> \date    05 Sep 2024
   !> \details 
   !>  Routine to obtain box size (nx, ny) given the mean diameter of the grid cell (meters),
   !     and the latitude (radians)
   !
   !-----------------------------------------------------------------------
   subroutine get_box_size_from_lat(lat, dx, nx, ny)
      
      implicit none

      real (kind=RKIND), intent(in) :: lat
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

   end subroutine get_box_size_from_lat


   !***********************************************************************
   !
   !  function get_tile_from_box_point
   !
   !> \brief   Routine to obtain a tile given a box pixel
   !> \author  Abishek Gopal
   !> \date    05 Sep 2024
   !> \details 
   !>  Routine to obtain a tile of type mpas_gwd_tile_type, given the linked 
   !    list of tiles tilesHead, box coordinates box_x, box_y, and path to 
   !    static dataset. The function first searches the linked list to locate
   !    the tile, and if search fails, adds the target tile to the linked list
   !    after reading in the data for the tile from disk 
   !-----------------------------------------------------------------------
   function get_tile_from_box_point(tilesHead, box_x, box_y, path, sub_path, last_tile) result(thisTile)

      implicit none

      type(mpas_gwd_tile_type), pointer, intent(inout) :: tilesHead
      integer, intent(in) :: box_x, box_y
      character(len=*), intent(in) :: path
      character(len=*), intent(in) :: sub_path
      type(mpas_gwd_tile_type), pointer, intent(in) :: last_tile

      type(mpas_gwd_tile_type), pointer :: thisTile

      integer :: tile_start_x, tile_start_y, tile_start_x_topo
      
      ! Need special handling for the x-coordinates of topo tiles, due to the shift by topo_shift
      ! in certain datasets. We use tile_start_x, tile_start_y to search for tiles and open landmask tiles,
      ! whereas tile_start_x_topo is only required to open the correct topo tiles from disk
      if (box_x > topo_shift) then
         tile_start_x_topo = floor( real(box_x - topo_shift - 1) / real(tile_x)) * tile_x + 1
      else
         tile_start_x_topo = floor( real(box_x + topo_shift - 1) / real(tile_x)) * tile_x + 1
      end if
      tile_start_x = floor( real(box_x - 1) / real(tile_x)) * tile_x + 1
      tile_start_y = floor( real(box_y - 1) / real(tile_y)) * tile_y + 1
      
      ! First check if the last tile contains the requested pixel
      if (associated(last_tile)) then
         if (last_tile%tile_start_x==tile_start_x .and. last_tile%tile_start_y==tile_start_y) then
            thisTile => last_tile
            return
         end if
      end if

      thisTile => tilesHead
      ! Loop over all tiles in the list
      do while (associated(thisTile))

         if (thisTile%tile_start_x==tile_start_x .and. thisTile%tile_start_y==tile_start_y) then 
            exit
         end if

         thisTile => thisTile % next

      end do   ! associated(thisTile)

      ! Could not find such a tile, so we add the tile to the front of the linked list
      if (.not. associated(thisTile)) then 
         thisTile => add_tile(tilesHead, tile_start_x, tile_start_y, tile_start_x_topo, path, sub_path)
      end if

   end function get_tile_from_box_point


   !***********************************************************************
   !
   !  function add_tile
   !
   !> \brief   Routine to read in a new topo and landuse tile, and add 
   !>           these tiles to the head of the linked list tilesHead
   !> \author  Abishek Gopal
   !> \date    05 Sep 2024
   !> \details 
   !>  Routine to read in a new topo and landuse tile, given the tile 
   !    coordinates
   !-----------------------------------------------------------------------
   function add_tile(tilesHead, tile_start_x, tile_start_y, tile_start_x_topo, path, sub_path) result(newTile)

      implicit none

      type(mpas_gwd_tile_type), pointer, intent(inout) :: tilesHead
      integer, intent(in) :: tile_start_x, tile_start_y, tile_start_x_topo
      character(len=*), intent(in) :: path
      character(len=*), intent(in) :: sub_path

      type(mpas_gwd_tile_type), pointer :: newTile

      integer :: iErr

      allocate(newTile)
      allocate(newTile%topo_array(tile_x,tile_y))
      allocate(newTile%landuse_array(tile_x,tile_y))
      newTile%tile_start_x = tile_start_x
      newTile%tile_start_y = tile_start_y
      newTile%next => tilesHead

      iErr = read_30s_topo_tile(path, sub_path, newTile%topo_array, tile_start_x_topo, newTile%tile_start_y)
      if (iErr /= 0) then
         call mpas_log_write('Error reading global 30-arc-sec topography for GWD statistics', messageType=MPAS_LOG_ERR)
         return
      end if

      iErr = read_30s_landuse_tile(path, newTile%landuse_array, newTile%tile_start_x, newTile%tile_start_y)
      if (iErr /= 0) then
         call mpas_log_write('Error reading global 30-arc-sec landuse for GWD statistics', messageType=MPAS_LOG_ERR)
         return
      end if

      tilesHead => newTile

   end function add_tile


   !***********************************************************************
   !
   !  function free_tile_list
   !
   !> \brief   Routine to deallocate all tiles in the list
   !> \author  Abishek Gopal
   !> \date    05 Sep 2024
   !> \details 
   !>  Routine to deallocate all tiles in the list
   !
   !-----------------------------------------------------------------------
   function free_tile_list(tilesHead) result(iErr)

      implicit none

      type(mpas_gwd_tile_type), pointer, intent(inout) :: tilesHead

      integer :: iErr
      
      type(mpas_gwd_tile_type), pointer :: thisTile

      ! loop over tiles
      do while (associated(tilesHead))
         thisTile => tilesHead
         tilesHead => thisTile % next
         deallocate(thisTile%topo_array)
         deallocate(thisTile%landuse_array)
         deallocate(thisTile)
      end do   ! associated(thisTile)
      
      iErr = 0

   end function free_tile_list


   !***********************************************************************
   !
   !  function read_30s_topo_tile
   !
   !> \brief   Reads a single tile of the global 30-arc-second topography into memory
   !> \author  Michael Duda
   !> \date    28 August 2017
   !> \details 
   !>  This subroutine reads a single tile of the global 30-arc-second topography from the subdirectory 
   !>  identified by the 'sub_path' argument within the 'path' provided as the first argument.
   !
   !-----------------------------------------------------------------------
   function read_30s_topo_tile(path, sub_path, topo, tile_start_x, tile_start_y) result(iErr)

      implicit none

      character(len=*), intent(in) :: path
      character(len=*), intent(in) :: sub_path
      real(kind=R4KIND), dimension(:,:), pointer, intent(inout) :: topo
      integer, intent(in) :: tile_start_x
      integer, intent(in) :: tile_start_y

      integer :: iErr
      integer, parameter :: tile_x = 1200       ! x-dimension of each tile of global 30-arc-second topography
      integer, parameter :: tile_y = 1200       ! y-dimension of each tile of global 30-arc-second topography
      integer, parameter :: tile_bdr = 3        ! number of layers of border/halo points surrounding each tile
      integer (c_int) :: istatus
      integer :: ix, iy
      integer (c_int) :: isigned, endian, wordsize, nx, ny, nz
      real (c_float) :: scalefactor
      real (c_float), dimension(:,:,:), pointer, contiguous :: tile
      type (c_ptr) :: tile_ptr
      character(len=StrKIND) :: filename
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

      iy = tile_start_y
      ix = tile_start_x

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
      
      topo = tile((tile_bdr+1):(tile_x+tile_bdr),(tile_bdr+1):(tile_y+tile_bdr),1)

      deallocate(tile)

      iErr = 0

   end function read_30s_topo_tile


   !***********************************************************************
   !
   !  function read_30s_landuse_tile
   !
   !> \brief   Reads a single tile of the global 30-arc-second landuse into memory
   !> \author  Michael Duda
   !> \date    14 March 2017
   !> \details 
   !>  This subroutine reads the a single tile of global 30-arc-second USGS landuse
   !>   from the subdirectory 'landuse_30s' of the path provided as an argument.
   !
   !-----------------------------------------------------------------------
   function read_30s_landuse_tile(path, landuse, tile_start_x, tile_start_y) result(iErr)

      implicit none

      character(len=*), intent(in) :: path
      integer (kind=I1KIND), dimension(:,:), pointer, intent(inout) :: landuse
      integer, intent(in) :: tile_start_x
      integer, intent(in) :: tile_start_y
      
      integer :: iErr
      integer (c_int) :: istatus
      integer (c_int) :: isigned, endian, wordsize, nx, ny, nz
      real (c_float) :: scalefactor
      real (c_float), dimension(:,:,:), pointer, contiguous :: tile
      type (c_ptr) :: tile_ptr
      character(len=StrKIND) :: filename
      character(kind=c_char), dimension(StrKIND+1) :: c_filename

      allocate(tile(tile_x,tile_y,1))
      tile_ptr = c_loc(tile)

      isigned  = 1
      endian   = 0
      wordsize = 1
      scalefactor = 1.0
      nx = tile_x
      ny = tile_y
      nz = 1

      write(filename,'(a,i5.5,a1,i5.5,a1,i5.5,a1,i5.5)') trim(path)//'/landuse_30s/', tile_start_x, '-', (tile_start_x+tile_x-1), '.', &
                                                         tile_start_y, '-', (tile_start_y+tile_y-1)
      call mpas_f_to_c_string(filename, c_filename)
      call read_geogrid(c_filename, tile_ptr, nx, ny, nz, isigned, endian, &
                        wordsize, istatus)
      tile(:,:,:) = tile(:,:,:) * scalefactor
      if (istatus /= 0) then
         call mpas_log_write('Error reading landuse tile '//trim(filename))
         iErr = 1
         return
      end if
      
      landuse = int(tile(:,:,1), kind=I1KIND)

      deallocate(tile)

      iErr = 0

   end function read_30s_landuse_tile


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
   integer (kind=I1KIND) function get_dom_landmask(box_landuse, nx, ny)

      implicit none

      integer (kind=I1KIND), dimension(:,:), pointer, intent(in) :: box_landuse     ! Subset of landuse covering a grid cell
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


   !***********************************************************************
   !
   !  subroutine get_box
   !
   !> \brief   Cuts out a rectangular box of data centered at a given (lat,lon)
   !> \author  Michael Duda, Abishek Gopal
   !> \date    Sep 2024
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
   subroutine get_box(lat, lon, nx, ny, path, sub_path, tilesHead, box, box_landuse, dxm, box_mean)

      implicit none

      real (kind=RKIND), intent(in) :: lat, lon
      integer, intent(in) :: nx, ny
      character(len=*), intent(in) :: path
      character(len=*), intent(in) :: sub_path
      type(mpas_gwd_tile_type), pointer, intent(inout) :: tilesHead
      real (kind=RKIND), dimension(:,:), pointer :: box               ! Subset of topography covering a grid cell
      integer (kind=I1KIND), dimension(:,:), pointer :: box_landuse   ! Subset of landuse covering a grid cell
      real (kind=RKIND), dimension(:,:), pointer :: dxm               ! Size (meters) in zonal direction of a grid cell
      real (kind=RKIND), intent(inout) :: box_mean                    ! Mean value of topography in box
      
      type(mpas_gwd_tile_type), pointer :: thisTile
      type(mpas_gwd_tile_type), pointer :: lastTile
      integer :: i, j, ii, jj, ic, jc, ix, jx
      real (kind=RKIND) :: sg_lat

      thisTile => null()
      lastTile => null()

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
      ic = nint((lon - start_lon) * pts_per_degree) + 1
      jc = nint((lat - start_lat) * pts_per_degree) + 1

      if (ic <= 0) ic = ic + topo_x
      if (ic > topo_x) ic = ic - topo_x

      !
      ! Extract sub-array (box) from global array; must properly account for 
      ! the periodicity in the longitude coordinate, as well as the poles
      !
      box_mean = 0.0
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
      
         ! Obtain tile for given box pixel from the linked list of tiles (tilesHead),
         ! which would involve reading in the data from disk if said tile is not already in memory
         thisTile => get_tile_from_box_point(tilesHead, ii, jj, path, sub_path, lastTile)
         ! Save the current tile to possibly speed up the next lookup
         lastTile => thisTile
         
         ix = (ii - thisTile%tile_start_x) + 1
         jx = (jj - thisTile%tile_start_y) + 1
         box(i,j) = thisTile%topo_array(ix, jx)
         box_landuse(i,j) = thisTile%landuse_array(ix, jx)
         sg_lat = (start_lat + (real(jj-1,RKIND) + 0.5) / pts_per_degree) / rad2deg  ! Add 0.5 for cell center
         dxm(i,j) = sg_delta * cos(sg_lat)
         box_mean = box_mean + box(i,j)

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
   real (kind=RKIND) function get_var(box, box_mean, nx, ny)

      implicit none

      real (kind=RKIND), dimension(:,:), pointer, intent(in) :: box   ! Subset of topography covering a grid cell
      real (kind=RKIND), intent(in) :: box_mean
      integer, intent(in) :: nx, ny

      integer :: i, j
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
   real (kind=RKIND) function get_con(box, box_landuse, box_mean, nx, ny)

      implicit none

      real (kind=RKIND), dimension(:,:), pointer, intent(in) :: box              ! Subset of topography covering a grid cell
      integer (kind=I1KIND), dimension(:,:), pointer, intent(in) :: box_landuse  ! Subset of landuse covering a grid cell
      real (kind=RKIND), intent(in) :: box_mean
      integer, intent(in) :: nx, ny

      integer :: i, j
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
   real (kind=RKIND) function get_oa1(box, box_mean, nx, ny)

      implicit none

      real (kind=RKIND), dimension(:,:), pointer, intent(in) :: box   ! Subset of topography covering a grid cell
      real (kind=RKIND), intent(in) :: box_mean
      integer, intent(in) :: nx, ny

      integer :: i, j
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
   real (kind=RKIND) function get_oa2(box, box_mean, nx, ny)

      implicit none

      real (kind=RKIND), dimension(:,:), pointer, intent(in) :: box   ! Subset of topography covering a grid cell
      real (kind=RKIND), intent(in) :: box_mean
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
   real (kind=RKIND) function get_oa3(box, box_mean, nx, ny)

      implicit none

      real (kind=RKIND), dimension(:,:), pointer, intent(in) :: box   ! Subset of topography covering a grid cell
      real (kind=RKIND), intent(in) :: box_mean
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
   real (kind=RKIND) function get_oa4(box, box_mean, nx, ny)

      implicit none

      real (kind=RKIND), dimension(:,:), pointer, intent(in) :: box   ! Subset of topography covering a grid cell
      real (kind=RKIND), intent(in) :: box_mean
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
   real (kind=RKIND) function get_ol1(box, nx, ny)

      implicit none

      real (kind=RKIND), dimension(:,:), pointer, intent(in) :: box   ! Subset of topography covering a grid cell
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
   real (kind=RKIND) function get_ol2(box, nx, ny)

      implicit none

      real (kind=RKIND), dimension(:,:), pointer, intent(in) :: box   ! Subset of topography covering a grid cell
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
   real (kind=RKIND) function get_ol3(box, nx, ny)

      implicit none

      real (kind=RKIND), dimension(:,:), pointer, intent(in) :: box   ! Subset of topography covering a grid cell
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
   real (kind=RKIND) function get_ol4(box, nx, ny)

      implicit none

      real (kind=RKIND), dimension(:,:), pointer, intent(in) :: box   ! Subset of topography covering a grid cell
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
   real (kind=RKIND) function get_elvmax(box, nx, ny)

      implicit none

      real (kind=RKIND), dimension(:,:), pointer, intent(in) :: box   ! Subset of topography covering a grid cell
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
   real (kind=RKIND) function get_htheta(box, dxm, nx, ny)

      implicit none

      real (kind=RKIND), dimension(:,:), pointer, intent(in) :: box   ! Subset of topography covering a grid cell
      real (kind=RKIND), dimension(:,:), pointer, intent(in) :: dxm   ! Size (meters) in zonal direction of a grid cell
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
   real (kind=RKIND) function get_hgamma(box, dxm, nx, ny)

      implicit none

      real (kind=RKIND), dimension(:,:), pointer, intent(in) :: box   ! Subset of topography covering a grid cell
      real (kind=RKIND), dimension(:,:), pointer, intent(in) :: dxm   ! Size (meters) in zonal direction of a grid cell
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
   real (kind=RKIND) function get_hsigma(box, dxm, nx, ny)

      implicit none

      real (kind=RKIND), dimension(:,:), pointer, intent(in) :: box   ! Subset of topography covering a grid cell
      real (kind=RKIND), dimension(:,:), pointer, intent(in) :: dxm   ! Size (meters) in zonal direction of a grid cell
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
