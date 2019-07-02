! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!==================================================================================================
 module mpas_init_atm_static
!==================================================================================================
 use atm_advection
 use mpas_dmpar
 use mpas_pool_routines
 use mpas_derived_types, only : MPAS_LOG_WARN, MPAS_LOG_ERR, MPAS_LOG_CRIT
 use mpas_log, only : mpas_log_write
 use init_atm_hinterp
 use init_atm_llxy
 use mpas_c_interfacing, only : mpas_f_to_c_string

 use mpas_atmphys_utilities

 use iso_c_binding, only : c_char, c_int, c_float, c_loc, c_ptr

 implicit none
 private
 public:: init_atm_static,           &
          init_atm_check_read_error, &
          nearest_cell,              &
          sphere_distance

 interface
    subroutine read_geogrid(fname, rarray, nx, ny, nz, isigned, endian, &
                            scalefactor, wordsize, status) bind(C)
       use iso_c_binding, only : c_char, c_int, c_float, c_ptr
       character (c_char), dimension(*), intent(in) :: fname
       type (c_ptr), value :: rarray
       integer (c_int), intent(in), value :: nx
       integer (c_int), intent(in), value :: ny
       integer (c_int), intent(in), value :: nz
       integer (c_int), intent(in), value :: isigned
       integer (c_int), intent(in), value :: endian
       real (c_float), intent(in), value :: scalefactor
       integer (c_int), intent(in), value :: wordsize
       integer (c_int), intent(inout) :: status
    end subroutine read_geogrid
 end interface

 contains

!==================================================================================================
 subroutine init_atm_static(mesh, dims, configs)
!==================================================================================================

 use iso_c_binding, only : c_float, c_int, c_long, c_loc, c_ptr

 use mpas_kd_tree, only : kdnode
 use mpas_kd_tree, only : mpas_kd_construct
 use mpas_kd_tree, only : mpas_kd_free
 use mpas_kd_tree, only : mpas_kd_search

 use mpas_stack, only : node
 use mpas_stack, only : payload_t
 use mpas_stack, only : mpas_stack_push
 use mpas_stack, only : mpas_stack_is_empty
 use mpas_stack, only : mpas_stack_free

 use mpas_geotile_manager, only : tile_t, hash_table
 use mpas_geotile_manager, only : mpas_get_tile
 use mpas_geotile_manager, only : mpas_get_tile_neighbors
 use mpas_geotile_manager, only : mpas_hash_to_latlon
 use mpas_geotile_manager, only : mpas_stack_pop_tile

 use mpas_parse_geoindex, only : mpas_parse_index

!inout arguments:
 type (mpas_pool_type), intent(inout) :: mesh
 type (mpas_pool_type), intent(in) :: dims
 type (mpas_pool_type), intent(in) :: configs

!constants
 integer, parameter :: nBdyLayers = 7   ! The number of relaxation layers plus the number of specified layers
                                        ! This value is used in determining whether extra checks are needed
                                        ! in the remapping of terrain, land use, and soil category pixels

!local variables:
 type (mpas_pool_type), pointer :: geog_pool 
 type(proj_info):: proj

 character(len=StrKIND) :: fname
 character(kind=c_char), dimension(StrKIND+1) :: c_fname
 character(len=StrKIND), pointer :: config_geog_data_path
 character(len=StrKIND), pointer :: config_landuse_data
 character(len=StrKIND), pointer :: config_topo_data
 character(len=StrKIND), pointer :: config_vegfrac_data
 character(len=StrKIND), pointer :: config_albedo_data
 character(len=StrKIND), pointer :: config_maxsnowalbedo_data
 character(len=StrKIND+1) :: geog_data_path      ! same as config_geog_data_path, but guaranteed to have a trailing slash
 character(len=StrKIND+1) :: geog_sub_path       ! subdirectory names in config_geog_data_path, with trailing slash

 integer:: isice_lu,iswater_lu,ismax_lu

 integer(c_int):: nx,ny,nz
 integer(c_int):: endian,isigned,istatus,wordsize
 integer:: i,j,k
 integer :: ii, jj
 integer:: iCell,iEdge,iVtx,iPoint,iTileStart,iTileEnd,jTileStart,jTileEnd
 integer,dimension(5) :: interp_list
 integer,dimension(:),allocatable  :: nhs
 integer,dimension(:,:),allocatable:: ncat
      
 real(kind=c_float):: scalefactor
 real(kind=RKIND):: f_scalefactor
 real(kind=c_float),dimension(:,:,:),pointer,contiguous :: rarray
 type(c_ptr) :: rarray_ptr

 real(kind=RKIND), pointer :: start_lat
 real(kind=RKIND), pointer :: start_lon
 real(kind=RKIND), pointer :: start_x
 real(kind=RKIND), pointer :: start_y

 integer, pointer :: supersample_fac

 real(kind=RKIND):: lat,lon,x,y,z
 real(kind=RKIND):: lat_pt,lon_pt
 real(kind=RKIND),dimension(:,:),allocatable  :: soiltemp_1deg
 real(kind=RKIND),dimension(:,:),allocatable  :: maxsnowalb
 real(kind=RKIND),dimension(:,:,:),allocatable:: vegfra

 integer, pointer :: nCells, nCellsSolve, nEdges, nVertices, maxEdges
 logical, pointer :: on_a_sphere
 real (kind=RKIND), pointer :: sphere_radius
 
 integer, dimension(:), pointer :: nEdgesOnCell
 integer, dimension(:,:), pointer :: cellsOnCell
 integer, dimension(:,:), pointer :: verticesOnCell
 real (kind=RKIND), dimension(:), pointer :: xCell, yCell, zCell
 real (kind=RKIND), dimension(:), pointer :: xVertex, yVertex, zVertex
 real (kind=RKIND), dimension(:), pointer :: xEdge, yEdge, zEdge
 real (kind=RKIND), dimension(:), pointer :: dvEdge, dcEdge
 real (kind=RKIND), dimension(:), pointer :: areaCell, areaTriangle
 real (kind=RKIND), dimension(:,:), pointer :: kiteAreasOnVertex
 real (kind=RKIND), dimension(:), pointer :: latCell, lonCell
 real (kind=RKIND), dimension(:), pointer :: latVertex, lonVertex
 real (kind=RKIND), dimension(:), pointer :: latEdge, lonEdge
 real (kind=RKIND), dimension(:), pointer :: fEdge, fVertex

 real (kind=RKIND), dimension(:), pointer :: ter
 real (kind=RKIND), dimension(:), pointer :: soiltemp
 real (kind=RKIND), dimension(:), pointer :: snoalb
 real (kind=RKIND), dimension(:), pointer :: shdmin, shdmax
 real (kind=RKIND), dimension(:,:), pointer :: greenfrac
 real (kind=RKIND), dimension(:,:), pointer :: albedo12m
 real (kind=RKIND) :: msgval, fillval
 integer, dimension(:), pointer :: lu_index
 integer, dimension(:), pointer :: soilcat_top
 integer, dimension(:), pointer :: landmask
 integer, dimension(:), pointer :: bdyMaskCell
 character(len=StrKIND), pointer :: mminlu

 real (kind=RKIND) :: xPixel, yPixel, zPixel

 type (hash_table), pointer, dimension(:, :) :: hash
 type (kdnode), dimension(:), allocatable :: kdCords
 type (kdnode), pointer :: tree => null(), res => null()

 type (node), pointer :: stack => null()

 type (tile_t), pointer :: tile => null()
 type (tile_t) :: up, down, left, right

 logical :: ALL_PIXELS_MAPPED_TO_HALO_CELLS

 real(kind=RKIND), pointer :: dx, dy
 integer, pointer :: tile_nx, tile_ny
 integer, pointer :: tile_bdr

 integer :: nTileX, nTileY ! The number of tiles in each direction
 integer :: pixel_nx, pixel_ny ! The number of pixel in each direction
 integer :: num_processed_tiles
 integer :: cell

 ! To handle possible values that are considerably large then the RKIND
 ! and to possible take care of differences in order of summation, we will
 ! briefly hold terrain values in an I8KIND integer array and then convert
 ! them to RKIND's when they are divided by nhs.
 !
 ! This will allow larger values to be held, but will disallow smaller
 ! resolution terrain values.
 !
 ! TODO: Check this and add more detail .
 !
 integer (kind=I8KIND), dimension(:), pointer :: ter_integer
 real (kind=RKIND) :: tval


!--------------------------------------------------------------------------------------------------


 call mpas_log_write('')
 call mpas_log_write('--- enter subroutine init_atm_static:')

 call mpas_pool_get_config(configs, 'config_geog_data_path', config_geog_data_path)
 call mpas_pool_get_config(configs, 'config_landuse_data', config_landuse_data)
 call mpas_pool_get_config(configs, 'config_topo_data', config_topo_data)
 call mpas_pool_get_config(configs, 'config_vegfrac_data', config_vegfrac_data)
 call mpas_pool_get_config(configs, 'config_albedo_data', config_albedo_data)
 call mpas_pool_get_config(configs, 'config_maxsnowalbedo_data', config_maxsnowalbedo_data)
 call mpas_pool_get_config(configs, 'config_supersample_factor', supersample_fac)

 write(geog_data_path, '(a)') config_geog_data_path
 i = len_trim(geog_data_path)
 if (geog_data_path(i:i) /= '/') then
    geog_data_path(i+1:i+1) = '/'
 end if

!
! Scale all distances and areas from a unit sphere to one with radius sphere_radius
!


 call mpas_pool_get_array(mesh, 'xCell', xCell)
 call mpas_pool_get_array(mesh, 'yCell', yCell)
 call mpas_pool_get_array(mesh, 'zCell', zCell)
 call mpas_pool_get_array(mesh, 'xVertex', xVertex)
 call mpas_pool_get_array(mesh, 'yVertex', yVertex)
 call mpas_pool_get_array(mesh, 'zVertex', zVertex)
 call mpas_pool_get_array(mesh, 'xEdge', xEdge)
 call mpas_pool_get_array(mesh, 'yEdge', yEdge)
 call mpas_pool_get_array(mesh, 'zEdge', zEdge)
 call mpas_pool_get_array(mesh, 'dcEdge', dcEdge)
 call mpas_pool_get_array(mesh, 'dvEdge', dvEdge)
 call mpas_pool_get_array(mesh, 'areaCell', areaCell)
 call mpas_pool_get_array(mesh, 'areaTriangle', areaTriangle)
 call mpas_pool_get_array(mesh, 'kiteAreasOnVertex', kiteAreasOnVertex)
 call mpas_pool_get_array(mesh, 'latCell', latCell)
 call mpas_pool_get_array(mesh, 'lonCell', lonCell)
 call mpas_pool_get_array(mesh, 'latEdge', latEdge)
 call mpas_pool_get_array(mesh, 'lonEdge', lonEdge)
 call mpas_pool_get_array(mesh, 'latVertex', latVertex)
 call mpas_pool_get_array(mesh, 'lonVertex', lonVertex)
 call mpas_pool_get_array(mesh, 'fEdge', fEdge)
 call mpas_pool_get_array(mesh, 'fVertex', fVertex)
 call mpas_pool_get_array(mesh, 'bdyMaskCell', bdyMaskCell)
 
 call mpas_pool_get_array(mesh, 'nEdgesOnCell', nEdgesOnCell)
 call mpas_pool_get_array(mesh, 'cellsOnCell', cellsOnCell)
 call mpas_pool_get_array(mesh, 'verticesOnCell', verticesOnCell)

 call mpas_pool_get_array(mesh, 'ter', ter)
 call mpas_pool_get_array(mesh, 'lu_index', lu_index)
 call mpas_pool_get_array(mesh, 'mminlu', mminlu)
 call mpas_pool_get_array(mesh, 'soilcat_top', soilcat_top)
 call mpas_pool_get_array(mesh, 'landmask', landmask)
 call mpas_pool_get_array(mesh, 'soiltemp', soiltemp)
 call mpas_pool_get_array(mesh, 'snoalb', snoalb)
 call mpas_pool_get_array(mesh, 'greenfrac', greenfrac)
 call mpas_pool_get_array(mesh, 'albedo12m', albedo12m)
 call mpas_pool_get_array(mesh, 'shdmin', shdmin)
 call mpas_pool_get_array(mesh, 'shdmax', shdmax)

 call mpas_pool_get_config(mesh, 'on_a_sphere', on_a_sphere)
 call mpas_pool_get_config(mesh, 'sphere_radius', sphere_radius)

 call mpas_pool_get_dimension(dims, 'nCells', nCells)
 call mpas_pool_get_dimension(dims, 'nEdges', nEdges)
 call mpas_pool_get_dimension(dims, 'nVertices', nVertices)
 call mpas_pool_get_dimension(dims, 'maxEdges', maxEdges)
 call mpas_pool_get_dimension(dims, 'nCellsSolve', nCellsSolve)

 xCell = xCell * sphere_radius
 yCell = yCell * sphere_radius
 zCell = zCell * sphere_radius
 xVertex = xVertex * sphere_radius
 yVertex = yVertex * sphere_radius
 zVertex = zVertex * sphere_radius
 xEdge = xEdge * sphere_radius
 yEdge = yEdge * sphere_radius
 zEdge = zEdge * sphere_radius
 dvEdge = dvEdge * sphere_radius
 dcEdge = dcEdge * sphere_radius
 areaCell = areaCell * sphere_radius**2.0
 areaTriangle = areaTriangle * sphere_radius**2.0
 kiteAreasOnVertex = kiteAreasOnVertex * sphere_radius**2.0


 ! Set up the KD-Tree of Grid Cell Cartisian Coordinates
 allocate(kdcords(nCells))
 do i = 1, nCells, 1
    call convert_lx(x, y, z, sphere_radius, latCell(i), lonCell(i))

    allocate(kdcords(i) % point(3))
    kdcords(i) % point = (/x, y, z/)
    kdcords(i) % cell = i

    ! Mark if a cell is a halo cell or not
    if(i > nCellsSolve) then
       kdcords(i) % is_halo = .TRUE.
    else
       kdcords(i) % is_halo = .FALSE.
    endif
 enddo
 tree => mpas_kd_construct(kdcords, 3)


!
! Initialize Coriolis parameter field on edges and vertices
!
 do iEdge=1,nEdges
    fEdge(iEdge)  = 2.0 * omega * sin(latEdge(iEdge))
 end do
 do iVtx=1,nVertices
    fVertex(iVtx) = 2.0 * omega * sin(latVertex(iVtx))
 end do


!
! Compute weights used in advection and deformation calculation
!
 call atm_initialize_advection_rk(mesh, nCells, nEdges, maxEdges, on_a_sphere, sphere_radius) 
 call atm_initialize_deformation_weights(mesh, nCells, on_a_sphere, sphere_radius) 


!
! Set land use and soil category parameters for water and ice
!
 surface_input_select0: select case(trim(config_landuse_data))
    case('USGS')
       isice_lu = 24
       iswater_lu = 16
       ismax_lu = 24
       write(mminlu,'(a)') 'USGS'
    case('MODIFIED_IGBP_MODIS_NOAH')
       isice_lu = 15
       iswater_lu = 17
       ismax_lu = 20
       write(mminlu,'(a)') 'MODIFIED_IGBP_MODIS_NOAH'
    case default
         call mpas_log_write('*****************************************************************', messageType=MPAS_LOG_ERR)
         call mpas_log_write('Invalid land use dataset '''//trim(config_landuse_data) &
                                       //''' selected for config_landuse_data',                   messageType=MPAS_LOG_ERR)
         call mpas_log_write('   Possible options are: ''USGS'', ''MODIFIED_IGBP_MODIS_NOAH''',   messageType=MPAS_LOG_ERR)
         call mpas_log_write('*****************************************************************', messageType=MPAS_LOG_ERR)
         call mpas_log_write('Please correct the namelist.', messageType=MPAS_LOG_CRIT)
 end select surface_input_select0


!
! Interpolate HGT
!

 select case(trim(config_topo_data))
    case('GTOPO30')
       call mpas_log_write('Using GTOPO30 terrain dataset')
       geog_sub_path = 'topo_30s/'
    case('GMTED2010')
       call mpas_log_write('Using GMTED2010 terrain dataset')
       geog_sub_path = 'topo_gmted2010_30s/'
    case('default')
       call mpas_log_write('*****************************************************************', messageType=MPAS_LOG_ERR)
       call mpas_log_write('Invalid topography dataset '''//trim(config_topo_data) &
                                     //''' selected for config_topo_data',                      messageType=MPAS_LOG_ERR)
       call mpas_log_write('   Possible options are: ''GTOPO30'', ''GMTED2010''',               messageType=MPAS_LOG_ERR)
       call mpas_log_write('*****************************************************************', messageType=MPAS_LOG_ERR)
       call mpas_log_write('Please correct the namelist.', messageType=MPAS_LOG_CRIT)
 end select

 ! Read in the data that describes this geog dataset
 fname = trim(geog_data_path)//trim(geog_sub_path)

 call mpas_pool_create_pool(geog_pool)
 call mpas_parse_index(fname, geog_pool)
 call mpas_pool_get_config(geog_pool, 'dx', dx) ! The distance between two pixels
 call mpas_pool_get_config(geog_pool, 'dy', dy) ! The distance between two pixels
 call mpas_pool_get_config(geog_pool, 'tile_x', tile_nx) ! The number of pixels between two tiles
 call mpas_pool_get_config(geog_pool, 'tile_y', tile_ny) ! The number of pixels between two tiles
 call mpas_pool_get_config(geog_pool, 'tile_bdr', tile_bdr)
 call mpas_pool_get_config(geog_pool, 'known_lat', start_lat)
 call mpas_pool_get_config(geog_pool, 'known_lon', start_lon)
 call mpas_pool_get_config(geog_pool, 'known_x', start_x)
 call mpas_pool_get_config(geog_pool, 'known_y', start_y)
 call mpas_pool_add_config(geog_pool, 'topo_dir_path', fname)

 f_endian = 0
 f_scalefactor = 1.0
 call mpas_pool_add_config(geog_pool, 'endian', f_endian)
 call mpas_pool_add_config(geog_pool, 'scalefactor', f_scalefactor)

 ! Initalize the hash table
 !
 ! The tile_hash is used when searching, getting and adding tiles.
 ! See get_tile(...), search_tile(...) and add_tile(...)
 ! Create a hash table, mapping out each tile for quicker searching as
 ! well as enabling us to grab a tile's neighbors.
 !
 nTileX = int((360. / dx)) / tile_nx
 nTileY = int((180. / dy)) / tile_ny
 allocate(hash(0:nTileX, 0:nTileY)) 

 ! Add these values to the geo_pool
 call mpas_pool_add_config(geog_pool, 'nTileX', nTileX)
 call mpas_pool_add_config(geog_pool, 'nTileY', nTileY)

 ! Determine the amount of global pixels for this dataset
 pixel_nx = 360 / dx
 pixel_ny = 180 / dy
 call mpas_pool_add_config(geog_pool, 'pixel_nx', pixel_nx)
 call mpas_pool_add_config(geog_pool, 'pixel_ny', pixel_ny)

 ! 
 ! To ensure that we map all pixels of a topography dataset/geo-grid in parallel we have developed
 ! the following algorithm:
 !
 ! We have the following data structures:
 !  1. A stack - That holds tiles that may or may not need to be processed
 !  2. A kdtree - That holds the locations of every cell in our decomposition
 !  3. A hash table - That enables us to see if a tile is loaded, or not
 !
 ! A rough description of the algorithm is:
 !
 ! Push a single tile on the stack. While the stack is not empty, pop a tile off the
 ! stack and map its tiles to a mesh cell using the KD tree.
 !
 ! If all the pixels in the tile map to halo cells, then don't add that tiles neighbors to
 ! the stack. If a single pixel does map to a non-halo cell, then add all of its neighbors
 ! (up, down, left, right) to the stack. 
 !
 ! And continue until the stack is empty.
 !
 ! An outer loop loops through all nCells to insure that all cells have had terrain values
 ! mapped to them.

 ! Pseudocode:
 !
 ! for cell in Ncells: 
 !  if(nhs == 0) 
 !     load the tile for that cell
 !     push tile
 !  do while(stack is not empty)
 !     pop tile
 !     if tile == proccseed then 
 !         cycle
 !
 !     mapped_all_to_halo = True
 !     for pixels in tile:
 !        tval = tile(pixel)
 !        call kd_search(pixel, nCell_result)
 !        ter(nCell_result) += tval 
 !        nhs(nCell_result) += 1
 !     
 !        if (nCell_result < nCellSolve) then
 !           mapped_all_to_halo = False
 !
 !     if(mapped_all_to_halo) then
 !        pass
 !     else
 !        push tile_neighbors_onto_stack
 !

 num_processed_tiles = 0
 allocate(nhs(nCells))
 allocate(ter_integer(nCells))

 nhs(:) = 0
 ter(:) = 0
 ter_integer(:) = 0

 do cell = 1, nCells, 1

    ! To insure that all the cells in our decompositions have values, check to see if 
    ! the nhs for that cells is greater then 0. If it is 0, then we have not added
    ! any terrain values to it, so push it onto the stack to insure it gets processed.
    ! 
    ! For instance if we have a discontinuous decomposition partition.

    if(nhs(cell) == 0) then
      call mpas_get_tile(latCell(cell), lonCell(cell), tile, hash, geog_pool)
      stack => mpas_stack_push(stack, tile)
    endif

    do while( .NOT. mpas_stack_is_empty(stack))
      tile => null()
      tile => mpas_stack_pop_tile(stack)

      if ( tile % is_processed ) then
         cycle
      else
         tile % is_processed = .TRUE.
      endif
   
      num_processed_tiles = num_processed_tiles + 1
      call mpas_log_write("Processing a terrain tile ... $i - "//trim(tile%fname), intArgs=(/num_processed_tiles/))

      ALL_PIXELS_MAPPED_TO_HALO_CELLS = .TRUE.

      ! For each pixel within a tile, calculate its Cartesian coordinate. Then, using its
      ! Cartesian coordinates, search the KD tree for the mesh cell that is closest and 
      ! assign that pixels terrain value to that mesh cell.
      !
      ! If we find that none of the pixels map a non-halo cell (ie < nCellsSolve) then 
      ! we know that this tile is outside of our processor decomposition, so most likely
      ! so is its neighbors, so don't add them to the stack.

      do j = 1 + tile_bdr, tile_ny + tile_bdr, 1
         do i = 1 + tile_bdr, tile_nx + tile_bdr, 1

            tval = tile % tile(i, j, 1)

            lat_pt = (j - (tile_bdr - 1) + real(tile % y(1)) - 1) * dy + start_lat
            lon_pt = (i - (tile_bdr - 1) + real(tile % x(1)) - 1) * dx + start_lon

            if (lon_pt < 0) then
               lon_pt = lon_pt + 360
            endif

            lat_pt = lat_pt * (pii / 180.0)
            lon_pt = lon_pt * (pii / 180.0)

            ! Convert the pixel ll x, y to lat, lon and then to Cartesian coordinates
            ! (tile_bdr - 1) represents the 1 + tile_bdr start, and the later - 1 represent the
            ! subtraction to achieve zero based.
            call convert_lx(x, y, z, sphere_radius, lat_pt, lon_pt)

            res => null()
            call mpas_kd_search(tree, (/x, y, z/), res)

            !
            ! For all but the outermost boundary cells, we can safely assume that the nearest
            ! model grid cell contains the pixel (else, a different cell would be nearest)
            !
            if (bdyMaskCell(res % cell) < nBdyLayers) then
               ter_integer(res % cell) = ter_integer(res % cell) + int(tval, kind=I8KIND)
               nhs(res % cell) = nhs(res % cell) + 1

               ! To insure that the algorithm is not processing tiles that are too far
               ! outside of its decomposition (i.e. tiles that do not map to any of this MPI
               ! process' cells, if all the pixels of a completely to halo cells, then we know
               ! we don't need to add this tiles neighbors to the stack.
               ! 
               ! However, if a single pixel maps to one of our cells then to be safe we should
               ! push on its surrounding tiles.
               if (res % cell <= nCellsSolve) then
                  ALL_PIXELS_MAPPED_TO_HALO_CELLS = .FALSE.
               endif

            ! For outermost boundary cells, additional work is needed to verify that the pixel
            ! actually lies within the nearest cell
            else
               zPixel = sphere_radius * sin(lat_pt)                  ! Model cell coordinates assume a "full" sphere radius
               xPixel = sphere_radius * cos(lon_pt) * cos(lat_pt)    ! at this point, so we need to ues the same radius
               yPixel = sphere_radius * sin(lon_pt) * cos(lat_pt)    ! for source pixel coordinates

               if (in_cell(xPixel, yPixel, zPixel, xCell(res % cell), yCell(res % cell), zCell(res % cell), &
                           nEdgesOnCell(res % cell), verticesOnCell(:,res % cell), xVertex, yVertex, zVertex)) then

                  ter_integer(res % cell) = ter_integer(res % cell) + int(tval, kind=I8KIND)
                  nhs(res % cell) = nhs(res % cell) + 1

                  ! Similarly, if we have a border cell that is not a halo cell, then we should
                  ! grab the current tiles surrounding tiles.
                  if (res % cell <= nCellsSolve) then
                     ALL_PIXELS_MAPPED_TO_HALO_CELLS = .FALSE.
                  endif

               endif
            endif
         enddo
      enddo

      if (ALL_PIXELS_MAPPED_TO_HALO_CELLS) then
         ! Pass - All tiles mapped to its halo cells, so don't add any tiles to the stack
      else
         ! Calculate the tile neighbors of the current tile and push them onto the stack
         call mpas_get_tile_neighbors(tile, up, down, left, right, geog_pool)
         call push_tile_neighbors(stack, up, down, left, right, hash, geog_pool)
      endif
    enddo
 enddo

 ! Since we are taking an average of all the pixels that map to  a cell, divide each cell's 
 ! terrain value by the number of pixels that we alloted to it.
 do i = 1, nCells, 1
    ter(i) = real(ter_integer(i), kind=R8KIND) / real(nhs(i), kind=R8KIND)
 enddo


 !! Deallocations
 deallocate(nhs)
 deallocate(ter_integer)

 ! Deallocate the stack
 call mpas_stack_free(stack, free_payload=.TRUE.)

 ! Use the hash table to deallocate each tile and then the hash itself.
 do i = 0, nTileX, 1
    do j = 0, nTileY, 1
       if(associated(hash(i, j) % ptr)) then
          deallocate(hash(i, j) % ptr % tile)
          deallocate(hash(i, j) % ptr)
       endif
    enddo
 enddo
 deallocate(hash)


 call mpas_pool_destroy_pool(geog_pool)

 call mpas_log_write('--- end interpolate TER')

!
! Interpolate LU_INDEX
!
 surface_input_select1: select case(trim(config_landuse_data))
    case('USGS')
       call mpas_log_write('Using 24-class USGS 30-arc-second land cover dataset')
       geog_sub_path = 'landuse_30s/'
    case('MODIFIED_IGBP_MODIS_NOAH')
       call mpas_log_write('Using 20-class MODIS 30-arc-second land cover dataset')
       geog_sub_path = 'modis_landuse_20class_30s/'
    case default
       call mpas_log_write('*****************************************************************', messageType=MPAS_LOG_ERR)
       call mpas_log_write('Invalid land use dataset '''//trim(config_landuse_data) &
                                     //''' selected for config_landuse_data',                   messageType=MPAS_LOG_ERR)
       call mpas_log_write('   Possible options are: ''USGS'', ''MODIFIED_IGBP_MODIS_NOAH''',   messageType=MPAS_LOG_ERR)
       call mpas_log_write('*****************************************************************', messageType=MPAS_LOG_ERR)
       call mpas_log_write('Please correct the namelist.', messageType=MPAS_LOG_CRIT)
 end select surface_input_select1

 allocate(tile_bdr)
 tile_bdr = 0
 isigned = .False.

 ! Read the index file for the choosen landuse dataset
 fname = trim(geog_data_path)//trim(geog_sub_path)

 call mpas_pool_create_pool(geog_pool)
 call mpas_parse_index(fname, geog_pool)
 call mpas_pool_get_config(geog_pool, 'dx', dx) ! The distance between two pixels
 call mpas_pool_get_config(geog_pool, 'dy', dy) ! The distance between two pixels
 call mpas_pool_get_config(geog_pool, 'tile_x', tile_nx) ! The number of pixels between two tiles
 call mpas_pool_get_config(geog_pool, 'tile_y', tile_ny) ! The number of pixels between two tiles
 call mpas_pool_get_config(geog_pool, 'known_lat', start_lat)
 call mpas_pool_get_config(geog_pool, 'known_lon', start_lon)
 call mpas_pool_get_config(geog_pool, 'known_x', start_x)
 call mpas_pool_get_config(geog_pool, 'known_y', start_y)
 call mpas_pool_add_config(geog_pool, 'tile_bdr', tile_bdr)
 call mpas_pool_add_config(geog_pool, 'topo_dir_path', fname)
 call mpas_pool_add_config(geog_pool, 'signed', isigned_bool)

 f_endian = 0
 f_scalefactor = 1.0
 call mpas_pool_add_config(geog_pool, 'endian', f_endian)
 call mpas_pool_add_config(geog_pool, 'scalefactor', f_scalefactor)

 ! Initalize the hash table
 !
 ! The tile_hash is used when searching, getting and adding tiles.
 ! See get_tile(...), search_tile(...) and add_tile(...)
 ! Create a hash table, mapping out each tile for quicker searching as
 ! well as enabling us to grab a tile's neighbors.
 !
 nTileX = int((360. / dx)) / tile_nx
 nTileY = int((180. / dy)) / tile_ny
 allocate(hash(0:nTileX, 0:nTileY)) 

 call mpas_log_write("nTileX: $i", intArgs=(/nTileX/))
 call mpas_log_write("nTiley: $i", intArgs=(/nTileY/))

 ! Add these values to the geo_pool
 call mpas_pool_add_config(geog_pool, 'nTileX', nTileX)
 call mpas_pool_add_config(geog_pool, 'nTileY', nTileY)

 ! Determine the amount of global pixels for this dataset
 pixel_nx = 360 / dx
 pixel_ny = 180 / dy
 call mpas_pool_add_config(geog_pool, 'pixel_nx', pixel_nx)
 call mpas_pool_add_config(geog_pool, 'pixel_ny', pixel_ny)

 call mpas_log_write("pixel_nx: $i", intArgs=(/pixel_nx/))
 call mpas_log_write("pixel_ny: $i", intArgs=(/pixel_ny/))

 num_processed_tiles = 0
 allocate(ncat(ismax_lu,nCells))
 ncat(:,:) = 0
 lu_index(:) = 0.0

 do cell = 1, nCells, 1

   if (all(ncat(:,cell) == 0)) then
      call mpas_get_tile(latCell(cell), lonCell(cell), tile, hash, geog_pool)
      stack => mpas_stack_push(stack, tile)
   endif

   do while ( .NOT. mpas_stack_is_empty(stack))
      tile => null()
      tile => mpas_stack_pop_tile(stack)

      if ( tile % is_processed ) then
         cycle
      else
         tile % is_processed = .TRUE.
      endif

      ALL_PIXELS_MAPPED_TO_HALO_CELLS = .TRUE.

      num_processed_tiles = num_processed_tiles + 1
      call mpas_log_write("Proccessing a lu_tile ... $i - "//trim(tile%fname), intArgs=(/num_processed_tiles/)) 
      do j = 1 + tile_bdr, tile_ny + tile_bdr
         do i = 1 + tile_bdr, tile_nx + tile_bdr

            !call mpas_log_write("about to read the tile: ")
            !write(0,*) tile % tile

            !call mpas_log_write("This Cat: i: $i j: $i - cat: $i ", intArgs=(/i, j, int(tile % tile(i,j))/))


            lat_pt = (j - (tile_bdr + 1) + real(tile % y(1)) - 1) * dy + start_lat
            lon_pt = (i - (tile_bdr + 1) + real(tile % x(1)) - 1) * dx + start_lon
         
            if (lon_pt < 0) then
               lon_pt = lon_pt + 360.0_RKIND
            endif

            lat_pt = lat_pt * (pii/180.0_RKIND)
            lon_pt = lon_pt * (pii/180.0_RKIND)

            call convert_lx(x, y, z, sphere_radius, lat_pt, lon_pt)
            call mpas_kd_search(tree, (/x, y, z/), res)

             !
             ! For all but the outermost boundary cells, we can safely assume that the nearest
             ! model grid cell contains the pixel (else, a different cell would be nearest)
             !
             if (bdyMaskCell(res % cell) < nBdyLayers) then
                ncat(int(tile % tile(i,j,1)), res % cell) = ncat(int(tile % tile(i,j,1)), res % cell) + 1

                if (res % cell <= nCellsSolve) then
                   ALL_PIXELS_MAPPED_TO_HALO_CELLS = .FALSE.
                endif

             ! For outermost boundary cells, additional work is needed to verify that the pixel
             ! actually lies within the nearest cell
             else
                zPixel = sphere_radius * sin(lat_pt)                  ! Model cell coordinates assume a "full" sphere radius
                xPixel = sphere_radius * cos(lon_pt) * cos(lat_pt)    ! at this point, so we need to ues the same radius
                yPixel = sphere_radius * sin(lon_pt) * cos(lat_pt)    ! for source pixel coordinates

                if (in_cell(xPixel, yPixel, zPixel, xCell(res % cell), yCell(res % cell), zCell(res % cell), &
                            nEdgesOnCell(res % cell), verticesOnCell(:, res % cell), xVertex, yVertex, zVertex)) then

                   ncat(int(tile % tile(i,j,1)), res % cell) = ncat(int(tile % tile(i,j,1)), res % cell) + 1
      
                   if (res % cell <= nCellsSolve) then
                      ALL_PIXELS_MAPPED_TO_HALO_CELLS = .FALSE.
                   endif
                end if
             end if
         enddo
      enddo
      if (ALL_PIXELS_MAPPED_TO_HALO_CELLS) then
         ! Pass - All pixels mapped to a halo cell, so don't add any neighboring tiles to the stack
      else
         call mpas_get_tile_neighbors(tile, up, down, left, right, geog_pool)
         call push_tile_neighbors(stack, up, down, left, right, hash, geog_pool)
      endif
   enddo
 enddo


 do iCell = 1, nCells
    lu_index(iCell) = 1
    do i = 2, ismax_lu
       if (ncat(i,iCell) > ncat(lu_index(iCell),iCell)) then
          lu_index(iCell) = i
       end if
    end do
 end do

 ! Deallocate the stack
 call mpas_stack_free(stack, free_payload=.TRUE.)
 deallocate(ncat)

 ! Use the hash table to deallocate each tile and then the hash itself.
 do i = 0, nTileX
    do j = 0, nTileY
       if(associated(hash(i, j) % ptr)) then
          deallocate(hash(i, j) % ptr % tile)
          deallocate(hash(i, j) % ptr)
       endif
    enddo
 enddo
 deallocate(hash)

 call mpas_pool_destroy_pool(geog_pool)

 call mpas_log_write('--- end interpolate LU_INDEX')

!
! Interpolate SOILCAT_TOP
!
   
 ! Read the index file for the choosen landuse dataset
 fname = trim(geog_data_path)//'soiltype_top_30s/'

 allocate(tile_bdr)
 tile_bdr = 0

 call mpas_pool_create_pool(geog_pool)
 call mpas_parse_index(fname, geog_pool)
 call mpas_pool_get_config(geog_pool, 'dx', dx) ! The distance between two pixels
 call mpas_pool_get_config(geog_pool, 'dy', dy) ! The distance between two pixels
 call mpas_pool_get_config(geog_pool, 'tile_x', tile_nx) ! The number of pixels between two tiles
 call mpas_pool_get_config(geog_pool, 'tile_y', tile_ny) ! The number of pixels between two tiles
 call mpas_pool_get_config(geog_pool, 'known_lat', start_lat)
 call mpas_pool_get_config(geog_pool, 'known_lon', start_lon)
 call mpas_pool_get_config(geog_pool, 'known_x', start_x)
 call mpas_pool_get_config(geog_pool, 'known_y', start_y)
 call mpas_pool_add_config(geog_pool, 'tile_bdr', tile_bdr)
 call mpas_pool_add_config(geog_pool, 'topo_dir_path', fname)

 f_endian = 0
 f_scalefactor = 1.0
 signed = 1
 call mpas_pool_add_config(geog_pool, 'endian', f_endian)
 call mpas_pool_add_config(geog_pool, 'scalefactor', f_scalefactor)
 call mpas_pool_add_config(geog_pool, 'signed', signed)

 nTileX = int((360.0_RKIND / dx)) / tile_nx
 nTileY = int((180.0_RKIND / dy)) / tile_ny
 allocate(hash(0:nTileX, 0:nTileY))

 call mpas_pool_add_config(geog_pool, 'nTileX', nTileX)
 call mpas_pool_add_config(geog_pool, 'nTileY', nTileY)

 pixel_nx = 360.0_RKIND / dx
 pixel_ny = 180.0_RKIND / dy
 call mpas_pool_add_config(geog_pool, 'pixel_nx', pixel_nx)
 call mpas_pool_add_config(geog_pool, 'pixel_ny', pixel_ny)

 allocate(ncat(16,nCells))
 ncat(:,:) = 0
 soilcat_top(:) = 0.0

 num_processed_tiles = 0

 do cell = 1, nCells

   if (all(ncat(:,cell) == 0)) then
      call mpas_get_tile(latCell(cell), lonCell(cell), tile, hash, geog_pool)
      stack => mpas_stack_push(stack, tile)
   endif

   do while ( .NOT. mpas_stack_is_empty(stack))
      tile => mpas_stack_pop_tile(stack)

      if ( tile % is_processed ) then
         cycle
      else
         tile % is_processed = .TRUE.
      endif

      ALL_PIXELS_MAPPED_TO_HALO_CELLS = .TRUE.
      
      num_processed_tiles = num_processed_tiles + 1
      call mpas_log_write("Processing a Soil category tile ... $i - "//trim(tile%fname), intArgs=(/num_processed_tiles/))

      do j = 1 + tile_bdr, tile_ny + tile_bdr
         do i = 1 + tile_bdr, tile_nx + tile_bdr
   
            lat_pt = real((j - (tile_bdr + 1) + tile % y(1)) - 1, kind=RKIND) * dy + start_lat
            lon_pt = real((i - (tile_bdr + 1) + tile % x(1)) - 1, kind=RKIND) * dx + start_lon

            if (lon_pt < 0.0_RKIND) then
               lon_pt = lon_pt + 360.0_RKIND
            endif

            lat_pt = lat_pt * (pii/180.0_RKIND)
            lon_pt = lon_pt * (pii/180.0_RKIND)

            call convert_lx(x, y, z, sphere_radius, lat_pt, lon_pt)
            call mpas_kd_search(tree, (/x, y, z/), res)

             !
             ! For all but the outermost boundary cells, we can safely assume that the nearest
             ! model grid cell contains the pixel (else, a different cell would be nearest)
             !
             if (bdyMaskCell(res % cell) < nBdyLayers) then
                ncat(int(tile % tile(i,j,1)), res % cell) = ncat(int(tile % tile(i,j,1)), res % cell) + 1

                if (res % cell <= nCellsSolve) then
                   ALL_PIXELS_MAPPED_TO_HALO_CELLS = .FALSE.
                endif
             ! For outermost boundary cells, additional work is needed to verify that the pixel
             ! actually lies within the nearest cell
             else
                zPixel = sphere_radius * sin(lat_pt)                  ! Model cell coordinates assume a "full" sphere radius
                xPixel = sphere_radius * cos(lon_pt) * cos(lat_pt)    ! at this point, so we need to ues the same radius
                yPixel = sphere_radius * sin(lon_pt) * cos(lat_pt)    ! for source pixel coordinates

                if (in_cell(xPixel, yPixel, zPixel, xCell(res % cell), yCell(res % cell), zCell(res % cell), &
                            nEdgesOnCell(res % cell), verticesOnCell(:,res % cell), xVertex, yVertex, zVertex)) then

                   ncat(int(tile % tile(i,j,1)), res % cell) = ncat(int(tile % tile(i,j,1)), res % cell) + 1

                   if (res % cell <= nCellsSolve) then
                      ALL_PIXELS_MAPPED_TO_HALO_CELLS = .FALSE.
                   endif
                end if
             end if
         enddo
      enddo
      if (.not. ALL_PIXELS_MAPPED_TO_HALO_CELLS) then
         call mpas_get_tile_neighbors(tile, up, down, left, right, geog_pool)
         call push_tile_neighbors(stack, up, down, left, right, hash, geog_pool)
      else
         write(0,*) "Not pushing anymore tiles"
      endif
   enddo
 enddo

 do iCell = 1,nCells
    soilcat_top(iCell) = 1
    do i = 2,16
       if(ncat(i,iCell) > ncat(soilcat_top(iCell),iCell)) then
          soilcat_top(iCell) = i
       end if
    end do
 end do
 deallocate(ncat)

 ! Deallocate the stack
 call mpas_stack_free(stack, free_payload=.TRUE.)

! Use the hash table to deallocate each tile and then the hash itself.
 do i = 0, nTileX
    do j = 0, nTileY
       if(associated(hash(i, j) % ptr)) then
          deallocate(hash(i, j) % ptr % tile)
          deallocate(hash(i, j) % ptr)
       endif
    enddo
 enddo
 deallocate(hash)

 call mpas_pool_destroy_pool(geog_pool)

 call mpas_log_write('--- end interpolate SOILCAT_TOP')


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! KLUDGE TO FIX SOIL TYPE OVER ANTARCTICA
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 where (lu_index == isice_lu) soilcat_top = 16

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! CORRECT INCONSISTENT SOIL AND LAND USE DATA
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 do iCell = 1,nCells
    if (lu_index(iCell) == iswater_lu .or. &
        soilcat_top(iCell) == 14) then
        if (lu_index(iCell) /= iswater_lu) then
            call mpas_log_write('Turning lu_index into water at $i', intArgs=(/iCell/))
            lu_index(iCell) = iswater_lu
        end if
        if (soilcat_top(iCell) /= 14) then
            call mpas_log_write('Turning soilcat_top into water at $i', intArgs=(/iCell/))
            soilcat_top(iCell) = 14
        end if
    end if
 end do


!
! Derive LANDMASK
!
 landmask(:) = 0
 do iCell=1, nCells
    if (lu_index(iCell) /= iswater_lu) landmask(iCell) = 1
 end do
 call mpas_log_write('--- end interpolate LANDMASK')


!
! Interpolate SOILTEMP:
!
 nx = 186
 ny = 186
 nz = 1
 isigned  = 0
 endian   = 0
 wordsize = 2
 scalefactor = 0.01
 allocate(rarray(nx,ny,nz))
 allocate(soiltemp_1deg(-2:363,-2:183))
 soiltemp(:) = 0.0

 rarray_ptr = c_loc(rarray)

 call map_set(PROJ_LATLON, proj,  &
              latinc = 1.0_RKIND, &
              loninc = 1.0_RKIND, &
              knowni = 1.0_RKIND, &
              knownj = 1.0_RKIND, &
              lat1 = -89.5_RKIND, &
              lon1 = -179.5_RKIND)

 write(fname,'(a,i5.5,a1,i5.5,a1,i5.5,a1,i5.5)') trim(geog_data_path)// &
       'soiltemp_1deg/',1,'-',180,'.',1,'-',180
 call mpas_log_write(trim(fname))
 call mpas_f_to_c_string(fname, c_fname)

 call read_geogrid(c_fname,rarray_ptr,nx,ny,nz,isigned, endian, &
                   scalefactor,wordsize,istatus)
 call init_atm_check_read_error(istatus, fname)
 soiltemp_1deg(-2:180,-2:183) = rarray(1:183,1:186,1)

 write(fname,'(a,i5.5,a1,i5.5,a1,i5.5,a1,i5.5)') trim(geog_data_path)// &
            'soiltemp_1deg/',181,'-',360,'.',1,'-',180
 call mpas_log_write(trim(fname))
 call mpas_f_to_c_string(fname, c_fname)

 call read_geogrid(c_fname,rarray_ptr,nx,ny,nz,isigned,endian, &
                        scalefactor,wordsize,istatus)
 call init_atm_check_read_error(istatus,fname)
 soiltemp_1deg(181:363,-2:183) = rarray(4:186,1:186,1)

 interp_list(1) = FOUR_POINT
 interp_list(2) = W_AVERAGE4
 interp_list(3) = W_AVERAGE16
 interp_list(4) = SEARCH
 interp_list(5) = 0

 do iCell = 1,nCells
  
    if(landmask(iCell) == 1) then
       lat = latCell(iCell) * DEG_PER_RAD
       lon = lonCell(iCell) * DEG_PER_RAD
       call latlon_to_ij(proj, lat, lon, x, y)
       if(x < 0.5) then
          lon = lon + 360.0
          call latlon_to_ij(proj, lat, lon, x, y)
       else if (x >= 360.5) then
          lon = lon - 360.0
          call latlon_to_ij(proj, lat, lon, x, y)
       end if
       if (y < 1.0) y = 1.0
       if (y > 179.0) y = 179.0
       soiltemp(iCell) = interp_sequence(x,y,1,soiltemp_1deg,-2,363,-2,183, &
                                           1,1,0.0_RKIND,interp_list,1)
    else
       soiltemp(iCell) = 0.0
    end if

 end do
 deallocate(rarray)
 deallocate(soiltemp_1deg)
 call mpas_log_write('--- end interpolate SOILTEMP')


!
! Interpolate SNOALB
!
 if (trim(config_maxsnowalbedo_data) == 'MODIS') then

    call mpas_log_write('Using MODIS 0.05-deg data for maximum snow albedo')
    if (supersample_fac > 1) then
       call mpas_log_write('   Dataset will be supersampled by a factor of $i', intArgs=(/supersample_fac/))
    end if

    allocate(nhs(nCells))
    nhs(:) = 0
    snoalb(:) = 0

    f_scalefactor = 0.01_RKIND

    fname = trim(geog_data_path)//'maxsnowalb_modis/'
    call mpas_pool_create_pool(geog_pool)
    call mpas_parse_index(fname, geog_pool)
    call mpas_pool_get_config(geog_pool, 'dx', dx)
    call mpas_pool_get_config(geog_pool, 'dy', dy)
    call mpas_pool_get_config(geog_pool, 'tile_x', tile_nx)
    call mpas_pool_get_config(geog_pool, 'tile_y', tile_ny)
    call mpas_pool_get_config(geog_pool, 'known_lat', start_lat)
    call mpas_pool_get_config(geog_pool, 'known_lon', start_lon)
    call mpas_pool_get_config(geog_pool, 'known_x', start_x)
    call mpas_pool_get_config(geog_pool, 'known_y', start_y)
    call mpas_pool_get_config(geog_pool, 'tile_bdr', tile_bdr)
    call mpas_pool_add_config(geog_pool, 'scalefactor', f_scalefactor)
    call mpas_pool_add_config(geog_pool, 'topo_dir_path', fname)

    endian = 0
    msgval = real(-999.0,kind=R4KIND)*real(0.01,kind=R4KIND)
    fillval = 0.0
    call mpas_pool_add_config(geog_pool, 'endian', endian)

    ! Allocate the hash table
    nTileX = int((360.0_RKIND / dx)) / tile_nx
    nTileY = int((180.0_RKIND / abs(dy))) / tile_ny
    allocate(hash(0:nTileX, 0:nTileY))

    call mpas_pool_add_config(geog_pool, 'nTileX', nTileX)
    call mpas_pool_add_config(geog_pool, 'nTileY', nTileY)

    ! Determine the amount of global pixels for this dataset
    pixel_nx = 360.0_RKIND / dx
    pixel_ny = 180.0_RKIND / dy
    call mpas_pool_add_config(geog_pool, 'pixel_nx', pixel_nx)
    call mpas_pool_add_config(geog_pool, 'pixel_ny', pixel_ny)

    num_processed_tiles = 0
    do cell = 1, nCells

       if(nhs(cell) == 0) then
         call mpas_get_tile(latCell(cell), lonCell(cell), tile, hash, geog_pool)
         stack => mpas_stack_push(stack, tile)
      endif

      do while ( .not. mpas_stack_is_empty(stack))
         tile => mpas_stack_pop_tile(stack)

         if ( tile % is_processed ) then
            cycle
         else
            tile % is_processed = .true.
         endif


         ALL_PIXELS_MAPPED_TO_HALO_CELLS = .true.

         num_processed_tiles = num_processed_tiles + 1
         call mpas_log_write("Processing a SNOALB tile ... $i - "//trim(tile%fname), intArgs=(/num_processed_tiles/))

         do j = supersample_fac * tile_bdr + 1, supersample_fac * (tile_ny - tile_bdr) 
            do i = supersample_fac * tile_bdr + 1, supersample_fac * (tile_nx - tile_bdr)
               ii = (i - 1) / supersample_fac + 1
               jj = (j - 1) / supersample_fac + 1

               lat_pt = real(j - (supersample_fac * tile_bdr + 1) &
                             + (supersample_fac * tile % y(1)) - 1, kind=RKIND) * (dy / supersample_fac) + start_lat 
               lon_pt = real(i - (supersample_fac * tile_bdr + 1) &
                             + (supersample_fac * tile % x(1)) - 1, kind=RKIND) * (dx / supersample_fac) + start_lon

               if (lon_pt < 0.0_RKIND) then
                  lon_pt = lon_pt + 360.0_RKIND
               endif

               lat_pt = lat_pt * (pii/180.0_RKIND)
               lon_pt = lon_pt * (pii/180.0_RKIND)


               call convert_lx(x, y, z, sphere_radius, lat_pt, lon_pt)
               call mpas_kd_search(tree, (/x, y, z/), res)

               if (tile % tile (ii,jj,1) /= msgval) then

                   !
                   ! This field only matters for land cells, and for all but the outermost boundary cells,
                   ! we can safely assume that the nearest model grid cell contains the pixel (else, a different
                   ! cell would be nearest)
                   !
                   if (landmask(res % cell) == 1 .and. bdyMaskCell(res % cell) < nBdyLayers) then
                      snoalb(res % cell) = snoalb(res % cell) + tile % tile(ii,jj,1)
                      nhs(res % cell) = nhs(res % cell) + 1

                      if ( res % cell <= nCellsSolve ) then
                         ALL_PIXELS_MAPPED_TO_HALO_CELLS = .false.
                      endif
                   ! For outermost land cells, additional work is needed to verify that the pixel
                   ! actually lies within the nearest cell
                   else if (landmask(res % cell) == 1) then
                      zPixel = sphere_radius * sin(lat_pt)                  ! Model cell coordinates assume a "full" sphere radius
                      xPixel = sphere_radius * cos(lon_pt) * cos(lat_pt)    ! at this point, so we need to ues the same radius
                      yPixel = sphere_radius * sin(lon_pt) * cos(lat_pt)    ! for source pixel coordinates

                      if (in_cell(xPixel, yPixel, zPixel, xCell(res % cell), yCell(res % cell), zCell(res % cell), &
                                  nEdgesOnCell(res % cell), verticesOnCell(:,res % cell), xVertex, yVertex, zVertex)) then
                         snoalb(res % cell) = snoalb(res % cell) + tile % tile(ii,jj,1)
                         nhs(res % cell) = nhs(res % cell) + 1
                      end if
         
                      if (res % cell <= nCellsSolve) then
                         ALL_PIXELS_MAPPED_TO_HALO_CELLS = .false.
                      endif
                  end if
               end if
            enddo
         enddo
         if (.not. ALL_PIXELS_MAPPED_TO_HALO_CELLS) then
            call mpas_get_tile_neighbors(tile, up, down, left, right, geog_pool)
            call push_tile_neighbors(stack, up, down, left, right, hash, geog_pool)
         endif
      enddo
    enddo

    do iCell = 1,nCells
       !
       ! Mismatches in land mask can lead to MPAS land points with no maximum snow albedo.
       ! Ideally, we would perform a search for nearby valid albedos, but for now using
       ! the fill value will at least allow the model to run. In general, the number of cells
       ! to be treated in this way tends to be a very small fraction of the total number of cells.
       !
       if (nhs(iCell) == 0) then
          snoalb(iCell) = fillval
       else
          snoalb(iCell) = snoalb(iCell) / real(nhs(iCell))
       end if
       snoalb(iCell) = 0.01_RKIND * snoalb(iCell)        ! Convert from percent to fraction
    end do
    deallocate(nhs)

    ! Deallocate the stack
    call mpas_stack_free(stack, free_payload=.true.)

    ! Use the hash table to deallocate each tile and then the hash itself
    do i = 0, nTileX
       do j = 0, nTileY
          if (associated(hash(i, j) % ptr)) then
               deallocate(hash(i, j) % ptr % tile)
               deallocate(hash(i, j) % ptr)
          endif
       enddo
    enddo
    deallocate(hash)

    call mpas_pool_destroy_pool(geog_pool)


 else if (trim(config_maxsnowalbedo_data) == 'NCEP') then

    call mpas_log_write('Using NCEP 1.0-deg data for maximum snow albedo')

    nx = 186
    ny = 186
    nz = 1
    isigned     = 0
    endian      = 0
    wordsize    = 1
    scalefactor = 1.0
    allocate(rarray(nx,ny,nz))
    allocate(maxsnowalb(-2:363,-2:183))
    snoalb(:) = 0.0

    rarray_ptr = c_loc(rarray)

    call map_set(PROJ_LATLON, proj,  &
                 latinc = 1.0_RKIND, &
                 loninc = 1.0_RKIND, &
                 knowni = 1.0_RKIND, &
                 knownj = 1.0_RKIND, &
                 lat1 = -89.5_RKIND, &
                 lon1 = -179.5_RKIND)

    write(fname,'(a,i5.5,a1,i5.5,a1,i5.5,a1,i5.5)') trim(geog_data_path)// &
          'maxsnowalb/',1,'-',180,'.',1,'-',180
    call mpas_log_write(trim(fname))
    call mpas_f_to_c_string(fname, c_fname)

    call read_geogrid(c_fname,rarray_ptr,nx,ny,nz,isigned,endian, &
                      scalefactor,wordsize,istatus)
    call init_atm_check_read_error(istatus,fname)
    maxsnowalb(-2:180,-2:183) = rarray(1:183,1:186,1)

    write(fname,'(a,i5.5,a1,i5.5,a1,i5.5,a1,i5.5)') trim(geog_data_path)// &
          'maxsnowalb/',181,'-',360,'.',1,'-',180
    call mpas_log_write(trim(fname))
    call mpas_f_to_c_string(fname, c_fname)

    call read_geogrid(c_fname,rarray_ptr,nx,ny,nz,isigned,endian, &
                      scalefactor,wordsize,istatus)
    call init_atm_check_read_error(istatus, fname)
    maxsnowalb(181:363,-2:183) = rarray(4:186,1:186,1)

    interp_list(1) = FOUR_POINT
    interp_list(2) = W_AVERAGE4
    interp_list(3) = W_AVERAGE16
    interp_list(4) = SEARCH
    interp_list(5) = 0

    do iCell = 1,nCells

       if(landmask(iCell) == 1) then
          lat = latCell(iCell) * DEG_PER_RAD
          lon = lonCell(iCell) * DEG_PER_RAD
          call latlon_to_ij(proj, lat, lon, x, y)
          if(x < 0.5) then
             lon = lon + 360.0
             call latlon_to_ij(proj, lat, lon, x, y)
          else if (x >= 360.5) then
             lon = lon - 360.0
             call latlon_to_ij(proj, lat, lon, x, y)
          end if
          if (y < 1.0) y = 1.0
          if (y > 179.0) y = 179.0
          snoalb(iCell) = interp_sequence(x,y,1,maxsnowalb,-2,363,-2,183, &
                                            1,1,0.0_RKIND,interp_list,1)
       else
          snoalb(iCell) = 0.0
       end if

    end do
    snoalb(:) = snoalb(:) / 100.0
    deallocate(rarray)
    deallocate(maxsnowalb)

 else

    call mpas_log_write('*****************************************************************', messageType=MPAS_LOG_ERR)
    call mpas_log_write('Invalid maximum snow albedo dataset '''//trim(config_maxsnowalbedo_data) &
                                  //''' selected for config_maxsnowalbedo_data',             messageType=MPAS_LOG_ERR)
    call mpas_log_write('   Possible options are: ''MODIS'', ''NCEP''',                      messageType=MPAS_LOG_ERR)
    call mpas_log_write('*****************************************************************', messageType=MPAS_LOG_ERR)
    call mpas_log_write('Please correct the namelist.', messageType=MPAS_LOG_CRIT)

 end if

 call mpas_log_write('--- end interpolate SNOALB')


!
! Interpolate GREENFRAC
!
 if (trim(config_vegfrac_data) == 'MODIS') then

    call mpas_log_write('Using MODIS FPAR 30-arc-second data for climatological monthly vegetation fraction')

    nx = 1200
    ny = 1200
    nz = 12
    isigned  = 0
    endian   = 0
    wordsize = 1
    scalefactor = 1.0
    msgval = 200.0
    fillval = 0.0
    allocate(rarray(nx,ny,nz))
    allocate(nhs(nCells))
    nhs(:) = 0
    greenfrac(:,:) = 0.0

    rarray_ptr = c_loc(rarray)

    start_lat = -89.99583
    start_lon = -179.99583
    geog_sub_path = 'greenfrac_fpar_modis/'

    do jTileStart = 1,20401,ny
       jTileEnd = jTileStart + ny - 1

       do iTileStart=1,42001,nx
          iTileEnd = iTileStart + nx - 1
          write(fname,'(a,i5.5,a1,i5.5,a1,i5.5,a1,i5.5)') trim(geog_data_path)//trim(geog_sub_path), &
                       iTileStart,'-',iTileEnd,'.',jTileStart,'-',jTileEnd
          call mpas_log_write(trim(fname))
          call mpas_f_to_c_string(fname, c_fname)

          call read_geogrid(c_fname,rarray_ptr,nx,ny,nz,isigned,endian, &
                            scalefactor,wordsize,istatus)
          call init_atm_check_read_error(istatus, fname)

          iPoint = 1
          do j=1,ny
          do i=1,nx
             lat_pt = start_lat + (jTileStart + j - 2) * 0.0083333333
             lon_pt = start_lon + (iTileStart + i - 2) * 0.0083333333
             lat_pt = lat_pt * PI / 180.0
             lon_pt = lon_pt * PI / 180.0

             iPoint = nearest_cell(lat_pt,lon_pt,iPoint,nCells,maxEdges, &
                                   nEdgesOnCell,cellsOnCell, &
                                   latCell,lonCell)

             !
             ! This field only matters for land cells, and for all but the outermost boundary cells,
             ! we can safely assume that the nearest model grid cell contains the pixel (else, a different
             ! cell would be nearest)
             !
             if (landmask(iPoint) == 1 .and. bdyMaskCell(iPoint) < nBdyLayers) then
                do k=1,nz
                   if (rarray(i,j,k) == msgval) then
                      rarray(i,j,k) = fillval
                   end if
                   greenfrac(k,iPoint) = greenfrac(k,iPoint) + rarray(i,j,k)
                end do
                nhs(iPoint) = nhs(iPoint) + 1

             ! For outermost land cells, additional work is needed to verify that the pixel
             ! actually lies within the nearest cell
             else if (landmask(iPoint) == 1) then
                zPixel = sphere_radius * sin(lat_pt)                  ! Model cell coordinates assume a "full" sphere radius
                xPixel = sphere_radius * cos(lon_pt) * cos(lat_pt)    ! at this point, so we need to ues the same radius
                yPixel = sphere_radius * sin(lon_pt) * cos(lat_pt)    ! for source pixel coordinates

                if (in_cell(xPixel, yPixel, zPixel, xCell(iPoint), yCell(iPoint), zCell(iPoint), &
                            nEdgesOnCell(iPoint), verticesOnCell(:,iPoint), xVertex, yVertex, zVertex)) then
                   do k=1,nz
                      if (rarray(i,j,k) == msgval) then
                         rarray(i,j,k) = fillval
                      end if
                      greenfrac(k,iPoint) = greenfrac(k,iPoint) + rarray(i,j,k)
                   end do
                   nhs(iPoint) = nhs(iPoint) + 1
                end if
             end if
          end do
          end do

       end do
    end do

    do iCell = 1,nCells
       ! For land points that have no overlap with valid data, and for water points,
       ! just use the fill value...
       if (nhs(iCell) == 0) then
          greenfrac(:,iCell) = fillval
       else
          greenfrac(:,iCell) = greenfrac(:,iCell) / real(nhs(iCell))
       end if
       shdmin(iCell) = minval(greenfrac(:,iCell))
       shdmax(iCell) = maxval(greenfrac(:,iCell))
    end do
    deallocate(rarray)
    deallocate(nhs)

 else if (trim(config_vegfrac_data) == 'NCEP') then

    call mpas_log_write('Using NCEP 0.144-deg data for climatological monthly vegetation fraction')

    nx = 1256
    ny = 1256
    nz = 12
    isigned     = 0
    endian      = 0
    wordsize    = 1
    scalefactor = 1.0
    allocate(rarray(nx,ny,nz))
    allocate(vegfra(-2:2503,-2:1253,12))
    greenfrac(:,:) = 0.0

    rarray_ptr = c_loc(rarray)

    call map_set(PROJ_LATLON, proj,    &
                 latinc = 0.144_RKIND, &
                 loninc = 0.144_RKIND, &
                 knowni = 1.0_RKIND,   &
                 knownj = 1.0_RKIND,   &
                 lat1 = -89.928_RKIND, &
                 lon1 = -179.928_RKIND)

    write(fname,'(a,i5.5,a1,i5.5,a1,i5.5,a1,i5.5)') trim(geog_data_path)// &
          'greenfrac/',1,'-',1250,'.',1,'-',1250
    call mpas_log_write(trim(fname))
    call mpas_f_to_c_string(fname, c_fname)

    call read_geogrid(c_fname,rarray_ptr,nx,ny,nz,isigned,endian, &
                      scalefactor,wordsize,istatus)
    call init_atm_check_read_error(istatus,fname)
    vegfra(-2:1250,-2:1253,1:12) = rarray(1:1253,1:1256,1:12)

    write(fname,'(a,i5.5,a1,i5.5,a1,i5.5,a1,i5.5)') trim(geog_data_path)// &
          'greenfrac/',1251,'-',2500,'.',1,'-',1250
    call mpas_log_write(trim(fname))
    call mpas_f_to_c_string(fname, c_fname)

    call read_geogrid(c_fname,rarray_ptr,nx,ny,nz,isigned,endian, &
                      scalefactor,wordsize,istatus)
    call init_atm_check_read_error(istatus,fname)
    vegfra(1251:2503,-2:1253,1:12) = rarray(4:1256,1:1256,1:12)

    do iCell = 1,nCells

       if (landmask(iCell) == 1) then
          lat = latCell(iCell) * DEG_PER_RAD
          lon = lonCell(iCell) * DEG_PER_RAD
          call latlon_to_ij(proj, lat, lon, x, y)
          if(x < 0.5) then
             lon = lon + 360.0
             call latlon_to_ij(proj, lat, lon, x, y)
          else if(x >= 2500.5) then
             lon = lon - 360.0
             call latlon_to_ij(proj, lat, lon, x, y)
          end if
          if (y < 1.0) y = 1.0
          if (y > 1249.0) y = 1249.0
          do k = 1,12
             greenfrac(k,iCell) = interp_sequence(x,y,k,vegfra,-2,2503,-2,1253, &
                                                    1,12,-1.e30_RKIND,interp_list,1)
          end do
       else
          greenfrac(:,iCell) = 0.0
       end if
       shdmin(iCell) = minval(greenfrac(:,iCell))
       shdmax(iCell) = maxval(greenfrac(:,iCell))

    end do
    deallocate(rarray)
    deallocate(vegfra)

 else

    call mpas_log_write('*****************************************************************', messageType=MPAS_LOG_ERR)
    call mpas_log_write('Invalid monthly vegetation fraction dataset '''//trim(config_vegfrac_data) &
                                  //''' selected for config_vegfrac_data',                   messageType=MPAS_LOG_ERR)
    call mpas_log_write('   Possible options are: ''MODIS'', ''NCEP''',                      messageType=MPAS_LOG_ERR)
    call mpas_log_write('*****************************************************************', messageType=MPAS_LOG_ERR)
    call mpas_log_write('Please correct the namelist.', messageType=MPAS_LOG_CRIT)

 end if

 call mpas_log_write('--- end interpolate GREENFRAC')


!
! Interpolate ALBEDO12M
!
 if (trim(config_albedo_data) == 'MODIS') then

    call mpas_log_write('Using MODIS 0.05-deg data for climatological monthly albedo')
    if (supersample_fac > 1) then
       call mpas_log_write('   Dataset will be supersampled by a factor of $i', intArgs=(/supersample_fac/))
    end if

    nx = 1206
    ny = 1206
    nz = 12
    isigned  = 1
    endian   = 0
    wordsize = 2
    scalefactor = 0.01
    msgval = real(-999.0,kind=R4KIND)*real(0.01,kind=R4KIND)
    fillval = 8.0
    allocate(rarray(nx,ny,nz))
    allocate(nhs(nCells))
    nhs(:) = 0
    albedo12m(:,:) = 0.0

    rarray_ptr = c_loc(rarray)

    start_lat = 90.0 - 0.05 * 0.5 / supersample_fac
    start_lon = -180.0 + 0.05 * 0.5 / supersample_fac
    geog_sub_path = 'albedo_modis/'

    do jTileStart = 1,02401,ny-6
       jTileEnd = jTileStart + ny - 1 - 6

       do iTileStart=1,06001,nx-6
          iTileEnd = iTileStart + nx - 1 - 6
          write(fname,'(a,i5.5,a1,i5.5,a1,i5.5,a1,i5.5)') trim(geog_data_path)//trim(geog_sub_path), &
                       iTileStart,'-',iTileEnd,'.',jTileStart,'-',jTileEnd
          call mpas_log_write(trim(fname))
          call mpas_f_to_c_string(fname, c_fname)

          call read_geogrid(c_fname,rarray_ptr,nx,ny,nz,isigned,endian, &
                            scalefactor,wordsize,istatus)
          call init_atm_check_read_error(istatus, fname)

          iPoint = 1
          do j=supersample_fac * 3 + 1, supersample_fac * (ny-3)
          do i=supersample_fac * 3 + 1, supersample_fac * (nx-3)
             ii = (i - 1) / supersample_fac + 1
             jj = (j - 1) / supersample_fac + 1

             lat_pt = start_lat - (supersample_fac*(jTileStart-1) + j - (supersample_fac*3+1)) * 0.05 / supersample_fac
             lon_pt = start_lon + (supersample_fac*(iTileStart-1) + i - (supersample_fac*3+1)) * 0.05 / supersample_fac
             lat_pt = lat_pt * PI / 180.0
             lon_pt = lon_pt * PI / 180.0

             iPoint = nearest_cell(lat_pt,lon_pt,iPoint,nCells,maxEdges, &
                                   nEdgesOnCell,cellsOnCell, &
                                   latCell,lonCell)

             !
             ! This field only matters for land cells, and for all but the outermost boundary cells,
             ! we can safely assume that the nearest model grid cell contains the pixel (else, a different
             ! cell would be nearest)
             !
             if (landmask(iPoint) == 1 .and. bdyMaskCell(iPoint) < nBdyLayers) then
                do k=1,nz
                   if (rarray(ii,jj,k) == msgval) then
                      rarray(ii,jj,k) = fillval
                   end if
                   albedo12m(k,iPoint) = albedo12m(k,iPoint) + rarray(ii,jj,k)
                end do
                nhs(iPoint) = nhs(iPoint) + 1

             ! For outermost land cells, additional work is needed to verify that the pixel
             ! actually lies within the nearest cell
             else if (landmask(iPoint) == 1) then
                zPixel = sphere_radius * sin(lat_pt)                  ! Model cell coordinates assume a "full" sphere radius
                xPixel = sphere_radius * cos(lon_pt) * cos(lat_pt)    ! at this point, so we need to ues the same radius
                yPixel = sphere_radius * sin(lon_pt) * cos(lat_pt)    ! for source pixel coordinates

                if (in_cell(xPixel, yPixel, zPixel, xCell(iPoint), yCell(iPoint), zCell(iPoint), &
                            nEdgesOnCell(iPoint), verticesOnCell(:,iPoint), xVertex, yVertex, zVertex)) then
                   do k=1,nz
                      if (rarray(ii,jj,k) == msgval) then
                         rarray(ii,jj,k) = fillval
                      end if
                      albedo12m(k,iPoint) = albedo12m(k,iPoint) + rarray(ii,jj,k)
                   end do
                   nhs(iPoint) = nhs(iPoint) + 1
                end if
             end if
          end do
          end do

       end do
    end do

    do iCell = 1,nCells
       ! For land points that have no overlap with valid data, and for water points,
       ! just use the fill value...
       if (nhs(iCell) == 0) then
          albedo12m(:,iCell) = fillval
       else
          albedo12m(:,iCell) = albedo12m(:,iCell) / real(nhs(iCell))
       end if
       if (lu_index(iCell) == isice_lu) then
          albedo12m(:,iCell) = 70.0
       end if
    end do
    deallocate(rarray)
    deallocate(nhs)

 else if (trim(config_albedo_data) == 'NCEP') then

    call mpas_log_write('Using NCEP 0.144-deg data for climatological monthly albedo')

    nx = 1256
    ny = 1256
    nz = 12
    isigned     = 0
    endian      = 0
    wordsize    = 1
    scalefactor = 1.0
    allocate(rarray(nx,ny,nz))
    allocate(vegfra(-2:2503,-2:1253,12))
    albedo12m(:,:) = 0.0

    rarray_ptr = c_loc(rarray)

    call map_set(PROJ_LATLON, proj,    &
                 latinc = 0.144_RKIND, &
                 loninc = 0.144_RKIND, &
                 knowni = 1.0_RKIND,   &
                 knownj = 1.0_RKIND,   &
                 lat1 = -89.928_RKIND, &
                 lon1 = -179.928_RKIND)

    write(fname,'(a,i5.5,a1,i5.5,a1,i5.5,a1,i5.5)') trim(geog_data_path)// &
          'albedo_ncep/',1,'-',1250,'.',1,'-',1250
    call mpas_log_write(trim(fname))
    call mpas_f_to_c_string(fname, c_fname)

    call read_geogrid(c_fname,rarray_ptr,nx,ny,nz,isigned,endian, &
                      scalefactor, wordsize, istatus)
    call init_atm_check_read_error(istatus,fname)
    vegfra(-2:1250,-2:1253,1:12) = rarray(1:1253,1:1256,1:12)

    write(fname,'(a,i5.5,a1,i5.5,a1,i5.5,a1,i5.5)') trim(geog_data_path)// &
          'albedo_ncep/',1251,'-',2500,'.',1,'-',1250
    call mpas_log_write(trim(fname))
    call mpas_f_to_c_string(fname, c_fname)

    call read_geogrid(c_fname,rarray_ptr,nx,ny,nz,isigned,endian, &
                      scalefactor,wordsize,istatus)
    call init_atm_check_read_error(istatus,fname)
    vegfra(1251:2503,-2:1253,1:12) = rarray(4:1256,1:1256,1:12)

    do iCell = 1,nCells

       if (landmask(iCell) == 1) then
          lat = latCell(iCell) * DEG_PER_RAD
          lon = lonCell(iCell) * DEG_PER_RAD
          call latlon_to_ij(proj, lat, lon, x, y)
          if(x < 0.5) then
             lon = lon + 360.0
             call latlon_to_ij(proj, lat, lon, x, y)
          else if(x >= 2500.5) then
             lon = lon - 360.0
             call latlon_to_ij(proj, lat, lon, x, y)
          end if
          if (y < 1.0) y = 1.0
          if (y > 1249.0) y = 1249.0
          do k = 1,12
             albedo12m(k,iCell) = interp_sequence(x,y,k,vegfra,-2,2503,-2,1253, &
                                                    1,12,0.0_RKIND,interp_list,1)
          end do
       else
          albedo12m(:,iCell) = 8.0
       end if
    end do
    deallocate(rarray)
    deallocate(vegfra)

 else

    call mpas_log_write('*****************************************************************', messageType=MPAS_LOG_ERR)
    call mpas_log_write('Invalid monthly albedo dataset '''//trim(config_albedo_data) &
                                  //''' selected for config_albedo_data',                    messageType=MPAS_LOG_ERR)
    call mpas_log_write('   Possible options are: ''MODIS'', ''NCEP''',                      messageType=MPAS_LOG_ERR)
    call mpas_log_write('*****************************************************************', messageType=MPAS_LOG_ERR)
    call mpas_log_write('Please correct the namelist.', messageType=MPAS_LOG_CRIT)

 end if

 call mpas_log_write('--- end interpolate ALBEDO12M')

 end subroutine init_atm_static

 !***********************************************************************
 !
 !  subroutine push_tile_neighbors
 !
 !> \brief   Push a tiles neighbors onto the stack
 !> \author  Miles A. Curry
 !> \date    03/29/2019
 !> \details 
 !>  Given that up, down, left, right contain their relevant hashes,
 !>  (from mpas_get_tile_neighbors) get and push the relevant tiles onto
 !>  the stack to be processed.
 !
 !-----------------------------------------------------------------------
 subroutine push_tile_neighbors(stack, up, down, left, right, tile_hash, geo_pool)

    use mpas_stack, only : node
    use mpas_stack, only : payload_t
    use mpas_stack, only : mpas_stack_is_empty
    use mpas_stack, only : mpas_stack_push

    use mpas_geotile_manager, only : tile_t, hash_table
    use mpas_geotile_manager, only : mpas_get_tile
    use mpas_geotile_manager, only : mpas_hash_to_latlon
    use mpas_geotile_manager, only : mpas_stack_pop_tile

    implicit none

    type (node), intent(inout), pointer :: stack
    type (tile_t), intent(inout) :: up
    type (tile_t), intent(inout) :: down
    type (tile_t), intent(inout) :: left
    type (tile_t), intent(inout) :: right
    type (hash_table), pointer, dimension(:, :), intent(inout) :: tile_hash
    type (mpas_pool_type), intent(inout), pointer :: geo_pool

    type (tile_t), pointer :: tile
    real (kind=RKIND) :: lat
    real (kind=RKIND) :: lon

    ! Right
    tile => null()
    call mpas_hash_to_latlon(right % hash_x, right % hash_y, lat, lon, geo_pool)
    call mpas_get_tile(lat, lon, tile, tile_hash, geo_pool)
    stack => mpas_stack_push(stack, tile)

    ! Left
    tile => null()
    call mpas_hash_to_latlon(left % hash_x, left % hash_y, lat, lon, geo_pool)
    call mpas_get_tile(lat, lon, tile, tile_hash, geo_pool)
    stack => mpas_stack_push(stack, tile)

    ! Up
    tile => null()
    call mpas_hash_to_latlon(up % hash_x, up % hash_y, lat, lon, geo_pool)
    call mpas_get_tile(lat, lon, tile, tile_hash, geo_pool)
    stack => mpas_stack_push(stack, tile)
    
    ! Down
    tile => null()
    call mpas_hash_to_latlon(down % hash_x, down % hash_y, lat, lon, geo_pool)
    call mpas_get_tile(lat, lon, tile, tile_hash, geo_pool)
    stack => mpas_stack_push(stack, tile)

 end subroutine push_tile_neighbors

!==================================================================================================
 subroutine init_atm_check_read_error(istatus, fname)
!==================================================================================================
 implicit none

 integer, intent(in) :: istatus
 character (len=*), intent(in) :: fname

 if (istatus /= 0) then
     call mpas_log_write('Could not read file '//trim(fname), messageType=MPAS_LOG_CRIT)
 end if

 end subroutine init_atm_check_read_error

 !***********************************************************************
 !
 !  function convert_lx
 !
 !> \brief   Convert a latitude and longitude to cartisian x, y, z
 !> \author  Michael G. Duda
 !> \details 
 !>  Convert a lat, lon coordinate to an x, y, z using a given radius.
 !
 !-----------------------------------------------------------------------
 subroutine convert_lx(x, y, z, radius, lat, lon)

    implicit none

    real (kind=RKIND), intent(in) :: radius
    real (kind=RKIND), intent(in) :: lat
    real (kind=RKIND), intent(in) :: lon
    real (kind=RKIND), intent(out) :: x, y, z

    z = radius * sin(lat)
    x = radius * cos(lon) * cos(lat)
    y = radius * sin(lon) * cos(lat)

 end subroutine convert_lx


!==================================================================================================
 integer function nearest_cell(target_lat, target_lon, start_cell, nCells, maxEdges, &
                               nEdgesOnCell, cellsOnCell, latCell, lonCell)
!==================================================================================================
 implicit none

 real (kind=RKIND), intent(in) :: target_lat, target_lon
 integer, intent(in) :: start_cell
 integer, intent(in) :: nCells, maxEdges
 integer, dimension(nCells), intent(in) :: nEdgesOnCell
 integer, dimension(maxEdges,nCells), intent(in) :: cellsOnCell
 real (kind=RKIND), dimension(nCells), intent(in) :: latCell, lonCell

 integer :: i
 integer :: iCell
 integer :: current_cell
 real (kind=RKIND) :: current_distance, d
 real (kind=RKIND) :: nearest_distance

 nearest_cell = start_cell
 current_cell = -1

 do while (nearest_cell /= current_cell)
    current_cell = nearest_cell
    current_distance = sphere_distance(latCell(current_cell), lonCell(current_cell), target_lat, &
                                       target_lon, 1.0_RKIND)
    nearest_cell = current_cell
    nearest_distance = current_distance
    do i = 1, nEdgesOnCell(current_cell)
       iCell = cellsOnCell(i,current_cell)
       if (iCell <= nCells) then
          d = sphere_distance(latCell(iCell), lonCell(iCell), target_lat, target_lon, 1.0_RKIND)
          if (d < nearest_distance) then
             nearest_cell = iCell
             nearest_distance = d
          end if
       end if
    end do
 end do

 end function nearest_cell

!==================================================================================================
 real (kind=RKIND) function sphere_distance(lat1, lon1, lat2, lon2, radius)

!Compute the great-circle distance between (lat1, lon1) and (lat2, lon2) on a
!sphere with given radius.
!==================================================================================================
 implicit none

 real (kind=RKIND), intent(in) :: lat1, lon1, lat2, lon2, radius
 real (kind=RKIND) :: arg1

 arg1 = sqrt( sin(0.5*(lat2-lat1))**2 +  &
              cos(lat1)*cos(lat2)*sin(0.5*(lon2-lon1))**2 )
 sphere_distance = 2.*radius*asin(arg1)

 end function sphere_distance


!-----------------------------------------------------------------------
!  routine mirror_point
!
!> \brief Finds the "mirror" of a point about a great-circle arc
!> \author Michael Duda
!> \date   7 March 2019
!> \details
!>  Given the endpoints of a great-circle arc (A,B) and a point, computes
!>  the location of the point on the opposite side of the arc along a great-
!>  circle arc that intersects (A,B) at a right angle, and such that the arc
!>  between the point and its mirror is bisected by (A,B).
!>
!>  Assumptions: A, B, and the point to be reflected all lie on the surface
!>  of the unit sphere.
!
!-----------------------------------------------------------------------
subroutine mirror_point(xPoint, yPoint, zPoint, xA, yA, zA, xB, yB, zB, xMirror, yMirror, zMirror)

   implicit none

   real(kind=RKIND), intent(in) :: xPoint, yPoint, zPoint
   real(kind=RKIND), intent(in) :: xA, yA, zA
   real(kind=RKIND), intent(in) :: xB, yB, zB
   real(kind=RKIND), intent(out) :: xMirror, yMirror, zMirror

   real(kind=RKIND) :: alpha

   !
   ! Find the spherical angle between arcs AP and AB (where P is the point to be reflected)
   !
   alpha = sphere_angle(xA, yA, zA, xPoint, yPoint, zPoint, xB, yB, zB)

   !
   ! Rotate the point to be reflected by twice alpha about the vector from the origin to A
   !
   call rotate_about_vector(xPoint, yPoint, zPoint, 2.0_RKIND * alpha, 0.0_RKIND, 0.0_RKIND, 0.0_RKIND, &
                            xA, yA, zA, xMirror, yMirror, zMirror)

end subroutine mirror_point


!-----------------------------------------------------------------------
!  routine rotate_about_vector
!
!> \brief Rotates a point about a vector in R3
!> \author Michael Duda
!> \date   7 March 2019
!> \details
!>  Rotates the point (x,y,z) through an angle theta about the vector
!>  originating at (a, b, c) and having direction (u, v, w).
!
!>  Reference: https://sites.google.com/site/glennmurray/Home/rotation-matrices-and-formulas/rotation-about-an-arbitrary-axis-in-3-dimensions
!
!-----------------------------------------------------------------------
subroutine rotate_about_vector(x, y, z, theta, a, b, c, u, v, w, xp, yp, zp)

   implicit none

   real (kind=RKIND), intent(in) :: x, y, z, theta, a, b, c, u, v, w
   real (kind=RKIND), intent(out) :: xp, yp, zp

   real (kind=RKIND) :: vw2, uw2, uv2
   real (kind=RKIND) :: m

   vw2 = v**2.0 + w**2.0
   uw2 = u**2.0 + w**2.0
   uv2 = u**2.0 + v**2.0
   m = sqrt(u**2.0 + v**2.0 + w**2.0)

   xp = (a*vw2 + u*(-b*v-c*w+u*x+v*y+w*z) + ((x-a)*vw2+u*(b*v+c*w-v*y-w*z))*cos(theta) + m*(-c*v+b*w-w*y+v*z)*sin(theta))/m**2.0
   yp = (b*uw2 + v*(-a*u-c*w+u*x+v*y+w*z) + ((y-b)*uw2+v*(a*u+c*w-u*x-w*z))*cos(theta) + m*( c*u-a*w+w*x-u*z)*sin(theta))/m**2.0
   zp = (c*uv2 + w*(-a*u-b*v+u*x+v*y+w*z) + ((z-c)*uv2+w*(a*u+b*v-u*x-v*y))*cos(theta) + m*(-b*u+a*v-v*x+u*y)*sin(theta))/m**2.0

end subroutine rotate_about_vector


!-----------------------------------------------------------------------
!  routine in_cell
!
!> \brief Determines whether a point is within a Voronoi cell
!> \author Michael Duda
!> \date   7 March 2019
!> \details
!>  Given a point on the surface of the sphere, the corner points of a Voronoi
!>  cell, and the generating point for that Voronoi cell, determines whether
!>  the given point is within the Voronoi cell.
!
!-----------------------------------------------------------------------
logical function in_cell(xPoint, yPoint, zPoint, xCell, yCell, zCell, &
                         nEdgesOnCell, verticesOnCell, xVertex, yVertex, zVertex)

   use mpas_geometry_utils, only : mpas_arc_length

   implicit none

   real(kind=RKIND), intent(in) :: xPoint, yPoint, zPoint
   real(kind=RKIND), intent(in) :: xCell, yCell, zCell
   integer, intent(in) :: nEdgesOnCell
   integer, dimension(:), intent(in) :: verticesOnCell
   real(kind=RKIND), dimension(:), intent(in) :: xVertex, yVertex, zVertex

   integer :: i
   integer :: vtx1, vtx2
   real(kind=RKIND) :: xNeighbor, yNeighbor, zNeighbor
   real(kind=RKIND) :: inDist, outDist
   real(kind=RKIND) :: radius
   real(kind=RKIND) :: radius_inv

   radius = sqrt(xCell * xCell + yCell * yCell + zCell * zCell)
   radius_inv = 1.0_RKIND / radius

   inDist = mpas_arc_length(xPoint, yPoint, zPoint, xCell, yCell, zCell)

   in_cell = .true.

   do i=1,nEdgesOnCell
      vtx1 = verticesOnCell(i)
      vtx2 = verticesOnCell(mod(i,nEdgesOnCell)+1)

      call mirror_point(xCell*radius_inv, yCell*radius_inv, zCell*radius_inv, &
                        xVertex(vtx1)*radius_inv, yVertex(vtx1)*radius_inv, zVertex(vtx1)*radius_inv, &
                        xVertex(vtx2)*radius_inv, yVertex(vtx2)*radius_inv, zVertex(vtx2)*radius_inv, &
                        xNeighbor, yNeighbor, zNeighbor)

      xNeighbor = xNeighbor * radius
      yNeighbor = yNeighbor * radius
      zNeighbor = zNeighbor * radius

      outDist = mpas_arc_length(xPoint, yPoint, zPoint, xNeighbor, yNeighbor, zNeighbor)

      if (outDist < inDist) then
         in_cell = .false.
         return
      end if

   end do

end function in_cell


!==================================================================================================
 end module mpas_init_atm_static
!==================================================================================================
