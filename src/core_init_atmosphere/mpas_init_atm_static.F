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

 use mpas_geometry_utils, only : mpas_in_cell
 use mpas_atmphys_utilities

 use mpas_kd_tree, only : mpas_kd_type, mpas_kd_construct, mpas_kd_free, mpas_kd_search
 use mpas_geotile_manager, only : mpas_geotile_mgr_type, mpas_geotile_type, mpas_latlon_to_xyz

 use iso_c_binding, only : c_char, c_int, c_float, c_loc, c_ptr

 implicit none
 private
 public:: init_atm_static,           &
          init_atm_check_read_error, &
          nearest_cell,              &
          sphere_distance

 !constants
 integer, parameter :: nBdyLayers = 7   ! The number of relaxation layers plus the number of specified layers
                                        ! This value is used in determining whether extra checks are needed
                                        ! in the remapping of terrain, land use, and soil category pixels

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

 ! Abstract interface for determining if the cell, iCell, needs to be added to the stack
 ! for processing. Returning .true. will indicate that the cell has not received any mappings
 ! and needs to be processed. .false. will indicate that the cell has received mappings.
 abstract interface
    function interp_criteria_function(iCell)
        integer, intent(in) :: iCell
        logical :: interp_criteria_function
    end function interp_criteria_function
 end interface

 ! Abstract interface to accumulate pixel values with the cell they map to.
 abstract interface
    subroutine interp_accumulation_function(iCell, i8val)
        use mpas_derived_types, only : I8KIND
        integer, intent(in) :: iCell
        integer (kind=I8KIND), intent(in) :: i8val
    end subroutine interp_accumulation_function
 end interface

 contains

!==================================================================================================
 subroutine init_atm_static(mesh, dims, configs)
!==================================================================================================

!inout arguments:
 type (mpas_pool_type), intent(inout) :: mesh
 type (mpas_pool_type), intent(in) :: dims
 type (mpas_pool_type), intent(in) :: configs


!local variables:
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

 integer(c_int):: nx,ny,nz
 integer(c_int):: endian,isigned,istatus,wordsize
 integer:: i,j,k
 integer :: ii, jj
 integer:: iCell,iEdge,iVtx,iPoint,iTileStart,iTileEnd,jTileStart,jTileEnd
 integer,dimension(5) :: interp_list
 integer,dimension(:),allocatable  :: nhs
 integer,dimension(:,:),allocatable:: ncat
      
 real(kind=RKIND), pointer :: scalefactor_ptr
 real(kind=RKIND) :: scalefactor
 real(kind=c_float),dimension(:,:,:),pointer,contiguous :: rarray
 type(c_ptr) :: rarray_ptr

 real(kind=RKIND):: start_lat
 real(kind=RKIND):: start_lon

 integer, pointer :: supersample_fac

 real(kind=RKIND):: lat,lon,x,y,z
 real(kind=RKIND):: lat_pt,lon_pt
 real(kind=RKIND),dimension(:,:),allocatable  :: soiltemp_1deg
 real(kind=RKIND),dimension(:,:),allocatable  :: maxsnowalb
 real(kind=RKIND),dimension(:,:,:),allocatable:: vegfra

 integer, pointer :: isice_lu, iswater_lu
 integer, pointer :: isice_lu_ptr, iswater_lu_ptr
 integer :: iswater_soil
 integer, pointer :: iswater_soil_ptr
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
 integer (kind=I8KIND), dimension(:), pointer :: ter_integer
 integer (kind=I8KIND), dimension(:,:), pointer :: greenfrac_int
 real (kind=RKIND), dimension(:), pointer :: soiltemp
 real (kind=RKIND), dimension(:), pointer :: snoalb
 integer (kind=I8KIND), dimension(:), pointer :: snoalb_integer
 real (kind=RKIND), dimension(:), pointer :: shdmin, shdmax
 real (kind=RKIND), dimension(:,:), pointer :: greenfrac
 real (kind=RKIND), dimension(:,:), pointer :: albedo12m
 integer (kind=I8KIND), dimension(:,:), pointer :: albedo12m_int
 real (kind=RKIND) :: msgval, fillval
 real (kind=RKIND), pointer :: missing_value
 integer, pointer :: category_min, category_max
 integer, dimension(:), pointer :: lu_index
 integer, dimension(:), pointer :: soilcat_top
 integer, dimension(:), pointer :: landmask
 integer, dimension(:), pointer :: bdyMaskCell
 character(len=StrKIND), pointer :: mminlu

 real (kind=RKIND) :: xPixel, yPixel, zPixel

 type (mpas_kd_type), dimension(:), pointer :: kd_points
 type (mpas_kd_type), pointer :: tree
 type (mpas_kd_type), pointer :: res

 type (mpas_geotile_mgr_type) :: mgr
 type (mpas_geotile_type), pointer :: tile

 real (kind=RKIND) :: tval
 integer (kind=I8KIND) :: i8val
 integer, pointer :: tile_bdr
 integer, pointer :: tile_nx, tile_ny, tile_nz
 integer, pointer :: tile_z_start, tile_z_end

 logical :: all_pixels_mapped_to_halo_cells
 integer :: ierr

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
 call mpas_pool_get_array(mesh, 'isice_lu', isice_lu)
 call mpas_pool_get_array(mesh, 'iswater_lu', iswater_lu)
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
 call mpas_pool_get_dimension(dims, 'nCellsSolve', nCellsSolve)
 call mpas_pool_get_dimension(dims, 'nEdges', nEdges)
 call mpas_pool_get_dimension(dims, 'nVertices', nVertices)
 call mpas_pool_get_dimension(dims, 'maxEdges', maxEdges)

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

!
! Initialize the KD-Tree
!
 allocate(kd_points(nCells))
 do i = 1, nCells
     allocate(kd_points(i) % point(3))
     kd_points(i) % point = (/xCell(i), yCell(i), zCell(i)/)
     kd_points(i) % id = i ! Cell ID
 enddo
 tree => null()
 tree => mpas_kd_construct(kd_points, 3)
 if (.not. associated(tree)) then
     call mpas_log_write('Error creating the KD-Tree for static interpolation', messageType=MPAS_LOG_CRIT)
 endif


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
       write(mminlu,'(a)') 'USGS'
    case('MODIFIED_IGBP_MODIS_NOAH')
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

 call mpas_log_write('--- end interpolate LU_INDEX')

!
! Interpolate SOILCAT_TOP
!
 geog_sub_path = 'soiltype_top_30s/'

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! KLUDGE TO FIX SOIL TYPE OVER ANTARCTICA
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 where (lu_index == isice_lu) soilcat_top = 16

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! CORRECT INCONSISTENT SOIL AND LAND USE DATA
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 do iCell = 1,nCells
    if (lu_index(iCell) == iswater_lu .or. &
        soilcat_top(iCell) == iswater_soil) then
        if (lu_index(iCell) /= iswater_lu) then
            call mpas_log_write('Turning lu_index into water at $i', intArgs=(/iCell/))
            lu_index(iCell) = iswater_lu
        end if
        if (soilcat_top(iCell) /= iswater_soil) then
            call mpas_log_write('Turning soilcat_top into water at $i', intArgs=(/iCell/))
            soilcat_top(iCell) = iswater_soil
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
                   wordsize,istatus)
 call init_atm_check_read_error(istatus, fname)
 rarray(:,:,:) = rarray(:,:,:) * scalefactor
 soiltemp_1deg(-2:180,-2:183) = rarray(1:183,1:186,1)

 write(fname,'(a,i5.5,a1,i5.5,a1,i5.5,a1,i5.5)') trim(geog_data_path)// &
            'soiltemp_1deg/',181,'-',360,'.',1,'-',180
 call mpas_log_write(trim(fname))
 call mpas_f_to_c_string(fname, c_fname)

 call read_geogrid(c_fname,rarray_ptr,nx,ny,nz,isigned,endian, &
                   wordsize,istatus)
 call init_atm_check_read_error(istatus,fname)
 rarray(:,:,:) = rarray(:,:,:) * scalefactor
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
                      wordsize,istatus)
    call init_atm_check_read_error(istatus,fname)
    rarray(:,:,:) = rarray(:,:,:) * scalefactor
    maxsnowalb(-2:180,-2:183) = rarray(1:183,1:186,1)

    write(fname,'(a,i5.5,a1,i5.5,a1,i5.5,a1,i5.5)') trim(geog_data_path)// &
          'maxsnowalb/',181,'-',360,'.',1,'-',180
    call mpas_log_write(trim(fname))
    call mpas_f_to_c_string(fname, c_fname)

    call read_geogrid(c_fname,rarray_ptr,nx,ny,nz,isigned,endian, &
                      wordsize,istatus)
    call init_atm_check_read_error(istatus, fname)
    rarray(:,:,:) = rarray(:,:,:) * scalefactor
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
                      wordsize,istatus)
    call init_atm_check_read_error(istatus,fname)
    rarray(:,:,:) = rarray(:,:,:) * scalefactor
    vegfra(-2:1250,-2:1253,1:12) = rarray(1:1253,1:1256,1:12)

    write(fname,'(a,i5.5,a1,i5.5,a1,i5.5,a1,i5.5)') trim(geog_data_path)// &
          'greenfrac/',1251,'-',2500,'.',1,'-',1250
    call mpas_log_write(trim(fname))
    call mpas_f_to_c_string(fname, c_fname)

    call read_geogrid(c_fname,rarray_ptr,nx,ny,nz,isigned,endian, &
                      wordsize,istatus)
    call init_atm_check_read_error(istatus,fname)
    rarray(:,:,:) = rarray(:,:,:) * scalefactor
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
                      wordsize, istatus)
    call init_atm_check_read_error(istatus,fname)
    rarray(:,:,:) = rarray(:,:,:) * scalefactor
    vegfra(-2:1250,-2:1253,1:12) = rarray(1:1253,1:1256,1:12)

    write(fname,'(a,i5.5,a1,i5.5,a1,i5.5,a1,i5.5)') trim(geog_data_path)// &
          'albedo_ncep/',1251,'-',2500,'.',1,'-',1250
    call mpas_log_write(trim(fname))
    call mpas_f_to_c_string(fname, c_fname)

    call read_geogrid(c_fname,rarray_ptr,nx,ny,nz,isigned,endian, &
                      wordsize,istatus)
    call init_atm_check_read_error(istatus,fname)
    rarray(:,:,:) = rarray(:,:,:) * scalefactor
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

!
! Deallocate and free the KD Tree
!
 call mpas_kd_free(tree)
 deallocate(kd_points)


 end subroutine init_atm_static

 !***********************************************************************
 !
 !  routine init_atm_map_static_data
 !
 !> \brief   Map values of static datasets to cells
 !> \author  Miles Curry
 !> \date    25 January 2020
 !> \details
 !>  Given a initialized geotile manager object, and an initialized KD tree built of
 !> xCell, yCell, zCells, and two function pointers, this subroutine maps values of each
 !> pixel to the cell it resides in. Because this routine uses a K-Dimensional tree
 !> to map pixels to cells, it can safely map pixels to cells on a non-cvt decomposition
 !> in parallel.
 !>
 !> The procedure interp_criteria will need to match the interp_criteria_function abstract
 !> interface. The procedure is used to determine if the cell passed in needs to be processed
 !> or not. Returning .true. in this function will add the tile that contains iCell to
 !> be processed contains. Returning .false. for all cells will result in no mapping
 !> done.
 !>
 !> The accumulation_method procedure will be called with the mappings between pixel values
 !> and the cells in which they map to. Currently, the accumulation_method does not need to
 !> return any values.
 !>
 !> TODO: Supersample factor information - How to document fields within the mgr pool?
 !
 !-----------------------------------------------------------------------
 subroutine init_atm_map_static_data(mesh, mgr, kdtree, interp_criteria, accumulation_method, supersample_fac)

    implicit none

    ! Input variables
    type (mpas_pool_type), intent(in) :: mesh
    type (mpas_geotile_mgr_type), intent(in) :: mgr
    type (mpas_kd_type), pointer, intent(in) :: kdtree
    procedure (interp_criteria_function) :: interp_criteria
    procedure (interp_accumulation_function) :: accumulation_method
    integer, intent(in), optional :: supersample_fac


    ! Local variables
    ! Mesh variables and dimensions
    integer, pointer :: nCells
    integer, pointer :: nCellsSolve
    integer, dimension(:), pointer :: bdyMaskCell
    integer, dimension(:), pointer :: nEdgesOnCell
    integer, dimension(:,:), pointer :: verticesOnCell
    real (kind=RKIND), dimension(:), pointer :: xCell, yCell, zCell
    real (kind=RKIND), dimension(:), pointer :: xVertex, yVertex, zVertex
    real (kind=RKIND), pointer :: sphere_radius
    real (kind=RKIND), dimension(:), pointer :: latCell, lonCell

    integer, pointer :: tile_bdr
    integer, pointer :: tile_nx, tile_ny, tile_nz
    integer, pointer :: tile_z_start, tile_z_end
    integer :: supersample_fac_lcl
    integer :: subsample_fac

    real (kind=RKIND) :: lat
    real (kind=RKIND) :: lon
    real (kind=RKIND) :: xPixel, yPixel, zPixel
    real, pointer :: scale_factor

    integer (kind=I8KIND) :: i8val
    integer :: iCell
    integer :: i, ii
    integer :: j, jj
    integer :: k

    logical :: all_pixels_mapped_to_halo_cells

    type (mpas_geotile_type), pointer :: tile
    type (mpas_kd_type), pointer :: res

    integer :: ierr

    call mpas_pool_get_dimension(mesh, 'nCells', nCells)
    call mpas_pool_get_dimension(mesh, 'nCellsSolve', nCellsSolve)
    call mpas_pool_get_config(mesh, 'sphere_radius', sphere_radius)

    call mpas_pool_get_array(mesh, 'latCell', latCell)
    call mpas_pool_get_array(mesh, 'lonCell', lonCell)
    call mpas_pool_get_array(mesh, 'bdyMaskCell', bdyMaskCell)
    call mpas_pool_get_array(mesh, 'xCell', xCell)
    call mpas_pool_get_array(mesh, 'yCell', yCell)
    call mpas_pool_get_array(mesh, 'zCell', zCell)
    call mpas_pool_get_array(mesh, 'xVertex', xVertex)
    call mpas_pool_get_array(mesh, 'yVertex', yVertex)
    call mpas_pool_get_array(mesh, 'zVertex', zVertex)
    call mpas_pool_get_array(mesh, 'nEdgesOnCell', nEdgesOnCell)
    call mpas_pool_get_array(mesh, 'verticesOnCell', verticesOnCell)
    call mpas_pool_get_array(mesh, 'bdyMaskCell', bdyMaskCell)

    call mpas_pool_get_config(mgr % pool, 'tile_bdr', tile_bdr)
    call mpas_pool_get_config(mgr % pool, 'tile_x', tile_nx)
    call mpas_pool_get_config(mgr % pool, 'tile_y', tile_ny)
    call mpas_pool_get_config(mgr % pool, 'tile_z_start', tile_z_start)
    call mpas_pool_get_config(mgr % pool, 'tile_z_end', tile_z_end)
    call mpas_pool_get_config(mgr % pool, 'scale_factor', scale_factor)

    ! Currently, the namelist option config_supersample_factor is only for max snow albedo and
    ! monthly climatological albedo. We should have additional hidden namelist options for each
    ! additional field as meshes that are higher resolutions are now being used for current
    ! MPAS datasets.
    if (present(supersample_fac)) then
        supersample_fac_lcl = supersample_fac
    else
        supersample_fac_lcl = 1
    end if

    if (supersample_fac_lcl > 1) then
       call mpas_log_write('   Dataset will be supersampled by a factor of $i', intArgs=(/supersample_fac_lcl/))
    end if

    ! TODO: How should we handle subsample_fac? We may not want users to change this values other
    ! than during the tutorial and development. So perhaps it is best set just hard coded and not
    ! as a hidden namelist.
    !
    ! If we do have make it a hidden namelist option perhaps we could have throw and error or a
    ! warning to really prompt the user if they want to sub-sample fields.
    subsample_fac = 1

    do iCell = 1, nCells
        !
        ! Insure all cells receive values by loading tiles that have not recivied values, when
        ! interp_criteria(iCell) == .true..
        !
        if (interp_criteria(iCell)) then
            tile => null()
            ierr = mgr % get_tile(latCell(iCell), lonCell(iCell), tile)
            ierr = mgr % push_tile(tile)
            if (ierr /= 0 .or. .not. associated(tile)) then
                call mpas_log_write('Could not get tile that contained cell $i', intArgs=(/iCell/), messageType=MPAS_LOG_ERR)
                return
            end if
        end if

        !
        ! Process each tile by removing it from the stack. Determine the closest cell center to each tile
        ! pixel by using a KD search and pass the pixel value and the cloest cell center id to the
        ! accumulation routine.
        !
        do while (.not. mgr % is_stack_empty())
            tile => mgr % pop_tile()

            if (tile % is_processed) then
                cycle
            end if

            call mpas_log_write('Processing tile: '//trim(tile % fname))
            all_pixels_mapped_to_halo_cells = .true.

            do j = supersample_fac_lcl * tile_bdr + 1, supersample_fac_lcl * (tile_ny + tile_bdr), subsample_fac
                do i = supersample_fac_lcl * tile_bdr + 1, supersample_fac_lcl * (tile_nx + tile_bdr), subsample_fac

                    ii = (i - 1) / supersample_fac_lcl + 1
                    jj = (j - 1) / supersample_fac_lcl + 1

                    call mgr % tile_to_latlon(tile, j, i, lat, lon, supersample_fac_lcl)
                    call mpas_latlon_to_xyz(xPixel, yPixel, zPixel, sphere_radius, lat, lon)
                    call mpas_kd_search(kdtree, (/xPixel, yPixel, zPixel/), res)

                    !
                    ! For outermost boundary cells, extra work is needed to be done to determine if a pixel actually
                    ! lies within the cell returned by mpas_kd_search
                    !
                    if (bdyMaskCell(res % id) == nBdyLayers) then
                        if (.not. mpas_in_cell(xPixel, yPixel, zPixel, xCell(res % id), yCell(res % id), zCell(res % id), &
                                               nEdgesOnCell(res % id), verticesOnCell(:,res % id), xVertex, yVertex, zVertex)) then
                            !
                            ! This pixel lays outside of res % cell and outside of the limited-area region, no futher processing is
                            ! needed
                            !
                            cycle
                        end if
                    end if

                    !
                    ! Send the value(s) of the pixel and the cell it lies in to the accumulation_method
                    ! TODO: This actually doesn't handle the k dimension correctly.
                    !
                    do k = tile_z_start, tile_z_end
                        i8val = int(tile % tile(ii,jj,k), kind=I8KIND)
                        call accumulation_method(res % id, i8val)
                    end do

                    if (res % id <= nCellsSolve) then
                        all_pixels_mapped_to_halo_cells = .false.
                    end if
                end do
            end do

            tile % is_processed = .true.

            !
            ! If a single pixel value maps to an owned cell (i.e. <= nCellsSolve) then
            ! it is possible that the neighboring tiles might contain pixels that map
            ! to this process' compute cells, so add them to the stack.
            !
            if (.not. all_pixels_mapped_to_halo_cells) then
                ierr = mgr % push_neighbors(tile)
            end if
        end do
    end do

 end subroutine init_atm_map_static_data

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


!==================================================================================================
 end module mpas_init_atm_static
!==================================================================================================
