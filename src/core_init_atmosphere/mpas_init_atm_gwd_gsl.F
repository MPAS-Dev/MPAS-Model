! Module mpas_init_atm_gwd_gsl
!
! This program calls subroutines which calculate the parameters
! required for the GSL subgrid-scale orographic gravity-wave drag (GWDO)
! suite on the MPAS mesh.  These parameters are for the small-scale
! GWD (Tsiringakis et al., 2017) and turbulent orographic form drag (TOFD)
! (Beljaars et al., 2004) schemes of the GSL drag suite.
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
! Brief description of program:  Creates orographic (oro_data) files
! needed by the GSL drag suite physics parameterization 
!
! Author: Michael Toy, NOAA/GSL
!
module mpas_init_atm_gwd_gsl

   use mpas_gsl_oro_data_sm_scale, only: calc_gsl_oro_data_sm_scale
   use mpas_gsl_oro_data_lg_scale, only: calc_gsl_oro_data_lg_scale

   use iso_c_binding, only : c_char, c_int, c_float, c_ptr, c_loc

   use mpas_derived_types, only : MPAS_LOG_ERR
   use mpas_framework
   use mpas_timekeeping
   use mpas_log, only : mpas_log_write
   use mpas_c_interfacing, only : mpas_f_to_c_string

   public :: calc_gsl_oro_data

   private

   integer, parameter :: I1KIND = selected_int_kind(2)


   contains


   subroutine calc_gsl_oro_data(domain,iErr)

      use mpas_derived_types
      use mpas_kind_types
      use mpas_timer
      use mpas_stream_manager

      implicit none

      type (domain_type), intent(inout) :: domain
      integer, intent(inout) :: iErr

      type (mpas_pool_type), pointer :: mesh, state
      integer :: iCell, i
      real (kind=RKIND) :: dc
      real (kind=RKIND), dimension(:), allocatable :: areaCell
      real (kind=RKIND), pointer :: config_gwd_cell_scaling
      integer, pointer :: nCells
      integer, pointer :: nEdges
      integer, dimension(:), pointer :: nEdgesOnCell
      integer, dimension(:,:), pointer :: edgesOnCell
      logical :: onUnitSphere
      real (kind=RKIND), pointer :: sphere_radius
      real (kind=RKIND), dimension(:), pointer :: latCell, lonCell, dcEdge
      real (kind=RKIND), dimension(:), pointer :: var2dls, conls, oa1ls, oa2ls, oa3ls, oa4ls,  &
                                                  ol1ls, ol2ls, ol3ls, ol4ls
      real (kind=RKIND), dimension(:), pointer :: var2dss, conss, oa1ss, oa2ss, oa3ss, oa4ss,  &
                                                  ol1ss, ol2ss, ol3ss, ol4ss
      character(len=StrKIND), pointer :: config_geog_data_path
      character(len=StrKIND) :: geog_sub_path
      character(len=StrKIND+1) :: geog_data_path      ! same as config_geog_data_path, but guaranteed to have a trailing slash
      real (kind=RKIND), parameter :: Re = 6371229.0_RKIND        ! Earth radius in MPAS-Atmosphere

      logical, dimension(:), allocatable :: duplicate_oro_data   ! flag for whether large-scale topographic
                                           ! statistics are duplicated from small-scale values due to grid
                                           ! size being less than 7.5km, i.e., not having enough 2.5 minute
                                           ! topographic points within a grid box

      call mpas_pool_get_subpool(domain % blocklist % structs, 'mesh', mesh)
      call mpas_pool_get_subpool(domain % blocklist % structs, 'state', state)

      call mpas_pool_get_config(mesh, 'sphere_radius', sphere_radius)
      call mpas_pool_get_config(domain % configs, 'config_gwd_cell_scaling', config_gwd_cell_scaling)

      !
      ! Retrieve pointers to arrays holding the latitudes and longitudes of cells,
      ! and arrays that will hold the computed GWDO statistics
      !
      call mpas_pool_get_dimension(mesh, 'nCells', nCells)
      call mpas_pool_get_dimension(mesh, 'nEdges', nEdges)
      call mpas_pool_get_array(mesh, 'latCell', latCell)
      call mpas_pool_get_array(mesh, 'lonCell', lonCell)
      call mpas_pool_get_array(mesh, 'nEdgesOnCell', nEdgesOnCell)
      call mpas_pool_get_array(mesh, 'edgesOnCell', edgesOnCell)
      call mpas_pool_get_array(mesh, 'dcEdge', dcEdge)
      call mpas_pool_get_array(mesh, 'var2dls', var2dls)
      call mpas_pool_get_array(mesh, 'conls', conls)
      call mpas_pool_get_array(mesh, 'ol1ls', ol1ls)
      call mpas_pool_get_array(mesh, 'ol2ls', ol2ls)
      call mpas_pool_get_array(mesh, 'ol3ls', ol3ls)
      call mpas_pool_get_array(mesh, 'ol4ls', ol4ls)
      call mpas_pool_get_array(mesh, 'oa1ls', oa1ls)
      call mpas_pool_get_array(mesh, 'oa2ls', oa2ls)
      call mpas_pool_get_array(mesh, 'oa3ls', oa3ls)
      call mpas_pool_get_array(mesh, 'oa4ls', oa4ls)
      call mpas_pool_get_array(mesh, 'var2dss', var2dss)
      call mpas_pool_get_array(mesh, 'conss', conss)
      call mpas_pool_get_array(mesh, 'ol1ss', ol1ss)
      call mpas_pool_get_array(mesh, 'ol2ss', ol2ss)
      call mpas_pool_get_array(mesh, 'ol3ss', ol3ss)
      call mpas_pool_get_array(mesh, 'ol4ss', ol4ss)
      call mpas_pool_get_array(mesh, 'oa1ss', oa1ss)
      call mpas_pool_get_array(mesh, 'oa2ss', oa2ss)
      call mpas_pool_get_array(mesh, 'oa3ss', oa3ss)
      call mpas_pool_get_array(mesh, 'oa4ss', oa4ss)


      !
      ! It is possible that this code is called before the mesh fields have been scaled
      ! up to "Earth-sized". Because we need "Earth" distances to cut out bounding
      ! boxes from topography, we try here to detect whether we are on an unscaled
      ! unit sphere or not: if the maximum dcEdge value is less than 1.0, assume this
      ! is the case.
      !
      if (maxval(dcEdge(1:nEdges)) < 1.0_RKIND) then
         call mpas_log_write('Computing GSL GWD statistics on a unit sphere')
         onUnitSphere = .true.
      else
         onUnitSphere = .false.
      end if

      if (config_gwd_cell_scaling /= 1.0) then
         call mpas_log_write('Using effective cell diameters scaled by a factor of $r', realArgs=(/config_gwd_cell_scaling/))
         call mpas_log_write('in the computation of GWD static fields.')
      end if

      allocate(areaCell(nCells))
      allocate(duplicate_oro_data(nCells))

      !
      ! Loop to compute approximate area of each MPAS horizontal grid cell
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

         ! Assume square shape with MPAS grid cell diameter as length of sides
         areaCell(iCell) = dc*dc

      end do

      call calc_gsl_oro_data_sm_scale(nCells,latCell,lonCell,areaCell,Re,     &
                                 var2dss,conss,oa1ss,oa2ss,oa3ss,oa4ss,       &
                                 ol1ss,ol2ss,ol3ss,ol4ss,                     &
                                 domain,duplicate_oro_data)

      call calc_gsl_oro_data_lg_scale(nCells,latCell,lonCell,areaCell,Re,  &
                                   var2dls,conls,oa1ls,oa2ls,oa3ls,oa4ls,  &
                                   ol1ls,ol2ls,ol3ls,ol4ls,domain,         &
                                   duplicate_oro_data)

      do iCell=1,nCells
      ! Re-assign large-scale statistics with small-scale values if necessary
         if ( duplicate_oro_data(iCell) ) then
            var2dls(iCell) = var2dss(iCell)
            conls(iCell) = conss(iCell)
            oa1ls(iCell) = oa1ss(iCell)
            oa2ls(iCell) = oa2ss(iCell)
            oa3ls(iCell) = oa3ss(iCell)
            oa4ls(iCell) = oa4ss(iCell)
            ol1ls(iCell) = ol1ss(iCell)
            ol2ls(iCell) = ol2ss(iCell)
            ol3ls(iCell) = ol3ss(iCell)
            ol4ls(iCell) = ol4ss(iCell)
         endif
      end do

      deallocate(areaCell)
      deallocate(duplicate_oro_data)

      call mpas_log_write('End module mpas_init_atm_gwd_gsl')

      iErr = 0

      return
   end subroutine calc_gsl_oro_data

end module mpas_init_atm_gwd_gsl
