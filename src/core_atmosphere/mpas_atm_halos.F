! Copyright (c) 2023, The University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
module mpas_atm_halos

   use mpas_derived_types
   use mpas_pool_routines
   use mpas_log, only : mpas_log_write, mpas_log_info

   !
   ! Abstract interface for routine used to communicate halos of fields
   ! in a named group
   !
   abstract interface
      subroutine halo_exchange_routine(domain, halo_group, ierr)

         use mpas_derived_types, only : domain_type

         type (domain_type), intent(inout) :: domain
         character(len=*), intent(in) :: halo_group
         integer, intent(out), optional :: ierr

      end subroutine halo_exchange_routine
   end interface

   procedure (halo_exchange_routine), pointer :: exchange_halo_group


   contains


   !-----------------------------------------------------------------------
   !  routine atm_build_halo_groups
   !
   !> \brief Builds halo exchange groups used throughout atmosphere core
   !> \author Michael Duda
   !> \date   5 June 2023
   !> \details
   !>  This routine builds the halo exchange groups that are used throughout
   !>  the atmosphere core, and it sets a function pointer,
   !>  exchange_halo_group, to the routine that may be used to exchange the
   !>  halos for all fields in a named group.
   !>
   !>  A value of 0 is returned if halo exchange groups have been
   !>  successfully set up and a non-zero value is returned otherwise.
   !
   !-----------------------------------------------------------------------
   subroutine atm_build_halo_groups(domain, ierr)

      use mpas_dmpar, only : mpas_dmpar_exch_group_create, mpas_dmpar_exch_group_add_field, &
                             mpas_dmpar_exch_group_full_halo_exch
      use mpas_halo, only  : mpas_halo_init, mpas_halo_exch_group_create, mpas_halo_exch_group_add_field, &
                             mpas_halo_exch_group_complete, mpas_halo_exch_group_full_halo_exch

      ! Arguments
      type (domain_type), intent(inout) :: domain
      integer, intent(inout) :: ierr

      ! Local variables
      character(len=StrKIND), pointer :: config_halo_exch_method


      !
      ! Determine from the namelist option config_halo_exch_method which halo exchange method to employ
      !
      call mpas_pool_get_config(domain % blocklist % configs, 'config_halo_exch_method', config_halo_exch_method)

      if (trim(config_halo_exch_method) == 'mpas_dmpar') then
         call mpas_log_write('')
         call mpas_log_write('*** Using ''mpas_dmpar'' routines for exchanging halos')
         call mpas_log_write('')

         !
         ! Set up halo exchange groups used during atmosphere core initialization
         !
         call mpas_dmpar_exch_group_create(domain, 'initialization:u')
         call mpas_dmpar_exch_group_add_field(domain, 'initialization:u', 'u', timeLevel=1, haloLayers=(/1,2,3/))

         call mpas_dmpar_exch_group_create(domain, 'initialization:pv_edge,ru,rw')
         call mpas_dmpar_exch_group_add_field(domain, 'initialization:pv_edge,ru,rw', 'pv_edge', timeLevel=1, haloLayers=(/1,2,3/))
         call mpas_dmpar_exch_group_add_field(domain, 'initialization:pv_edge,ru,rw', 'ru', timeLevel=1, haloLayers=(/1,2,3/))
         call mpas_dmpar_exch_group_add_field(domain, 'initialization:pv_edge,ru,rw', 'rw', timeLevel=1, haloLayers=(/1,2/))

         !
         ! Set up halo exchange groups used by dynamics
         !
         call mpas_dmpar_exch_group_create(domain, 'dynamics:theta_m,scalars,pressure_p,rtheta_p')
         call mpas_dmpar_exch_group_add_field(domain, 'dynamics:theta_m,scalars,pressure_p,rtheta_p', 'theta_m', &
                                              timeLevel=1, haloLayers=(/1,2/))
         call mpas_dmpar_exch_group_add_field(domain, 'dynamics:theta_m,scalars,pressure_p,rtheta_p', 'scalars', &
                                              timeLevel=1, haloLayers=(/1,2/))
         call mpas_dmpar_exch_group_add_field(domain, 'dynamics:theta_m,scalars,pressure_p,rtheta_p', 'pressure_p', &
                                              timeLevel=1, haloLayers=(/1,2/))
         call mpas_dmpar_exch_group_add_field(domain, 'dynamics:theta_m,scalars,pressure_p,rtheta_p', 'rtheta_p', &
                                              timeLevel=1, haloLayers=(/1,2/))

         !CR: SMALLER STENCIL?: call mpas_dmpar_exch_halo_field(block % diag % rw_p, (/ 1 /))
         !CR: SMALLER STENCIL?: call mpas_dmpar_exch_halo_field(block % diag % ru_p, (/ 2 /))
         call mpas_dmpar_exch_group_create(domain, 'dynamics:rw_p,ru_p,rho_pp,rtheta_pp')
         call mpas_dmpar_exch_group_add_field(domain, 'dynamics:rw_p,ru_p,rho_pp,rtheta_pp', 'rw_p', &
                                              timeLevel=1, haloLayers=(/1/))
         call mpas_dmpar_exch_group_add_field(domain, 'dynamics:rw_p,ru_p,rho_pp,rtheta_pp', 'ru_p', &
                                              timeLevel=1, haloLayers=(/2/))
         call mpas_dmpar_exch_group_add_field(domain, 'dynamics:rw_p,ru_p,rho_pp,rtheta_pp', 'rho_pp', &
                                              timeLevel=1, haloLayers=(/1,2/))
         call mpas_dmpar_exch_group_add_field(domain, 'dynamics:rw_p,ru_p,rho_pp,rtheta_pp', 'rtheta_pp', &
                                              timeLevel=1, haloLayers=(/2/))

         call mpas_dmpar_exch_group_create(domain, 'dynamics:w,pv_edge,rho_edge')
         call mpas_dmpar_exch_group_add_field(domain, 'dynamics:w,pv_edge,rho_edge', 'w', &
                                              timeLevel=2, haloLayers=(/1,2/))
         call mpas_dmpar_exch_group_add_field(domain, 'dynamics:w,pv_edge,rho_edge', 'pv_edge', &
                                              timeLevel=1, haloLayers=(/1,2/))
         call mpas_dmpar_exch_group_add_field(domain, 'dynamics:w,pv_edge,rho_edge', 'rho_edge', &
                                              timeLevel=1, haloLayers=(/1,2/))

         call mpas_dmpar_exch_group_create(domain, 'dynamics:w,pv_edge,rho_edge,scalars')
         call mpas_dmpar_exch_group_add_field(domain, 'dynamics:w,pv_edge,rho_edge,scalars', 'w', &
                                              timeLevel=2, haloLayers=(/1,2/))
         call mpas_dmpar_exch_group_add_field(domain, 'dynamics:w,pv_edge,rho_edge,scalars', 'pv_edge', &
                                              timeLevel=1, haloLayers=(/1,2/))
         call mpas_dmpar_exch_group_add_field(domain, 'dynamics:w,pv_edge,rho_edge,scalars', 'rho_edge', &
                                              timeLevel=1, haloLayers=(/1,2/))
         call mpas_dmpar_exch_group_add_field(domain, 'dynamics:w,pv_edge,rho_edge,scalars', 'scalars', &
                                              timeLevel=2, haloLayers=(/1,2/))

         call mpas_dmpar_exch_group_create(domain, 'dynamics:theta_m,pressure_p,rtheta_p')
         call mpas_dmpar_exch_group_add_field(domain, 'dynamics:theta_m,pressure_p,rtheta_p', 'theta_m', &
                                              timeLevel=2, haloLayers=(/1,2/))
         call mpas_dmpar_exch_group_add_field(domain, 'dynamics:theta_m,pressure_p,rtheta_p', 'pressure_p', &
                                              timeLevel=1, haloLayers=(/1,2/))
         call mpas_dmpar_exch_group_add_field(domain, 'dynamics:theta_m,pressure_p,rtheta_p', 'rtheta_p', &
                                              timeLevel=1, haloLayers=(/1,2/))


         call mpas_dmpar_exch_group_create(domain, 'dynamics:exner')
         call mpas_dmpar_exch_group_add_field(domain, 'dynamics:exner', 'exner', timeLevel=1, haloLayers=(/1,2/))

         call mpas_dmpar_exch_group_create(domain, 'dynamics:tend_u')
         call mpas_dmpar_exch_group_add_field(domain, 'dynamics:tend_u', 'tend_u', timeLevel=1, haloLayers=(/1/))

         call mpas_dmpar_exch_group_create(domain, 'dynamics:rho_pp')
         call mpas_dmpar_exch_group_add_field(domain, 'dynamics:rho_pp', 'rho_pp', timeLevel=1, haloLayers=(/1/))

         call mpas_dmpar_exch_group_create(domain, 'dynamics:rtheta_pp')
         call mpas_dmpar_exch_group_add_field(domain, 'dynamics:rtheta_pp', 'rtheta_pp', timeLevel=1, haloLayers=(/1/))

         call mpas_dmpar_exch_group_create(domain, 'dynamics:u_123')
         call mpas_dmpar_exch_group_add_field(domain, 'dynamics:u_123', 'u', timeLevel=2, haloLayers=(/1,2,3/))

         call mpas_dmpar_exch_group_create(domain, 'dynamics:u_3')
         call mpas_dmpar_exch_group_add_field(domain, 'dynamics:u_3', 'u', timeLevel=2, haloLayers=(/3/))

         call mpas_dmpar_exch_group_create(domain, 'dynamics:scalars')
         call mpas_dmpar_exch_group_add_field(domain, 'dynamics:scalars', 'scalars', timeLevel=2, haloLayers=(/1,2/))

         call mpas_dmpar_exch_group_create(domain, 'dynamics:scalars_old')
         call mpas_dmpar_exch_group_add_field(domain, 'dynamics:scalars_old', 'scalars', timeLevel=1, haloLayers=(/1,2/))

         call mpas_dmpar_exch_group_create(domain, 'dynamics:w')
         call mpas_dmpar_exch_group_add_field(domain, 'dynamics:w', 'w', timeLevel=2, haloLayers=(/1,2/))

         call mpas_dmpar_exch_group_create(domain, 'dynamics:scale')
         call mpas_dmpar_exch_group_add_field(domain, 'dynamics:scale', 'scale', timeLevel=1, haloLayers=(/1,2/))

#ifdef DO_PHYSICS
         !
         ! Set up halo exchange groups used by physics
         !
         call mpas_dmpar_exch_group_create(domain, 'physics:blten')
         call mpas_dmpar_exch_group_add_field(domain, 'physics:blten', 'rublten', timeLevel=1, haloLayers=(/1,2/))
         call mpas_dmpar_exch_group_add_field(domain, 'physics:blten', 'rvblten', timeLevel=1, haloLayers=(/1,2/))

         call mpas_dmpar_exch_group_create(domain, 'physics:cuten')
         call mpas_dmpar_exch_group_add_field(domain, 'physics:cuten', 'rucuten', timeLevel=1, haloLayers=(/1,2/))
         call mpas_dmpar_exch_group_add_field(domain, 'physics:cuten', 'rvcuten', timeLevel=1, haloLayers=(/1,2/))
#endif

         !
         ! Set routine to exchange a halo group
         !
         exchange_halo_group => mpas_dmpar_exch_group_full_halo_exch

      else if (trim(config_halo_exch_method) == 'mpas_halo') then

         call mpas_log_write('')
         call mpas_log_write('*** Using ''mpas_halo'' routines for exchanging halos')
         call mpas_log_write('')

         call mpas_halo_init(domain)

         !
         ! Set up halo exchange groups used during atmosphere core initialization
         !
         call mpas_halo_exch_group_create(domain, 'initialization:u')
         call mpas_halo_exch_group_add_field(domain, 'initialization:u', 'u', timeLevel=1, haloLayers=(/1,2,3/))
         call mpas_halo_exch_group_complete(domain, 'initialization:u')

         call mpas_halo_exch_group_create(domain, 'initialization:pv_edge,ru,rw')
         call mpas_halo_exch_group_add_field(domain, 'initialization:pv_edge,ru,rw', 'pv_edge', timeLevel=1, haloLayers=(/1,2,3/))
         call mpas_halo_exch_group_add_field(domain, 'initialization:pv_edge,ru,rw', 'ru', timeLevel=1, haloLayers=(/1,2,3/))
         call mpas_halo_exch_group_add_field(domain, 'initialization:pv_edge,ru,rw', 'rw', timeLevel=1, haloLayers=(/1,2/))
         call mpas_halo_exch_group_complete(domain, 'initialization:pv_edge,ru,rw')

         !
         ! Set up halo exchange groups used by dynamics
         !
         call mpas_halo_exch_group_create(domain, 'dynamics:theta_m,scalars,pressure_p,rtheta_p')
         call mpas_halo_exch_group_add_field(domain, 'dynamics:theta_m,scalars,pressure_p,rtheta_p', 'theta_m', &
                                             timeLevel=1, haloLayers=(/1,2/))
         call mpas_halo_exch_group_add_field(domain, 'dynamics:theta_m,scalars,pressure_p,rtheta_p', 'scalars', &
                                             timeLevel=1, haloLayers=(/1,2/))
         call mpas_halo_exch_group_add_field(domain, 'dynamics:theta_m,scalars,pressure_p,rtheta_p', 'pressure_p', &
                                             timeLevel=1, haloLayers=(/1,2/))
         call mpas_halo_exch_group_add_field(domain, 'dynamics:theta_m,scalars,pressure_p,rtheta_p', 'rtheta_p', &
                                             timeLevel=1, haloLayers=(/1,2/))
         call mpas_halo_exch_group_complete(domain, 'dynamics:theta_m,scalars,pressure_p,rtheta_p')

         call mpas_halo_exch_group_create(domain, 'dynamics:rw_p,ru_p,rho_pp,rtheta_pp')
         call mpas_halo_exch_group_add_field(domain, 'dynamics:rw_p,ru_p,rho_pp,rtheta_pp', 'rw_p', &
                                             timeLevel=1, haloLayers=(/1/))
         call mpas_halo_exch_group_add_field(domain, 'dynamics:rw_p,ru_p,rho_pp,rtheta_pp', 'ru_p', &
                                             timeLevel=1, haloLayers=(/2/))
         call mpas_halo_exch_group_add_field(domain, 'dynamics:rw_p,ru_p,rho_pp,rtheta_pp', 'rho_pp', &
                                             timeLevel=1, haloLayers=(/1,2/))
         call mpas_halo_exch_group_add_field(domain, 'dynamics:rw_p,ru_p,rho_pp,rtheta_pp', 'rtheta_pp', &
                                             timeLevel=1, haloLayers=(/2/))
         call mpas_halo_exch_group_complete(domain, 'dynamics:rw_p,ru_p,rho_pp,rtheta_pp')

         call mpas_halo_exch_group_create(domain, 'dynamics:w,pv_edge,rho_edge')
         call mpas_halo_exch_group_add_field(domain, 'dynamics:w,pv_edge,rho_edge', 'w', timeLevel=2, haloLayers=(/1,2/))
         call mpas_halo_exch_group_add_field(domain, 'dynamics:w,pv_edge,rho_edge', 'pv_edge', timeLevel=1, haloLayers=(/1,2/))
         call mpas_halo_exch_group_add_field(domain, 'dynamics:w,pv_edge,rho_edge', 'rho_edge', timeLevel=1, haloLayers=(/1,2/))
         call mpas_halo_exch_group_complete(domain, 'dynamics:w,pv_edge,rho_edge')

         call mpas_halo_exch_group_create(domain, 'dynamics:w,pv_edge,rho_edge,scalars')
         call mpas_halo_exch_group_add_field(domain, 'dynamics:w,pv_edge,rho_edge,scalars', 'w', &
                                             timeLevel=2, haloLayers=(/1,2/))
         call mpas_halo_exch_group_add_field(domain, 'dynamics:w,pv_edge,rho_edge,scalars', 'pv_edge', &
                                             timeLevel=1, haloLayers=(/1,2/))
         call mpas_halo_exch_group_add_field(domain, 'dynamics:w,pv_edge,rho_edge,scalars', 'rho_edge', &
                                             timeLevel=1, haloLayers=(/1,2/))
         call mpas_halo_exch_group_add_field(domain, 'dynamics:w,pv_edge,rho_edge,scalars', 'scalars', &
                                             timeLevel=2, haloLayers=(/1,2/))
         call mpas_halo_exch_group_complete(domain, 'dynamics:w,pv_edge,rho_edge,scalars')

         call mpas_halo_exch_group_create(domain, 'dynamics:theta_m,pressure_p,rtheta_p')
         call mpas_halo_exch_group_add_field(domain, 'dynamics:theta_m,pressure_p,rtheta_p', 'theta_m', &
                                             timeLevel=2, haloLayers=(/1,2/))
         call mpas_halo_exch_group_add_field(domain, 'dynamics:theta_m,pressure_p,rtheta_p', 'pressure_p', &
                                             timeLevel=1, haloLayers=(/1,2/))
         call mpas_halo_exch_group_add_field(domain, 'dynamics:theta_m,pressure_p,rtheta_p', 'rtheta_p', &
                                             timeLevel=1, haloLayers=(/1,2/))
         call mpas_halo_exch_group_complete(domain, 'dynamics:theta_m,pressure_p,rtheta_p')


         call mpas_halo_exch_group_create(domain, 'dynamics:exner')
         call mpas_halo_exch_group_add_field(domain, 'dynamics:exner', 'exner', timeLevel=1, haloLayers=(/1,2/))
         call mpas_halo_exch_group_complete(domain, 'dynamics:exner')

         call mpas_halo_exch_group_create(domain, 'dynamics:tend_u')
         call mpas_halo_exch_group_add_field(domain, 'dynamics:tend_u', 'tend_u', timeLevel=1, haloLayers=(/1/))
         call mpas_halo_exch_group_complete(domain, 'dynamics:tend_u')

         call mpas_halo_exch_group_create(domain, 'dynamics:rho_pp')
         call mpas_halo_exch_group_add_field(domain, 'dynamics:rho_pp', 'rho_pp', timeLevel=1, haloLayers=(/1/))
         call mpas_halo_exch_group_complete(domain, 'dynamics:rho_pp')

         call mpas_halo_exch_group_create(domain, 'dynamics:rtheta_pp')
         call mpas_halo_exch_group_add_field(domain, 'dynamics:rtheta_pp', 'rtheta_pp', timeLevel=1, haloLayers=(/1/))
         call mpas_halo_exch_group_complete(domain, 'dynamics:rtheta_pp')

         call mpas_halo_exch_group_create(domain, 'dynamics:u_123')
         call mpas_halo_exch_group_add_field(domain, 'dynamics:u_123', 'u', timeLevel=2, haloLayers=(/1,2,3/))
         call mpas_halo_exch_group_complete(domain, 'dynamics:u_123')

         call mpas_halo_exch_group_create(domain, 'dynamics:u_3')
         call mpas_halo_exch_group_add_field(domain, 'dynamics:u_3', 'u', timeLevel=2, haloLayers=(/3/))
         call mpas_halo_exch_group_complete(domain, 'dynamics:u_3')

         call mpas_halo_exch_group_create(domain, 'dynamics:scalars')
         call mpas_halo_exch_group_add_field(domain, 'dynamics:scalars', 'scalars', timeLevel=2, haloLayers=(/1,2/))
         call mpas_halo_exch_group_complete(domain, 'dynamics:scalars')

         call mpas_halo_exch_group_create(domain, 'dynamics:scalars_old')
         call mpas_halo_exch_group_add_field(domain, 'dynamics:scalars_old', 'scalars', timeLevel=1, haloLayers=(/1,2/))
         call mpas_halo_exch_group_complete(domain, 'dynamics:scalars_old')

         call mpas_halo_exch_group_create(domain, 'dynamics:w')
         call mpas_halo_exch_group_add_field(domain, 'dynamics:w', 'w', timeLevel=2, haloLayers=(/1,2/))
         call mpas_halo_exch_group_complete(domain, 'dynamics:w')

         call mpas_halo_exch_group_create(domain, 'dynamics:scale')
         call mpas_halo_exch_group_add_field(domain, 'dynamics:scale', 'scale', timeLevel=1, haloLayers=(/1,2/))
         call mpas_halo_exch_group_complete(domain, 'dynamics:scale')

#ifdef DO_PHYSICS
         !
         ! Set up halo exchange groups used by physics
         !
         call mpas_halo_exch_group_create(domain, 'physics:blten')
         call mpas_halo_exch_group_add_field(domain, 'physics:blten', 'rublten', timeLevel=1, haloLayers=(/1,2/))
         call mpas_halo_exch_group_add_field(domain, 'physics:blten', 'rvblten', timeLevel=1, haloLayers=(/1,2/))
         call mpas_halo_exch_group_complete(domain, 'physics:blten')

         call mpas_halo_exch_group_create(domain, 'physics:cuten')
         call mpas_halo_exch_group_add_field(domain, 'physics:cuten', 'rucuten', timeLevel=1, haloLayers=(/1,2/))
         call mpas_halo_exch_group_add_field(domain, 'physics:cuten', 'rvcuten', timeLevel=1, haloLayers=(/1,2/))
         call mpas_halo_exch_group_complete(domain, 'physics:cuten')
#endif

         !
         ! Set routine to exchange a halo group
         !
         exchange_halo_group => mpas_halo_exch_group_full_halo_exch

      else

         !
         ! Invalid method for exchanging halos
         !
         ierr = 1
         call mpas_log_write('Invalid method for exchanging halos specified by ''config_halo_exch_method'': ' // &
                             trim(config_halo_exch_method), messageType=MPAS_LOG_ERR)
         return

      end if

      ierr = 0

   end subroutine atm_build_halo_groups


   !-----------------------------------------------------------------------
   !  routine atm_destroy_halo_groups
   !
   !> \brief Destroys halo exchange groups used throughout atmosphere core
   !> \author Michael Duda
   !> \date   5 June 2023
   !> \details
   !>  This routine destroys the halo exchange groups that are used throughout
   !>  the atmosphere core, freeing up any resources that were used by these
   !>  halo exchange groups.
   !>
   !>  A value of 0 is returned if halo exchange groups have been
   !>  successfully destroyed and a non-zero value is returned otherwise.
   !
   !-----------------------------------------------------------------------
   subroutine atm_destroy_halo_groups(domain, ierr)

      use mpas_dmpar, only : mpas_dmpar_exch_group_destroy
      use mpas_halo, only  : mpas_halo_exch_group_destroy, mpas_halo_finalize

      ! Arguments
      type (domain_type), intent(inout) :: domain
      integer, intent(inout) :: ierr

      ! Local variables
      character(len=StrKIND), pointer :: config_halo_exch_method


      call mpas_pool_get_config(domain % blocklist % configs, 'config_halo_exch_method', config_halo_exch_method)

      if (trim(config_halo_exch_method) == 'mpas_dmpar') then
         !
         ! Destroy halo exchange groups used only during initialization
         !
         call mpas_dmpar_exch_group_destroy(domain, 'initialization:u')
         call mpas_dmpar_exch_group_destroy(domain, 'initialization:pv_edge,ru,rw')

         !
         ! Destroy halo exchange groups used by dynamics
         !
         call mpas_dmpar_exch_group_destroy(domain, 'dynamics:theta_m,scalars,pressure_p,rtheta_p')
         call mpas_dmpar_exch_group_destroy(domain, 'dynamics:rw_p,ru_p,rho_pp,rtheta_pp')
         call mpas_dmpar_exch_group_destroy(domain, 'dynamics:w,pv_edge,rho_edge')
         call mpas_dmpar_exch_group_destroy(domain, 'dynamics:w,pv_edge,rho_edge,scalars')
         call mpas_dmpar_exch_group_destroy(domain, 'dynamics:theta_m,pressure_p,rtheta_p')

         call mpas_dmpar_exch_group_destroy(domain, 'dynamics:exner')
         call mpas_dmpar_exch_group_destroy(domain, 'dynamics:tend_u')
         call mpas_dmpar_exch_group_destroy(domain, 'dynamics:rho_pp')
         call mpas_dmpar_exch_group_destroy(domain, 'dynamics:rtheta_pp')
         call mpas_dmpar_exch_group_destroy(domain, 'dynamics:u_123')
         call mpas_dmpar_exch_group_destroy(domain, 'dynamics:u_3')
         call mpas_dmpar_exch_group_destroy(domain, 'dynamics:scalars')
         call mpas_dmpar_exch_group_destroy(domain, 'dynamics:scalars_old')
         call mpas_dmpar_exch_group_destroy(domain, 'dynamics:w')
         call mpas_dmpar_exch_group_destroy(domain, 'dynamics:scale')

#ifdef DO_PHYSICS
         !
         ! Destroy halo exchange groups used by physics
         !
         call mpas_dmpar_exch_group_destroy(domain, 'physics:blten')
         call mpas_dmpar_exch_group_destroy(domain, 'physics:cuten')
#endif

      else if (trim(config_halo_exch_method) == 'mpas_halo') then

         !
         ! Destroy halo exchange groups used only during initialization
         !
         call mpas_halo_exch_group_destroy(domain, 'initialization:u')
         call mpas_halo_exch_group_destroy(domain, 'initialization:pv_edge,ru,rw')

         !
         ! Destroy halo exchange groups used by dynamics
         !
         call mpas_halo_exch_group_destroy(domain, 'dynamics:theta_m,scalars,pressure_p,rtheta_p')
         call mpas_halo_exch_group_destroy(domain, 'dynamics:rw_p,ru_p,rho_pp,rtheta_pp')
         call mpas_halo_exch_group_destroy(domain, 'dynamics:w,pv_edge,rho_edge')
         call mpas_halo_exch_group_destroy(domain, 'dynamics:w,pv_edge,rho_edge,scalars')
         call mpas_halo_exch_group_destroy(domain, 'dynamics:theta_m,pressure_p,rtheta_p')

         call mpas_halo_exch_group_destroy(domain, 'dynamics:exner')
         call mpas_halo_exch_group_destroy(domain, 'dynamics:tend_u')
         call mpas_halo_exch_group_destroy(domain, 'dynamics:rho_pp')
         call mpas_halo_exch_group_destroy(domain, 'dynamics:rtheta_pp')
         call mpas_halo_exch_group_destroy(domain, 'dynamics:u_123')
         call mpas_halo_exch_group_destroy(domain, 'dynamics:u_3')
         call mpas_halo_exch_group_destroy(domain, 'dynamics:scalars')
         call mpas_halo_exch_group_destroy(domain, 'dynamics:scalars_old')
         call mpas_halo_exch_group_destroy(domain, 'dynamics:w')
         call mpas_halo_exch_group_destroy(domain, 'dynamics:scale')

#ifdef DO_PHYSICS
         !
         ! Destroy halo exchange groups used by physics
         !
         call mpas_halo_exch_group_destroy(domain, 'physics:blten')
         call mpas_halo_exch_group_destroy(domain, 'physics:cuten')
#endif

         call mpas_halo_finalize(domain)

      else

         !
         ! Invalid method for exchanging halos - an error should have already occurred in atm_build_halo_groups()
         !
         ierr = 1
         return

      end if

      ierr = 0

   end subroutine atm_destroy_halo_groups

end module mpas_atm_halos

