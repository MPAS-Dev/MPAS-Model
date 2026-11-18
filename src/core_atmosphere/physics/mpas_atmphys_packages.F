! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!=================================================================================================================
 module mpas_atmphys_packages
 use mpas_kind_types
 use mpas_derived_types,only : mpas_pool_type,mpas_io_context_type,MPAS_LOG_ERR
 use mpas_pool_routines,only : mpas_pool_get_config,mpas_pool_get_package
 use mpas_log,only : mpas_log_write

 implicit none
 private
 public:: atmphys_setup_packages

!mpas_atmphys_packages contains the definitions of all physics packages.
!Laura D. Fowler (laura@ucar.edu) / 2016-03-10.


 contains


!=================================================================================================================
 function atmphys_setup_packages(configs,packages,iocontext) result(ierr)
!=================================================================================================================

!inout arguments:
 type (mpas_pool_type), intent(inout) :: configs
 type (mpas_pool_type), intent(inout) :: packages
 type (mpas_io_context_type), intent(inout) :: iocontext

!local variables:
 character(len=StrKIND),pointer:: config_microp_scheme
 character(len=StrKIND),pointer:: config_convection_scheme
 character(len=StrKIND),pointer:: config_pbl_scheme
 character(len=StrKIND),pointer:: config_lsm_scheme
 logical,pointer:: mp_kessler_in,mp_thompson_in,mp_thompson_aers_in,mp_wsm6_in
 logical,pointer:: cu_grell_freitas_in,cu_kain_fritsch_in,cu_ntiedtke_in
 logical,pointer:: bl_mynn_in,bl_ysu_in
 integer,pointer:: config_p3_nCat
 logical,pointer:: config_p3_trplMomI,config_p3_liqfrac,config_p3_aerosols,config_p3_ic_bulk_ice
 logical,pointer:: mp_p3_in,mp_p3_cat2_in,mp_p3_cat3_in
 logical,pointer:: mp_p3_3mom_in,mp_p3_3mom_cat2_in,mp_p3_3mom_cat3_in
 logical,pointer:: mp_p3_lf_in,mp_p3_lf_cat2_in,mp_p3_lf_cat3_in,mp_p3_pbl_in,mp_p3_aers_in,mp_p3_icice_in
 logical,pointer:: bl_ysu_nop3_in,bl_mynn_nop3_in,cu_ntiedtke_nop3_in
 logical,pointer:: sf_noahmp_in

 integer :: ierr

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write('')
!call mpas_log_write('--- enter subroutine atmphys_setup_packages:')

 ierr = 0

 call mpas_log_write('----- Setting up package variables -----')
 call mpas_log_write('')

!--- initialization of all packages for parameterizations of cloud microphysics:

 call mpas_pool_get_config(configs,'config_microp_scheme',config_microp_scheme)

 call mpas_pool_get_config(configs,'config_p3_nCat',config_p3_nCat)

 nullify(mp_kessler_in)
 call mpas_pool_get_package(packages,'mp_kessler_inActive',mp_kessler_in)

 nullify(mp_thompson_in)
 call mpas_pool_get_package(packages,'mp_thompson_inActive',mp_thompson_in)

 nullify(mp_thompson_aers_in)
 call mpas_pool_get_package(packages,'mp_thompson_aers_inActive',mp_thompson_aers_in)

 nullify(mp_p3_in)
 call mpas_pool_get_package(packages,'mp_p3_inActive',mp_p3_in)

 nullify(mp_p3_cat2_in)
 call mpas_pool_get_package(packages,'mp_p3_cat2_inActive',mp_p3_cat2_in)

 nullify(mp_p3_cat3_in)
 call mpas_pool_get_package(packages,'mp_p3_cat3_inActive',mp_p3_cat3_in)

 nullify(mp_wsm6_in)
 call mpas_pool_get_package(packages,'mp_wsm6_inActive',mp_wsm6_in)

 if(.not.associated(mp_kessler_in      )  .or. &
    .not.associated(mp_thompson_in     ) .or. &
    .not.associated(mp_thompson_aers_in) .or. &
    .not.associated(mp_p3_in           ) .or. &
    .not.associated(mp_p3_cat2_in      ) .or. &
    .not.associated(mp_p3_cat3_in      ) .or. &
    .not.associated(mp_wsm6_in)) then
    call mpas_log_write('====================================================================================',messageType=MPAS_LOG_ERR)
    call mpas_log_write('* Error while setting up packages for cloud microphysics options in atmosphere core.',messageType=MPAS_LOG_ERR)
    call mpas_log_write('====================================================================================',messageType=MPAS_LOG_ERR)
    ierr = 1
    return
 endif

 mp_kessler_in       = .false.
 mp_thompson_in      = .false.
 mp_thompson_aers_in = .false.
 mp_p3_in            = .false.
 mp_p3_cat2_in       = .false.
 mp_p3_cat3_in       = .false.
 mp_wsm6_in          = .false.

 if(config_microp_scheme == 'mp_kessler') then
    mp_kessler_in = .true.
 elseif(config_microp_scheme == 'mp_thompson') then
    mp_thompson_in = .true.
 elseif(config_microp_scheme == 'mp_thompson_aerosols') then
    mp_thompson_aers_in = .true.
 elseif(config_microp_scheme == 'mp_p3') then
    if(config_p3_nCat < 1 .or. config_p3_nCat > 3) then
       call mpas_log_write('====================================================================================',messageType=MPAS_LOG_ERR)
       call mpas_log_write('* Error: config_p3_nCat must be 1, 2, or 3 for mp_p3.',messageType=MPAS_LOG_ERR)
       call mpas_log_write('====================================================================================',messageType=MPAS_LOG_ERR)
       ierr = 1
       return
    endif
    mp_p3_in = .true.
    if(config_p3_nCat >= 2) mp_p3_cat2_in = .true.
    if(config_p3_nCat >= 3) mp_p3_cat3_in = .true.
 elseif(config_microp_scheme == 'mp_wsm6') then
    mp_wsm6_in = .true.
 endif

 call mpas_log_write('    mp_kessler_in           = $l', logicArgs=(/mp_kessler_in/))
 call mpas_log_write('    mp_thompson_in          = $l', logicArgs=(/mp_thompson_in/))
 call mpas_log_write('    mp_thompson_aers_in     = $l', logicArgs=(/mp_thompson_aers_in/))
 call mpas_log_write('    mp_p3_in                = $l', logicArgs=(/mp_p3_in/))
 call mpas_log_write('    mp_p3_cat2_in           = $l', logicArgs=(/mp_p3_cat2_in/))
 call mpas_log_write('    mp_p3_cat3_in           = $l', logicArgs=(/mp_p3_cat3_in/))
 call mpas_log_write('    mp_wsm6_in              = $l', logicArgs=(/mp_wsm6_in/))

!--- initialization of all packages for parameterizations of convection:

 call mpas_pool_get_config(configs,'config_convection_scheme',config_convection_scheme)

 nullify(cu_grell_freitas_in)
 call mpas_pool_get_package(packages,'cu_grell_freitas_inActive',cu_grell_freitas_in)

 nullify(cu_kain_fritsch_in)
 call mpas_pool_get_package(packages,'cu_kain_fritsch_inActive',cu_kain_fritsch_in)

 nullify(cu_ntiedtke_in)
 call mpas_pool_get_package(packages,'cu_ntiedtke_inActive',cu_ntiedtke_in)

 if(.not.associated(cu_grell_freitas_in) .or. &
    .not.associated(cu_kain_fritsch_in)  .or. &
    .not.associated(cu_ntiedtke_in)    ) then
    call mpas_log_write('====================================================================================',messageType=MPAS_LOG_ERR)
    call mpas_log_write('* Error while setting up packages for convection options in atmosphere core.',        messageType=MPAS_LOG_ERR)
    call mpas_log_write('====================================================================================',messageType=MPAS_LOG_ERR)
    ierr = 1
    return
 endif

 cu_grell_freitas_in = .false.
 cu_kain_fritsch_in  = .false.
 cu_ntiedtke_in      = .false.

 if(config_convection_scheme=='cu_grell_freitas') then
    cu_grell_freitas_in = .true.
 elseif(config_convection_scheme == 'cu_kain_fritsch') then
    cu_kain_fritsch_in = .true.
 elseif(config_convection_scheme == 'cu_tiedtke' .or. &
        config_convection_scheme == 'cu_ntiedtke') then
    cu_ntiedtke_in = .true.
 endif

 call mpas_log_write('    cu_grell_freitas_in     = $l', logicArgs=(/cu_grell_freitas_in/))
 call mpas_log_write('    cu_kain_fritsch_in      = $l', logicArgs=(/cu_kain_fritsch_in/))
 call mpas_log_write('    cu_ntiedtke_in          = $l', logicArgs=(/cu_ntiedtke_in/))

!--- initialization of all packages for parameterizations of surface layer and planetary boundary layer:

 call mpas_pool_get_config(configs,'config_pbl_scheme',config_pbl_scheme)

 nullify(bl_mynn_in)
 call mpas_pool_get_package(packages,'bl_mynn_inActive',bl_mynn_in)

 nullify(bl_ysu_in)
 call mpas_pool_get_package(packages,'bl_ysu_inActive',bl_ysu_in)

 if(.not.associated(bl_mynn_in) .or. &
    .not.associated(bl_ysu_in)) then
    call mpas_log_write('====================================================================================',messageType=MPAS_LOG_ERR)
    call mpas_log_write('* Error while setting up packages for planetary layer  options in atmosphere core.',  messageType=MPAS_LOG_ERR)
    call mpas_log_write('====================================================================================',messageType=MPAS_LOG_ERR)
    ierr = 1
    return
 endif

 bl_mynn_in = .false.
 bl_ysu_in  = .false.

 if(config_pbl_scheme=='bl_mynn') then
    bl_mynn_in = .true.
 elseif(config_pbl_scheme == 'bl_ysu') then
    bl_ysu_in = .true.
 endif

 call mpas_log_write('    bl_mynn_in              = $l', logicArgs=(/bl_mynn_in/))
 call mpas_log_write('    bl_ysu_in               = $l', logicArgs=(/bl_ysu_in/))
 call mpas_log_write('')

!--- initialization of the packages for the optional P3 prognostic scalars (one package per option and ice category).
!    The *_nop3_in packages are the PBL and convection packages without P3, which has no bulk qi, qs and ni scalars;
!    mp_p3_icice_in allocates qi, qs and qg with P3 to read the ice of the initial conditions (config_p3_ic_bulk_ice).

 call mpas_pool_get_config(configs,'config_p3_trplMomI',config_p3_trplMomI)
 call mpas_pool_get_config(configs,'config_p3_liqfrac',config_p3_liqfrac)
 call mpas_pool_get_config(configs,'config_p3_aerosols',config_p3_aerosols)
 call mpas_pool_get_config(configs,'config_p3_ic_bulk_ice',config_p3_ic_bulk_ice)

 nullify(mp_p3_3mom_in)
 call mpas_pool_get_package(packages,'mp_p3_3mom_inActive',mp_p3_3mom_in)
 nullify(mp_p3_3mom_cat2_in)
 call mpas_pool_get_package(packages,'mp_p3_3mom_cat2_inActive',mp_p3_3mom_cat2_in)
 nullify(mp_p3_3mom_cat3_in)
 call mpas_pool_get_package(packages,'mp_p3_3mom_cat3_inActive',mp_p3_3mom_cat3_in)
 nullify(mp_p3_lf_in)
 call mpas_pool_get_package(packages,'mp_p3_lf_inActive',mp_p3_lf_in)
 nullify(mp_p3_lf_cat2_in)
 call mpas_pool_get_package(packages,'mp_p3_lf_cat2_inActive',mp_p3_lf_cat2_in)
 nullify(mp_p3_lf_cat3_in)
 call mpas_pool_get_package(packages,'mp_p3_lf_cat3_inActive',mp_p3_lf_cat3_in)
 nullify(mp_p3_pbl_in)
 call mpas_pool_get_package(packages,'mp_p3_pbl_inActive',mp_p3_pbl_in)
 nullify(mp_p3_aers_in)
 call mpas_pool_get_package(packages,'mp_p3_aers_inActive',mp_p3_aers_in)
 nullify(mp_p3_icice_in)
 call mpas_pool_get_package(packages,'mp_p3_icice_inActive',mp_p3_icice_in)
 nullify(bl_ysu_nop3_in)
 call mpas_pool_get_package(packages,'bl_ysu_nop3_inActive',bl_ysu_nop3_in)
 nullify(bl_mynn_nop3_in)
 call mpas_pool_get_package(packages,'bl_mynn_nop3_inActive',bl_mynn_nop3_in)
 nullify(cu_ntiedtke_nop3_in)
 call mpas_pool_get_package(packages,'cu_ntiedtke_nop3_inActive',cu_ntiedtke_nop3_in)

 if(.not.associated(mp_p3_3mom_in     ) .or. .not.associated(mp_p3_3mom_cat2_in ) .or. &
    .not.associated(mp_p3_3mom_cat3_in) .or. .not.associated(mp_p3_lf_in        ) .or. &
    .not.associated(mp_p3_lf_cat2_in  ) .or. .not.associated(mp_p3_lf_cat3_in   ) .or. &
    .not.associated(mp_p3_pbl_in      ) .or. .not.associated(mp_p3_aers_in      ) .or. &
    .not.associated(mp_p3_icice_in    ) .or. .not.associated(bl_ysu_nop3_in     ) .or. &
    .not.associated(bl_mynn_nop3_in   ) .or. .not.associated(cu_ntiedtke_nop3_in)) then
    call mpas_log_write('====================================================================================',messageType=MPAS_LOG_ERR)
    call mpas_log_write('* Error while setting up packages for P3 optional prognostic scalars.',messageType=MPAS_LOG_ERR)
    call mpas_log_write('====================================================================================',messageType=MPAS_LOG_ERR)
    ierr = 1
    return
 endif

 mp_p3_3mom_in      = mp_p3_in      .and. config_p3_trplMomI
 mp_p3_3mom_cat2_in = mp_p3_cat2_in .and. config_p3_trplMomI
 mp_p3_3mom_cat3_in = mp_p3_cat3_in .and. config_p3_trplMomI
 mp_p3_lf_in        = mp_p3_in      .and. config_p3_liqfrac
 mp_p3_lf_cat2_in   = mp_p3_cat2_in .and. config_p3_liqfrac
 mp_p3_lf_cat3_in   = mp_p3_cat3_in .and. config_p3_liqfrac
 mp_p3_pbl_in       = mp_p3_in      .and. (bl_ysu_in .or. bl_mynn_in)
 mp_p3_aers_in      = mp_p3_in      .and. config_p3_aerosols
 mp_p3_icice_in     = mp_p3_in      .and. config_p3_ic_bulk_ice

 bl_ysu_nop3_in      = bl_ysu_in      .and. .not. mp_p3_in
 bl_mynn_nop3_in     = bl_mynn_in     .and. .not. mp_p3_in
 cu_ntiedtke_nop3_in = cu_ntiedtke_in .and. .not. mp_p3_in

 if(mp_p3_in) then
    call mpas_log_write('    mp_p3_3mom_in           = $l', logicArgs=(/mp_p3_3mom_in/))
    call mpas_log_write('    mp_p3_3mom_cat2_in      = $l', logicArgs=(/mp_p3_3mom_cat2_in/))
    call mpas_log_write('    mp_p3_3mom_cat3_in      = $l', logicArgs=(/mp_p3_3mom_cat3_in/))
    call mpas_log_write('    mp_p3_lf_in             = $l', logicArgs=(/mp_p3_lf_in/))
    call mpas_log_write('    mp_p3_lf_cat2_in        = $l', logicArgs=(/mp_p3_lf_cat2_in/))
    call mpas_log_write('    mp_p3_lf_cat3_in        = $l', logicArgs=(/mp_p3_lf_cat3_in/))
    call mpas_log_write('    mp_p3_pbl_in            = $l', logicArgs=(/mp_p3_pbl_in/))
    call mpas_log_write('    mp_p3_aers_in           = $l', logicArgs=(/mp_p3_aers_in/))
    call mpas_log_write('    mp_p3_icice_in          = $l', logicArgs=(/mp_p3_icice_in/))
    call mpas_log_write('')
 endif

!--- initialization of all packages for parameterizations of land surface processes:

 call mpas_pool_get_config(configs,'config_lsm_scheme',config_lsm_scheme)

 nullify(sf_noahmp_in)
 call mpas_pool_get_package(packages,'sf_noahmp_inActive',sf_noahmp_in)

 if(.not.associated(sf_noahmp_in)) then
    call mpas_log_write('====================================================================================',messageType=MPAS_LOG_ERR)
    call mpas_log_write('* Error while setting up packages for land surface options in atmosphere core.'    ,  messageType=MPAS_LOG_ERR)
    call mpas_log_write('====================================================================================',messageType=MPAS_LOG_ERR)
    ierr = 1
    return
 endif

 if(config_lsm_scheme=='sf_noahmp') then
    sf_noahmp_in = .true.
 endif

 call mpas_log_write('    sf_noahmp_in            = $l', logicArgs=(/sf_noahmp_in/))
 call mpas_log_write('')


 end function atmphys_setup_packages

!=================================================================================================================
 end module mpas_atmphys_packages
!=================================================================================================================



