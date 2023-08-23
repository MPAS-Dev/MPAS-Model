!>\file mp_nssl.F90
!! This file contains NSSL 2/3-moment MP scheme wrappers.


!>\defgroup nsslmp NSSL MP Module
!! This module contains the front end to NSSL microphysics scheme.
module mp_nssl

    use machine, only : kind_phys
    use module_mp_nssl_2mom, only : nssl_2mom_init, nssl_2mom_driver

    implicit none

    public :: mp_nssl_init, mp_nssl_run

    private
    logical :: is_initialized = .False.
    real :: nssl_qccn
!    integer :: is_theta_or_temp = 2 ! 1=theta, 2=temperature

    contains

!>\ingroup nsslmp
!> This subroutine is a wrapper around the nssl_2mom_init().
!>@{
!> \section arg_table_mp_nssl_init Argument Table
!! \htmlinclude mp_nssl_init.html
!!
    subroutine mp_nssl_init( errflg, errmsg, threads, restart, &
                              mpirank, mpiroot,    &
                              con_g, con_rd, con_cp, con_rv,  &
                              con_t0c, con_cliq, con_csol, con_eps,   &
                              imp_physics, imp_physics_nssl,  &
                              nssl_cccn, nssl_alphah, nssl_alphahl, &
                              nssl_alphar, nssl_ehw0, nssl_ehlw0,   &
                              nssl_ccn_on, nssl_hail_on, nssl_invertccn, &
                              nssl_3moment, nssl_ccn_is_ccna ) 
                              

        use module_mp_nssl_2mom, only: nssl_2mom_init, nssl_2mom_init_const

        implicit none

!         integer,                   intent(in)    :: ncol
!         integer,                   intent(in)    :: nlev
         character(len=*),          intent(  out) :: errmsg
         integer,                   intent(  out) :: errflg
         integer,                   intent(in)    :: threads
         logical,                   intent(in)    :: restart
         real(kind_phys), intent(in) :: con_g, con_rd, con_cp, con_rv, &
                             con_t0c, con_cliq, con_csol, con_eps

         integer,                   intent(in)    :: mpirank
         integer,                   intent(in)    :: mpiroot
         integer,                   intent(in)    :: imp_physics
         integer,                   intent(in)    :: imp_physics_nssl
         integer,                   intent(inout) :: nssl_ccn_is_ccna
         real(kind_phys),           intent(in)    :: nssl_cccn, nssl_alphah, nssl_alphahl
         real(kind_phys),           intent(in)    :: nssl_alphar, nssl_ehw0, nssl_ehlw0
         logical,                   intent(in)    :: nssl_ccn_on, nssl_hail_on, nssl_invertccn, &
                                                     nssl_3moment
!         integer,                   intent(in)    :: nssl_theta_or_temp

         ! Local variables: dimensions used in nssl_init
         integer               :: ims,ime, jms,jme, kms,kme, nx, nz, i,k
         real :: nssl_params(20)
         integer :: ihailv,ipc
         

 ! Initialize the CCPP error handling variables
        errflg = 0
        errmsg = ''

!            write(0,*) 'nssl_init: nlev,ncol,rank = ',nlev,ncol,mpirank

        if ( is_initialized ) return

        IF ( .not. is_initialized ) THEN ! only do this on first call
         if (mpirank==mpiroot) then
!           write(0,*) ' ----------------------------------------------------------------------------------------------------------------'
!           write(0,*) ' --- CCPP NSSL MP scheme init ---'
!           write(0,*) ' ----------------------------------------------------------------------------------------------------------------'
!           write(6,*) ' ----------------------------------------------------------------------------------------------------------------'
!           write(6,*) ' --- CCPP NSSL MP scheme init ---'
!           write(6,*) ' ----------------------------------------------------------------------------------------------------------------'
         end if
        
! update this when ccn_flag is active?
         if ( imp_physics /= imp_physics_nssl ) then
            write(errmsg,'(*(a))') "Logic error: namelist choice of microphysics is different from NSSL"
            errflg = 1
            return
         end if

         ! set some physical constants in NSSL microphysics to be consistent with parent model
         call nssl_2mom_init_const(  &
           con_g, con_rd, con_cp, con_rv, con_t0c, con_cliq, con_csol, con_eps )
         
         
         ! Set internal dimensions
!        ims = 1
!        ime = ncol
!        nx = ncol
!        jms = 1
!        jme = 1
!        kms = 1
!        kme = nlev
!        nz = nlev


         
!         is_theta_or_temp = nssl_theta_or_temp
         
         nssl_params(:) = 0.0
       !  nssl_params(1)  = nssl_cccn    ! use direct interface instead
       !  nssl_params(2)  = nssl_alphah  ! use direct interface instead
       !  nssl_params(3)  = nssl_alphahl ! use direct interface instead
         nssl_params(4)  = 4.e5 ! nssl_cnoh -- not used for 2-moment
         nssl_params(5)  = 4.e4 ! nssl_cnohl-- not used for 2-moment
         nssl_params(6)  = 4.e5 ! nssl_cnor-- not used for 2-moment
         nssl_params(7)  = 4.e6 ! nssl_cnos-- not used for 2-moment
         nssl_params(8)  = 500. ! nssl_rho_qh
         nssl_params(9)  = 800. ! nssl_rho_qhl
         nssl_params(10) = 100. ! nssl_rho_qs
         
         nssl_qccn = nssl_cccn/1.225
      !   if (mpirank==mpiroot) then
      !    write(*,*) 'nssl_init: nssl_qccn = ',nssl_qccn
      !   endif
         
         IF ( nssl_hail_on ) THEN
           ihailv = 1
         ELSE
           ihailv = -1
         ENDIF
         
         IF ( nssl_3moment ) THEN
           ipc = 8
         ELSE
           ipc = 5
         ENDIF

!           write(0,*) 'call nssl_2mom_init'
         CALL nssl_2mom_init(nssl_params=nssl_params,ipctmp=ipc,mixphase=0,   &
                nssl_ehw0=nssl_ehw0,nssl_ehlw0=nssl_ehlw0,errmsg=errmsg, &
                nssl_alphar=nssl_alphar,                                                    &
                nssl_alphah=nssl_alphah,                                                    &
                nssl_alphahl=nssl_alphahl,                                                  &
                nssl_density_on=.true.,       &
                nssl_hail_on= nssl_hail_on,                                        &
                nssl_ccn_on = nssl_ccn_on,                                         &
                nssl_cccn=nssl_cccn, ccn_is_ccna=nssl_ccn_is_ccna,                          &
                errflg=errflg,myrank=mpirank,mpiroot=mpiroot)

         ! For restart runs, the init is done here
         if (restart) then
           is_initialized = .true.
           return
         end if

!        Other initialization operation here....

         is_initialized = .true.

         ENDIF ! .not. is_initialized
         
         return

    end subroutine mp_nssl_init
!>@}

!>\ingroup nsslmp
!>\section gen_nssl NSSL MP General Algorithm: interface to driver
!>@{
!> \section arg_table_mp_nssl_run Argument Table
!! \htmlinclude mp_nssl_run.html
!!

! This subroutine is not used or needed in MPAS since driver is called directly;
    subroutine mp_nssl_run(ncol, nlev, con_g, con_rd, mpirank, &
!                             spechum, cccn, qc, qr, qi, qs, qh, qhl,         &
                             spechum, cccn, cccna, qc, qr, qi, qs, qh, qhl,         &
                             ccw, crw, cci, csw, chw, chl, vh, vhl,          &
                             zrw, zhw, zhl,                                  &
                              tgrs, prslk, prsl, phii, omega, dtp,           &
                              prcp, rain, graupel, ice, snow, sr,            &
                             refl_10cm, do_radar_ref, first_time_step, restart, &
                             re_cloud, re_ice, re_snow, re_rain,             &
                             nleffr, nieffr, nseffr, nreffr,                 &
                             imp_physics, convert_dry_rho,                   &
                             imp_physics_nssl, nssl_ccn_on,                  &
                             nssl_hail_on, nssl_invertccn, nssl_3moment,     &
                             ntccn, ntccna,    &
                             errflg, errmsg)

        use module_mp_nssl_2mom, only: calcnfromq, na

        implicit none
        integer, intent(in) :: ncol, nlev
         real(kind_phys),           intent(in   ) :: con_g
         real(kind_phys),           intent(in   ) :: con_rd
         integer,                   intent(in)    :: mpirank
         ! Hydrometeors
         logical,                   intent(in   ) :: convert_dry_rho
         real(kind_phys),           intent(inout) :: spechum(:,:) !(1:ncol,1:nlev)
         real(kind_phys),           intent(inout) :: cccn(:,:) !(1:ncol,1:nlev)
         real(kind_phys),           intent(inout) :: cccna(:,:) !(1:ncol,1:nlev)
         real(kind_phys),           intent(inout) :: qc (:,:) !(1:ncol,1:nlev)
         real(kind_phys),           intent(inout) :: qr (:,:) !(1:ncol,1:nlev)
         real(kind_phys),           intent(inout) :: qi (:,:) !(1:ncol,1:nlev)
         real(kind_phys),           intent(inout) :: qs (:,:) !(1:ncol,1:nlev)
         real(kind_phys),           intent(inout) :: qh (:,:) !(1:ncol,1:nlev) graupel
         real(kind_phys),           intent(inout) :: qhl(:,:) !(1:ncol,1:nlev) hail
         real(kind_phys),           intent(inout) :: ccw(:,:) !(1:ncol,1:nlev)
         real(kind_phys),           intent(inout) :: crw(:,:) !(1:ncol,1:nlev)
         real(kind_phys),           intent(inout) :: cci(:,:) !(1:ncol,1:nlev)
         real(kind_phys),           intent(inout) :: csw(:,:) !(1:ncol,1:nlev)
         real(kind_phys),           intent(inout) :: chw(:,:) !(1:ncol,1:nlev) graupel number 
         real(kind_phys),           intent(inout) :: chl(:,:) !(1:ncol,1:nlev) hail number
         real(kind_phys),           intent(inout) :: vh (:,:) !(1:ncol,1:nlev) graupel volume 
         real(kind_phys),           intent(inout) :: vhl(:,:) !(1:ncol,1:nlev) hail volume
         real(kind_phys),           intent(inout) :: zrw(:,:) !(1:ncol,1:nlev) rain reflectivity
         real(kind_phys),           intent(inout) :: zhw(:,:) !(1:ncol,1:nlev) graupel reflectivity
         real(kind_phys),           intent(inout) :: zhl(:,:) !(1:ncol,1:nlev) hail reflectivity
         ! State variables and timestep information
         real(kind_phys),           intent(inout) :: tgrs (:,:) !(1:ncol,1:nlev)
         real(kind_phys),           intent(in   ) :: prsl (:,:) !(1:ncol,1:nlev)
         real(kind_phys),           intent(in   ) :: prslk(:,:) !(1:ncol,1:nlev)
         real(kind_phys),           intent(in   ) :: phii (:,:) !(1:ncol,1:nlev+1)
         real(kind_phys),           intent(in   ) :: omega(:,:) !(1:ncol,1:nlev)
         real(kind_phys),           intent(in   ) :: dtp
         ! Precip/rain/snow/graupel fall amounts and fraction of frozen precip
         real(kind_phys),           intent(  out) :: prcp   (:) !(1:ncol)
         real(kind_phys),           intent(  out) :: rain   (:) !(1:ncol)
         real(kind_phys),           intent(  out) :: graupel(:) !(1:ncol)
         real(kind_phys),           intent(  out) :: ice    (:) !(1:ncol)
         real(kind_phys),           intent(  out) :: snow   (:) !(1:ncol)
         real(kind_phys),           intent(  out) :: sr     (:) !(1:ncol)
         ! Radar reflectivity
         real(kind_phys),           intent(inout) :: refl_10cm(:,:) !(1:ncol,1:nlev)
         logical,                   intent(in   ) :: do_radar_ref, first_time_step
         logical,                   intent(in)    :: restart
         ! Cloud effective radii
         real(kind_phys),  intent(inout) :: re_cloud(:,:) ! (1:ncol,1:nlev)
         real(kind_phys),  intent(inout) :: re_ice(:,:) ! (1:ncol,1:nlev)
         real(kind_phys),  intent(inout) :: re_snow(:,:) ! (1:ncol,1:nlev)
         real(kind_phys),  intent(inout) :: re_rain(:,:) ! (1:ncol,1:nlev)
         integer, intent(in) :: nleffr, nieffr, nseffr, nreffr
         integer,                   intent(in)    :: imp_physics
         integer,                   intent(in)    :: imp_physics_nssl
         logical,                   intent(in)    :: nssl_ccn_on, nssl_hail_on, nssl_invertccn, nssl_3moment
         integer,                   intent(in)    :: ntccn, ntccna
         real(kind_phys), parameter :: zscale = 1.d0, zscaleinv = 1./zscale
        
        integer,          intent(out)   :: errflg
        character(len=*), intent(out)   :: errmsg


         ! Local variables

         ! Air density
         real(kind_phys) :: rho(1:ncol,1:nlev)              !< kg m-3
         ! Hydrometeors
         real(kind_phys) :: qv_mp(1:ncol,1:nlev)            !< kg kg-1 (dry mixing ratio)
         real(kind_phys) :: qc_mp(1:ncol,1:nlev)            !< kg kg-1 (dry mixing ratio)
         real(kind_phys) :: qr_mp(1:ncol,1:nlev)            !< kg kg-1 (dry mixing ratio)
         real(kind_phys) :: qi_mp(1:ncol,1:nlev)            !< kg kg-1 (dry mixing ratio)
         real(kind_phys) :: qs_mp(1:ncol,1:nlev)            !< kg kg-1 (dry mixing ratio)
         real(kind_phys) :: qh_mp(1:ncol,1:nlev)            !< kg kg-1 (graupel dry mixing ratio)
         real(kind_phys) :: qhl_mp(1:ncol,1:nlev)           !< kg kg-1 (hail dry mixing ratio)
         real(kind_phys) :: nc_mp(1:ncol,1:nlev)            !< droplet num. conc.
         real(kind_phys) :: nr_mp(1:ncol,1:nlev)            !< rain num. conc.
         real(kind_phys) :: ni_mp(1:ncol,1:nlev)            !< ice crystal num. conc.
         real(kind_phys) :: ns_mp(1:ncol,1:nlev)            !< snow num. conc.
         real(kind_phys) :: nh_mp(1:ncol,1:nlev)            !< graupel num. conc.
         real(kind_phys) :: nhl_mp(1:ncol,1:nlev)           !< hail num. conc.
         real(kind_phys) :: cn_mp(1:ncol,1:nlev) 
         real(kind_phys) :: cna_mp(1:ncol,1:nlev) 
         real(kind_phys) :: cccn_mp(1:ncol,1:nlev) 
         real(kind_phys) :: cccna_mp(1:ncol,1:nlev) 
         real(kind_phys) :: vh_mp(1:ncol,1:nlev)           !< m3 kg-1 (volume mixing ratio)
         ! create temporaries for hail in case it does not exist
         !real(kind_phys) :: chl_mp(1:ncol,1:nlev)           !< kg-1 (number mixing ratio)
         real(kind_phys) :: vhl_mp(1:ncol,1:nlev)           !< m3 kg-1 (volume mixing ratio)
         real(kind_phys) :: zrw_mp(1:ncol,1:nlev)           !< m6 kg-1 (reflectivity)
         real(kind_phys) :: zhw_mp(1:ncol,1:nlev)           !< m6 kg-1 (reflectivity)
         real(kind_phys) :: zhl_mp(1:ncol,1:nlev)           !< m6 kg-1 (reflectivity)
         ! Vertical velocity and level width
         real(kind_phys) :: w(1:ncol,1:nlev)                !< m s-1
         real(kind_phys) :: dz(1:ncol,1:nlev)               !< m

         ! Rain/snow/graupel fall amounts
         real(kind_phys) :: rain_mp(1:ncol)                 ! mm, dummy, not used
         real(kind_phys) :: graupel_mp(1:ncol)              ! mm, dummy, not used
         real(kind_phys) :: ice_mp(1:ncol)                  ! mm, dummy, not used
         real(kind_phys) :: snow_mp(1:ncol)                 ! mm, dummy, not used
         real(kind_phys) :: delta_rain_mp(1:ncol)           ! mm
         real(kind_phys) :: delta_graupel_mp(1:ncol)        ! mm
         real(kind_phys) :: delta_ice_mp(1:ncol)            ! mm
         real(kind_phys) :: delta_snow_mp(1:ncol)           ! mm

         real(kind_phys) :: xrain_mp(1:ncol)                 ! mm, dummy, not used
         real(kind_phys) :: xgraupel_mp(1:ncol)              ! mm, dummy, not used
         real(kind_phys) :: xice_mp(1:ncol)                  ! mm, dummy, not used
         real(kind_phys) :: xsnow_mp(1:ncol)                 ! mm, dummy, not used
         real(kind_phys) :: xdelta_rain_mp(1:ncol)           ! mm
         real(kind_phys) :: xdelta_graupel_mp(1:ncol)        ! mm
         real(kind_phys) :: xdelta_ice_mp(1:ncol)            ! mm
         real(kind_phys) :: xdelta_snow_mp(1:ncol)           ! mm

         ! Radar reflectivity
         logical         :: diagflag                        ! must be true if do_radar_ref is true, not used otherwise
         integer         :: do_radar_ref_mp                 ! integer instead of logical do_radar_ref
         ! Effective cloud radii
         logical         :: do_effective_radii
         real(kind_phys) :: re_cloud_mp(1:ncol,1:nlev)      ! m
         real(kind_phys) :: re_ice_mp(1:ncol,1:nlev)        ! m
         real(kind_phys) :: re_snow_mp(1:ncol,1:nlev)       ! m
         real(kind_phys) :: re_rain_mp(1:ncol,1:nlev)       ! m
         integer         :: has_reqc
         integer         :: has_reqi
         integer         :: has_reqs
         integer         :: has_reqr
         ! Dimensions used in driver
         integer         :: ids,ide, jds,jde, kds,kde, &
                            ims,ime, jms,jme, kms,kme, &
                            its,ite, jts,jte, kts,kte, i,j,k
         integer :: itimestep ! timestep counter
         integer :: ntmul, n
         real, parameter    :: dtpmax = 60. ! allow up to dt=75 (1.25*60)
         real(kind_phys)    :: dtptmp
         integer, parameter :: ndebug = 0
         logical :: invertccn
         real :: cwmas
         
         real(kind_phys), allocatable :: an(:,:,:,:) ! temporary scalar array


        errflg = 0
        errmsg = ''

!            write(0,*) 'nssl_run: nlev,ncol,rank = ',nlev,ncol,mpirank

        IF ( ndebug >= 1 ) write(0,*) 'In physics nssl_run'


         ! Check initialization state
         if (.not.is_initialized) then
            write(errmsg, fmt='((a))') 'mp_nssl_run called before mp_nssl_init'
            errflg = 1
            return
         end if
         
         invertccn = nssl_invertccn

         !> - Convert specific humidity/moist mixing ratios to dry mixing ratios
         ! NOTE: Implied loops!
         qv_mp = spechum/(1.0_kind_phys-spechum)
         IF ( convert_dry_rho ) THEN
         qc_mp = qc/(1.0_kind_phys-spechum)
         qr_mp = qr/(1.0_kind_phys-spechum)
         qi_mp = qi/(1.0_kind_phys-spechum)
         qs_mp = qs/(1.0_kind_phys-spechum)
         qh_mp = qh/(1.0_kind_phys-spechum)
         
         IF ( nssl_ccn_on ) cccn_mp = cccn/(1.0_kind_phys-spechum)
!         cccna_mp = cccna/(1.0_kind_phys-spechum)
         nc_mp = ccw/(1.0_kind_phys-spechum)
         nr_mp = crw/(1.0_kind_phys-spechum)
         ni_mp = cci/(1.0_kind_phys-spechum)
         ns_mp = csw/(1.0_kind_phys-spechum)
         nh_mp = chw/(1.0_kind_phys-spechum)
         vh_mp = vh/(1.0_kind_phys-spechum)
         IF ( nssl_3moment ) THEN
           zrw_mp = zrw/(1.0_kind_phys-spechum)*zscaleinv
           zhw_mp = zhw/(1.0_kind_phys-spechum)*zscaleinv
         ENDIF
         IF ( nssl_hail_on ) THEN
           qhl_mp = qhl/(1.0_kind_phys-spechum)
           nhl_mp = chl/(1.0_kind_phys-spechum)
           vhl_mp = vhl/(1.0_kind_phys-spechum)
           IF ( nssl_3moment ) THEN
             zhl_mp = zhl/(1.0_kind_phys-spechum)*zscaleinv
           ENDIF
         ENDIF
         ELSE
!         qv_mp = spechum ! /(1.0_kind_phys-spechum)
         qc_mp = qc ! /(1.0_kind_phys-spechum)
         qr_mp = qr ! /(1.0_kind_phys-spechum)
         qi_mp = qi ! /(1.0_kind_phys-spechum)
         qs_mp = qs ! /(1.0_kind_phys-spechum)
         qh_mp = qh ! /(1.0_kind_phys-spechum)
         IF ( nssl_ccn_on ) cccn_mp = cccn
!         cccna_mp = cccna
         nc_mp = ccw
         nr_mp = crw
         ni_mp = cci
         ns_mp = csw
         nh_mp = chw
         vh_mp = vh
         IF ( nssl_3moment ) THEN
           zrw_mp = zrw*zscaleinv
           zhw_mp = zhw*zscaleinv
         ENDIF
         IF ( nssl_hail_on ) THEN
           qhl_mp = qhl ! /(1.0_kind_phys-spechum)
           nhl_mp = chl
           vhl_mp = vhl
           IF ( nssl_3moment ) THEN
             zhl_mp = zhl*zscaleinv
           ENDIF
         ENDIF
         
         ENDIF
         
         IF ( nssl_hail_on ) THEN
!           nhl_mp = chl
!           vhl_mp = vhl
         ELSE
           qhl_mp = 0
           nhl_mp = 0
           vhl_mp = 0
         ENDIF

!           IF ( .false. ) THEN
!             write(6,*) 'nsslrun: qc,max ccw = ',mpirank,maxval(qc_mp),maxval(nc_mp),sum(nc_mp)
!              IF ( mpirank == 1 ) THEN
!               DO k=1,nlev
!                 DO i=1,ncol
!                   IF ( qc_mp(i,k) > 1.e-6 .and. nc_mp(i,k) <= 1.e-9 ) THEN
!                     write(6,*) 'i,k,qc,nc,ccn = ',i,k,qc_mp(i,k),nc_mp(i,k),cccn_mp(i,k)
!                   ENDIF
!                 ENDDO
!               ENDDO
!              ENDIF
!            ENDIF

     !     IF ( first_time_step ) THEN
     !       write(0,*) 'mp_nssl_run: qi,qs,qh maxval: ',maxval(qi),maxval(qs),maxval(qh)
     !       write(0,*) 'mp_nssl_run: ni,ns,nh maxval: ',maxval(ni_mp),maxval(ns_mp),maxval(nh_mp)
     !     ENDIF

         
         !> - Density of air in kg m-3
         rho = prsl/(con_rd*tgrs)

         !> - Convert omega in Pa s-1 to vertical velocity w in m s-1
         w = -omega/(rho*con_g)

         !> - Layer thickness in m from geopotential in m2 s-2
         dz = (phii(:,2:nlev+1) - phii(:,1:nlev)) / con_g

         ! Accumulated values inside scheme, not used;
         ! only use delta and add to inout variables (different units)
         rain_mp          = 0
         graupel_mp       = 0
         ice_mp           = 0
         snow_mp          = 0
         delta_rain_mp    = 0
         delta_graupel_mp = 0
         delta_ice_mp     = 0
         delta_snow_mp    = 0
         xrain_mp          = 0
         xgraupel_mp       = 0
         xice_mp           = 0
         xsnow_mp          = 0
         xdelta_rain_mp    = 0
         xdelta_graupel_mp = 0
         xdelta_ice_mp     = 0
         xdelta_snow_mp    = 0
         IF ( ndebug > 1 ) THEN
         write(*,*) 'Max q before micro'
         write(*,*) 'qc = ',1000.*maxval(qc_mp)
         write(*,*) 'qr = ',1000.*maxval(qr_mp)
         write(*,*) 'qi = ',1000.*maxval(qi_mp)
         write(*,*) 'qs = ',1000.*maxval(qs_mp)
         write(*,*) 'qh = ',1000.*maxval(qh_mp)
         IF ( nssl_hail_on ) write(*,*) 'qhl = ',1000.*maxval(qhl_mp)
         write(*,*) 'ccw = ',1.e-6*maxval(ccw*rho)
         ENDIF

         ! Flags for calculating radar reflectivity; diagflag is redundant
         if (do_radar_ref) then
             diagflag = .true.
             do_radar_ref_mp = 1
         else
             diagflag = .false.
             do_radar_ref_mp = 0
         end if

          do_effective_radii = .false.
         IF ( nleffr > 0 .and. nieffr > 0 .and. nseffr > 0 .and. nreffr > 0 ) THEN
         ! if (present(re_cloud) .and. present(re_ice) .and. present(re_snow)) then
             do_effective_radii = .true.
             has_reqc = 1
             has_reqi = 1
             has_reqs = 1
             has_reqr = 1
         else if (nleffr < 1 .and. nieffr < 1 .and. nseffr < 1 .and. nreffr < 1 ) then
             do_effective_radii = .false.
             has_reqc = 0
             has_reqi = 0
             has_reqs = 0
             has_reqr = 0
         else
             write(errmsg,fmt='(*(a))') 'Logic error in mp_nssl_run:',  &
                                        ' hydrometeor radius calculation logic problem'
             errflg = 1
             return
         end if
         ! Initialize to zero, intent(out) variables
         re_cloud_mp = 0
         re_ice_mp   = 0
         re_snow_mp  = 0
         re_rain_mp  = 0

         ! Set internal dimensions
         ids = 1
         ims = 1
         its = 1
         ide = ncol
         ime = ncol
         ite = ncol
         jds = 1
         jms = 1
         jts = 1
         jde = 1
         jme = 1
         jte = 1
         kds = 1
         kms = 1
         kts = 1
         kde = nlev
         kme = nlev
         kte = nlev


       IF ( ndebug >= 1 )  write(0,*) 'call nssl_2mom_driver'

        IF ( dtp > 1.25001*dtpmax ) THEN
           ntmul = Max(2, Nint( dtp/dtpmax ) )
           dtptmp = dtp/ntmul
        ELSE
           dtptmp = dtp
           ntmul = 1
        ENDIF
        
        IF ( first_time_step .and. .not. restart ) THEN
          itimestep = 0 ! gets incremented to 1 in call loop
          IF ( nssl_ccn_on ) THEN
            IF ( invertccn ) THEN
              cccn_mp = 0
              !cccn = nssl_qccn
            ELSE
              cccn_mp = nssl_qccn
            ENDIF
          ENDIF
        ELSE
          itimestep = 2
        ENDIF
         
        IF ( .false. ) THEN ! disable for now, as logic in the NSSL driver does this, but may switch back to here
         ! incoming droplet field may have some inconsistent number concentrations (e.g., from PBL)
         ! so check for that, otherwise mass may be zapped into vapor
         allocate( an(ncol,1,nlev,na) )
         an(:,:,:,:) = 0.0 ! needed for workspace in routine

         cwmas = 1000.*0.523599*(2.*9.e-6)**3
         
         call calcnfromq(nx=ncol,ny=1,nz=nlev,an=an,na=na,nor=0,norz=0,dn=rho, &
     &    qcw=qc_mp,qci=qi_mp, &
     &    ccw=nc_mp,cci=ni_mp,  &
     &    cccn=cccn_mp,qv=qv_mp, invertccn_flag=nssl_invertccn, cwmasin=cwmas )

!           IF ( .false. ) THEN
!             write(6,*) 'nsslrun2: qc,max ccw = ',mpirank,maxval(qc_mp),maxval(nc_mp),sum(nc_mp)
!              IF ( mpirank == 1 ) THEN
!               DO k=1,nlev
!                 DO i=1,ncol
!                   IF ( qc_mp(i,k) > 1.e-6 .and. nc_mp(i,k) <= 1.e-9 ) THEN
!                     write(6,*) 'i2,k,qc,nc,ccn = ',i,k,qc_mp(i,k),nc_mp(i,k),cccn_mp(i,k)
!                   ENDIF
!                 ENDDO
!               ENDDO
!              ENDIF
!            ENDIF


         deallocate( an )
        ENDIF
         
       IF ( nssl_ccn_on ) THEN
         IF ( invertccn ) THEN
            ! cn_mp = Max(0.0, nssl_qccn - Max(0.0,cccn_mp)) 
           ! Flip CCN concentrations from 'activated' to 'unactivated' (allows BC condition to be zero) 
               cn_mp = nssl_qccn - cccn_mp
               cn_mp = Max(0.0_kind_phys, cn_mp) 

         ELSE
            cn_mp = cccn_mp
         ENDIF
          IF ( ntccna > 0 ) THEN
            ! not in use yet
!         cna_mp = cccna
          ELSE 
            cna_mp = 0
          ENDIF
        ENDIF
       
       IF ( .true. ) THEN
        DO n = 1,ntmul
        
        itimestep = itimestep + 1



!         IF ( nssl_ccn_on )  THEN


         CALL nssl_2mom_driver(                         &
                    ITIMESTEP=itimestep,                &
#ifdef USE_THETA
                     TH=tgrs,                           &
#else
                     tt=tgrs,                           &
#endif
                     QV=qv_mp,                          &
                     QC=qc_mp,                          &
                     QR=qr_mp,                          &
                     QI=qi_mp,                          &
                     QS=qs_mp,                          &
                     QH=qh_mp,                          &
                     QHL=qhl_mp,                        &
                     CCW=nc_mp,                         &
                     CRW=nr_mp,                         &
                     CCI=ni_mp,                         &
                     CSW=ns_mp,                         &
                     CHW=nh_mp,                         &
                     CHL=nhl_mp,                        &
                     VHW=vh_mp,                         &
                     VHL=vhl_mp,                        &
                     cn=cn_mp, f_cn=( nssl_ccn_on ),    &
                     ZRW=zrw_mp,                        &
                     ZHW=zhw_mp,                        &
                     ZHL=zhl_mp,                        &
!                    cna=cna_mp, f_cna=( ntccna > 0 ),  & ! for future use
                     cna=cna_mp, f_cna=.false. ,        &
                     PII=prslk,                         &
                     P=prsl,                            &
                     W=w,                               &
                     DZ=dz,                             &
                     DTP=dtptmp,                        &
                     DN=rho,                            &
                     rainnc=xrain_mp, rainncv=xdelta_rain_mp, &
                     snownc=xsnow_mp, snowncv=xdelta_snow_mp, &
!                     icenc=ice_mp, icencv=delta_ice_mp,      &
                     GRPLNC=xgraupel_mp, GRPLNCV=xdelta_graupel_mp, sr=sr,      &
                     dbz      = refl_10cm,              &
!                     nssl_progn=.false.,               &
                     diagflag = diagflag,               &
                     errmsg=errmsg,errflg=errflg,       &
                     re_cloud=re_cloud_mp,              &
                     re_ice=re_ice_mp,                  &
                     re_snow=re_snow_mp,                &
                     re_rain=re_rain_mp,                &
                     has_reqc=has_reqc,                 & ! ala G. Thompson
                     has_reqi=has_reqi,                 & ! ala G. Thompson
                     has_reqs=has_reqs,                 & ! ala G. Thompson
                     has_reqr=has_reqr,                 &
                  IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde, &
                  IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme, &
                  ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte  &
                                                                    )


           
           
           DO i = 1,ncol
             delta_rain_mp(i) = delta_rain_mp(i) + xdelta_rain_mp(i) ! this is liquid equivalent of all precip
             delta_graupel_mp(i) = delta_graupel_mp(i) + xdelta_graupel_mp(i) ! this is liquid equivalent of graupel
             delta_ice_mp(i) = delta_ice_mp(i) + xdelta_ice_mp(i)
             delta_snow_mp(i) = delta_snow_mp(i) + xdelta_snow_mp(i)
           ENDDO

          ENDDO
          
          ENDIF


         IF ( nssl_ccn_on )  THEN
           IF ( invertccn ) THEN
              cccn_mp = Max(0.0_kind_phys, nssl_qccn - cn_mp )
!              cccn_mp = nssl_qccn - cn_mp
           ELSE
              cccn_mp = cn_mp
           ENDIF
!           cccna = cna_mp ! cna not in use yet for ccpp
          ENDIF
          
! test code
!          IF ( ntccna > 1 .and. do_effective_radii ) THEN
!            cccna = re_ice_mp*1.0E6_kind_phys
!          ENDIF

        IF ( ndebug > 1 ) write(0,*) 'done nssl_2mom_driver'

         if (errflg/=0) return

         IF ( ndebug > 1 ) THEN
         write(*,*) 'Max q after micro'
         write(*,*) 'qc = ',1000.*maxval(qc_mp)
         write(*,*) 'qr = ',1000.*maxval(qr_mp)
         write(*,*) 'qi = ',1000.*maxval(qi_mp)
         write(*,*) 'qs = ',1000.*maxval(qs_mp)
         write(*,*) 'qh = ',1000.*maxval(qh_mp)
         IF ( nssl_hail_on ) THEN
           write(*,*) 'qhl = ',1000.*maxval(qhl_mp)
         ENDIF
         write(*,*) 'ccw = ',1.e-6*maxval(ccw*rho)
           IF ( 1000.*maxval(qc_mp) > 0.5 .or. 1000.*maxval(qi_mp) > 0.09 .or. 1000.*maxval(qs_mp) > 0.1 ) THEN
             IF ( nssl_ccn_on ) THEN
             write(*,*) 'qc, ccn, ccw, tt, qi+qs by height'
             DO k = 1,nlev
               write(*,*) qc_mp(1,k)*1000., cccn(1,k)*rho(1,k)*1.e-6, ccw(1,k)*rho(1,k)*1.e-6, tgrs(1,k), (qs_mp(1,k)+qi_mp(1,k))*1000. ! cccn(1,k)*1.e-6
             ENDDO
             ELSE
             write(*,*) 'qc, ccn, ccw, tt, qi+qs by height'
             DO k = 1,nlev
               write(*,*) qc_mp(1,k)*1000., cccn(1,k)*rho(1,k)*1.e-6, 0.0, tgrs(1,k), (qs_mp(1,k)+qi_mp(1,k))*1000. ! cccn(1,k)*1.e-6
             ENDDO
             ENDIF
           ENDIF
         ENDIF


         !> - Convert dry mixing ratios to specific humidity/moist mixing ratios
         spechum = qv_mp/(1.0_kind_phys+qv_mp)
         IF ( convert_dry_rho ) THEN
         qc      = qc_mp/(1.0_kind_phys+qv_mp)
         qr      = qr_mp/(1.0_kind_phys+qv_mp)
         qi      = qi_mp/(1.0_kind_phys+qv_mp)
         qs      = qs_mp/(1.0_kind_phys+qv_mp)
         qh      = qh_mp/(1.0_kind_phys+qv_mp)
         IF ( nssl_ccn_on ) cccn    = cccn_mp/(1.0_kind_phys+qv_mp)
!         cccna   = cccna_mp/(1.0_kind_phys+qv_mp)
         ccw      = nc_mp/(1.0_kind_phys+qv_mp)
         crw      = nr_mp/(1.0_kind_phys+qv_mp)
         cci      = ni_mp/(1.0_kind_phys+qv_mp)
         csw      = ns_mp/(1.0_kind_phys+qv_mp)
         chw      = nh_mp/(1.0_kind_phys+qv_mp)
         vh       = vh_mp/(1.0_kind_phys+qv_mp)
          IF ( nssl_3moment ) THEN
           zrw = zrw_mp/(1.0_kind_phys+qv_mp)*zscale
           zhw = zhw_mp/(1.0_kind_phys+qv_mp)*zscale
          ENDIF
         IF ( nssl_hail_on ) THEN
          qhl     = qhl_mp/(1.0_kind_phys+qv_mp)
          chl     = nhl_mp/(1.0_kind_phys+qv_mp)
          vhl     = vhl_mp/(1.0_kind_phys+qv_mp)
          IF ( nssl_3moment ) THEN
           zhl = zhl_mp/(1.0_kind_phys+qv_mp)*zscale
          ENDIF
         ENDIF
         ELSE
!         spechum = qv_mp ! /(1.0_kind_phys+qv_mp)
         qc      = qc_mp ! /(1.0_kind_phys+qv_mp)
         qr      = qr_mp ! /(1.0_kind_phys+qv_mp)
         qi      = qi_mp ! /(1.0_kind_phys+qv_mp)
         qs      = qs_mp ! /(1.0_kind_phys+qv_mp)
         qh      = qh_mp ! /(1.0_kind_phys+qv_mp)
         IF ( nssl_ccn_on ) cccn    = cccn_mp
!         cccna   = cccna_mp
         ccw      = nc_mp
         crw      = nr_mp
         cci      = ni_mp
         csw      = ns_mp
         chw      = nh_mp
         vh       = vh_mp
          IF ( nssl_3moment ) THEN
           zrw = zrw_mp*zscale
           zhw = zhw_mp*zscale
          ENDIF
         IF ( nssl_hail_on ) THEN
          qhl     = qhl_mp ! /(1.0_kind_phys+qv_mp)
          chl     = nhl_mp
          vhl     = vhl_mp
          IF ( nssl_3moment ) THEN
           zhl = zhl_mp*zscale
          ENDIF
         ENDIF
         
         ENDIF

!        write(0,*) 'mp_nssl: done q'

         !> - Convert rainfall deltas from mm to m (on physics timestep); add to inout variables
         ! "rain" in NSSL MP refers to precipitation (total of liquid rainfall+snow+graupel+ice)
         
         prcp    = max(0.0, delta_rain_mp/1000.0_kind_phys)
         graupel = max(0.0, delta_graupel_mp/1000.0_kind_phys)
         ice     = max(0.0, delta_ice_mp/1000.0_kind_phys)
         snow    = max(0.0, delta_snow_mp/1000.0_kind_phys)
         rain    = max(0.0, (delta_rain_mp - (delta_graupel_mp + delta_ice_mp + delta_snow_mp))/1000.0_kind_phys)

!        write(0,*) 'mp_nssl: done precip'

         if (do_effective_radii) then
            ! Convert m to micron
            re_cloud = re_cloud_mp*1.0E6_kind_phys
            re_ice   = re_ice_mp*1.0E6_kind_phys
            re_snow  = re_snow_mp*1.0E6_kind_phys
            re_rain  = re_rain_mp*1.0E6_kind_phys
         end if

        IF ( ndebug >= 1 ) write(0,*) 'mp_nssl: end'

    end subroutine mp_nssl_run
!>@}

!>\ingroup nsslmp
!> This subroutine is a dummy finalize for nssl_2mom.
!>@{
!> \section arg_table_mp_nssl_finalize Argument Table
!! \htmlinclude mp_nssl_finalize.html
!!
      subroutine mp_nssl_finalize(errmsg, errflg)

         implicit none

         character(len=*),          intent(  out) :: errmsg
         integer,                   intent(  out) :: errflg

         ! Initialize the CCPP error handling variables
         errmsg = ''
         errflg = 0


      end subroutine mp_nssl_finalize
!>@}

end module mp_nssl
