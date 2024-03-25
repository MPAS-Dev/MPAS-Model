!=================================================================================================================
 module module_bl_mynn
 use mpas_kind_types,only: kind_phys => RKIND
 use mpas_log

 use bl_mynn,only: bl_mynn_run
 use bl_mynn_post,only: bl_mynn_post_run
 use bl_mynn_pre,only: bl_mynn_pre_run

 implicit none
 private
 public:: mynn_bl_driver


 contains


!=================================================================================================================
 subroutine mynn_bl_driver &
                 (ids               , ide               , jds                , jde                , &
                  kds               , kde               , ims                , ime                , &
                  jms               , jme               , kms                , kme                , &
                  its               , ite               , jts                , jte                , &
                  kts               , kte               , f_qc               , f_qi               , &
                  f_qs              , f_qoz             , f_nc               , f_ni               , &
                  f_nifa            , f_nwfa            , f_nbca             , initflag           , &
                  do_restart        , do_DAcycling      , icloud_bl          , delt               , &
                  dx                , xland             , ps                 , ts                 , &
                  qsfc              , ust               , ch                 , hfx                , &
                  qfx               , rmol              , wspd               , znt                , &
                  uoce              , voce              , dz                 , u                  , &
                  v                 , w                 , th                 , tt                 , &
                  p                 , exner             , rho                , qv                 , &
                  qc                , qi                , qs                 , nc                 , &
                  ni                , nifa              , nwfa               ,  nbca              , &
                  qoz               , rthraten          , pblh               , kpbl               , &
                  cldfra_bl         , qc_bl             , qi_bl              , maxwidth           , &
                  maxmf             , ktop_plume        , ztop_plume         , qke                , &
                  qke_adv           , tsq               , qsq                , cov                , &
                  el_pbl            , rublten           , rvblten            , rthblten           , &
                  rqvblten          , rqcblten          , rqiblten           , rqsblten           , &
                  rncblten          , rniblten          , rnifablten         , rnwfablten         , &
                  rnbcablten        , rqozblten         , edmf_a             , edmf_w             , &
                  edmf_qt           , edmf_thl          , edmf_ent           , edmf_qc            , &
                  sub_thl           , sub_sqv           , det_thl            , det_sqv            , &
                  exch_h            , exch_m            , dqke               , qwt                , &
                  qshear            , qbuoy             , qdiss              , sh3d               , &
                  sm3d              , spp_pbl           , pattern_spp        ,                      &
                  bl_mynn_tkeadvect , bl_mynn_tkebudget , bl_mynn_cloudpdf   , bl_mynn_mixlength  , &
                  bl_mynn_closure   , bl_mynn_stfunc    , bl_mynn_topdown    , bl_mynn_scaleaware , &
                  bl_mynn_dheat_opt , bl_mynn_edmf      , bl_mynn_edmf_dd    , bl_mynn_edmf_mom   , &
                  bl_mynn_edmf_tke  , bl_mynn_output    , bl_mynn_mixscalars , bl_mynn_cloudmix   , &
                  bl_mynn_mixqt     , errmsg            , errflg                                    &
#if(WRF_CHEM == 1)
                 ,mix_chem   , nchem        , kdvel       , ndvel        , chem3         , vd3d   , &
                  frp_mean   , emis_ant_no                                                          &
#endif
               )

!=================================================================================================================

!--- input arguments:
 logical,intent(in):: &
    f_qc,               &! if true,the physics package includes the cloud liquid water mixing ratio.
    f_qi,               &! if true,the physics package includes the cloud ice mixing ratio.
    f_qs,               &! if true,the physics package includes the snow mixing ratio.
    f_qoz,              &! if true,the physics package includes the ozone mixing ratio.
    f_nc,               &! if true,the physics package includes the cloud liquid water number concentration.
    f_ni,               &! if true,the physics package includes the cloud ice number concentration.
    f_nifa,             &! if true,the physics package includes the "ice-friendly" aerosol number concentration.
    f_nwfa,             &! if true,the physics package includes the "water-friendly" aerosol number concentration.
    f_nbca               ! if true,the physics package includes the number concentration of black carbon.

 logical,intent(in):: &
    bl_mynn_tkeadvect    !

 logical,intent(in):: &
    do_restart,         &!
    do_DAcycling         !

 integer,intent(in):: &
    ids,ide,jds,jde,kds,kde, &
    ims,ime,jms,jme,kms,kme, &
    its,ite,jts,jte,kts,kte

 integer,intent(in):: &
    bl_mynn_cloudpdf,   &!
    bl_mynn_mixlength,  &!
    bl_mynn_stfunc,     &!
    bl_mynn_topdown,    &!
    bl_mynn_scaleaware, &!
    bl_mynn_dheat_opt,  &!
    bl_mynn_edmf,       &!
    bl_mynn_edmf_dd,    &!
    bl_mynn_edmf_mom,   &!
    bl_mynn_edmf_tke,   &!
    bl_mynn_output,     &!
    bl_mynn_mixscalars, &!
    bl_mynn_cloudmix,   &!
    bl_mynn_mixqt,      &!
    bl_mynn_tkebudget    !
 
 integer,intent(in):: &
    initflag,           &!
    icloud_bl,          &!
    spp_pbl              !

 real(kind=kind_phys),intent(in):: &
    bl_mynn_closure

 real(kind=kind_phys),intent(in):: &
    delt                 !

 real(kind=kind_phys),intent(in),dimension(ims:ime,jms:jme):: &
    dx,                 &!
    xland,              &!
    ps,                 &!
    ts,                 &!
    qsfc,               &!
    ust,                &!
    ch,                 &!
    hfx,                &!
    qfx,                &!
    rmol,               &!
    wspd,               &!
    uoce,               &!
    voce,               &!
    znt                  !

 real(kind=kind_phys),intent(in),dimension(ims:ime,kms:kme,jms:jme):: &
    dz,      &!
    u,       &!
    w,       &!
    v,       &!
    th,      &!
    tt,      &!
    p,       &!
    exner,   &!
    rho,     &!
    qv,      &!
    rthraten  !

 real(kind=kind_phys),intent(in),dimension(ims:ime,kms:kme,jms:jme),optional:: &
    qc,      &!
    qi,      &!
    qs,      &!
    qoz,     &!
    nc,      &!
    ni,      &!
    nifa,    &!
    nwfa,    &!
    nbca

 real(kind=kind_phys),intent(in),dimension(ims:ime,kms:kme,jms:jme),optional:: &
    pattern_spp   !


!--- inout arguments:
 integer,intent(inout),dimension(ims:ime,jms:jme):: &
    kpbl,        &!
    ktop_plume    !

 real(kind=kind_phys),intent(inout),dimension(ims:ime,jms:jme):: &
    pblh          !

 real(kind=kind_phys),intent(inout),dimension(ims:ime,kms:kme,jms:jme):: &
    cldfra_bl,   &!
    qc_bl,       &!
    qi_bl         !

 real(kind=kind_phys),intent(inout),dimension(ims:ime,kms:kme,jms:jme):: &
    el_pbl,      &!
    qke,         &!
    qke_adv,     &!
    cov,         &!
    qsq,         &!
    tsq,         &!
    sh3d,        &!
    sm3d

 real(kind=kind_phys),intent(inout),dimension(ims:ime,kms:kme,jms:jme):: &
    rublten,     &!
    rvblten,     &!
    rthblten,    &!
    rqvblten      !

 real(kind=kind_phys),intent(inout),dimension(ims:ime,kms:kme,jms:jme),optional:: &
    rqcblten,    &!
    rqiblten,    &!
    rqsblten,    &!
    rqozblten,   &!
    rncblten,    &!
    rniblten,    &!
    rnifablten,  &!
    rnwfablten,  &!
    rnbcablten    !

 real(kind=kind_phys),intent(inout),dimension(ims:ime,kms:kme,jms:jme),optional:: &
    edmf_a,      &!
    edmf_w,      &!
    edmf_qt,     &!
    edmf_thl,    &!
    edmf_ent,    &!
    edmf_qc,     &!
    sub_thl,     &!
    sub_sqv,     &!
    det_thl,     &!
    det_sqv       !


!--- output arguments:
 character(len=*),intent(out):: &
    errmsg        ! output error message (-).

 integer,intent(out):: &
    errflg        ! output error flag (-).

 real(kind=kind_phys),intent(out),dimension(ims:ime,jms:jme):: &
    maxwidth,    &!
    maxmf,       &!
    ztop_plume

 real(kind=kind_phys),intent(out),dimension(ims:ime,kms:kme,jms:jme):: &
    exch_h,      &!
    exch_m        !

 real(kind=kind_phys),intent(out),dimension(ims:ime,kms:kme,jms:jme),optional:: &
    dqke,        &!
    qwt,         &!
    qshear,      &!
    qbuoy,       &!
    qdiss         !

#if(WRF_CHEM == 1)
!--- input arguments for PBL and free-tropospheric mixing of chemical species:
 logical,intent(in):: mix_chem
 integer,intent(in):: kdvel,nchem,ndvel

 real(kind=kind_phys),intent(in),dimension(ims:ime,jms:jme):: frp_mean,ems_ant_no
 real(kind=kind_phys),intent(in),dimension(ims:ime,kdvel,jms:jme,ndvel):: vd3d
 real(kind=kind_phys),intent(inout),dimension(ims:ime,kms:kme,jms:jme,nchem):: chem3

 real(kind=RKIND),dimension(its:ite):: frp_mean_hv,emsant_no_hv
 real(kind=RKIND),dimension(its:ite,kdvel,ndvel):: vd_hv
 real(kind=RKIND),dimension(its:ite,kts:kte,nchem):: chem_hv
#endif


!local variables and arrays:
 logical:: mynn_edmf_l,mynn_edmf_dd_l,mynn_edmf_mom_l,mynn_edmf_tke_l
 logical:: mynn_mixscalars_l,mynn_mixclouds_l,mynn_mixqt_l
 logical:: mynn_tkebudget_l
 logical:: mynn_output_l,mynn_dheatopt_l,mynn_scaleaware_l,mynn_topdown_l

 integer:: i,k,j

 integer:: dheat_opt
 integer,dimension(its:ite):: &
    kpbl_hv,ktopplume_hv

 real(kind=kind_phys):: denom

 real(kind=kind_phys),dimension(its:ite):: &
    dx_hv,xland_hv,ps_hv,ts_hv,qsfc_hv,ust_hv,ch_hv,hfx_hv,qfx_hv, &
    rmol_hv,wspd_hv,uoce_hv,voce_hv,znt_hv

 real(kind=kind_phys),dimension(its:ite,kts:kte):: &
    dz_hv,u_hv,v_hv,th_hv,tt_hv,p_hv,exner_hv,rho_hv,qv_hv,rthraten_hv

 real(kind=kind_phys),dimension(its:ite,kts:kme):: &
    w_hv

 real(kind=kind_phys),dimension(its:ite,kts:kte):: &
    qc_hv,qi_hv,qs_hv,nc_hv,ni_hv,nifa_hv,nwfa_hv,nbca_hv,qoz_hv

 real(kind=kind_phys),dimension(its:ite,kts:kte):: &
    pattern_spp_hv

 real(kind=kind_phys),dimension(its:ite):: &
    pblh_hv

 real(kind=kind_phys),dimension(its:ite,kts:kte):: &
    cldfrabl_hv,qcbl_hv,qibl_hv,elpbl_hv,qke_hv,qkeadv_hv,cov_hv,qsq_hv,tsq_hv,sh3d_hv,sm3d_hv

 real(kind=kind_phys),dimension(its:ite,kts:kte):: &
    rublten_hv,rvblten_hv,rthblten_hv,rqvblten_hv,rqcblten_hv,rqiblten_hv,rqsblten_hv, &
    rncblten_hv,rniblten_hv,rnifablten_hv,rnwfablten_hv,rnbcablten_hv,rqozblten_hv

 real(kind=kind_phys),dimension(its:ite,kts:kte):: &
    edmfa_hv,edmfw_hv,edmfqt_hv,edmfthl_hv,edmfent_hv,edmfqc_hv, &
    subthl_hv,subsqv_hv,detthl_hv,detsqv_hv

 real(kind=kind_phys),dimension(its:ite):: &
    maxwidth_hv,maxmf_hv,ztopplume_hv

 real(kind=kind_phys),dimension(its:ite,kts:kte):: &
    exchh_hv,exchm_hv,dqke_hv,qwt_hv,qshear_hv,qbuoy_hv,qdiss_hv

 real(kind=kind_phys),dimension(its:ite,kts:kte):: &
    sqv_hv,sqc_hv,sqi_hv,sqs_hv

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write(' ')
!call mpas_log_write('--- enter subroutine mynn_bl_driver:')

 errmsg = " "
 errflg = 0

 mynn_edmf_l     = .false.
 mynn_edmf_dd_l  = .false.
 mynn_edmf_mom_l = .false.
 mynn_edmf_tke_l = .false.
 if(bl_mynn_edmf     == 1) mynn_edmf_l     = .true.
 if(bl_mynn_edmf_dd  == 1) mynn_edmf_dd_l  = .true.
 if(bl_mynn_edmf_mom == 1) mynn_edmf_mom_l = .true.
 if(bl_mynn_edmf_tke == 1) mynn_edmf_tke_l = .true.

 mynn_mixscalars_l = .false.
 mynn_mixclouds_l  = .false.
 mynn_mixqt_l      = .false.
 if(bl_mynn_mixscalars == 1) mynn_mixscalars_l = .true.
 if(bl_mynn_cloudmix   == 1) mynn_mixclouds_l  = .true.
 if(bl_mynn_mixqt      == 1) mynn_mixqt_l       = .true.

 mynn_tkebudget_l = .false.
 if(bl_mynn_tkebudget == 1) mynn_tkebudget_l = .true.

 mynn_output_l     = .false.
 mynn_dheatopt_l   = .false.
 mynn_scaleaware_l = .false.
 mynn_topdown_l    = .false.
 if(bl_mynn_output     == 1) mynn_output_l     = .true.
 if(bl_mynn_dheat_opt  == 1) mynn_dheatopt_l   = .true.
 if(bl_mynn_scaleaware == 1) mynn_scaleaware_l = .true.
 if(bl_mynn_topdown    == 1) mynn_topdown_l    = .true.

 dheat_opt = bl_mynn_dheat_opt

 do j = jts,jte

    !--- input arguments
    do i = its,ite
       dx_hv(i)    = dx(i,j)
       xland_hv(i) = xland(i,j)
       ps_hv(i)    = ps(i,j)
       ts_hv(i)    = ts(i,j)
       qsfc_hv(i)  = qsfc(i,j)
       ust_hv(i)   = ust(i,j)
       ch_hv(i)    = ch(i,j)
       hfx_hv(i)   = hfx(i,j)
       qfx_hv(i)   = qfx(i,j)
       rmol_hv(i)  = rmol(i,j)
       wspd_hv(i)  = wspd(i,j)
       uoce_hv(i)  = uoce(i,j)
       voce_hv(i)  = voce(i,j)
       znt_hv(i)   = znt(i,j)
    enddo
    do k = kts,kte
       do i = its,ite
          dz_hv(i,k)       = dz(i,k,j)
          u_hv(i,k)        = u(i,k,j)
          v_hv(i,k)        = v(i,k,j)
          w_hv(i,k)        = w(i,k,j)
          th_hv(i,k)       = th(i,k,j)
          tt_hv(i,k)       = tt(i,k,j)
          p_hv(i,k)        = p(i,k,j)
          exner_hv(i,k)    = exner(i,k,j)
          rho_hv(i,k)      = rho(i,k,j)
          qv_hv(i,k)       = qv(i,k,j)
          rthraten_hv(i,k) = rthraten(i,k,j)
       enddo
    enddo
    do i = its,ite
       w_hv(i,kte+1) = w(i,kte+1,j)
    enddo

    !--- input arguments for cloud mixing ratios and number concentrations; input argument
    !    for the ozone mixing ratio; input arguments for aerosols from the aerosol-aware
    !    Thompson cloud microphysics:
    do k = kts,kte
       do i = its,ite
          qc_hv(i,k)   = 0._kind_phys
          qi_hv(i,k)   = 0._kind_phys
          qs_hv(i,k)   = 0._kind_phys
          qoz_hv(i,k)  = 0._kind_phys
          nc_hv(i,k)   = 0._kind_phys
          ni_hv(i,k)   = 0._kind_phys
          nifa_hv(i,k) = 0._kind_phys
          nwfa_hv(i,k) = 0._kind_phys
          nbca_hv(i,k) = 0._kind_phys
       enddo
    enddo
    if(f_qc .and. present(qc)) then
       do k = kts,kte
          do i = its,ite
             qc_hv(i,k) = qc(i,k,j)
          enddo
       enddo
    endif
    if(f_qi .and. present(qi)) then
       do k = kts,kte
          do i = its,ite
             qi_hv(i,k) = qi(i,k,j)
          enddo
       enddo
    endif
    if(f_qs .and. present(qs)) then
       do k = kts,kte
          do i = its,ite
             qs_hv(i,k) = qs(i,k,j)
          enddo
       enddo
    endif
    if(f_nc .and. present(nc)) then
       do k = kts,kte
          do i = its,ite
             nc_hv(i,k) = nc(i,k,j)
          enddo
       enddo
    endif
    if(f_ni .and. present(ni)) then
       do k = kts,kte
          do i = its,ite
             ni_hv(i,k) = ni(i,k,j)
          enddo
       enddo
    endif
    if(f_nifa .and. present(nifa)) then
       do k = kts,kte
          do i = its,ite
             nifa_hv(i,k) = nifa(i,k,j)
          enddo
       enddo
    endif
    if(f_nwfa .and. present(nwfa)) then
       do k = kts,kte
          do i = its,ite
             nwfa_hv(i,k) = nwfa(i,k,j)
          enddo
       enddo
    endif
    if(f_nbca .and. present(nbca)) then
       do k = kts,kte
          do i = its,ite
             nbca_hv(i,k) = nbca(i,k,j)
          enddo
       enddo
    endif
    if(f_qoz .and. present(qoz)) then
       do k = kts,kte
          do i = its,ite
             qoz_hv(i,k) = qoz(i,k,j)
          enddo
       enddo
    endif

    !--- conversion from mixing ratios to specific contents:
    call bl_mynn_pre_run(its,ite,kte,f_qc,f_qi,f_qs,qv_hv,qc_hv,qi_hv,qs_hv,sqv_hv,sqc_hv, &
                         sqi_hv,sqs_hv,errmsg,errflg)

    !--- initialization of the stochastic forcing in the PBL:
    if(spp_pbl > 0 .and. present(pattern_spp)) then
       do k = kts,kte
          do i = its,ite
             pattern_spp_hv(i,k) = pattern_spp(i,k,j)
          enddo
       enddo
    else
       do k = kts,kte
          do i = its,ite
             pattern_spp_hv(i,k) = 0._kind_phys
          enddo
       enddo
    endif

    !--- inout arguments:
    do i = its,ite
       pblh_hv(i) = pblh(i,j)
       kpbl_hv(i) = kpbl(i,j)
       ktopplume_hv(i) = ktop_plume(i,j)
    enddo

    do k = kts,kte
       do i = its,ite
          cldfrabl_hv(i,k) = cldfra_bl(i,k,j)
          qcbl_hv(i,k)     = qc_bl(i,k,j)
          qibl_hv(i,k)     = qi_bl(i,k,j)
       enddo
    enddo

    do k = kts,kte
       do i = its,ite
          elpbl_hv(i,k)  = el_pbl(i,k,j)
          qke_hv(i,k)    = qke(i,k,j)
          qkeadv_hv(i,k) = qke_adv(i,k,j)
          cov_hv(i,k)    = cov(i,k,j)
          tsq_hv(i,k)    = tsq(i,k,j)   
          qsq_hv(i,k)    = qsq(i,k,j)
          sh3d_hv(i,k)   = sh3d(i,k,j)
          sm3d_hv(i,k)   = sm3d(i,k,j)
       enddo
    enddo

#if(WRF_CHEM == 1)
    do i = its,ite
       do ic = 1,nchem
          do k = kts,kte
             chem_hv(i,k,ic) = chem3d(i,k,j,ic)
          enddo
       enddo
       do ic = 1,ndvel
          do k = 1,kdvel
             vd_hv(i,k,ic) = vd3d(i,k,j,ic)
          enddo
       enddo

       frp_mean_hv(i)   = frp_mean(i,j)
       emisant_no_hv(i) = emis_ant_no(i,j)
    enddo
#endif

    do k = kts,kte
       do i = its,ite
          rqcblten_hv(i,k)   = 0._kind_phys
          rqiblten_hv(i,k)   = 0._kind_phys
          rqsblten_hv(i,k)   = 0._kind_phys
          rqozblten_hv(i,k)  = 0._kind_phys
          rncblten_hv(i,k)   = 0._kind_phys
          rniblten_hv(i,k)   = 0._kind_phys
          rnifablten_hv(i,k) = 0._kind_phys
          rnwfablten_hv(i,k) = 0._kind_phys
          rnbcablten_hv(i,k) = 0._kind_phys
       enddo
    enddo

    call bl_mynn_run ( &
            initflag        = initflag       , restart     = do_restart     , cycling     = do_DAcycling  , &
            delt            = delt           , dz          = dz_hv          , dx          = dx_hv         , &
            znt             = znt_hv         , u           = u_hv           , v           = v_hv          , &
            w               = w_hv           , th          = th_hv          , sqv         = sqv_hv        , &
            sqc             = sqc_hv         , sqi         = sqi_hv         , sqs         = sqs_hv        , &
            qnc             = nc_hv          , qni         = ni_hv          , qnwfa       = nwfa_hv       , &
            qnifa           = nifa_hv        , qnbca       = nbca_hv        , qozone      = qoz_hv        , &
            p               = p_hv           , exner       = exner_hv       , rho         = rho_hv        , &
            tt              = tt_hv          , xland       = xland_hv       , ts          = ts_hv         , &
            qsfc            = qsfc_hv        , ps          = ps_hv          , ust         = ust_hv        , &
            ch              = ch_hv          , hfx         = hfx_hv         , qfx         = qfx_hv        , &
            rmol            = rmol_hv        , wspd        = wspd_hv        , uoce        = uoce_hv       , &
            voce            = voce_hv        , qke         = qke_hv         , qke_adv     = qkeadv_hv     , &
            tsq             = tsq_hv         , qsq         = qsq_hv         , cov         = cov_hv        , &
            rthraten        = rthraten_hv    , rublten     = rublten_hv     , rvblten     = rvblten_hv    , &
            rthblten        = rthblten_hv    , rqvblten    = rqvblten_hv    , rqcblten    = rqcblten_hv   , &
            rqiblten        = rqiblten_hv    , rqsblten    = rqsblten_hv    , rqncblten   = rncblten_hv   , &
            rqniblten       = rniblten_hv    , rqnwfablten = rnwfablten_hv  , rqnifablten = rnifablten_hv , &
            rqnbcablten     = rnbcablten_hv  , rqozblten   = rqozblten_hv   , exch_h      = exchh_hv      , &
            exch_m          = exchm_hv       , pblh        = pblh_hv        , kpbl        = kpbl_hv       , &
            el_pbl          = elpbl_hv       , dqke        = dqke_hv        , qwt         = qwt_hv        , &
            qshear          = qshear_hv      , qbuoy       = qbuoy_hv       , qdiss       = qdiss_hv      , &
            sh              = sh3d_hv        , sm          = sm3d_hv        , qc_bl       = qcbl_hv       , &
            qi_bl           = qibl_hv        , cldfra_bl   = cldfrabl_hv    , icloud_bl   = icloud_bl     , &
            edmf_a          = edmfa_hv       , edmf_w      = edmfw_hv       , edmf_qt     = edmfqt_hv     , &
            edmf_thl        = edmfthl_hv     , edmf_ent    = edmfent_hv     , edmf_qc     = edmfqc_hv     , &
            sub_thl         = subthl_hv      , sub_sqv     = subsqv_hv      , det_thl     = detthl_hv     , &
            det_sqv         = detsqv_hv      , maxwidth    = maxwidth_hv    , maxmf       = maxmf_hv      , &
            ktop_plume      = ktopplume_hv   , ztop_plume  = ztopplume_hv   , spp_pbl     = spp_pbl       , &
            flag_qc         = f_qc           , flag_qi     = f_qi           , flag_qs     = f_qs          , &
            flag_qoz        = f_qoz          , flag_qnc    = f_nc           , flag_qni    = f_ni          , &
            flag_qnwfa      = f_nwfa         , flag_qnifa  = f_nifa         , flag_qnbca  = f_nbca        , &
            pattern_spp_pbl = pattern_spp_hv                                                                &
#if(WRF_CHEM == 1)
           ,mix_chem  = mix_chem  , enh_mix = enh_mix , rrfs_sd     = rrfs_sd        , &
            smoke_dbg = smoke_dbg , nchem   = nchem   , kdvel       = kdvel          , &
            ndvel     = ndvel     , chem    = chem_hv , emis_ant_no = emisant_no_hv  , &
            frp       = frp_hv    , vdep    = vd_hv                                    &
#endif
           ,bl_mynn_tkeadvect  = bl_mynn_tkeadvect  , &
            bl_mynn_tkebudget  = mynn_tkebudget_l   , &
            bl_mynn_cloudpdf   = bl_mynn_cloudpdf   , &
            bl_mynn_mixlength  = bl_mynn_mixlength  , &
            bl_mynn_stfunc     = bl_mynn_stfunc     , &
            bl_mynn_dheatopt   = mynn_dheatopt_l    , &
            bl_mynn_scaleaware = mynn_scaleaware_l  , &
            bl_mynn_topdown    = mynn_topdown_l     , &
            bl_mynn_closure    = bl_mynn_closure    , &
            bl_mynn_edmf       = mynn_edmf_l        , &
            bl_mynn_edmf_dd    = mynn_edmf_dd_l     , &
            bl_mynn_edmf_mom   = mynn_edmf_mom_l    , &
            bl_mynn_edmf_tke   = mynn_edmf_tke_l    , &
            bl_mynn_mixscalars = mynn_mixscalars_l  , &
            bl_mynn_output     = mynn_output_l      , &
            bl_mynn_cloudmix   = mynn_mixclouds_l   , &
            bl_mynn_mixqt      = mynn_mixqt_l       , &
            its = its , ite = ite , kts = kts , kte = kte , kme = kme , errmsg = errmsg , errflg = errflg )


    !--- conversion of tendencies in terms of specific contents to in terms of mixing ratios:
    call  bl_mynn_post_run(its,ite,kte,f_qc,f_qi,f_qs,delt,qv_hv,qc_hv,qi_hv,qs_hv,rqvblten_hv,rqcblten_hv, &
                           rqiblten_hv,rqsblten_hv,errmsg,errflg)

    !--- inout arguments:
    do i = its,ite
       pblh(i,j)  = pblh_hv(i)
       kpbl(i,j)  = kpbl_hv(i)
       ktop_plume(i,j) = ktopplume_hv(i)
    enddo
    do k = kts,kte
       do i = its,ite
          cldfra_bl(i,k,j) = cldfrabl_hv(i,k)
          qc_bl(i,k,j)     = qcbl_hv(i,k)
          qi_bl(i,k,j)     = qibl_hv(i,k)
       enddo
    enddo

    do k = kts,kte
       do i = its,ite
          el_pbl(i,k,j)  = elpbl_hv(i,k)
          qke(i,k,j)     = qke_hv(i,k)
          qke_adv(i,k,j) = qkeadv_hv(i,k)
          cov(i,k,j)     = cov_hv(i,k)
          tsq(i,k,j)     = tsq_hv(i,k)
          qsq(i,k,j)     = qsq_hv(i,k)
          sh3d(i,k,j)    = sh3d_hv(i,k)
          sm3d(i,k,j)    = sm3d_hv(i,k)
       enddo
    enddo

    !--- inout tendencies:
    do k = kts,kte
       do i = its,ite
          rublten(i,k,j)    = rublten_hv(i,k) 
          rvblten(i,k,j)    = rvblten_hv(i,k) 
          rthblten(i,k,j)   = rthblten_hv(i,k) 
          rqvblten(i,k,j)   = rqvblten_hv(i,k) 
       enddo
    enddo
    if(f_qc .and. present(rqcblten)) then
       do k = kts,kte
          do i = its,ite
             rqcblten(i,k,j) = rqcblten_hv(i,k) 
          enddo
       enddo
    endif
    if(f_qi .and. present(rqiblten)) then
       do k = kts,kte
          do i = its,ite
             rqiblten(i,k,j) = rqiblten_hv(i,k) 
          enddo
       enddo
    endif
    if(f_qs .and. present(rqsblten)) then
       do k = kts,kte
          do i = its,ite
             rqsblten(i,k,j) = rqsblten_hv(i,k)
          enddo
       enddo
    endif
    if(f_qoz .and. present(rqozblten)) then
       do k = kts,kte
          do i = its,ite
             rqozblten(i,k,j) = rqozblten_hv(i,k) 
          enddo
       enddo
    endif
    if(f_nc .and. present(rncblten)) then
       do k = kts,kte
          do i = its,ite
             rncblten(i,k,j) = rncblten_hv(i,k) 
          enddo
       enddo
    endif
    if(f_ni .and. present(rniblten)) then
       do k = kts,kte
          do i = its,ite
             rniblten(i,k,j) = rniblten_hv(i,k) 
          enddo
       enddo
    endif
    if(f_nifa .and. present(rnifablten)) then
       do k = kts,kte
          do i = its,ite
             rnifablten(i,k,j) = rnifablten_hv(i,k) 
          enddo
       enddo
    endif
    if(f_nwfa .and. present(rnwfablten)) then
       do k = kts,kte
          do i = its,ite
             rnwfablten(i,k,j) = rnwfablten_hv(i,k) 
          enddo
       enddo
    endif
    if(f_nbca .and. present(rnbcablten)) then
       do k = kts,kte
          do i = its,ite
             rnbcablten(i,k,j) = rnbcablten_hv(i,k) 
          enddo
       enddo
    endif

    do k = kts,kte
       do i = its,ite
          edmf_a(i,k,j)   = edmfa_hv(i,k)
          edmf_w(i,k,j)   = edmfw_hv(i,k)
          edmf_qt(i,k,j)  = edmfqt_hv(i,k)
          edmf_thl(i,k,j) = edmfthl_hv(i,k)
          edmf_ent(i,k,j) = edmfent_hv(i,k)
          edmf_qc(i,k,j)  = edmfqc_hv(i,k)
          sub_thl(i,k,j)  = subthl_hv(i,k)
          sub_sqv(i,k,j)  = subsqv_hv(i,k)
          det_thl(i,k,j)  = detthl_hv(i,k)
          det_sqv(i,k,j)  = detsqv_hv(i,k)
       enddo
    enddo

    !--- output arguments:
    do i = its,ite
       maxwidth(i,j)   = maxwidth_hv(i)
       maxmf(i,j)      = maxmf_hv(i)
       ztop_plume(i,j) = ztopplume_hv(i)
    enddo

    do k = kts,kte
       do i = its,ite
          exch_h(i,k,j) = exchh_hv(i,k)
          exch_m(i,k,j) = exchm_hv(i,k)
       enddo
    enddo

    if(present(qwt)   .and. present(qbuoy) .and. present(qshear) .and. &
       present(qdiss) .and. present(dqke)) then
       do k = kts,kte
          do i = its,ite
             dqke(i,k,j)   = dqke_hv(i,k)
             qwt(i,k,j)    = qwt_hv(i,k)
             qshear(i,k,j) = qshear_hv(i,k)
             qbuoy(i,k,j)  = qbuoy_hv(i,k)
             qdiss(i,k,j)  = qdiss_hv(i,k)
          enddo
       enddo
    endif

 enddo

!call mpas_log_write('--- end subroutine mynn_bl_driver:')

 end subroutine mynn_bl_driver

!=================================================================================================================
 end module module_bl_mynn
!=================================================================================================================

