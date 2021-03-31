!=================================================================================================================
 module module_bl_mynn
 use mpas_kind_types,only: kind_phys => RKIND
 use mpas_log

 use mpas_atmphys_constants,only: &
    cp,      &
    karman,  &
    gravity, &
    P0,      &
    ep_1,    &
    ep_2,    &
    rcp,     &
    xlv
 use bl_mynn,only: bl_mynn_run
 use bl_mynn_mpas,only: bl_mynn_mpas_run

 implicit none
 private
 public:: mynn_bl_driver


 contains


!=================================================================================================================
 subroutine mynn_bl_driver &
               (ids        , ide        , jds        , jde          , kds           , kde        , &
                ims        , ime        , jms        , jme          , kms           , kme        , &
                its        , ite        , jts        , jte          , kts           , kte        , &
                f_qc       , f_qi       , f_qoz      , f_nc         , f_ni          , f_nifa     , &
                f_nwfa     , initflag   , do_restart , do_DAcycling , grav_settling , icloud_bl  , &
                delt       , dx         , xland      , ps           , ts            , qsfc       , &
                qcg        , ust        , ch         , hfx          , qfx           , rmol       , &
                wspd       , znt        , vdfg       , uoce         , voce          , dz         , &
                u          , v          , w          , th           , tt            , p          , &
                exner      , rho        , qv         , qc           , qi            , nc         , &
                ni         , nifa       , nwfa       , qoz          , rthraten      , pblh       , &
                kpbl       , delta      , wstar      , cldfra_bl    , qc_bl         , qi_bl      , &
                nupdraft   , ktop_plume , maxmf      , qke          , qke_adv       , tsq        , &
                qsq        , cov        , el_pbl     , rublten      , rvblten       , rthblten   , &
                rqvblten   , rqcblten   , rqiblten   , rncblten     , rniblten      , rnifablten , &
                rnwfablten , rqozblten  , edmf_a     , edmf_w       , edmf_qt       , edmf_thl   , &
                edmf_ent   , edmf_qc    , sub_thl    , sub_sqv      , det_thl       , det_sqv    , &
                exch_h     , exch_m     , dqke       , qwt          , qshear        , qbuoy      , &
                qdiss      , sh3d       , spp_pbl    , pattern_spp  ,                              &
                bl_mynn_closure  , bl_mynn_cloudpdf , bl_mynn_mixlength , bl_mynn_edmf       , &
                bl_mynn_edmf_mom , bl_mynn_edmf_tke , bl_mynn_output    , bl_mynn_mixscalars , &
                bl_mynn_cloudmix , bl_mynn_mixqt    , bl_mynn_tkeadvect , bl_mynn_tkebudget  , &
                errmsg           , errflg                                                      &
               )
!=================================================================================================================

!--- input arguments:
 logical,intent(in):: &
    f_qc,               &! if true,the physics package includes the cloud liquid water mixing ratio.
    f_qi,               &! if true,the physics package includes the cloud ice mixing ratio.
    f_qoz,              &! if true,the physics package includes the ozone mixing ratio.
    f_nc,               &! if true,the physics package includes the cloud liquid water number concentration.
    f_ni,               &! if true,the physics package includes the cloud ice number concentration.
    f_nifa,             &! if true,the physics package includes the "ice-friendly" aerosol number concentration.
    f_nwfa               ! if true,the physics package includes the "water-friendly" aerosol number concentration.

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
    bl_mynn_closure,    &!
    bl_mynn_cloudpdf,   &!
    bl_mynn_mixlength,  &!
    bl_mynn_edmf,       &!
    bl_mynn_edmf_mom,   &!
    bl_mynn_edmf_tke,   &!
    bl_mynn_output,     &!
    bl_mynn_mixscalars, &!
    bl_mynn_cloudmix,   &!
    bl_mynn_mixqt,      &!
    bl_mynn_tkebudget    !
 
 integer,intent(in):: &
    initflag,           &!
    grav_settling,      &!
    icloud_bl,          &!
    spp_pbl              !

 real(kind=kind_phys),intent(in):: &
    delt                 !

 real(kind=kind_phys),intent(in),dimension(ims:ime,jms:jme):: &
    dx,                 &!
    xland,              &!
    ps,                 &!
    ts,                 &!
    qcg,                &!
    qsfc,               &!
    ust,                &!
    ch,                 &!
    hfx,                &!
    qfx,                &!
    rmol,               &!
    wspd,               &!
    vdfg,               &!
    uoce,               &!
    voce,               &!
    znt                  !

 real(kind=kind_phys),intent(in),dimension(ims:ime,kms:kme,jms:jme):: &
    dz,      &!
    u,       &!
    v,       &!
    th,      &!
    tt,      &!
    p,       &!
    exner,   &!
    rho,     &!
    qv,      &!
    rthraten  !

 real(kind=kind_phys),intent(in),dimension(ims:ime,kms:kme,jms:jme):: &
    w         !

 real(kind=kind_phys),intent(in),dimension(ims:ime,kms:kme,jms:jme):: &
    qc,      &!
    qi,      &!
    qoz,     &!
    nc,      &!
    ni,      &!
    nifa,    &!
    nwfa      !

 real(kind=kind_phys),intent(in),dimension(ims:ime,kms:kme,jms:jme):: &
    pattern_spp   !

!--- inout arguments:
 integer,intent(inout),dimension(ims:ime,jms:jme):: &
    kpbl,        &!
    nupdraft,    &!
    ktop_plume    !

 real(kind=kind_phys),intent(inout),dimension(ims:ime,jms:jme):: &
    pblh,        &!
    delta,       &!
    wstar         !

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
    sh3d          !

 real(kind=kind_phys),intent(inout),dimension(ims:ime,kms:kme,jms:jme):: &
    rublten,     &!
    rvblten,     &!
    rthblten,    &!
    rqvblten,    &!
    rqcblten,    &!
    rqiblten,    &!
    rqozblten,   &!
    rncblten,    &!
    rniblten,    &!
    rnifablten,  &!
    rnwfablten    !

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
    maxmf

 real(kind=kind_phys),intent(out),dimension(ims:ime,kms:kme,jms:jme):: &
    exch_h,      &!
    exch_m        !

 real(kind=kind_phys),intent(out),dimension(ims:ime,kms:kme,jms:jme):: &
    dqke,        &!
    qwt,         &!
    qshear,      &!
    qbuoy,       &!
    qdiss         !


!local variables and arrays:
 integer:: i,k,j

 integer,dimension(its:ite):: &
    kpbl_hv,nupdraft_hv,ktopplume_hv

 real(kind=kind_phys):: denom

 real(kind=kind_phys),dimension(its:ite):: &
    dx_hv,xland_hv,ps_hv,ts_hv,qcg_hv,qsfc_hv,ust_hv,ch_hv,hfx_hv,qfx_hv, &
    rmol_hv,wspd_hv,vdfg_hv,uoce_hv,voce_hv,znt_hv

 real(kind=kind_phys),dimension(its:ite,kts:kte):: &
    dz_hv,u_hv,v_hv,th_hv,tt_hv,p_hv,exner_hv,rho_hv,qv_hv,rthraten_hv

 real(kind=kind_phys),dimension(its:ite,kts:kme):: &
    w_hv

 real(kind=kind_phys),dimension(its:ite,kts:kte):: &
    qc_hv,qi_hv,nc_hv,ni_hv,nifa_hv,nwfa_hv,qoz_hv

 real(kind=kind_phys),dimension(its:ite,kts:kte):: &
    pattern_spp_hv

 real(kind=kind_phys),dimension(its:ite):: &
    pblh_hv,delta_hv,wstar_hv

 real(kind=kind_phys),dimension(its:ite,kts:kte):: &
    cldfrabl_hv,qcbl_hv,qibl_hv,elpbl_hv,qke_hv,qkeadv_hv,cov_hv,qsq_hv,tsq_hv,sh3d_hv

 real(kind=kind_phys),dimension(its:ite,kts:kte):: &
    rublten_hv,rvblten_hv,rthblten_hv,rqvblten_hv,rqcblten_hv,rqiblten_hv, &
    rncblten_hv,rniblten_hv,rnifablten_hv,rnwfablten_hv,rqozblten_hv

 real(kind=kind_phys),dimension(its:ite,kts:kte):: &
    edmfa_hv,edmfw_hv,edmfqt_hv,edmfthl_hv,edmfent_hv,edmfqc_hv, &
    subthl_hv,subsqv_hv,detthl_hv,detsqv_hv

 real(kind=kind_phys),dimension(its:ite):: &
    maxmf_hv

 real(kind=kind_phys),dimension(its:ite,kts:kte):: &
    exchh_hv,exchm_hv,dqke_hv,qwt_hv,qshear_hv,qbuoy_hv,qdiss_hv

 real(kind=kind_phys),dimension(its:ite,kts:kte):: &
    sqv_hv,sqc_hv,sqi_hv

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write(' ')
!call mpas_log_write('--- enter subroutine mynn_bl_driver:')

 errmsg = " "
 errflg = 0
 
 do j = jts,jte

    !--- input arguments
    do i = its,ite
       dx_hv(i)    = dx(i,j)
       xland_hv(i) = xland(i,j)
       ps_hv(i)    = ps(i,j)
       ts_hv(i)    = ts(i,j)
       qcg_hv(i)   = qcg(i,j)
       qsfc_hv(i)  = qsfc(i,j)
       ust_hv(i)   = ust(i,j)
       ch_hv(i)    = ch(i,j)
       hfx_hv(i)   = hfx(i,j)
       qfx_hv(i)   = qfx(i,j)
       rmol_hv(i)  = rmol(i,j)
       wspd_hv(i)  = wspd(i,j)
       vdfg_hv(i)  = vdfg(i,j)
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
          qoz_hv(i,k)  = 0._kind_phys
          nc_hv(i,k)   = 0._kind_phys
          ni_hv(i,k)   = 0._kind_phys
          nifa_hv(i,k) = 0._kind_phys
          nwfa_hv(i,k) = 0._kind_phys
       enddo
    enddo
    if(f_qc) then
       do k = kts,kte
          do i = its,ite
             qc_hv(i,k) = qc(i,k,j)
          enddo
       enddo
    endif
    if(f_qi) then
       do k = kts,kte
          do i = its,ite
             qi_hv(i,k) = qi(i,k,j)
          enddo
       enddo
    endif
    if(f_nc) then
       do k = kts,kte
          do i = its,ite
             nc_hv(i,k) = nc(i,k,j)
          enddo
       enddo
    endif
    if(f_ni) then
       do k = kts,kte
          do i = its,ite
             ni_hv(i,k) = ni(i,k,j)
          enddo
       enddo
    endif
    if(f_nifa) then
       do k = kts,kte
          do i = its,ite
             nifa_hv(i,k) = nifa(i,k,j)
          enddo
       enddo
    endif
    if(f_nwfa) then
       do k = kts,kte
          do i = its,ite
             nwfa_hv(i,k) = nwfa(i,k,j)
          enddo
       enddo
    endif
    if(f_qoz) then
       do k = kts,kte
          do i = its,ite
             qoz_hv(i,k) = qoz(i,k,j)
          enddo
       enddo
    endif

    !--- conversion from mixing ratios to specific contents:
    call bl_mynn_mpas_run(its,ite,kte,f_qc,f_qi,qv_hv,qc_hv,qi_hv,sqv_hv,sqc_hv,sqi_hv,errmsg,errflg)

    !--- initialization of the stochastic forcing in the PBL:
    do k = kts,kte
       do i = its,ite
          pattern_spp_hv(i,k) = pattern_spp(i,k,j)
       enddo
    enddo

    !--- inout arguments:
    do i = its,ite
       pblh_hv(i)  = pblh(i,j)
       kpbl_hv(i)  = kpbl(i,j)
       delta_hv(i) = delta(i,j)
       wstar_hv(i) = wstar(i,j)
       nupdraft_hv(i)  = nupdraft(i,j)
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
       enddo
    enddo

    !--- output arguments:
    do i = its,ite
       maxmf(i,j) = 0._kind_phys
    enddo

    do k = kts,kte
       do i = its,ite
          exch_h(i,k,j) = 0._kind_phys
          exch_m(i,k,j) = 0._kind_phys

          dqke(i,k,j)   = 0._kind_phys
          qwt(i,k,j)    = 0._kind_phys
          qshear(i,k,j) = 0._kind_phys
          qbuoy(i,k,j)  = 0._kind_phys
          qdiss(i,k,j)  = 0._kind_phys
       enddo
    enddo


    call bl_mynn_run &
             (initflag        , do_restart         , do_DAcycling     , &
             grav_settling    , delt               , dz_hv            , &
             dx_hv            , znt_hv             , u_hv             , &
             v_hv             , w_hv               , th_hv            , &
             sqv_hv           , sqc_hv             , sqi_hv           , &
             nc_hv            , ni_hv              , nwfa_hv          , &
             nifa_hv          , qoz_hv             , p_hv             , &
             exner_hv         , rho_hv             , tt_hv            , &
             xland_hv         , ts_hv              , qsfc_hv          , &
             qcg_hv           , ps_hv              , ust_hv           , &
             ch_hv            , hfx_hv             , qfx_hv           , &
             rmol_hv          , wspd_hv            , uoce_hv          , &
             voce_hv          , vdfg_hv            , qke_hv           , &
             qkeadv_hv        , bl_mynn_tkeadvect  , tsq_hv           , &
             qsq_hv           , cov_hv             , rublten_hv       , &
             rvblten_hv       , rthblten_hv        , rqvblten_hv      , &
             rqcblten_hv      , rqiblten_hv        , rncblten_hv      , &
             rniblten_hv      , rnwfablten_hv      , rnifablten_hv    , &
             rqozblten_hv     , exchh_hv           , exchm_hv         , &
             pblh_hv          , kpbl_hv            , elpbl_hv         , &
             dqke_hv          , qwt_hv             , qshear_hv        , &
             qbuoy_hv         , qdiss_hv           , wstar_hv         , &
             delta_hv         , bl_mynn_tkebudget  , bl_mynn_cloudpdf , &
             sh3d_hv          , bl_mynn_mixlength  , icloud_bl        , &
             qcbl_hv          , qibl_hv            , cldfrabl_hv      , &
             bl_mynn_closure  , bl_mynn_edmf       , bl_mynn_edmf_mom , &
             bl_mynn_edmf_tke , bl_mynn_mixscalars , bl_mynn_output   , &
             bl_mynn_cloudmix , bl_mynn_mixqt      , edmfa_hv         , &
             edmfw_hv         , edmfqt_hv          , edmfthl_hv       , &
             edmfent_hv       , edmfqc_hv          , subthl_hv        , &
             subsqv_hv        , detthl_hv          , detsqv_hv        , &
             nupdraft_hv      , maxmf_hv           , ktopplume_hv     , &
             spp_pbl          , pattern_spp_hv     , rthraten_hv      , &
             f_qc             , f_qi               , f_nc             , &
             f_ni             , f_nwfa             , f_nifa           , &
             !--- constants:
             cp , karman , gravity , P0 , ep_1 , ep_2 , rcp , xlv     , &
             its, ite , kts , kte , kme , errmsg , errflg               &
            )



    !--- inout arguments:
    do i = its,ite
       pblh(i,j)  = pblh_hv(i)
       kpbl(i,j)  = kpbl_hv(i)
       delta(i,j) = delta_hv(i)
       wstar(i,j) = wstar_hv(i)
       nupdraft(i,j)   = nupdraft_hv(i)
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
    if(f_qc) then
       do k = kts,kte
          do i = its,ite
             rqcblten(i,k,j)   = rqcblten_hv(i,k) 
          enddo
       enddo
    endif
    if(f_qi) then
       do k = kts,kte
          do i = its,ite
             rqiblten(i,k,j)   = rqiblten_hv(i,k) 
          enddo
       enddo
    endif
    if(f_qoz) then
       do k = kts,kte
          do i = its,ite
             rqozblten(i,k,j)  = rqozblten_hv(i,k) 
          enddo
       enddo
    endif
    if(f_nc) then
       do k = kts,kte
          do i = its,ite
             rncblten(i,k,j)   = rncblten_hv(i,k) 
          enddo
       enddo
    endif
    if(f_ni) then
       do k = kts,kte
          do i = its,ite
             rniblten(i,k,j)   = rniblten_hv(i,k) 
          enddo
       enddo
    endif
    if(f_nifa) then
       do k = kts,kte
          do i = its,ite
             rnifablten(i,k,j) = rnifablten_hv(i,k) 
          enddo
       enddo
    endif
    if(f_nwfa) then
       do k = kts,kte
          do i = its,ite
             rnwfablten(i,k,j) = rnwfablten_hv(i,k) 
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
       maxmf(i,j) = maxmf_hv(i)
    enddo

    do k = kts,kte
       do i = its,ite
          exch_h(i,k,j) = exchh_hv(i,k)
          exch_m(i,k,j) = exchm_hv(i,k)
       enddo
    enddo

 enddo

!call mpas_log_write('--- end subroutine mynn_bl_driver:')

 end subroutine mynn_bl_driver

!=================================================================================================================
 end module module_bl_mynn
!=================================================================================================================

