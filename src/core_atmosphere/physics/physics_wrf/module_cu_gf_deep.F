!=================================================================================================================
!this module is the Grell-Freitas parameterization of deep convection, and was provided by Georg Grell and Haiqiin
!Li on November 6, 2019. module_cu_gf_deep is currently used in the CCPP.
!Laura D. Fowler (laura@ucar.edu) / 2019-12-12.

!main modifications relative to the original sourcecode:
! * when using MPAS, redirect kind_phys to that shared in the MPAS sourcecode, and use physics constants already
!   defined in the MPAS sourcecode, for sake of consistency.
! * when using MPAS, all the subroutines shared by the shallow and deep convection schemes are grouped in module
!   module_cu_gf_shared.F. this allows for more flexibility when we want to run these schemes separately.
! * modified the calls to subroutines get_cloud_bc and call get_zu_zd_pdf_fim so that the sourcecode runs
!   efficiently with the -g option with the ifort compiler.
! * all other changes are "cosmetic".
!Laura D. Fowler (laura@ucar.edu) / 2019-12-12).


!=================================================================================================================
!>\file cu_gf_deep.F90 
!! This file is the Grell-Freitas deep convection scheme.

!>\defgroup cu_gf_deep_group Grell-Freitas Deep Convection Module
!>\ingroup cu_gf_group
!! This is Grell-Freitas deep convection scheme module
module module_cu_gf_deep

#if defined(mpas)
 use mpas_log
 use mpas_kind_types,only: kind_phys => RKIND
 use mpas_atmphys_constants,only: cp,g=>gravity,r_v=>R_v,xlv

 use module_cu_gf_ctrans,only: cup_transport_clev,    &
                               cup_transport_dn,      &
                               cup_transport_tends,   &
                               cup_up_moisture_ctrans

 use module_cu_gf_shared,only: cup_env,cup_env_clev,cup_kbcon,cup_minimi,cup_up_aa0,   &
                               get_cloud_bc,get_inversion_layers,get_lateral_massflux, &
                               get_zu_zd_pdf_fim,rates_up_pdf

 implicit none
 private
 public:: cu_gf_deep_run

#else

     use machine , only : kind_phys
     real(kind=kind_phys), parameter::g=9.81
     real(kind=kind_phys), parameter:: cp=1004.
     real(kind=kind_phys), parameter:: xlv=2.5e6
     real(kind=kind_phys), parameter::r_v=461.

#endif
     real(kind=kind_phys), parameter :: tcrit=258.
!> tuning constant for cloudwater/ice detrainment
     real(kind=kind_phys), parameter:: c1= 0.003 !.002 ! .0005
!> parameter to turn on or off evaporation of rainwater as done in sas
     integer, parameter :: irainevap=0
!> max allowed fractional coverage (frh_thresh)
     real(kind=kind_phys), parameter::frh_thresh = .9
!> rh threshold. if fractional coverage ~ frh_thres, do not use cupa any further
     real(kind=kind_phys), parameter::rh_thresh = .97
!> tuning constant for j. brown closure (ichoice = 4,5,6)
     real(kind=kind_phys), parameter::betajb=1.2
!> tuning for shallow and mid convection. ec uses 1.5
     integer, parameter:: use_excess=0
     real(kind=kind_phys), parameter :: fluxtune=1.5
!> flag to turn off or modify mom transport by downdrafts
     real(kind=kind_phys), parameter :: pgcd = 0.1
!
!> aerosol awareness, do not user yet!
     integer, parameter :: autoconv=1
     integer, parameter :: aeroevap=1
     real(kind=kind_phys), parameter :: ccnclean=250.
!> still 16 ensembles for clousres
     integer, parameter:: maxens3=16

!---meltglac-------------------------------------------------
 logical, parameter :: melt_glac      = .true.  !<- turn on/off ice phase/melting
 real(kind=kind_phys), parameter ::  &
  t_0     = 273.16,  & !< k
  t_ice   = 250.16,  & !< k
  xlf     = 0.333e6    !< latent heat of freezing (j k-1 kg-1)
!---meltglac-------------------------------------------------
!-----srf-08aug2017-----begin
 real(kind=kind_phys), parameter ::    qrc_crit= 2.e-4
!-----srf-08aug2017-----end

contains

!>\ingroup cu_gf_deep_group
!> \section general_gf_deep GF Deep Convection General Algorithm
!> @{
   subroutine cu_gf_deep_run(        &          
               itf,ktf,its,ite, kts,kte  &
              ,dicycle       &  ! diurnal cycle flag
              ,ichoice       &  ! choice of closure, use "0" for ensemble average
              ,ipr           &  ! this flag can be used for debugging prints
              ,ccn           &  ! not well tested yet
              ,dtime         &  ! dt over which forcing is applied
              ,imid          &  ! flag to turn on mid level convection
              ,kpbl          &  ! level of boundary layer height
              ,dhdt          &  ! boundary layer forcing (one closure for shallow)
              ,xland         &  ! land mask
              ,zo            &  ! heights above surface
              ,forcing       &  ! only diagnostic
              ,t             &  ! t before forcing
              ,q             &  ! q before forcing
              ,z1            &  ! terrain
              ,tn            &  ! t including forcing
              ,qo            &  ! q including forcing
              ,po            &  ! pressure (mb)
              ,psur          &  ! surface pressure (mb)
              ,us            &  ! u on mass points
              ,vs            &  ! v on mass points
              ,rho           &  ! density
              ,hfx           &  ! w/m2, positive upward
              ,qfx           &  ! w/m2, positive upward
              ,area          &  ! area of grid cell.
              ,dx            &  ! dx is grid point dependent here
              ,mconv         &  ! integrated vertical advection of moisture
              ,omeg          &  ! omega (pa/s)
              ,csum          &  ! used to implement memory, set to zero if not avail
              ,cnvwt         &  ! gfs needs this
              ,zuo           &  ! nomalized updraft mass flux
              ,zdo           &  ! nomalized downdraft mass flux
              ,zdm           &  ! nomalized downdraft mass flux from mid scheme
              ,edto          &  !
              ,edtm          &  !
              ,xmb_out       &  ! the xmb's may be needed for dicycle
              ,xmbm_in       &  !
              ,xmbs_in       &  !
              ,pre           &  !
              ,outu          &  ! momentum tendencies at mass points
              ,outv          &  !
              ,outt          &  ! temperature tendencies
              ,outq          &  ! q tendencies
              ,outqc         &  ! ql/qice tendencies
              ,kbcon         &  ! lfc of parcel from k22
              ,ktop          &  ! cloud top
              ,k22           &  !
              ,cupclw        &  ! used for direct coupling to radiation, but with tuning factors
              ,ierr          &  ! ierr flags are error flags, used for debugging
              ,ierrc         &  ! !the following should be set to zero if not available
              ,nc,ni,nifa,nwfa,ntrans,qtrans,outnc,outni,outnifa,outnwfa,outqtrans &
              ,rand_mom      &  ! for stochastics mom, if temporal and spatial patterns exist
              ,rand_vmas     &  ! for stochastics vertmass, if temporal and spatial patterns exist
              ,rand_clos     &  ! for stochastics closures, if temporal and spatial patterns exist
              ,nranflag      &  ! flag to what you want perturbed
                                !! 1 = momentum transport 
                                !! 2 = normalized vertical mass flux profile
                                !! 3 = closures
                                !! more is possible, talk to developer or
                                !! implement yourself. pattern is expected to be
                                !! betwee -1 and +1
#if ( wrf_dfi_radar == 1 )
              ,do_capsuppress,cap_suppress_j    &    !         
#endif
              ,jmin,tropics)                         !

   implicit none

     integer                                                &
        ,intent (in   )                   ::                &
        nranflag,itf,ktf,its,ite, kts,kte,ipr,imid
     integer, intent (in   )              ::                &
        ichoice
     real(kind=kind_phys),  dimension (its:ite,4)                 &
        ,intent (in  )                   ::  rand_clos
     real(kind=kind_phys),  dimension (its:ite)                   &
        ,intent (in  )                   ::  rand_mom,rand_vmas

#if ( wrf_dfi_radar == 1 )
!
!  option of cap suppress:
!        do_capsuppress = 1   do
!        do_capsuppress = other   don't
!
!
   integer,      intent(in   ) ,optional   :: do_capsuppress
   real(kind=kind_phys), dimension( its:ite ) :: cap_suppress_j
#endif
  !
  ! 
  !
      real(kind=kind_phys),    dimension (its:ite,1:maxens3) :: xf_ens,pr_ens
  ! outtem = output temp tendency (per s)
  ! outq   = output q tendency (per s)
  ! outqc  = output qc tendency (per s)
  ! pre    = output precip
     real(kind=kind_phys),    dimension (its:ite,kts:kte)                              &
        ,intent (inout  )                   ::                         &
        cnvwt,outu,outv,outt,outq,outqc,cupclw
     real(kind=kind_phys),    dimension (its:ite)                                      &
        ,intent (inout  )                   ::                         &
        pre,xmb_out
     real(kind=kind_phys),    dimension (its:ite)                                      &
        ,intent (in  )                   ::                            &
        hfx,qfx,xmbm_in,xmbs_in
     integer,    dimension (its:ite)                                   &
        ,intent (inout  )                ::                            &
        kbcon,ktop
     integer,    dimension (its:ite)                                   &
        ,intent (in  )                   ::                            &
        kpbl,tropics
  !
  ! basic environmental input includes moisture convergence (mconv)
  ! omega (omeg), windspeed (us,vs), and a flag (ierr) to turn off
  ! convection for this call only and at that particular gridpoint
  !
     real(kind=kind_phys),    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        dhdt,rho,t,po,us,vs,tn
     real(kind=kind_phys),    dimension (its:ite,kts:kte)                              &
        ,intent (inout   )                ::                           &
        omeg
     real(kind=kind_phys),    dimension (its:ite,kts:kte)                              &
        ,intent (inout)                   ::                           &
         q,qo,zuo,zdo,zdm
     real(kind=kind_phys), dimension (its:ite)                                         &
        ,intent (in   )                   ::                           &
        area,dx,ccn,z1,psur,xland
     real(kind=kind_phys), dimension (its:ite)                                         &
        ,intent (inout   )                ::                           &
        mconv

       
       real(kind=kind_phys)                                                            &
        ,intent (in   )                   ::                           &
        dtime


!
!  local ensemble dependent variables in this routine
!
     real(kind=kind_phys),    dimension (its:ite,1)  ::                                &
        xaa0_ens
     real(kind=kind_phys),    dimension (its:ite,1)  ::                                &
        edtc
     real(kind=kind_phys),    dimension (its:ite,kts:kte,1) ::                         &
        dellat_ens,dellaqc_ens,dellaq_ens,pwo_ens
!
!
!
!***************** the following are your basic environmental
!                  variables. they carry a "_cup" if they are
!                  on model cloud levels (staggered). they carry
!                  an "o"-ending (z becomes zo), if they are the forced
!                  variables. they are preceded by x (z becomes xz)
!                  to indicate modification by some typ of cloud
!
  ! z           = heights of model levels
  ! q           = environmental mixing ratio
  ! qes         = environmental saturation mixing ratio
  ! t           = environmental temp
  ! p           = environmental pressure
  ! he          = environmental moist static energy
  ! hes         = environmental saturation moist static energy
  ! z_cup       = heights of model cloud levels
  ! q_cup       = environmental q on model cloud levels
  ! qes_cup     = saturation q on model cloud levels
  ! t_cup       = temperature (kelvin) on model cloud levels
  ! p_cup       = environmental pressure
  ! he_cup = moist static energy on model cloud levels
  ! hes_cup = saturation moist static energy on model cloud levels
  ! gamma_cup = gamma on model cloud levels
!
!
  ! hcd = moist static energy in downdraft
  ! zd normalized downdraft mass flux
  ! dby = buoancy term
  ! entr = entrainment rate
  ! zd   = downdraft normalized mass flux
  ! entr= entrainment rate
  ! hcd = h in model cloud
  ! bu = buoancy term
  ! zd = normalized downdraft mass flux
  ! gamma_cup = gamma on model cloud levels
  ! qcd = cloud q (including liquid water) after entrainment
  ! qrch = saturation q in cloud
  ! pwd = evaporate at that level
  ! pwev = total normalized integrated evaoprate (i2)
  ! entr= entrainment rate
  ! z1 = terrain elevation
  ! entr = downdraft entrainment rate
  ! jmin = downdraft originating level
  ! kdet = level above ground where downdraft start detraining
  ! psur        = surface pressure
  ! z1          = terrain elevation
  ! pr_ens = precipitation ensemble
  ! xf_ens = mass flux ensembles
  ! omeg = omega from large scale model
  ! mconv = moisture convergence from large scale model
  ! zd      = downdraft normalized mass flux
  ! zu      = updraft normalized mass flux
  ! dir     = "storm motion"
  ! mbdt    = arbitrary numerical parameter
  ! dtime   = dt over which forcing is applied
  ! kbcon       = lfc of parcel from k22
  ! k22         = updraft originating level
  ! ichoice       = flag if only want one closure (usually set to zero!)
  ! dby = buoancy term
  ! ktop = cloud top (output)
  ! xmb    = total base mass flux
  ! hc = cloud moist static energy
  ! hkb = moist static energy at originating level

     real(kind=kind_phys),    dimension (its:ite,kts:kte) ::                            &
        entr_rate_2d,mentrd_rate_2d,he,hes,qes,z, heo,heso,qeso,zo,     &                    
        xhe,xhes,xqes,xz,xt,xq,qes_cup,q_cup,he_cup,hes_cup,z_cup,      &
        p_cup,gamma_cup,t_cup, qeso_cup,qo_cup,heo_cup,heso_cup,        &
        zo_cup,po_cup,gammao_cup,tn_cup,                                &    
        xqes_cup,xq_cup,xhe_cup,xhes_cup,xz_cup,                        &
        xt_cup, dby,hc,zu,clw_all,                                      &
        dbyo,qco,qrcdo,pwdo,pwo,hcdo,qcdo,dbydo,hco,qrco,               &
        dbyt,xdby,xhc,xzu,                                              &

  ! cd  = detrainment function for updraft
  ! cdd = detrainment function for downdraft
  ! dellat = change of temperature per unit mass flux of cloud ensemble
  ! dellaq = change of q per unit mass flux of cloud ensemble
  ! dellaqc = change of qc per unit mass flux of cloud ensemble

        cd,cdd,dellah,dellaq,dellat,dellaqc,                            &
        u_cup,v_cup,uc,vc,ucd,vcd,dellu,dellv

  ! aa0 cloud work function for downdraft
  ! edt = epsilon
  ! aa0     = cloud work function without forcing effects
  ! aa1     = cloud work function with forcing effects
  ! xaa0    = cloud work function with cloud effects (ensemble dependent)
  ! edt     = epsilon

     real(kind=kind_phys),    dimension (its:ite) ::                                     &
       edt,edto,edtm,aa1,aa0,xaa0,hkb,                                        &
       hkbo,xhkb,                                                        &
       xmb,pwavo,                                                        &
       pwevo,bu,bud,cap_max,                                             &
       cap_max_increment,closure_n,psum,psumh,sig,sigd
     real(kind=kind_phys),    dimension (its:ite) ::                                     &
        axx,edtmax,edtmin,entr_rate
     integer,    dimension (its:ite) ::                                  &
       kzdown,kdet,k22,jmin,kstabi,kstabm,k22x,xland1,                   &  
       ktopdby,kbconx,ierr2,ierr3,kbmax

     integer,  dimension (its:ite), intent(inout) :: ierr
     integer,  dimension (its:ite), intent(in) :: csum
     integer                              ::                             &
       iloop,nens3,ki,kk,i,k
     real(kind=kind_phys)                            ::                             &
      dz,dzo,mbdt,radius,                                                &
      zcutdown,depth_min,zkbmax,z_detr,zktop,                            &
      dh,cap_maxs,trash,trash2,frh,sig_thresh
     real(kind=kind_phys) entdo,dp,subin,detdo,entup,                                    &
      detup,subdown,entdoj,entupk,detupk,totmas

     real(kind=kind_phys), dimension (its:ite) :: lambau,flux_tun,zws,ztexec,zqexec

     integer :: jprnt,jmini,start_k22
     logical :: keep_going,flg(its:ite)
     
     character*50 :: ierrc(its:ite)
     character*4  :: cumulus
     real(kind=kind_phys),    dimension (its:ite,kts:kte) ::                              &
       up_massentr,up_massdetr,c1d                                        &
      ,up_massentro,up_massdetro,dd_massentro,dd_massdetro
     real(kind=kind_phys),    dimension (its:ite,kts:kte) ::                              &
       up_massentru,up_massdetru,dd_massentru,dd_massdetru
     real(kind=kind_phys) c1_max,buo_flux,pgcon,pgc,blqe
    
     real(kind=kind_phys) :: xff_mid(its:ite,2)
     integer :: iversion=1
     real(kind=kind_phys) :: denom,h_entr,umean,t_star,dq
     integer, intent(in) :: dicycle
     real(kind=kind_phys),    dimension (its:ite) :: aa1_bl,hkbo_bl,tau_bl,tau_ecmwf,wmean
     real(kind=kind_phys), dimension (its:ite,kts:kte) :: tn_bl, qo_bl, qeso_bl, heo_bl, heso_bl             &
                                              ,qeso_cup_bl,qo_cup_bl, heo_cup_bl,heso_cup_bl &
                                              ,gammao_cup_bl,tn_cup_bl,hco_bl,dbyo_bl
     real(kind=kind_phys), dimension(its:ite) :: xf_dicycle
     real(kind=kind_phys), intent(inout), dimension(its:ite,10) :: forcing
     integer :: turn,pmin_lev(its:ite),start_level(its:ite),ktopkeep(its:ite)
     real(kind=kind_phys),    dimension (its:ite,kts:kte) :: dtempdz
     integer, dimension (its:ite,kts:kte) ::  k_inv_layers 
 
! rainevap from sas
     real(kind=kind_phys) zuh2(40)
     real(kind=kind_phys), dimension (its:ite) :: rntot,delqev,delq2,qevap,rn,qcond
     real(kind=kind_phys) :: rain,t1,q1,elocp,evef,el2orc,evfact,evfactl,g_rain,e_dn,c_up
     real(kind=kind_phys) :: pgeoh,dts,fp,fpi,pmin,x_add,beta,beta_u
     real(kind=kind_phys) :: cbeg,cmid,cend,const_a,const_b,const_c
!---meltglac-------------------------------------------------

     real(kind=kind_phys),    dimension (its:ite,kts:kte) ::  p_liq_ice,melting_layer,melting

!--- MPAS (modified calls to the subroutines get_cloud_bc and get_zu_zd_pdf_fim. added convective transport of
!    the number concentration of cloud liquid water, cloud ice, and "ice-" and "water-friendly" aerosols from
!    the Thompson cloud microphysics scheme. also added the convective transport of passive tracers):
     integer,intent(in):: ntrans
     real(kind=kind_phys),intent(in),dimension(its:ite,kts:kte):: nc,ni,nifa,nwfa
     real(kind=kind_phys),intent(in),dimension(its:its,kts:kte,ntrans):: qtrans
     real(kind=kind_phys),intent(inout),dimension(its:ite,kts:kte):: outnc,outni,outnifa,outnwfa
     real(kind=kind_phys),intent(inout),dimension(its:ite,kts:kte,ntrans):: outqtrans

     !local variables:
     integer:: n

     real(kind=kind_phys),dimension(kts:kte):: dummy1,dummy2
     real(kind=kind_phys),dimension(its:ite,kts:kte)       :: ncl2,nil2,nifal2,nwfal2
     real(kind=kind_phys),dimension(its:ite,kts:kte)       :: nc_up,ni_up,nifa_up,nwfa_up
     real(kind=kind_phys),dimension(its:ite,kts:kte)       :: nc_dn,ni_dn,nifa_dn,nwfa_dn
     real(kind=kind_phys),dimension(its:ite,kts:kte,ntrans):: qtransl2
     real(kind=kind_phys),dimension(its:ite,kts:kte,ntrans):: qtrans_up
     real(kind=kind_phys),dimension(its:ite,kts:kte,ntrans):: qtrans_dn

     !initialization:
     do k = kts, ktf
        do i = its, itf
           outnc(i,k)   = 0._kind_phys
           outni(i,k)   = 0._kind_phys
           outnifa(i,k) = 0._kind_phys
           outnwfa(i,k) = 0._kind_phys

           nc_up(i,k)   = 0._kind_phys
           ni_up(i,k)   = 0._kind_phys
           nifa_up(i,k) = 0._kind_phys
           nwfa_up(i,k) = 0._kind_phys

           nc_dn(i,k)   = 0._kind_phys
           ni_dn(i,k)   = 0._kind_phys
           nifa_dn(i,k) = 0._kind_phys
           nwfa_dn(i,k) = 0._kind_phys
        enddo
     enddo
     do n = 1, ntrans
        do k = kts, ktf
           do i = its, itf
              qtrans_up(i,k,n) = 0._kind_phys
              qtrans_dn(i,k,n) = 0._kind_phys
           enddo
        enddo
     enddo

     do k = kts, ktf
        do i = its, itf
           ncl2(i,k)   = 0._kind_phys
           nil2(i,k)   = 0._kind_phys
           nifal2(i,k) = 0._kind_phys
           nwfal2(i,k) = 0._kind_phys
        enddo
     enddo
     do n = 1, ntrans
        do k = kts, ktf
           do i = its, itf
              qtransl2(i,k,n) = 0._kind_phys
           enddo
        enddo
     enddo
     call cup_transport_clev(its,ite,itf,kts,kte,ktf,ntrans,qtrans,nc,ni,nifa,nwfa,qtransl2, &
                             ncl2,nil2,nifal2,nwfal2)
!    do i = its, itf
!       do k = kts, ktf
!          call mpas_log_write('$i $i $r $r $r $r $r $r',intArgs=(/i,k/),    &
!                    realArgs=(/ncl2(i,k),nil2(i,k),nifal2(i,k),nwfal2(i,k), &
!                    (qtransl2(i,k,n),n=1,ntrans)/))
!       enddo
!       call mpas_log_write(' ')
!    enddo
!--- MPAS end.

!---meltglac-------------------------------------------------
      melting_layer(:,:)=0.
      melting(:,:)=0.
      flux_tun(:)=fluxtune
!      if(imid.eq.1)flux_tun(:)=fluxtune+.5
      cumulus='deep'
      if(imid.eq.1)cumulus='mid'
      pmin=150.
      if(imid.eq.1)pmin=75.
      ktopdby(:)=0
      c1_max=c1
      elocp=xlv/cp
      el2orc=xlv*xlv/(r_v*cp)
      evfact=.4 ! .2
      evfactl=.2
     !evfact=.0   ! for 4F5f
     !evfactl=.4 


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!proportionality constant to estimate pressure gradient of updraft (zhang and wu, 2003, jas
!
! ecmwf
       pgcon=0.
       lambau(:)=2.0
       if(imid.eq.1)lambau(:)=2.0
! here random must be between -1 and 1
       if(nranflag == 1)then
           lambau(:)=1.5+rand_mom(:)
       endif
! sas
!     lambau=0.
!     pgcon=-.55
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ztexec(:)     = 0.
      zqexec(:)     = 0.
      zws(:)        = 0.

      do i=its,itf
         !- buoyancy flux (h+le)
         buo_flux= (hfx(i)/cp+0.608*t(i,1)*qfx(i)/xlv)/rho(i,1)
         pgeoh = zo(i,2)*g
         !-convective-scale velocity w*
         zws(i) = max(0.,flux_tun(i)*0.41*buo_flux*zo(i,2)*g/t(i,1))
         if(zws(i) > tiny(pgeoh)) then
            !-convective-scale velocity w*
            zws(i) = 1.2*zws(i)**.3333
            !- temperature excess 
            ztexec(i)     = max(flux_tun(i)*hfx(i)/(rho(i,1)*zws(i)*cp),0.0)
            !- moisture  excess
            zqexec(i)     = max(flux_tun(i)*qfx(i)/xlv/(rho(i,1)*zws(i)),0.)
         endif
         !- zws for shallow convection closure (grant 2001)
         !- height of the pbl
         zws(i) = max(0.,.001-flux_tun(i)*0.41*buo_flux*zo(i,kpbl(i))*g/t(i,kpbl(i)))
         zws(i) = 1.2*zws(i)**.3333
         zws(i) = zws(i)*rho(i,kpbl(i)) !check if zrho is correct
      enddo
!     cap_maxs=225.
!     if(imid.eq.1)cap_maxs=150.
      cap_maxs=75. ! 150.
!     if(imid.eq.1)cap_maxs=100.
      do i=its,itf
        edto(i)=0.
        closure_n(i)=16.
        xmb_out(i)=0.
        cap_max(i)=cap_maxs
        cap_max_increment(i)=20.
!        if(imid.eq.1)cap_max_increment(i)=10.
!
! for water or ice
!
        xland1(i)=int(xland(i)+.0001) ! 1.
        if(xland(i).gt.1.5 .or. xland(i).lt.0.5)then
            xland1(i)=0
!            if(imid.eq.0)cap_max(i)=cap_maxs-25.
!            if(imid.eq.1)cap_max(i)=cap_maxs-50.
            cap_max_increment(i)=20.
        else
            if(ztexec(i).gt.0.)cap_max(i)=cap_max(i)+25.
            if(ztexec(i).lt.0.)cap_max(i)=cap_max(i)-25.
        endif
        ierrc(i)=" "
!       cap_max_increment(i)=1.
      enddo
      if(use_excess == 0 )then
       ztexec(:)=0
       zqexec(:)=0
      endif
!
!--- initial entrainment rate (these may be changed later on in the
!--- program
!
      start_level(:)=kte
      do i=its,ite
         c1d(i,:)= 0. !c1 ! 0. ! c1 ! max(.003,c1+float(csum(i))*.0001)
         entr_rate(i)=7.e-5 - min(20.,float(csum(i))) * 3.e-6
         if(xland1(i) == 0)entr_rate(i)=7.e-5
         if(imid.eq.1)entr_rate(i)=3.e-4
!         if(imid.eq.1)c1d(i,:)=c1  ! comment to test warm bias 08/14/17
         radius=.2/entr_rate(i)
         frh=min(1.,3.14*radius*radius/area(i))
         if(frh > frh_thresh)then
            frh=frh_thresh
            radius=sqrt(frh*area(i)/3.14)
            entr_rate(i)=.2/radius
         endif
         sig(i)=(1.-frh)**2
      enddo
      sig_thresh = (1.-frh_thresh)**2

      
!
!--- entrainment of mass
!
!
!--- initial detrainmentrates
!
      do k=kts,ktf
      do i=its,itf
        cnvwt(i,k)=0.
        zuo(i,k)=0.
        zdo(i,k)=0.
        z(i,k)=zo(i,k)
        xz(i,k)=zo(i,k)
        cupclw(i,k)=0.
        cd(i,k)=.1*entr_rate(i) !1.e-9 ! 1.*entr_rate
        if(imid.eq.1)cd(i,k)=.5*entr_rate(i)
        cdd(i,k)=1.e-9
        hcdo(i,k)=0.
        qrcdo(i,k)=0.
        dellaqc(i,k)=0.
      enddo
      enddo
!
!--- max/min allowed value for epsilon (ratio downdraft base mass flux/updraft
!    base mass flux
!
      edtmax(:)=1.
      if(imid.eq.1)edtmax(:)=.15
      edtmin(:)=.1
      if(imid.eq.1)edtmin(:)=.05
!
!--- minimum depth (m), clouds must have
!
      depth_min=1000.
      if(imid.eq.1)depth_min=500.
!
!--- maximum depth (mb) of capping 
!--- inversion (larger cap = no convection)
!
      do i=its,itf
!        if(imid.eq.0)then
!          edtmax(i)=max(0.5,.8-float(csum(i))*.015) !.3)
!          if(xland1(i) == 1 )edtmax(i)=max(0.7,1.-float(csum(i))*.015) !.3)
!        endif
        kbmax(i)=1
        aa0(i)=0.
        aa1(i)=0.
        edt(i)=0.
        kstabm(i)=ktf-1
        ierr2(i)=0
        ierr3(i)=0
        x_add=0.
      enddo
!     do i=its,itf
!         cap_max(i)=cap_maxs
!         cap_max3(i)=25.

!     enddo
!
!--- max height(m) above ground where updraft air can originate
!
      zkbmax=4000.
      if(imid.eq.1)zkbmax=2000.
!
!--- height(m) above which no downdrafts are allowed to originate
!
      zcutdown=4000.
!
!--- depth(m) over which downdraft detrains all its mass
!
      z_detr=500.
!     if(imid.eq.1)z_detr=800.
!

!
!--- environmental conditions, first heights
!
      do i=its,itf
            do k=1,maxens3
               xf_ens(i,k)=0.
               pr_ens(i,k)=0.
            enddo
      enddo

!
!> - Call cup_env() to calculate moist static energy, heights, qes
!
      call cup_env(z,qes,he,hes,t,q,po,z1,                         &
           psur,ierr,tcrit,-1,                                     &
           itf,ktf,                                                &
           its,ite, kts,kte)
      call cup_env(zo,qeso,heo,heso,tn,qo,po,z1,                   &
           psur,ierr,tcrit,-1,                                     &
           itf,ktf,                                                &
           its,ite, kts,kte)

!
!> - Call cup_env_clev() to calculate environmental values on cloud levels
!
      call cup_env_clev(t,qes,q,he,hes,z,po,qes_cup,q_cup,he_cup,  &
           hes_cup,z_cup,p_cup,gamma_cup,t_cup,psur,               &
           ierr,z1,                                                &
           itf,ktf,                                                &
           its,ite, kts,kte)
      call cup_env_clev(tn,qeso,qo,heo,heso,zo,po,qeso_cup,qo_cup, &
           heo_cup,heso_cup,zo_cup,po_cup,gammao_cup,tn_cup,psur,  &
           ierr,z1,                                                &
           itf,ktf,                                                &
           its,ite, kts,kte)
!---meltglac-------------------------------------------------
!> - Call get_partition_liq_ice() to calculate partition between liq/ice cloud contents
      call get_partition_liq_ice(ierr,tn,po_cup,p_liq_ice,melting_layer,&
                                 itf,ktf,its,ite,kts,kte,cumulus)
!---meltglac-------------------------------------------------
      do i=its,itf
        if(ierr(i).eq.0)then
          if(kpbl(i).gt.5 .and. imid.eq.1)cap_max(i)=po_cup(i,kpbl(i))
          u_cup(i,kts)=us(i,kts)
          v_cup(i,kts)=vs(i,kts)
          do k=kts+1,ktf
           u_cup(i,k)=.5*(us(i,k-1)+us(i,k))
           v_cup(i,k)=.5*(vs(i,k-1)+vs(i,k))
          enddo
        endif
      enddo
      do i=its,itf
        if(ierr(i).eq.0)then
        do k=kts,ktf
          if(zo_cup(i,k).gt.zkbmax+z1(i))then
            kbmax(i)=k
            go to 25
          endif
        enddo
 25     continue
!
!> - Compute the level where detrainment for downdraft starts (\p kdet)
!
        do k=kts,ktf
          if(zo_cup(i,k).gt.z_detr+z1(i))then
            kdet(i)=k
            go to 26
          endif
        enddo
 26     continue
!
        endif
      enddo
!
!
!
!> - Determine level with highest moist static energy content (\p k22)
!
      start_k22=2
       do 36 i=its,itf
         if(ierr(i).eq.0)then
            k22(i)=maxloc(heo_cup(i,start_k22:kbmax(i)+2),1)+start_k22-1
            if(k22(i).ge.kbmax(i))then
             ierr(i)=2
             ierrc(i)="could not find k22"
             ktop(i)=0
             k22(i)=0
             kbcon(i)=0
           endif
         endif
 36   continue
!
!> - call get_cloud_bc() and cup_kbcon() to determine the 
!! level of convective cloud base (\p kbcon)
!
      do i = its,ite
         hkb(i)  = 0._kind_phys
         hkbo(i) = 0._kind_phys
      enddo
      do i = its,itf
         if(ierr(i) .eq. 0) then
            x_add = xlv*zqexec(i)+cp*ztexec(i)
!           call get_cloud_bc(kte,he_cup(i,1:kte),hkb(i),k22(i),x_add)
!           call get_cloud_bc(kte,heo_cup(i,1:kte),hkbo(i),k22(i),x_add)
            do k = kts,kte
               dummy1(k) = he_cup(i,k)
               dummy2(k) = heo_cup(i,k)
            enddo
            call get_cloud_bc(kte,dummy1,hkb(i),k22(i),x_add)
            call get_cloud_bc(kte,dummy2,hkbo(i),k22(i),x_add)
         endif
      enddo
      jprnt=0
      iloop=1
      if(imid.eq.1)iloop=5
      call cup_kbcon(ierrc,cap_max_increment,iloop,k22,kbcon,heo_cup,heso_cup,  &
           hkbo,ierr,kbmax,po_cup,cap_max,                                      &
           ztexec,zqexec,                                                       &
           jprnt,itf,ktf,                                                       &
           its,ite, kts,kte,                                                    &
           z_cup,entr_rate,heo,imid)
!
!--- increase detrainment in stable layers
!
      call cup_minimi(heso_cup,kbcon,kstabm,kstabi,ierr,                        &
           itf,ktf,                                                             &
           its,ite, kts,kte)
      do i=its,itf
         if(ierr(i) == 0)then
           frh = min(qo_cup(i,kbcon(i))/qeso_cup(i,kbcon(i)),1.)
           if(frh >= rh_thresh .and. sig(i) <= sig_thresh )then
             ierr(i)=231
             cycle
           endif
!
!    never go too low...
!
!           if(imid.eq.0 .and. xland1(i).eq.0)x_add=150.
           x_add=0.
           do k=kbcon(i)+1,ktf
             if(po(i,kbcon(i))-po(i,k) > pmin+x_add)then
                pmin_lev(i)=k
                exit
             endif
           enddo
!
! initial conditions for updraft
!
            start_level(i)=k22(i)
            x_add = xlv*zqexec(i)+cp*ztexec(i)
!           call get_cloud_bc(kte,he_cup(i,1:kte),hkb(i),k22(i),x_add)
            do k = kts,kte
               dummy1(k) = he_cup(i,k)
            enddo
            call get_cloud_bc(kte,dummy1,hkb(i),k22(i),x_add)
         endif
      enddo
!
!--- get inversion layers for mid level cloud tops
!
      if(imid.eq.1)then
      call get_inversion_layers(ierr,p_cup,t_cup,z_cup,q_cup,qes_cup,k_inv_layers, &
                               kbcon,kstabi,dtempdz,itf,ktf,its,ite, kts,kte)
      endif
      do i=its,itf
         if(kstabi(i).lt.kbcon(i))then
           kbcon(i)=1
           ierr(i)=42
         endif
         do k=kts,ktf
            entr_rate_2d(i,k)=entr_rate(i)
         enddo
         if(ierr(i).eq.0)then
!         if(imid.eq.0 .and. pmin_lev(i).lt.kbcon(i)+3)pmin_lev(i)=kbcon(i)+3
            kbcon(i)=max(2,kbcon(i))
            do k=kts+1,ktf
               frh = min(qo_cup(i,k)/qeso_cup(i,k),1.)
               entr_rate_2d(i,k)=entr_rate(i) *(1.3-frh)
            enddo
            if(imid.eq.1)then
                if(k_inv_layers(i,2).gt.0 .and.   &
                  (po_cup(i,k22(i))-po_cup(i,k_inv_layers(i,2))).lt.500.)then

                 ktop(i)=min(kstabi(i),k_inv_layers(i,2))
                 ktopdby(i)=ktop(i)
               else
                 do k=kbcon(i)+1,ktf
                  if((po_cup(i,k22(i))-po_cup(i,k)).gt.500.)then
                    ktop(i)=k
                    ktopdby(i)=ktop(i)
                    exit
                  endif
                 enddo
               endif ! k_inv_lay
            endif

          endif
      enddo
!
!> - Call rates_up_pdf() to get normalized mass flux, entrainment and detrainmentrates for updraft
!
      i=0
      !- for mid level clouds we do not allow clouds taller than where stability
      !- changes
      if(imid.eq.1)then
          call rates_up_pdf(rand_vmas,ipr,'mid',ktop,ierr,po_cup,entr_rate_2d,hkbo,heo,heso_cup,zo_cup, &
                            xland1,kstabi,k22,kbcon,its,ite,itf,kts,kte,ktf,zuo,kpbl,ktopdby,csum,pmin_lev)
      else
          call rates_up_pdf(rand_vmas,ipr,'deep',ktop,ierr,po_cup,entr_rate_2d,hkbo,heo,heso_cup,zo_cup, &
                            xland1,kstabi,k22,kbcon,its,ite,itf,kts,kte,ktf,zuo,kbcon,ktopdby,csum,pmin_lev)
      endif
!
!
!
      do i=its,itf
       if(ierr(i).eq.0)then

         if(k22(i).gt.1)then
            do k=1,k22(i) -1
              zuo(i,k)=0.
              zu (i,k)=0.
              xzu(i,k)=0.
            enddo
         endif
         do k=k22(i),ktop(i)
          xzu(i,k)= zuo(i,k)
          zu (i,k)= zuo(i,k)
         enddo
         do k=ktop(i)+1,kte
           zuo(i,k)=0.
           zu (i,k)=0.
           xzu(i,k)=0.
         enddo
        endif
      enddo
!
!> - Call get_lateral_massflux() to calculate mass entrainment and detrainment
!
     if(imid.eq.1)then
      call get_lateral_massflux(itf,ktf, its,ite, kts,kte                                &
                                ,ierr,ktop,zo_cup,zuo,cd,entr_rate_2d                    &
                                ,up_massentro, up_massdetro ,up_massentr, up_massdetr    &
                                ,'mid',kbcon,k22,up_massentru,up_massdetru,lambau)
     else
      call get_lateral_massflux(itf,ktf, its,ite, kts,kte                                &
                                ,ierr,ktop,zo_cup,zuo,cd,entr_rate_2d                    &
                                ,up_massentro, up_massdetro ,up_massentr, up_massdetr    &
                                ,'deep',kbcon,k22,up_massentru,up_massdetru,lambau)
     endif


!
!   note: ktop here already includes overshooting, ktopdby is without
!   overshooting
!
      do k=kts,ktf
       do i=its,itf
         uc  (i,k)=0.
         vc  (i,k)=0.
         hc  (i,k)=0.
         dby (i,k)=0.
         hco (i,k)=0.
         dbyo(i,k)=0.
       enddo
      enddo
      do i=its,itf
       if(ierr(i).eq.0)then
         do k=1,start_level(i)
            uc(i,k)=u_cup(i,k)
            vc(i,k)=v_cup(i,k)
         enddo
         do k=1,start_level(i)-1
            hc (i,k)=he_cup(i,k)
            hco(i,k)=heo_cup(i,k)
         enddo
         k=start_level(i)
         hc (i,k)=hkb(i)
         hco(i,k)=hkbo(i)
       endif 
      enddo

!
!---meltglac-------------------------------------------------
     !
     !--- 1st guess for moist static energy and dbyo (not including ice phase)
     !
      do i=its,itf
       ktopkeep(i)=0
       dbyt(i,:)=0.
       if(ierr(i) /= 0) cycle                 
       ktopkeep(i)=ktop(i)
       do k=start_level(i) +1,ktop(i)  !mass cons option
         
          denom=zuo(i,k-1)-.5*up_massdetro(i,k-1)+up_massentro(i,k-1)
          if(denom.lt.1.e-8)then
           ierr(i)=51
           exit
          endif
          hco(i,k)=(hco(i,k-1)*zuo(i,k-1)-.5*up_massdetro(i,k-1)*hco(i,k-1)+            &
                                             up_massentro(i,k-1)*heo(i,k-1))   /        &
                         (zuo(i,k-1)-.5*up_massdetro(i,k-1)+up_massentro(i,k-1))
          dbyo(i,k)=hco(i,k)-heso_cup(i,k)
       enddo
       ! for now no overshooting (only very little)
       kk=maxloc(dbyt(i,:),1)
       ki=maxloc(zuo(i,:),1)
       do k=ktop(i)-1,kbcon(i),-1
           if(dbyo(i,k).gt.0.)then
              ktopkeep(i)=k+1
              exit
           endif
        enddo
        ktop(i)=ktopkeep(i)
        if(ierr(i).eq.0)ktop(i)=ktopkeep(i)
      enddo
      do 37 i=its,itf
         kzdown(i)=0
         if(ierr(i).eq.0)then
            zktop=(zo_cup(i,ktop(i))-z1(i))*.6
            if(imid.eq.1)zktop=(zo_cup(i,ktop(i))-z1(i))*.4
            zktop=min(zktop+z1(i),zcutdown+z1(i))
            do k=kts,ktf
              if(zo_cup(i,k).gt.zktop)then
                 kzdown(i)=k
                 kzdown(i)=min(kzdown(i),kstabi(i)-1)  !
                 go to 37
              endif
              enddo
         endif
 37   continue
!
!--- downdraft originating level - jmin
!
      call cup_minimi(heso_cup,k22,kzdown,jmin,ierr, &
           itf,ktf, &
           its,ite, kts,kte)
      do 100 i=its,itf
         if(ierr(i).eq.0)then
!
!-----srf-08aug2017-----begin
!        if(imid .ne. 1 .and. melt_glac) jmin(i)=max(jmin(i),maxloc(melting_layer(i,:),1))
!-----srf-08aug2017-----end

!--- check whether it would have buoyancy, if there where
!--- no entrainment/detrainment
!
         jmini = jmin(i)
         keep_going = .true.
         do while ( keep_going )
           keep_going = .false.
           if ( jmini - 1 .lt. kdet(i)   ) kdet(i) = jmini-1
           if ( jmini     .ge. ktop(i)-1 ) jmini = ktop(i) - 2
           ki = jmini
           hcdo(i,ki)=heso_cup(i,ki)
           dz=zo_cup(i,ki+1)-zo_cup(i,ki)
           dh=0.
           do k=ki-1,1,-1
             hcdo(i,k)=heso_cup(i,jmini)
             dz=zo_cup(i,k+1)-zo_cup(i,k)
             dh=dh+dz*(hcdo(i,k)-heso_cup(i,k))
             if(dh.gt.0.)then
               jmini=jmini-1
               if ( jmini .gt. 5 ) then
                 keep_going = .true.
               else
                 ierr(i) = 9
                 ierrc(i) = "could not find jmini9"
                 exit
               endif
             endif
           enddo
         enddo
         jmin(i) = jmini 
         if ( jmini .le. 5 ) then
           ierr(i)=4
           ierrc(i) = "could not find jmini4"
         endif
       endif
100   continue
      do i=its,itf
       if(ierr(i) /= 0) cycle                 
!         do k=kbcon(i)+1,ktop(i)-1
!c         do k=jmin(i)+1,ktop(i)-1
!c          c1d(i,k)=c1
!c         enddo
         !if(imid.eq.1)c1d(i,:)=0.
!         do k=kts,ktop(i)
!          if(po(i,k).gt.700.)then
!           c1d(i,k)=0.
!          elseif(po(i,k).gt.600.)then
!           c1d(i,k)=0.001
!          elseif(po(i,k).gt.500.)then
!           c1d(i,k)=0.002
!          elseif(po(i,k).gt.400.)then
!           c1d(i,k)=0.003
!          elseif(po(i,k).gt.300.)then
!           c1d(i,k)=0.004
!          elseif(po(i,k).gt.200.)then
!           c1d(i,k)=0.005
!          endif
!         enddo
!         if(imid.eq.1)c1d(i,:)=0.003
         
       do k=ktop(i)+1,ktf
           hco(i,k)=heso_cup(i,k)
           dbyo(i,k)=0.
       enddo
      enddo
     !
!> - Call cup_up_moisture() to calculate moisture properties of updraft
     !
      if(imid.eq.1)then
        call cup_up_moisture('mid',ierr,zo_cup,qco,qrco,pwo,pwavo,               &
             p_cup,kbcon,ktop,dbyo,clw_all,xland1,                               &
             qo,gammao_cup,zuo,qeso_cup,k22,qo_cup,                              &
             zqexec,ccn,rho,c1d,tn_cup,up_massentr,up_massdetr,psum,psumh,       &
             1,itf,ktf,                                                          &
             its,ite, kts,kte)
      else
!--- for deep convection, replaced the subroutine cup_up_moisture with cup_up_moisture_ctrans in
!    module_cu_gf_ctrans.F. cup_up_moisture_ctrans is a cleaned-up version of cup_up_moisture and
!    includes the calculation of the in-cloud "ice-" and "water-friendly" aerosols (nifa_up and
!    nwfa-up) from the Thompson cloud microphysics. for now, nc_up and ni_up are simply holders.
!    cup_up_moisture_ctrans does not work with the "berry" autoconversion option.
         call cup_up_moisture_ctrans('deep',ierr,k22,kbcon,ktop,autoconv,zo_cup, &
                     tn_cup,qo_cup,qeso_cup,gammao_cup,qo,dbyo,c1d,up_massentr,  &
                     up_massdetr,zuo,qco,qrco,pwo,clw_all,pwavo,psum,            &
                     ntrans,nc,ni,nifa,nwfa,qtrans,ncl2,nil2,nifal2,nwfal2,      &
                     qtransl2,nc_up,ni_up,nifa_up,nwfa_up,qtrans_up,             &
                     its,ite,itf,kts,kte,ktf)
!        call cup_up_moisture('deep',ierr,zo_cup,qco,qrco,pwo,pwavo,             &
!            p_cup,kbcon,ktop,dbyo,clw_all,xland1,                               &
!            qo,gammao_cup,zuo,qeso_cup,k22,qo_cup,                              &
!            zqexec,ccn,rho,c1d,tn_cup,up_massentr,up_massdetr,psum,psumh,       &
!            1,itf,ktf,                                                          &
!            its,ite, kts,kte)
     endif
!     !--- get melting profile 
!     call get_melting_profile(ierr,tn_cup,po_cup, p_liq_ice,melting_layer,qrco    &
!                             ,pwo,edto,pwdo,melting                                & 
!                             ,itf,ktf,its,ite, kts,kte, cumulus                    )
!---meltglac-------------------------------------------------


      do i=its,itf

       ktopkeep(i)=0
       dbyt(i,:)=0.
       if(ierr(i) /= 0) cycle                 
       ktopkeep(i)=ktop(i)
       do k=start_level(i) +1,ktop(i)  !mass cons option
         
          denom=zuo(i,k-1)-.5*up_massdetro(i,k-1)+up_massentro(i,k-1)
          if(denom.lt.1.e-8)then
           ierr(i)=51
           exit
          endif

          hc(i,k)=(hc(i,k-1)*zu(i,k-1)-.5*up_massdetr(i,k-1)*hc(i,k-1)+                 &
                                          up_massentr(i,k-1)*he(i,k-1))   /             &
                            (zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))
          uc(i,k)=(uc(i,k-1)*zu(i,k-1)-.5*up_massdetru(i,k-1)*uc(i,k-1)+                &
                                          up_massentru(i,k-1)*us(i,k-1)                 &
                            -pgcon*.5*(zu(i,k)+zu(i,k-1))*(u_cup(i,k)-u_cup(i,k-1))) /  &
                           (zu(i,k-1)-.5*up_massdetru(i,k-1)+up_massentru(i,k-1))
          vc(i,k)=(vc(i,k-1)*zu(i,k-1)-.5*up_massdetru(i,k-1)*vc(i,k-1)+                &
                                          up_massentru(i,k-1)*vs(i,k-1)                 &
                         -pgcon*.5*(zu(i,k)+zu(i,k-1))*(v_cup(i,k)-v_cup(i,k-1))) /     &
                         (zu(i,k-1)-.5*up_massdetru(i,k-1)+up_massentru(i,k-1))
          dby(i,k)=hc(i,k)-hes_cup(i,k)
          hco(i,k)=(hco(i,k-1)*zuo(i,k-1)-.5*up_massdetro(i,k-1)*hco(i,k-1)+            &
                                             up_massentro(i,k-1)*heo(i,k-1))   /        &
                         (zuo(i,k-1)-.5*up_massdetro(i,k-1)+up_massentro(i,k-1))
!---meltglac-------------------------------------------------
          !
          !- include glaciation effects on hc,hco	  
          !                    ------ ice content --------     
          hc (i,k)= hc (i,k)+(1.-p_liq_ice(i,k))*qrco(i,k)*xlf
          hco(i,k)= hco(i,k)+(1.-p_liq_ice(i,k))*qrco(i,k)*xlf

          dby(i,k)=hc(i,k)-hes_cup(i,k)
!---meltglac-------------------------------------------------
          dbyo(i,k)=hco(i,k)-heso_cup(i,k)
          dz=zo_cup(i,k+1)-zo_cup(i,k)
          dbyt(i,k)=dbyt(i,k-1)+dbyo(i,k)*dz

       enddo
! for now no overshooting (only very little)
       kk=maxloc(dbyt(i,:),1)
       ki=maxloc(zuo(i,:),1)
!       if(ipr .eq.1)write(16,*)'cupgf2',kk,ki
!       if(kk.lt.ki+3)then
!         ierr(i)=423
!       endif
!
        do k=ktop(i)-1,kbcon(i),-1
           if(dbyo(i,k).gt.0.)then
              ktopkeep(i)=k+1
              exit
           endif
        enddo
        ktop(i)=ktopkeep(i)
        if(ierr(i).eq.0)ktop(i)=ktopkeep(i)
      enddo
41    continue
      do i=its,itf
       if(ierr(i) /= 0) cycle                 
       do k=ktop(i)+1,ktf
           hc(i,k)=hes_cup(i,k)
           uc(i,k)=u_cup(i,k)
           vc(i,k)=v_cup(i,k)
           hco(i,k)=heso_cup(i,k)
           dby(i,k)=0.
           dbyo(i,k)=0.
           zu(i,k)=0.
           zuo(i,k)=0.
           cd(i,k)=0.
           entr_rate_2d(i,k)=0.
           up_massentr(i,k)=0.
           up_massdetr(i,k)=0.
           up_massentro(i,k)=0.
           up_massdetro(i,k)=0.
       enddo
      enddo
!
      do i=its,itf
        if(ierr(i)/=0)cycle
        if(ktop(i).lt.kbcon(i)+2)then
              ierr(i)=5
              ierrc(i)='ktop too small deep'
              ktop(i)=0
        endif
      enddo
!!      do 37 i=its,itf
!         kzdown(i)=0
!         if(ierr(i).eq.0)then
!            zktop=(zo_cup(i,ktop(i))-z1(i))*.6
!            if(imid.eq.1)zktop=(zo_cup(i,ktop(i))-z1(i))*.4
!            zktop=min(zktop+z1(i),zcutdown+z1(i))
!            do k=kts,ktf
!              if(zo_cup(i,k).gt.zktop)then
!                 kzdown(i)=k
!                 kzdown(i)=min(kzdown(i),kstabi(i)-1)  !
!                 go to 37
!              endif
!              enddo
!         endif
! 37   continue
!!
!!--- downdraft originating level - jmin
!!
!      call cup_minimi(heso_cup,k22,kzdown,jmin,ierr, &
!           itf,ktf, &
!           its,ite, kts,kte)
!      do 100 i=its,itf
!         if(ierr(i).eq.0)then
!!
!!-----srf-08aug2017-----begin
!!        if(imid .ne. 1 .and. melt_glac) jmin(i)=max(jmin(i),maxloc(melting_layer(i,:),1))
!!-----srf-08aug2017-----end
!
!!--- check whether it would have buoyancy, if there where
!!--- no entrainment/detrainment
!!
!         jmini = jmin(i)
!         keep_going = .true.
!         do while ( keep_going )
!           keep_going = .false.
!           if ( jmini - 1 .lt. kdet(i)   ) kdet(i) = jmini-1
!           if ( jmini     .ge. ktop(i)-1 ) jmini = ktop(i) - 2
!           ki = jmini
!           hcdo(i,ki)=heso_cup(i,ki)
!           dz=zo_cup(i,ki+1)-zo_cup(i,ki)
!           dh=0.
!           do k=ki-1,1,-1
!             hcdo(i,k)=heso_cup(i,jmini)
!             dz=zo_cup(i,k+1)-zo_cup(i,k)
!             dh=dh+dz*(hcdo(i,k)-heso_cup(i,k))
!             if(dh.gt.0.)then
!               jmini=jmini-1
!               if ( jmini .gt. 5 ) then
!                 keep_going = .true.
!               else
!                 ierr(i) = 9
!                 ierrc(i) = "could not find jmini9"
!                 exit
!               endif
!             endif
!           enddo
!         enddo
!         jmin(i) = jmini 
!         if ( jmini .le. 5 ) then
!           ierr(i)=4
!           ierrc(i) = "could not find jmini4"
!         endif
!       endif
!100   continue
!!
! - must have at least depth_min m between cloud convective base
!     and cloud top.
!
      do i=its,itf
         if(ierr(i).eq.0)then
            if ( jmin(i) - 1 .lt. kdet(i)   ) kdet(i) = jmin(i)-1
            if(-zo_cup(i,kbcon(i))+zo_cup(i,ktop(i)).lt.depth_min)then
               ierr(i)=6
               ierrc(i)="cloud depth very shallow"
            endif
         endif
      enddo

!
!--- normalized downdraft mass flux profile,also work on bottom detrainment
!--- in this routine
!
      do k=kts,ktf
      do i=its,itf
       zdo(i,k)=0.
       cdd(i,k)=0.
       dd_massentro(i,k)=0.
       dd_massdetro(i,k)=0.
       dd_massentru(i,k)=0.
       dd_massdetru(i,k)=0.
       hcdo(i,k)=heso_cup(i,k)
       ucd(i,k)=u_cup(i,k)
       vcd(i,k)=v_cup(i,k)
       dbydo(i,k)=0.
       mentrd_rate_2d(i,k)=entr_rate(i)
      enddo
      enddo
      do i=its,itf
        if(ierr(i)/=0)cycle
        beta=max(.025,.055-float(csum(i))*.0015)  !.02
        if(imid.eq.0 .and. xland1(i) == 0)then
              edtmax(i)=max(0.1,.4-float(csum(i))*.015) !.3)
        endif
        if(imid.eq.1)beta=.025
        bud(i)=0.
        cdd(i,1:jmin(i))=.1*entr_rate(i)
        cdd(i,jmin(i))=0.
        dd_massdetro(i,:)=0.
        dd_massentro(i,:)=0.
!       call get_zu_zd_pdf_fim(0,po_cup(i,:),rand_vmas(i),0.0_kind_phys,ipr,xland1(i),zuh2,"down",ierr(i), &
!                              kdet(i),jmin(i)+1,zdo(i,:),kts,kte,ktf,beta,kpbl(i),csum(i),pmin_lev(i))
        do k = kts,kte
           dummy1(k) = po_cup(i,k)
           dummy2(k) = zdo(i,k)
        enddo
        call get_zu_zd_pdf_fim(0,dummy1,rand_vmas(i),0.0_kind_phys,ipr,xland1(i),zuh2,"down",ierr(i), &
                               kdet(i),jmin(i)+1,dummy2,kts,kte,ktf,beta,kpbl(i),csum(i),pmin_lev(i))
        do k = kts,kte
           zdo(i,k) = dummy2(k)
        enddo

        if(zdo(i,jmin(i)) .lt.1.e-8)then
          zdo(i,jmin(i))=0.
          jmin(i)=jmin(i)-1
          cdd(i,jmin(i):ktf)=0.
          zdo(i,jmin(i)+1:ktf)=0.
          if(zdo(i,jmin(i)) .lt.1.e-8)then
             ierr(i)=876
             cycle
          endif
        endif
        
        do ki=jmin(i)  ,maxloc(zdo(i,:),1),-1
          !=> from jmin to maximum value zd -> change entrainment
          dzo=zo_cup(i,ki+1)-zo_cup(i,ki)
          dd_massdetro(i,ki)=cdd(i,ki)*dzo*zdo(i,ki+1)
          dd_massentro(i,ki)=zdo(i,ki)-zdo(i,ki+1)+dd_massdetro(i,ki)
          if(dd_massentro(i,ki).lt.0.)then
             dd_massentro(i,ki)=0.
             dd_massdetro(i,ki)=zdo(i,ki+1)-zdo(i,ki)
             if(zdo(i,ki+1).gt.0.)cdd(i,ki)=dd_massdetro(i,ki)/(dzo*zdo(i,ki+1))
          endif
          if(zdo(i,ki+1).gt.0.)mentrd_rate_2d(i,ki)=dd_massentro(i,ki)/(dzo*zdo(i,ki+1))
        enddo
        mentrd_rate_2d(i,1)=0.
        do ki=maxloc(zdo(i,:),1)-1,1,-1
          !=> from maximum value zd to surface -> change detrainment
          dzo=zo_cup(i,ki+1)-zo_cup(i,ki)
          dd_massentro(i,ki)=mentrd_rate_2d(i,ki)*dzo*zdo(i,ki+1)
          dd_massdetro(i,ki) = zdo(i,ki+1)+dd_massentro(i,ki)-zdo(i,ki)
          if(dd_massdetro(i,ki).lt.0.)then
            dd_massdetro(i,ki)=0.
            dd_massentro(i,ki)=zdo(i,ki)-zdo(i,ki+1)
            if(zdo(i,ki+1).gt.0.)mentrd_rate_2d(i,ki)=dd_massentro(i,ki)/(dzo*zdo(i,ki+1))
          endif
          if(zdo(i,ki+1).gt.0.)cdd(i,ki)= dd_massdetro(i,ki)/(dzo*zdo(i,ki+1))
        enddo
!         cbeg=800. !po_cup(i,kbcon(i)) !850.
!         cend=min(po_cup(i,ktop(i)),200.)
!         cmid=.5*(cbeg+cend) !600.
!         const_b=c1/((cmid*cmid-cbeg*cbeg)*(cbeg-cend)/(cend*cend-cbeg*cbeg)+cmid-cbeg)
!         const_a=const_b*(cbeg-cend)/(cend*cend-cbeg*cbeg)
!         const_c=-const_a*cbeg*cbeg-const_b*cbeg
!         do k=kbcon(i)+1,ktop(i)-1
!           c1d(i,k)=const_a*po_cup(i,k)*po_cup(i,k)+const_b*po_cup(i,k)+const_c
!           c1d(i,k)=max(0.,c1d(i,k))
!!           c1d(i,k)=c1
!         enddo
!!         if(imid.eq.1)c1d(i,:)=0.
!!        do k=1,jmin(i)
!!         c1d(i,k)=0.
!!        enddo
!!         c1d(i,jmin(i)-2)=c1/40.
!!         if(imid.eq.1)c1d(i,jmin(i)-2)=c1/20.
!!        do k=jmin(i)-1,ktop(i)
!!          dz=zo_cup(i,ktop(i))-zo_cup(i,jmin(i))
!!          c1d(i,k)=c1d(i,k-1)+c1*(zo_cup(i,k+1)-zo_cup(i,k))/dz
!!          c1d(i,k)=max(0.,c1d(i,k))
!!          c1d(i,k)=min(.002,c1d(i,k))
!!        enddo
!
!
! downdraft moist static energy + moisture budget
          do k=2,jmin(i)+1
           dd_massentru(i,k-1)=dd_massentro(i,k-1)+lambau(i)*dd_massdetro(i,k-1)
           dd_massdetru(i,k-1)=dd_massdetro(i,k-1)+lambau(i)*dd_massdetro(i,k-1)
          enddo
            dbydo(i,jmin(i))=hcdo(i,jmin(i))-heso_cup(i,jmin(i))
            bud(i)=dbydo(i,jmin(i))*(zo_cup(i,jmin(i)+1)-zo_cup(i,jmin(i)))
            ucd(i,jmin(i)+1)=.5*(uc(i,jmin(i)+1)+u_cup(i,jmin(i)+1))
            do ki=jmin(i)  ,1,-1
             dzo=zo_cup(i,ki+1)-zo_cup(i,ki)
             h_entr=.5*(heo(i,ki)+.5*(hco(i,ki)+hco(i,ki+1)))
             ucd(i,ki)=(ucd(i,ki+1)*zdo(i,ki+1)                                   &
                         -.5*dd_massdetru(i,ki)*ucd(i,ki+1)+                      &
                        dd_massentru(i,ki)*us(i,ki)                               &
                        -pgcon*zdo(i,ki+1)*(us(i,ki+1)-us(i,ki)))   /             &
                        (zdo(i,ki+1)-.5*dd_massdetru(i,ki)+dd_massentru(i,ki))
             vcd(i,ki)=(vcd(i,ki+1)*zdo(i,ki+1)                                   &
                         -.5*dd_massdetru(i,ki)*vcd(i,ki+1)+                      &
                        dd_massentru(i,ki)*vs(i,ki)                               &
                        -pgcon*zdo(i,ki+1)*(vs(i,ki+1)-vs(i,ki)))   /             &
                        (zdo(i,ki+1)-.5*dd_massdetru(i,ki)+dd_massentru(i,ki))
             hcdo(i,ki)=(hcdo(i,ki+1)*zdo(i,ki+1)                                 &
                         -.5*dd_massdetro(i,ki)*hcdo(i,ki+1)+                     &
                        dd_massentro(i,ki)*h_entr)   /                            &
                        (zdo(i,ki+1)-.5*dd_massdetro(i,ki)+dd_massentro(i,ki))
             dbydo(i,ki)=hcdo(i,ki)-heso_cup(i,ki)
             bud(i)=bud(i)+dbydo(i,ki)*dzo
            enddo
        !  endif

        if(bud(i).gt.0)then
          ierr(i)=7
          ierrc(i)='downdraft is not negatively buoyant '
        endif
      enddo
!
!> - Call cup_dd_moisture() to calculate moisture properties of downdraft
!
      call cup_dd_moisture(ierrc,zdo,hcdo,heso_cup,qcdo,qeso_cup,                &
           pwdo,qo_cup,zo_cup,dd_massentro,dd_massdetro,jmin,ierr,gammao_cup,    &
           pwevo,bu,qrcdo,qo,heo,1,                                              &
           itf,ktf,                                                              &
           its,ite, kts,kte)
!
!--- MPAS: downdrafts for nc, ni, nifa, and nwfa:
 call cup_transport_dn(ierr,jmin,zdo,dd_massentro,dd_massdetro,ntrans,nc,ni,nifa,nwfa, &
                  qtrans,ncl2,nil2,nifal2,nwfal2,qtransl2,nc_dn,ni_dn,nifa_dn,nwfa_dn, &
                  qtrans_dn,its,ite,itf,kts,kte,ktf)
!
!---meltglac-------------------------------------------------
!--- calculate moisture properties of updraft
!
!      if(imid.eq.1)then
!        call cup_up_moisture('mid',ierr,zo_cup,qco,qrco,pwo,pwavo,               &
!             p_cup,kbcon,ktop,dbyo,clw_all,xland1,                               &
!             qo,gammao_cup,zuo,qeso_cup,k22,qo_cup,                              &
!             zqexec,ccn,rho,c1d,tn_cup,up_massentr,up_massdetr,psum,psumh,       &
!             1,itf,ktf,                                                          &
!             its,ite, kts,kte)
!      else
!         call cup_up_moisture('deep',ierr,zo_cup,qco,qrco,pwo,pwavo,             &
!             p_cup,kbcon,ktop,dbyo,clw_all,xland1,                               &
!             qo,gammao_cup,zuo,qeso_cup,k22,qo_cup,                              &
!             zqexec,ccn,rho,c1d,tn_cup,up_massentr,up_massdetr,psum,psumh,       &
!             1,itf,ktf,                                                          &
!             its,ite, kts,kte)
!      endif
!---meltglac-------------------------------------------------
      do i=its,itf
        if(ierr(i)/=0)cycle
        do k=kts+1,ktop(i)
          dp=100.*(po_cup(i,1)-po_cup(i,2))
          cupclw(i,k)=qrco(i,k)        ! my mod
          cnvwt(i,k)=zuo(i,k)*cupclw(i,k)*g/dp
        enddo
      enddo
!
!> - Call cup_up_aa0() to calculate workfunctions for updrafts
!
      call cup_up_aa0(aa0,z,zu,dby,gamma_cup,t_cup,                              &
           kbcon,ktop,ierr,                                                      &
           itf,ktf,                                                              &
           its,ite, kts,kte)
      call cup_up_aa0(aa1,zo,zuo,dbyo,gammao_cup,tn_cup,                         &
           kbcon,ktop,ierr,                                                      &
           itf,ktf,                                                              &
           its,ite, kts,kte)
      do i=its,itf
        if(ierr(i)/=0)cycle
           if(aa1(i).eq.0.)then
               ierr(i)=17
               ierrc(i)="cloud work function zero"
           endif
      enddo
!
!--- diurnal cycle closure 
!
      !--- aa1 from boundary layer (bl) processes only
      aa1_bl       (:) = 0.0
      xf_dicycle   (:) = 0.0
      tau_ecmwf    (:) = 0.
      !- way to calculate the fraction of cape consumed by shallow convection
      iversion=1 ! ecmwf  
      !iversion=0 ! orig    
      !
      ! betchold et al 2008 time-scale of cape removal
!
! wmean is of no meaning over land....
! still working on replacing it over water
!
      do i=its,itf
            if(ierr(i).eq.0)then
                !- mean vertical velocity 
                wmean(i) = 3.0 ! m/s ! in the future change for wmean == integral( w dz) / cloud_depth
                if(imid.eq.1)wmean(i) = 3.0
                !- time-scale cape removal from  betchold et al. 2008
                tau_ecmwf(i)=( zo_cup(i,ktop(i))- zo_cup(i,kbcon(i)) ) / wmean(i) 
                tau_ecmwf(i)=max(tau_ecmwf(i),720.)
                tau_ecmwf(i)= tau_ecmwf(i) * (1.0061 + 1.23e-2 * (dx(i)/1000.))! dx(i) must be in meters 
            endif
      enddo
      tau_bl(:)     = 0.
      !
      if(dicycle == 1) then
        do i=its,itf
            
            if(ierr(i).eq.0)then
                if(xland1(i) ==  0 ) then
                  !- over water
                  umean= 2.0+sqrt(0.5*(us(i,1)**2+vs(i,1)**2+us(i,kbcon(i))**2+vs(i,kbcon(i))**2))
                  tau_bl(i) = (zo_cup(i,kbcon(i))- z1(i)) /umean        
                else
                  !- over land
                  tau_bl(i) =( zo_cup(i,ktopdby(i))- zo_cup(i,kbcon(i)) ) / wmean(i)
                endif

            endif
        enddo

        if(iversion == 1) then 
        !-- version ecmwf
        t_star=1.  

           !-- calculate pcape from bl forcing only
!> - Call cup_up_aa1bl() to calculate ECMWF version diurnal cycle closure
            call cup_up_aa1bl(aa1_bl,t,tn,q,qo,dtime,                            &
                              zo_cup,zuo,dbyo_bl,gammao_cup_bl,tn_cup_bl,        &
                              kbcon,ktop,ierr,                                   &
                              itf,ktf,its,ite, kts,kte)

            do i=its,itf

            if(ierr(i).eq.0)then

               !- only for convection rooting in the pbl
               !if(zo_cup(i,kbcon(i))-z1(i) > zo(i,kpbl(i)+1)) then 
               !   aa1_bl(i) = 0.0
               !else
               !- multiply aa1_bl the " time-scale" - tau_bl
               !  aa1_bl(i) = max(0.,aa1_bl(i)/t_star* tau_bl(i))
                  aa1_bl(i) = ( aa1_bl(i)/t_star)* tau_bl(i)
               !endif 
            endif
            enddo
            
        else
        
          !- version for real cloud-work function
          
          !-get the profiles modified only by bl tendencies
          do i=its,itf
           tn_bl(i,:)=0.;qo_bl(i,:)=0.
           if ( ierr(i) == 0 )then
            !below kbcon -> modify profiles
            tn_bl(i,1:kbcon(i)) = tn(i,1:kbcon(i))
            qo_bl(i,1:kbcon(i)) = qo(i,1:kbcon(i))
                 !above kbcon -> keep environment profiles
            tn_bl(i,kbcon(i)+1:ktf) = t(i,kbcon(i)+1:ktf)
            qo_bl(i,kbcon(i)+1:ktf) = q(i,kbcon(i)+1:ktf)
           endif 
          enddo
          !--- calculate moist static energy, heights, qes, ... only by bl tendencies
          call cup_env(zo,qeso_bl,heo_bl,heso_bl,tn_bl,qo_bl,po,z1,                              &
                     psur,ierr,tcrit,-1,                                                         &
                     itf,ktf, its,ite, kts,kte)
          !--- environmental values on cloud levels only by bl tendencies
          call cup_env_clev(tn_bl,qeso_bl,qo_bl,heo_bl,heso_bl,zo,po,qeso_cup_bl,qo_cup_bl,      &
                              heo_cup_bl,heso_cup_bl,zo_cup,po_cup,gammao_cup_bl,tn_cup_bl,psur, &
                              ierr,z1,                                                           &
                              itf,ktf,its,ite, kts,kte)
          do i=its,itf
            if(ierr(i).eq.0)then
               hkbo_bl(i)=heo_cup_bl(i,k22(i)) 
            endif ! ierr
          enddo
          do k=kts,ktf
           do i=its,itf
             hco_bl (i,k)=0.
             dbyo_bl(i,k)=0.
           enddo
          enddo
          do i=its,itf
            if(ierr(i).eq.0)then
             do k=1,kbcon(i)-1
              hco_bl(i,k)=hkbo_bl(i)
             enddo
             k=kbcon(i)
             hco_bl (i,k)=hkbo_bl(i)
             dbyo_bl(i,k)=hkbo_bl(i) - heso_cup_bl(i,k)
            endif
          enddo
!          
!          
          do i=its,itf
            if(ierr(i).eq.0)then
               do k=kbcon(i)+1,ktop(i)
                    hco_bl(i,k)=(hco_bl(i,k-1)*zuo(i,k-1)-.5*up_massdetro(i,k-1)*hco_bl(i,k-1)+ &
                               up_massentro(i,k-1)*heo_bl(i,k-1))   /                           &
                               (zuo(i,k-1)-.5*up_massdetro(i,k-1)+up_massentro(i,k-1))
                    dbyo_bl(i,k)=hco_bl(i,k)-heso_cup_bl(i,k)
               enddo
               do k=ktop(i)+1,ktf
                  hco_bl (i,k)=heso_cup_bl(i,k)
                  dbyo_bl(i,k)=0.0
               enddo
            endif
          enddo
        
          !--- calculate workfunctions for updrafts
          call cup_up_aa0(aa1_bl,zo,zuo,dbyo_bl,gammao_cup_bl,tn_cup_bl,        &
                        kbcon,ktop,ierr,                                        &
                        itf,ktf,its,ite, kts,kte)

          do i=its,itf
            
            if(ierr(i).eq.0)then
                !- get the increment on aa0 due the bl processes
                aa1_bl(i) = aa1_bl(i) - aa0(i)
                !- only for convection rooting in the pbl
                !if(zo_cup(i,kbcon(i))-z1(i) > 500.0) then !- instead 500 -> zo_cup(kpbl(i))
                !   aa1_bl(i) = 0.0
                !else
                !   !- multiply aa1_bl the "normalized time-scale" - tau_bl/ model_timestep
                   aa1_bl(i) = aa1_bl(i)* tau_bl(i)/ dtime
                !endif 
                print*,'aa0,aa1bl=',aa0(i),aa1_bl(i),aa0(i)-aa1_bl(i),tau_bl(i)!,dtime,xland(i)     
            endif
           enddo
        endif
     endif  ! version of implementation


       axx(:)=aa1(:)

!
!> - Call cup_dd_edt() to determine downdraft strength in terms of windshear
!
      call cup_dd_edt(ierr,us,vs,zo,ktop,kbcon,edt,po,pwavo,  &
           pwo,ccn,pwevo,edtmax,edtmin,edtc,psum,psumh,       &
           rho,aeroevap,itf,ktf,                              &
           its,ite, kts,kte)
        do i=its,itf
        if(ierr(i)/=0)cycle
            edto(i)=edtc(i,1)
        enddo

!> - Call get_melting_profile() to get melting profile
     call get_melting_profile(ierr,tn_cup,po_cup, p_liq_ice,melting_layer,qrco    &
                             ,pwo,edto,pwdo,melting                                &
                             ,itf,ktf,its,ite, kts,kte, cumulus                    )
        do k=kts,ktf
        do i=its,itf
           dellat_ens (i,k,1)=0.
           dellaq_ens (i,k,1)=0.
           dellaqc_ens(i,k,1)=0.
           pwo_ens    (i,k,1)=0.
        enddo
        enddo
!
!--- change per unit mass that a model cloud would modify the environment
!
!--- 1. in bottom layer
!
      do k=kts,kte
      do i=its,itf
        dellu  (i,k)=0.
        dellv  (i,k)=0.
        dellah (i,k)=0.
        dellat (i,k)=0.
        dellaq (i,k)=0.
        dellaqc(i,k)=0.
      enddo
      enddo
!
!----------------------------------------------  cloud level ktop
!
!- - - - - - - - - - - - - - - - - - - - - - - - model level ktop-1
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!
!----------------------------------------------  cloud level k+2
!
!- - - - - - - - - - - - - - - - - - - - - - - - model level k+1
!
!----------------------------------------------  cloud level k+1
!
!- - - - - - - - - - - - - - - - - - - - - - - - model level k
!
!----------------------------------------------  cloud level k
!
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!
!----------------------------------------------  cloud level 3
!
!- - - - - - - - - - - - - - - - - - - - - - - - model level 2
!
!----------------------------------------------  cloud level 2
!
!- - - - - - - - - - - - - - - - - - - - - - - - model level 1

      do i=its,itf
        if(ierr(i)/=0)cycle
         dp=100.*(po_cup(i,1)-po_cup(i,2))
         dellu(i,1)=pgcd*(edto(i)*zdo(i,2)*ucd(i,2)   &
                         -edto(i)*zdo(i,2)*u_cup(i,2))*g/dp &
                      -zuo(i,2)*(uc (i,2)-u_cup(i,2)) *g/dp
         dellv(i,1)=pgcd*(edto(i)*zdo(i,2)*vcd(i,2)   &
                         -edto(i)*zdo(i,2)*v_cup(i,2))*g/dp &
                      -zuo(i,2)*(vc (i,2)-v_cup(i,2)) *g/dp

         do k=kts+1,ktop(i)
            ! these three are only used at or near mass detrainment and/or entrainment levels
            pgc=pgcon
            entupk=0.
            if(k == k22(i)-1) entupk=zuo(i,k+1)
            detupk=0.
            entdoj=0.
            ! detrainment and entrainment for fowndrafts
            detdo=edto(i)*dd_massdetro(i,k)
            entdo=edto(i)*dd_massentro(i,k)
            ! entrainment/detrainment for updraft
            entup=up_massentro(i,k)
            detup=up_massdetro(i,k)
            ! subsidence by downdrafts only
            subin=-zdo(i,k+1)*edto(i)
            subdown=-zdo(i,k)*edto(i)
            !         special levels
            if(k.eq.ktop(i))then
               detupk=zuo(i,ktop(i))
               subin=0.
               subdown=0.
               detdo=0.
               entdo=0.
               entup=0.
               detup=0.
            endif
            totmas=subin-subdown+detup-entup-entdo+ &
                   detdo-entupk-entdoj+detupk+zuo(i,k+1)-zuo(i,k)
            if(abs(totmas).gt.1.e-6)then
               write(0,123)'totmas=',k22(i),kbcon(i),k,entup,detup,edto(i),zdo(i,k+1),dd_massdetro(i,k),dd_massentro(i,k)
123     format(a7,1x,3i3,2e12.4,1(1x,f5.2),3e12.4)
            endif
            dp=100.*(po_cup(i,k)-po_cup(i,k+1))
             pgc=pgcon
            if(k.ge.ktop(i))pgc=0.

             dellu(i,k) =-(zuo(i,k+1)*(uc (i,k+1)-u_cup(i,k+1) ) -                               &
                            zuo(i,k  )*(uc (i,k  )-u_cup(i,k  ) ) )*g/dp                         &
                          +(zdo(i,k+1)*(ucd(i,k+1)-u_cup(i,k+1) ) -                              &
                            zdo(i,k  )*(ucd(i,k  )-u_cup(i,k  ) ) )*g/dp*edto(i)*pgcd
             dellv(i,k) =-(zuo(i,k+1)*(vc (i,k+1)-v_cup(i,k+1) ) -                               &
                            zuo(i,k  )*(vc (i,k  )-v_cup(i,k  ) ) )*g/dp                         &
                         +(zdo(i,k+1)*(vcd(i,k+1)-v_cup(i,k+1) ) -                               &
                            zdo(i,k  )*(vcd(i,k  )-v_cup(i,k  ) ) )*g/dp*edto(i)*pgcd
 
       enddo   ! k

    enddo


    do i=its,itf
        !trash  = 0.0
        !trash2 = 0.0
        if(ierr(i).eq.0)then

         dp=100.*(po_cup(i,1)-po_cup(i,2))

         dellah(i,1)=(edto(i)*zdo(i,2)*hcdo(i,2)          &
                     -edto(i)*zdo(i,2)*heo_cup(i,2))*g/dp &
                   -zuo(i,2)*(hco(i,2)-heo_cup(i,2))*g/dp

         dellaq (i,1)=(edto(i)*zdo(i,2)*qcdo(i,2)         &
                      -edto(i)*zdo(i,2)*qo_cup(i,2))*g/dp &
                    -zuo(i,2)*(qco(i,2)-qo_cup(i,2))*g/dp

         g_rain=  0.5*(pwo (i,1)+pwo (i,2))*g/dp
         e_dn  = -0.5*(pwdo(i,1)+pwdo(i,2))*g/dp*edto(i)  ! pwdo < 0 and e_dn must > 0
         dellaq(i,1) = dellaq(i,1)+ e_dn-g_rain
         
         !--- conservation check
         !- water mass balance
         !trash = trash  + (dellaq(i,1)+dellaqc(i,1)+g_rain-e_dn)*dp/g          
         !- h  budget
         !trash2 = trash2+ (dellah(i,1))*dp/g
         

         do k=kts+1,ktop(i)
            dp=100.*(po_cup(i,k)-po_cup(i,k+1))
            ! these three are only used at or near mass detrainment and/or entrainment levels

            dellah(i,k) =-(zuo(i,k+1)*(hco (i,k+1)-heo_cup(i,k+1) ) -                 &
                           zuo(i,k  )*(hco (i,k  )-heo_cup(i,k  ) ) )*g/dp            &
                         +(zdo(i,k+1)*(hcdo(i,k+1)-heo_cup(i,k+1) ) -                 &
                           zdo(i,k  )*(hcdo(i,k  )-heo_cup(i,k  ) ) )*g/dp*edto(i)
                        
!---meltglac-------------------------------------------------

           dellah(i,k) = dellah(i,k) + xlf*((1.-p_liq_ice(i,k))*0.5*(qrco(i,k+1)+qrco(i,k)) &
                                     - melting(i,k))*g/dp

!---meltglac-------------------------------------------------

            !- check h conservation 
            ! trash2 = trash2+ (dellah(i,k))*dp/g
        
        
            !-- take out cloud liquid water for detrainment
            detup=up_massdetro(i,k)
            dz=zo_cup(i,k)-zo_cup(i,k-1)
!!            if(k.lt.ktop(i) .and. k.ge.jmin(i)) then
!!            if(k.lt.ktop(i) .and. c1d(i,k).gt.0) then
            if(k.lt.ktop(i)) then
                dellaqc(i,k) = zuo(i,k)*c1d(i,k)*qrco(i,k)*dz/dp*g 
            else
                dellaqc(i,k)=  detup*0.5*(qrco(i,k+1)+qrco(i,k)) *g/dp
            endif
!!            if(imid.eq.1) dellaqc(i,k)=  detup*0.5*(qrco(i,k+1)+qrco(i,k)) *g/dp
!            if(k.eq.ktop(i))dellaqc(i,k)= detup*0.5*(qrco(i,k+1)+qrco(i,k)) *g/dp
!            !---
            g_rain=  0.5*(pwo (i,k)+pwo (i,k+1))*g/dp
            e_dn  = -0.5*(pwdo(i,k)+pwdo(i,k+1))*g/dp*edto(i) ! pwdo < 0 and e_dn must > 0
            !-- condensation source term = detrained + flux divergence of
            !-- cloud liquid water (qrco) + converted to rain
        
            c_up = dellaqc(i,k)+(zuo(i,k+1)* qrco(i,k+1) -                           &
                                zuo(i,k  )* qrco(i,k  )  )*g/dp + g_rain
!            c_up = dellaqc(i,k)+ g_rain
            !-- water vapor budget
            !-- = flux divergence z*(q_c - q_env)_up_and_down                        &
            !--   - condensation term + evaporation
            dellaq(i,k) =-(zuo(i,k+1)*(qco (i,k+1)-qo_cup(i,k+1) ) -                 &
                           zuo(i,k  )*(qco (i,k  )-qo_cup(i,k  ) ) )*g/dp            &
                         +(zdo(i,k+1)*(qcdo(i,k+1)-qo_cup(i,k+1) ) -                 &
                           zdo(i,k  )*(qcdo(i,k  )-qo_cup(i,k  ) ) )*g/dp*edto(i)    &
                         - c_up + e_dn
            !- check water conservation liq+condensed (including rainfall)
            ! trash= trash+ (dellaq(i,k)+dellaqc(i,k)+ g_rain-e_dn)*dp/g

         enddo   ! k
        endif

      enddo
444   format(1x,i2,1x,7e12.4) !,1x,f7.2,2x,e13.5)
!
!--- using dellas, calculate changed environmental profiles
!
      mbdt=.1
      do i=its,itf
      xaa0_ens(i,1)=0.
      enddo

      do i=its,itf
         if(ierr(i).eq.0)then
           do k=kts,ktf
            xhe(i,k)=dellah(i,k)*mbdt+heo(i,k)
!            xq(i,k)=max(1.e-16,(dellaqc(i,k)+dellaq(i,k))*mbdt+qo(i,k))
            xq(i,k)=max(1.e-16,dellaq(i,k)*mbdt+qo(i,k))
            dellat(i,k)=(1./cp)*(dellah(i,k)-xlv*dellaq(i,k))
!            xt(i,k)= (dellat(i,k)-xlv/cp*dellaqc(i,k))*mbdt+tn(i,k)
            xt(i,k)= dellat(i,k)*mbdt+tn(i,k)
            xt(i,k)=max(190.,xt(i,k))
           enddo
         endif
      enddo
      do i=its,itf
      if(ierr(i).eq.0)then
      xhe(i,ktf)=heo(i,ktf)
      xq(i,ktf)=qo(i,ktf)
      xt(i,ktf)=tn(i,ktf)
      endif
      enddo
!
!--- calculate moist static energy, heights, qes
!
      call cup_env(xz,xqes,xhe,xhes,xt,xq,po,z1,                   &
           psur,ierr,tcrit,-1,                                     &
           itf,ktf,                                                &
           its,ite, kts,kte)
!
!--- environmental values on cloud levels
!
      call cup_env_clev(xt,xqes,xq,xhe,xhes,xz,po,xqes_cup,xq_cup, &
           xhe_cup,xhes_cup,xz_cup,po_cup,gamma_cup,xt_cup,psur,   &
           ierr,z1,                                                &
           itf,ktf,                                                &
           its,ite, kts,kte)
!
!
!**************************** static control
!
!--- moist static energy inside cloud
!
      do i = its,ite
         xhkb(i) = 0._kind_phys
         do k = kts,ktf
            xhc(i,k)  = 0._kind_phys
            xdby(i,k) = 0._kind_phys
         enddo
      enddo
      do i = its,itf
         if(ierr(i) .eq. 0) then
            x_add = xlv*zqexec(i)+cp*ztexec(i)
!           call get_cloud_bc(kte,xhe_cup(i,1:kte),xhkb(i),k22(i),x_add)
            do k = kts,kte
               dummy1(k) = xhe_cup(i,k)
            enddo
            call get_cloud_bc(kte,dummy1,xhkb(i),k22(i),x_add)
            do k = 1,start_level(i)-1
               xhc(i,k) = xhe_cup(i,k)
            enddo
            k = start_level(i)
            xhc(i,k) = xhkb(i)
         endif
      enddo
!
!
      do i=its,itf
       if(ierr(i).eq.0)then
        do k=start_level(i)  +1,ktop(i)
         xhc(i,k)=(xhc(i,k-1)*xzu(i,k-1)-.5*up_massdetro(i,k-1)*xhc(i,k-1)  + &
                                            up_massentro(i,k-1)*xhe(i,k-1)) / &
                             (xzu(i,k-1)-.5*up_massdetro(i,k-1)+up_massentro(i,k-1))


!---meltglac-------------------------------------------------
          !
          !- include glaciation effects on xhc  
          !                          ------ ice content --------     
          xhc (i,k)= xhc (i,k)+ xlf*(1.-p_liq_ice(i,k))*qrco(i,k)
!---meltglac-------------------------------------------------

         xdby(i,k)=xhc(i,k)-xhes_cup(i,k)
        enddo
        do k=ktop(i)+1,ktf
           xhc (i,k)=xhes_cup(i,k)
           xdby(i,k)=0.
        enddo
       endif
      enddo

!
!--- workfunctions for updraft
!
      call cup_up_aa0(xaa0,xz,xzu,xdby,gamma_cup,xt_cup, &
           kbcon,ktop,ierr,                              &
           itf,ktf,                                      &
           its,ite, kts,kte)
      do i=its,itf 
         if(ierr(i).eq.0)then
           xaa0_ens(i,1)=xaa0(i)
           do k=kts,ktop(i)
                 do nens3=1,maxens3
                 if(nens3.eq.7)then
!--- b=0
                 pr_ens(i,nens3)=pr_ens(i,nens3)  &
                                    +pwo(i,k)+edto(i)*pwdo(i,k) 
!--- b=beta
                 else if(nens3.eq.8)then
                 pr_ens(i,nens3)=pr_ens(i,nens3)+ &
                                    pwo(i,k)+edto(i)*pwdo(i,k)
!--- b=beta/2
                 else if(nens3.eq.9)then
                 pr_ens(i,nens3)=pr_ens(i,nens3)  &
                                 +  pwo(i,k)+edto(i)*pwdo(i,k)
                 else
                 pr_ens(i,nens3)=pr_ens(i,nens3)+ &
                                    pwo(i,k) +edto(i)*pwdo(i,k)
                 endif
                 enddo
           enddo
         if(pr_ens(i,7).lt.1.e-6)then
            ierr(i)=18
            ierrc(i)="total normalized condensate too small"
            do nens3=1,maxens3
               pr_ens(i,nens3)=0.
            enddo
         endif
         do nens3=1,maxens3
           if(pr_ens(i,nens3).lt.1.e-5)then
            pr_ens(i,nens3)=0.
           endif
         enddo
         endif
      enddo
 200  continue
!
!--- large scale forcing
!
!
!------- check wether aa0 should have been zero, assuming this 
!        ensemble is chosen
!
!
      do i=its,itf
         ierr2(i)=ierr(i)
         ierr3(i)=ierr(i)
         k22x(i)=k22(i)
      enddo
        call cup_maximi(heo_cup,2,kbmax,k22x,ierr,                        &
             itf,ktf,                                                     &
             its,ite, kts,kte)
        iloop=2
        call cup_kbcon(ierrc,cap_max_increment,iloop,k22x,kbconx,heo_cup, &
             heso_cup,hkbo,ierr2,kbmax,po_cup,cap_max,                    &
             ztexec,zqexec,                                               &
             0,itf,ktf,                                                   &
             its,ite, kts,kte,                                            &
             z_cup,entr_rate,heo,imid)
        iloop=3
        call cup_kbcon(ierrc,cap_max_increment,iloop,k22x,kbconx,heo_cup, &
             heso_cup,hkbo,ierr3,kbmax,po_cup,cap_max,                    &
             ztexec,zqexec,                                               &
             0,itf,ktf,                                                   &
             its,ite, kts,kte,                                            &
             z_cup,entr_rate,heo,imid)
!
!--- calculate cloud base mass flux
!

      do i = its,itf
        mconv(i) = 0
        if(ierr(i)/=0)cycle
        do k=1,ktop(i)
          dq=(qo_cup(i,k+1)-qo_cup(i,k))
          mconv(i)=mconv(i)+omeg(i,k)*dq/g
        enddo
      enddo
      call cup_forcing_ens_3d(closure_n,xland1,aa0,aa1,xaa0_ens,mbdt,dtime, &
           ierr,ierr2,ierr3,xf_ens,axx,forcing,                             &
           maxens3,mconv,rand_clos,                                         &
           po_cup,ktop,omeg,zdo,zdm,k22,zuo,pr_ens,edto,edtm,kbcon,         &
           ichoice,                                                         &
           imid,ipr,itf,ktf,                                                &
           its,ite, kts,kte,                                                &
           dicycle,tau_ecmwf,aa1_bl,xf_dicycle)
!
      do k=kts,ktf
      do i=its,itf
        if(ierr(i).eq.0)then
           dellat_ens (i,k,1)=dellat(i,k)
           dellaq_ens (i,k,1)=dellaq(i,k)
           dellaqc_ens(i,k,1)=dellaqc(i,k)
           pwo_ens    (i,k,1)=pwo(i,k) +edto(i)*pwdo(i,k)
        else 
           dellat_ens (i,k,1)=0.
           dellaq_ens (i,k,1)=0.
           dellaqc_ens(i,k,1)=0.
           pwo_ens    (i,k,1)=0.
        endif
      enddo
      enddo
 250  continue
!
!--- feedback
!
       if(imid.eq.1 .and. ichoice .le.2)then
         do i=its,itf
          !-boundary layer qe 
          xff_mid(i,1)=0.
          xff_mid(i,2)=0.
          if(ierr(i).eq.0)then
            blqe=0.
            trash=0.
            if(k22(i).lt.kpbl(i)+1)then
               do k=1,kpbl(i)
                  blqe=blqe+100.*dhdt(i,k)*(po_cup(i,k)-po_cup(i,k+1))/g
               enddo
               trash=max((hco(i,kbcon(i))-heo_cup(i,kbcon(i))),1.e1)
               xff_mid(i,1)=max(0.,blqe/trash)
               xff_mid(i,1)=min(0.1,xff_mid(i,1))
             endif
             xff_mid(i,2)=min(0.1,.03*zws(i))
          endif
         enddo
       endif
       call cup_output_ens_3d(xff_mid,xf_ens,ierr,dellat_ens,dellaq_ens, &
            dellaqc_ens,outt,                                            &
            outq,outqc,zuo,pre,pwo_ens,xmb,ktop,                         &
            edto,pwdo,'deep',ierr2,ierr3,                                &
            po_cup,pr_ens,maxens3,                                       &
            sig,closure_n,xland1,xmbm_in,xmbs_in,                        &
            ichoice,imid,ipr,itf,ktf,                                    &
            its,ite, kts,kte,                                            &
            dicycle,xf_dicycle )

!---------------evap below cloud base

      call rain_evap_below_cloudbase(itf,ktf,its,ite,                    &
           kts,kte,ierr,kbcon,xmb,psur,xland,qo_cup,                     &
           po_cup,qes_cup,pwavo,edto,pwevo,pre,outt,outq)      !,outbuoy)

      k=1
      do i=its,itf
          if(ierr(i).eq.0 .and.pre(i).gt.0.) then
             pre(i)=max(pre(i),0.)
             xmb_out(i)=xmb(i)
             outu(i,1)=dellu(i,1)*xmb(i) 
             outv(i,1)=dellv(i,1)*xmb(i)
             do k=kts+1,ktop(i)
               outu(i,k)=.25*(dellu(i,k-1)+2.*dellu(i,k)+dellu(i,k+1))*xmb(i) 
               outv(i,k)=.25*(dellv(i,k-1)+2.*dellv(i,k)+dellv(i,k+1))*xmb(i) 
             enddo
          elseif(ierr(i).ne.0 .or. pre(i).eq.0.)then
             ktop(i)=0
             do k=kts,kte
               outt(i,k)=0.
               outq(i,k)=0.
               outqc(i,k)=0.
               outu(i,k)=0.
               outv(i,k)=0.
             enddo
          endif
      enddo
!
!--- MPAS:
!     call cup_transport_tends(ierr,k22,kbcon,ktop,jmin,ntrans,xmb,p_cup,ncl2,nil2,nifal2,  &
!                              nwfal2,qtransl2,nc_up,ni_up,nifa_up,nwfa_up,qtrans_up,nc_dn, &
!                              ni_dn,nifa_dn,nwfa_dn,qtrans_dn,edto,zuo,zdo,outnc,outni,    &
!                              outnifa,outnwfa,outqtrans,its,ite,itf,kts,kte,ktf)
 
! rain evaporation as in sas
!
      if(irainevap.eq.1)then
      do i = its,itf
       rntot(i) = 0.
       delqev(i) = 0.
       delq2(i) = 0.
       rn(i)    = 0.
       rntot(i)    = 0.
       rain=0.
       if(ierr(i).eq.0)then
         do k = ktop(i), 1, -1
              rain =  pwo(i,k) + edto(i) * pwdo(i,k)
              rntot(i) = rntot(i) + rain * xmb(i)* .001 * dtime
         enddo
       endif
      enddo
      do i = its,itf
         qevap(i) = 0.
         flg(i) = .true.
         if(ierr(i).eq.0)then
         evef = edt(i) * evfact
         if(xland(i).gt.0.5 .and. xland(i).lt.1.5) evef=edt(i) * evfactl
         do k = ktop(i), 1, -1
              rain =  pwo(i,k) + edto(i) * pwdo(i,k)
              rn(i) = rn(i) + rain * xmb(i) * .001 * dtime
            if(po(i,k).gt.400.)then
              if(flg(i))then
              q1=qo(i,k)+(outq(i,k))*dtime
              t1=tn(i,k)+(outt(i,k))*dtime
              qcond(i) = evef * (q1 - qeso(i,k))            &
     &                 / (1. + el2orc * qeso(i,k) / t1**2)
              dp = -100.*(p_cup(i,k+1)-p_cup(i,k))
              if(rn(i).gt.0. .and. qcond(i).lt.0.) then
                qevap(i) = -qcond(i) * (1.-exp(-.32*sqrt(dtime*rn(i))))
                qevap(i) = min(qevap(i), rn(i)*1000.*g/dp)
                delq2(i) = delqev(i) + .001 * qevap(i) * dp / g
              endif
              if(rn(i).gt.0..and.qcond(i).lt.0..and. &
     &           delq2(i).gt.rntot(i)) then
                qevap(i) = 1000.* g * (rntot(i) - delqev(i)) / dp
                flg(i) = .false.
              endif
              if(rn(i).gt.0..and.qevap(i).gt.0.) then
                outq(i,k) = outq(i,k) + qevap(i)/dtime
                outt(i,k) = outt(i,k) - elocp * qevap(i)/dtime
                rn(i) = max(0.,rn(i) - .001 * qevap(i) * dp / g)
                pre(i) = pre(i) - qevap(i) * dp /g/dtime
                pre(i)=max(pre(i),0.)
                delqev(i) = delqev(i) + .001*dp*qevap(i)/g
              endif
            endif ! 400mb
          endif
        enddo
!       pre(i)=1000.*rn(i)/dtime
      endif
      enddo
      endif
!
! since kinetic energy is being dissipated, add heating accordingly (from ecmwf)
!
      do i=its,itf
          if(ierr(i).eq.0) then
             dts=0.
             fpi=0.
             do k=kts,ktop(i)
                dp=(po_cup(i,k)-po_cup(i,k+1))*100.
!total ke dissiptaion estimate
                dts= dts -(outu(i,k)*us(i,k)+outv(i,k)*vs(i,k))*dp/g
! fpi needed for calcualtion of conversion to pot. energyintegrated 
                fpi = fpi  +sqrt(outu(i,k)*outu(i,k) + outv(i,k)*outv(i,k))*dp
             enddo
             if(fpi.gt.0.)then
                do k=kts,ktop(i)
                   fp= sqrt((outu(i,k)*outu(i,k)+outv(i,k)*outv(i,k)))/fpi
                   outt(i,k)=outt(i,k)+fp*dts*g/cp
                enddo
             endif
          endif
      enddo


!
!---------------------------done------------------------------
!

   end subroutine cu_gf_deep_run
!> @}

!>\ingroup cu_gf_deep_group


   subroutine fct1d3 (ktop,n,dt,z,tracr,massflx,trflx_in,dellac,g)

! --- modify a 1-D array of tracer fluxes for the purpose of maintaining
! --- monotonicity (including positive-definiteness) in the tracer field
! --- during tracer transport.

! --- the underlying transport equation is   (d tracr/dt) = - (d trflx/dz)
! --- where  dz = |z(k+1)-z(k)| (k=1,...,n) and  trflx = massflx * tracr
! --- physical dimensions of tracr,trflx,dz are arbitrary to some extent
! --- but are subject to the constraint dim[trflx] = dim[tracr*(dz/dt)].

! --- note: tracr is carried in grid cells while z and fluxes are carried on
! --- interfaces. interface variables at index k are at grid location k-1/2.
! --- sign convention: mass fluxes are considered positive in +k direction.

! --- massflx and trflx_in  must be provided independently to allow the
! --- algorithm to generate an auxiliary low-order (diffusive) tracer flux
! --- as a stepping stone toward the final product trflx_out.

   implicit none
   integer,intent(in) :: n,ktop                                 ! number of grid cells
   real(kind=kind_phys)   ,intent(in) :: dt,g                   ! transport time step
   real(kind=kind_phys)   ,intent(in) :: z(n+0)                 ! location of cell interfaces
   real(kind=kind_phys)   ,intent(in) :: tracr(n)               ! the transported variable
   real(kind=kind_phys)   ,intent(in) :: massflx(n+0)           ! mass flux across interfaces
   real(kind=kind_phys)   ,intent(in) :: trflx_in(n+0)          ! original tracer flux
   real(kind=kind_phys)   ,intent(out):: dellac(n+0)            ! modified tracr flux
   real(kind=kind_phys)   :: trflx_out(n+0)                     ! modified tracr flux
   integer k,km1,kp1
   logical :: NaN, error=.false., vrbos=.true.
   real(kind=kind_phys) dtovdz(n),trmax(n),trmin(n),flx_lo(n+0),antifx(n+0),clipped(n+0),  &
        soln_hi(n),totlin(n),totlout(n),soln_lo(n),clipin(n),clipout(n),arg
   real(kind=kind_phys),parameter :: epsil=1.e-22               ! prevent division by zero
   real(kind=kind_phys),parameter :: damp=1.                    ! damper of antidff flux (1=no damping)
   NaN(arg) = .not. (arg.ge.0. .or. arg.le.0.)                  ! NaN detector
   dtovdz(:)=0.
   soln_lo(:)=0.
   antifx(:)=0.
   clipin(:)=0.
   totlin(:)=0.
   totlout(:)=0.
   clipout(:)=0.
   flx_lo(:)=0.
   trmin(:)=0.
   trmax(:)=0.
   clipped(:)=0.
   trflx_out(:)=0.
   do k=1,ktop
     dtovdz(k)=.01*dt/abs(z(k+1)-z(k))*g                        ! time step / grid spacing
     if (z(k).eq.z(k+1)) error=.true.
   end do
!  if (vrbos .or. error) print '(a/(8es10.3))','(fct1d) dtovdz =',dtovdz

   do k=2,ktop
     if (massflx(k).ge.0.) then
       flx_lo(k)=massflx(k)*tracr(k-1)          ! low-order flux, upstream
     else
       flx_lo(k)=massflx(k)*tracr(k)            ! low-order flux, upstream
     end if
     antifx(k)=trflx_in(k)-flx_lo(k)            ! antidiffusive flux
   end do
   flx_lo(     1)=trflx_in(  1)
   flx_lo(ktop+1)=trflx_in(ktop+1)
   antifx(     1)=0.
   antifx(ktop+1)=0.
! --- clip low-ord fluxes to make sure they don't violate positive-definiteness
   do k=1,ktop
     totlout(k)=max(0.,flx_lo(k+1))-min(0.,flx_lo(k  ))         ! total flux out
     clipout(k)=min(1.,tracr(k)/max(epsil,totlout(k))/ (1.0001*dtovdz(k)))
   end do

   do k=2,ktop
    if (massflx(k).ge.0.)  then
      flx_lo(k)=flx_lo(k)*clipout(k-1)
    else
      flx_lo(k)=flx_lo(k)*clipout(k)
    end if
   end do
   if (massflx(  1).lt.0.) flx_lo(  1)=flx_lo(  1)*clipout(1)
   if (massflx(ktop+1).gt.0.)flx_lo(ktop+1)=flx_lo(ktop+1)*clipout(ktop)

! --- a positive-definite low-order (diffusive) solution can now be  constructed

   do k=1,ktop
     soln_lo(k)=tracr(k)-(flx_lo(k+1)-flx_lo(k))*dtovdz(k)      ! low-ord solutn
     dellac(k)=-(flx_lo(k+1)-flx_lo(k))*dtovdz(k)/dt
    !dellac(k)=soln_lo(k)
   end do
   return
   do k=1,ktop
     km1=max(1,k-1)
     kp1=min(ktop,k+1)
     trmax(k)=       max(soln_lo(km1),soln_lo(k),soln_lo(kp1),  &
                         tracr  (km1),tracr  (k),tracr  (kp1))  ! upper bound
     trmin(k)=max(0.,min(soln_lo(km1),soln_lo(k),soln_lo(kp1),  &
                         tracr  (km1),tracr  (k),tracr  (kp1))) ! lower bound
   end do

   do k=1,ktop
     totlin (k)=max(0.,antifx(k  ))-min(0.,antifx(k+1))         ! total flux in
     totlout(k)=max(0.,antifx(k+1))-min(0.,antifx(k  ))         ! total flux out

     clipin (k)=min(damp,(trmax(k)-soln_lo(k))/max(epsil,totlin (k))    &
       / (1.0001*dtovdz(k)))
     clipout(k)=min(damp,(soln_lo(k)-trmin(k))/max(epsil,totlout(k))    &
       / (1.0001*dtovdz(k)))

     if (NaN(clipin(k)))  print *,'(fct1d) error: clipin is NaN,  k=',k
     if (NaN(clipout(k))) print *,'(fct1d) error: clipout is NaN,  k=',k

     if (clipin(k).lt.0.) then
!       print 100,'(fct1d) error: clipin < 0 at k =',k,			&
!       'clipin',clipin(k),'trmax',trmax(k),'soln_lo',soln_lo(k),	&
!       'totlin',totlin(k),'dt/dz',dtovdz(k)
       error=.true.
     end if
     if (clipout(k).lt.0.) then
!       print 100,'(fct1d) error: clipout < 0 at k =',k,			&
!       'clipout',clipout(k),'trmin',trmin(k),'soln_lo',soln_lo(k),	&
!       'totlout',totlout(k),'dt/dz',dtovdz(k)
       error=.true.
     end if
! 100 format (a,i3/(4(a10,"=",es9.2)))
   end do

   do k=2,ktop
     if (antifx(k).gt.0.)  then
       clipped(k)=antifx(k)*min(clipout(k-1),clipin(k))
     else
       clipped(k)=antifx(k)*min(clipout(k),clipin(k-1))
     end if
     trflx_out(k)=flx_lo(k)+clipped(k)
     if (NaN(trflx_out(k)))  then
       print *,'(fct1d) error: trflx_out is NaN,  k=',k
       error=.true.
     end if
   end do
   trflx_out(     1)=trflx_in(     1)
   trflx_out(ktop+1)=trflx_in(ktop+1)
   do k=1,ktop
     soln_hi(k)=tracr(k)-(trflx_out(k+1)-trflx_out(k))*dtovdz(k)
     dellac(k)=-g*(trflx_out(k+1)-trflx_out(k))*dtovdz(k)/dt
     !dellac(k)=soln_hi(k)
   end do

   if (vrbos .or. error) then
!     do k=2,ktop
!       write(32,99)k,						&
!       'tracr(k)', tracr(k),					&
!       'flx_in(k)', trflx_in(k),				&
!       'flx_in(k+1)', trflx_in(k+1),				&
!       'flx_lo(k)', flx_lo(k),					&
!       'flx_lo(k+1)', flx_lo(k+1),				&
!       'soln_lo(k)', soln_lo(k),				&
!       'trmin(k)', trmin(k),					&
!       'trmax(k)', trmax(k),					&
!       'totlin(k)', totlin(k),					&
!       'totlout(k)', totlout(k),				&
!       'clipin(k-1)', clipin(k-1),				&
!       'clipin(k)', clipin(k),					&
!       'clipout(k-1)', clipout(k-1),				&
!       'clipout(k)', clipout(k),				&
!       'antifx(k)', antifx(k),					&
!       'antifx(k+1)', antifx(k+1),				&
!       'clipped(k)', clipped(k),				&
!       'clipped(k+1)', clipped(k+1),				&
!       'flx_out(k)', trflx_out(k),				&
!       'flx_out(k+1)', trflx_out(k+1),				&
!       'dt/dz(k)', dtovdz(k),					&
!       'final', tracr(k)-(trflx_out(k+1)-trflx_out(k))*dtovdz(k)
! 99    format ('(trc1d)   k =',i4/(3(a13,'=',es13.6)))
!     end do
     if (error) stop '(fct1d error)'
   end if

   return
   end subroutine fct1d3

   subroutine rain_evap_below_cloudbase(itf,ktf, its,ite, kts,kte,ierr,    &
                   kbcon,xmb,psur,xland,qo_cup,                            &
                   po_cup,qes_cup,pwavo,edto,pwevo,pre,outt,outq) !,outbuoy)

   implicit none
   real(kind=kind_phys), parameter :: alp1=5.44e-4 & !1/sec
                     ,alp2=5.09e-3 & !unitless
                     ,alp3=0.5777  & !unitless
                     ,c_conv=0.05    !conv fraction area, unitless


   integer                           ,intent(in)    :: itf,ktf, its,ite, kts,kte
   integer, dimension(its:ite)       ,intent(in)    :: ierr,kbcon
   real(kind=kind_phys), dimension(its:ite)        ,intent(in)    ::psur,xland,pwavo,edto,pwevo,xmb
   real(kind=kind_phys), dimension(its:ite,kts:kte),intent(in)    :: po_cup,qo_cup,qes_cup
   real(kind=kind_phys), dimension(its:ite)        ,intent(inout) :: pre
   real(kind=kind_phys), dimension(its:ite,kts:kte),intent(inout) :: outt,outq  !,outbuoy

  !real,    dimension(its:ite)        ,intent(out)      :: tot_evap_bcb
  !real,    dimension(its:ite,kts:kte),intent(out) :: evap_bcb,net_prec_bcb

  !-- locals
   integer :: i,k
   real(kind=kind_phys) :: RH_cr , del_t,del_q,dp,q_deficit
   real(kind=kind_phys),  dimension(its:ite,kts:kte) :: evap_bcb,net_prec_bcb
   real(kind=kind_phys),  dimension(its:ite)         :: tot_evap_bcb

   do i=its,itf
     evap_bcb    (i,:)= 0.0
     net_prec_bcb(i,:)= 0.0
     tot_evap_bcb(i)  = 0.0
     if(ierr(i) /= 0) cycle

    !-- critical rel humidity
     RH_cr=0.9*xland(i)+0.7*(1-xland(i))
    !RH_cr=1.

     !-- net precipitation (after downdraft evap) at cloud base, available to
     !evap
     k=kbcon(i)
    !net_prec_bcb(i,k) = xmb(i)*(pwavo(i)+edto(i)*pwevo(i)) !-- pwevo<0.
     net_prec_bcb(i,k) = pre(i)

     do k=kbcon(i)-1, kts, -1

       q_deficit = max(0.,(RH_cr*qes_cup(i,k) -qo_cup(i,k)))

       if(q_deficit < 1.e-6) then
          net_prec_bcb(i,k)= net_prec_bcb(i,k+1)
          cycle
       endif

       dp = 100.*(po_cup(i,k)-po_cup(i,k+1))

      !--units here: kg[water]/kg[air}/sec
       evap_bcb(i,k) = c_conv * alp1 * q_deficit * &
                      ( sqrt(po_cup(i,k)/psur(i))/alp2 *net_prec_bcb(i,k+1)/c_conv )**alp3

      !--units here: kg[water]/kg[air}/sec * kg[air]/m3 * m = kg[water]/m2/sec
       evap_bcb(i,k)= evap_bcb(i,k)*dp/g

       if((net_prec_bcb(i,k+1) - evap_bcb(i,k)).lt.0.) cycle
       if((pre(i) - evap_bcb(i,k)).lt.0.) cycle
       net_prec_bcb(i,k)= net_prec_bcb(i,k+1) - evap_bcb(i,k)

       tot_evap_bcb(i) = tot_evap_bcb(i)+evap_bcb(i,k)

      !-- feedback
       del_q =  evap_bcb(i,k)*g/dp          ! > 0., units: kg[water]/kg[air}/sec
       del_t = -evap_bcb(i,k)*g/dp*(xlv/cp) ! < 0., units: K/sec

!       print*,"ebcb2",k,del_q*86400,del_t*86400

       outq   (i,k) = outq   (i,k) + del_q
       outt   (i,k) = outt   (i,k) + del_t
      !outbuoy(i,k) = outbuoy(i,k) + cp*del_t+xlv*del_q

       pre(i) = pre(i) - evap_bcb(i,k)
      enddo
    enddo

   end subroutine rain_evap_below_cloudbase



   subroutine cup_dd_edt(ierr,us,vs,z,ktop,kbcon,edt,p,pwav, &
              pw,ccn,pwev,edtmax,edtmin,edtc,psum2,psumh,    &
              rho,aeroevap,itf,ktf,                          &
              its,ite, kts,kte                     )

   implicit none

     integer                                                 &
        ,intent (in   )                   ::                 &
        aeroevap,itf,ktf,                                    &
        its,ite, kts,kte
  !
  ! ierr error value, maybe modified in this routine
  !
     real(kind=kind_phys),    dimension (its:ite,kts:kte)                    &
        ,intent (in   )                   ::                 &
        rho,us,vs,z,p,pw
     real(kind=kind_phys),    dimension (its:ite,1)                          &
        ,intent (out  )                   ::                 &
        edtc
     real(kind=kind_phys),    dimension (its:ite)                            &
        ,intent (out  )                   ::                 &
        edt
     real(kind=kind_phys),    dimension (its:ite)                            &
        ,intent (in   )                   ::                 &
        pwav,pwev,ccn,psum2,psumh,edtmax,edtmin
     integer, dimension (its:ite)                            &
        ,intent (in   )                   ::                 &
        ktop,kbcon
     integer, dimension (its:ite)                            &
        ,intent (inout)                   ::                 &
        ierr
!
!  local variables in this routine
!

     integer i,k,kk
     real(kind=kind_phys)    einc,pef,pefb,prezk,zkbc
     real(kind=kind_phys),    dimension (its:ite)         ::                 &
      vshear,sdp,vws
     real(kind=kind_phys) :: prop_c,pefc,aeroadd,alpha3,beta3
     prop_c=8. !10.386
     alpha3 = 1.9
     beta3  = -1.13
     pefc=0.

!
!--- determine downdraft strength in terms of windshear
!
! */ calculate an average wind shear over the depth of the cloud
!
       do i=its,itf
        edt(i)=0.
        vws(i)=0.
        sdp(i)=0.
        vshear(i)=0.
       enddo
       do i=its,itf
        edtc(i,1)=0.
       enddo
       do kk = kts,ktf-1
         do 62 i=its,itf
          if(ierr(i).ne.0)go to 62
          if (kk .le. min0(ktop(i),ktf) .and. kk .ge. kbcon(i)) then
             vws(i) = vws(i)+                                        &
              (abs((us(i,kk+1)-us(i,kk))/(z(i,kk+1)-z(i,kk)))        &
          +   abs((vs(i,kk+1)-vs(i,kk))/(z(i,kk+1)-z(i,kk)))) *      &
              (p(i,kk) - p(i,kk+1))
            sdp(i) = sdp(i) + p(i,kk) - p(i,kk+1)
          endif
          if (kk .eq. ktf-1)vshear(i) = 1.e3 * vws(i) / sdp(i)
   62   continue
       end do
      do i=its,itf
         if(ierr(i).eq.0)then
            pef=(1.591-.639*vshear(i)+.0953*(vshear(i)**2)            &
               -.00496*(vshear(i)**3))
            if(pef.gt.0.9)pef=0.9
            if(pef.lt.0.1)pef=0.1
!
!--- cloud base precip efficiency
!
            zkbc=z(i,kbcon(i))*3.281e-3
            prezk=.02
            if(zkbc.gt.3.)then
               prezk=.96729352+zkbc*(-.70034167+zkbc*(.162179896+zkbc &
               *(- 1.2569798e-2+zkbc*(4.2772e-4-zkbc*5.44e-6))))
            endif
            if(zkbc.gt.25)then
               prezk=2.4
            endif
            pefb=1./(1.+prezk)
            if(pefb.gt.0.9)pefb=0.9
            if(pefb.lt.0.1)pefb=0.1
            edt(i)=1.-.5*(pefb+pef)
            if(aeroevap.gt.1)then
               aeroadd=(ccnclean**beta3)*((psumh(i))**(alpha3-1)) !*1.e6
!              prop_c=.9/aeroadd
               prop_c=.5*(pefb+pef)/aeroadd
               aeroadd=(ccn(i)**beta3)*((psum2(i))**(alpha3-1)) !*1.e6
               aeroadd=prop_c*aeroadd
               pefc=aeroadd
               if(pefc.gt.0.9)pefc=0.9
               if(pefc.lt.0.1)pefc=0.1
               edt(i)=1.-pefc
               if(aeroevap.eq.2)edt(i)=1.-.25*(pefb+pef+2.*pefc)
            endif


!--- edt here is 1-precipeff!
            einc=.2*edt(i)
            edtc(i,1)=edt(i)-einc
         endif
      enddo
      do i=its,itf
         if(ierr(i).eq.0)then
               edtc(i,1)=-edtc(i,1)*pwav(i)/pwev(i)
               if(edtc(i,1).gt.edtmax(i))edtc(i,1)=edtmax(i)
               if(edtc(i,1).lt.edtmin(i))edtc(i,1)=edtmin(i)
         endif
      enddo

   end subroutine cup_dd_edt

!>\ingroup cu_gf_deep_group
   subroutine cup_dd_moisture(ierrc,zd,hcd,hes_cup,qcd,qes_cup,  &
              pwd,q_cup,z_cup,dd_massentr,dd_massdetr,jmin,ierr, &
              gamma_cup,pwev,bu,qrcd,                            &
              q,he,iloop,                                        &
              itf,ktf,                                           &
              its,ite, kts,kte                     )

   implicit none

     integer                                                     &
        ,intent (in   )                   ::                     &
                                  itf,ktf,                       &
                                  its,ite, kts,kte
  ! cdd= detrainment function 
  ! q = environmental q on model levels
  ! q_cup = environmental q on model cloud levels
  ! qes_cup = saturation q on model cloud levels
  ! hes_cup = saturation h on model cloud levels
  ! hcd = h in model cloud
  ! bu = buoancy term
  ! zd = normalized downdraft mass flux
  ! gamma_cup = gamma on model cloud levels
  ! mentr_rate = entrainment rate
  ! qcd = cloud q (including liquid water) after entrainment
  ! qrch = saturation q in cloud
  ! pwd = evaporate at that level
  ! pwev = total normalized integrated evaoprate (i2)
  ! entr= entrainment rate 
  !
     real(kind=kind_phys),    dimension (its:ite,kts:kte)               &
        ,intent (in   )                   ::            &
        zd,hes_cup,hcd,qes_cup,q_cup,z_cup,             &
        dd_massentr,dd_massdetr,gamma_cup,q,he 
     integer                                            &
        ,intent (in   )                   ::            &
        iloop
     integer, dimension (its:ite)                       &
        ,intent (in   )                   ::            &
        jmin
     integer, dimension (its:ite)                       &
        ,intent (inout)                   ::            &
        ierr
     real(kind=kind_phys),    dimension (its:ite,kts:kte)&
        ,intent (out  )                   ::            &
        qcd,qrcd,pwd
     real(kind=kind_phys),    dimension (its:ite)&
        ,intent (out  )                   ::            &
        pwev,bu
     character*50 :: ierrc(its:ite)
!
!  local variables in this routine
!

     integer                              ::            &
        i,k,ki
     real(kind=kind_phys)                                 ::            &
        denom,dh,dz,dqeva

      do i=its,itf
         bu(i)=0.
         pwev(i)=0.
      enddo
      do k=kts,ktf
      do i=its,itf
         qcd(i,k)=0.
         qrcd(i,k)=0.
         pwd(i,k)=0.
      enddo
      enddo
!
!
!
      do 100 i=its,itf
      if(ierr(i).eq.0)then
      k=jmin(i)
      dz=z_cup(i,k+1)-z_cup(i,k)
      qcd(i,k)=q_cup(i,k)
      dh=hcd(i,k)-hes_cup(i,k)
      if(dh.lt.0)then
        qrcd(i,k)=(qes_cup(i,k)+(1./xlv)*(gamma_cup(i,k) &
                  /(1.+gamma_cup(i,k)))*dh)
        else
          qrcd(i,k)=qes_cup(i,k)
        endif
      pwd(i,jmin(i))=zd(i,jmin(i))*min(0.,qcd(i,k)-qrcd(i,k))
      qcd(i,k)=qrcd(i,k)
      pwev(i)=pwev(i)+pwd(i,jmin(i)) ! *dz
!
      bu(i)=dz*dh
      do ki=jmin(i)-1,1,-1
         dz=z_cup(i,ki+1)-z_cup(i,ki)
!        qcd(i,ki)=(qcd(i,ki+1)*(1.-.5*cdd(i,ki+1)*dz) &
!                 +entr*dz*q(i,ki) &
!                )/(1.+entr*dz-.5*cdd(i,ki+1)*dz)
!        dz=qcd(i,ki)
!print*,"i=",i," k=",ki," qcd(i,ki+1)=",qcd(i,ki+1)
!print*,"zd=",zd(i,ki+1)," dd_ma=",dd_massdetr(i,ki)," q=",q(i,ki)
!joe-added check for non-zero denominator:
         denom=zd(i,ki+1)-.5*dd_massdetr(i,ki)+dd_massentr(i,ki)
         if(denom.lt.1.e-16)then
            ierr(i)=51
            exit
         endif
         qcd(i,ki)=(qcd(i,ki+1)*zd(i,ki+1)                    &
                  -.5*dd_massdetr(i,ki)*qcd(i,ki+1)+          &
                  dd_massentr(i,ki)*q(i,ki))   /              &
                  (zd(i,ki+1)-.5*dd_massdetr(i,ki)+dd_massentr(i,ki))
!
!--- to be negatively buoyant, hcd should be smaller than hes!
!--- ideally, dh should be negative till dd hits ground, but that is not always
!--- the case
!
         dh=hcd(i,ki)-hes_cup(i,ki)
         bu(i)=bu(i)+dz*dh
         qrcd(i,ki)=qes_cup(i,ki)+(1./xlv)*(gamma_cup(i,ki)   &
                  /(1.+gamma_cup(i,ki)))*dh
         dqeva=qcd(i,ki)-qrcd(i,ki)
         if(dqeva.gt.0.)then
          dqeva=0.
          qrcd(i,ki)=qcd(i,ki)
         endif
         pwd(i,ki)=zd(i,ki)*dqeva
         qcd(i,ki)=qrcd(i,ki)
         pwev(i)=pwev(i)+pwd(i,ki) ! *dz
!        if(iloop.eq.1.and.i.eq.102.and.j.eq.62)then
!         print *,'in cup_dd_moi ', hcd(i,ki),hes_cup(i,ki),dh,dqeva
!        endif
      enddo
!
!--- end loop over i
       if( (pwev(i).eq.0.) .and. (iloop.eq.1))then
!        print *,'problem with buoy in cup_dd_moisture',i
         ierr(i)=7
         ierrc(i)="problem with buoy in cup_dd_moisture"
       endif
       if(bu(i).ge.0.and.iloop.eq.1)then
!        print *,'problem with buoy in cup_dd_moisture',i
         ierr(i)=7
         ierrc(i)="problem2 with buoy in cup_dd_moisture"
       endif
      endif
100    continue

   end subroutine cup_dd_moisture

   subroutine cup_forcing_ens_3d(closure_n,xland,aa0,aa1,xaa0,mbdt,dtime,ierr,ierr2,ierr3,&
              xf_ens,axx,forcing,maxens3,mconv,rand_clos,             &
              p_cup,ktop,omeg,zd,zdm,k22,zu,pr_ens,edt,edtm,kbcon,    &
              ichoice,                                                &
              imid,ipr,itf,ktf,                                       &
              its,ite, kts,kte,                                       &
              dicycle,tau_ecmwf,aa1_bl,xf_dicycle  )

   implicit none

     integer                                                          &
        ,intent (in   )                   ::                          &
        imid,ipr,itf,ktf,                                             &
        its,ite, kts,kte
     integer, intent (in   )              ::                          &
        maxens3
  !
  ! ierr error value, maybe modified in this routine
  ! pr_ens = precipitation ensemble
  ! xf_ens = mass flux ensembles
  ! massfln = downdraft mass flux ensembles used in next timestep
  ! omeg = omega from large scale model
  ! mconv = moisture convergence from large scale model
  ! zd      = downdraft normalized mass flux
  ! zu      = updraft normalized mass flux
  ! aa0     = cloud work function without forcing effects
  ! aa1     = cloud work function with forcing effects
  ! xaa0    = cloud work function with cloud effects (ensemble dependent)
  ! edt     = epsilon
  ! dir     = "storm motion"
  ! mbdt    = arbitrary numerical parameter
  ! dtime   = dt over which forcing is applied
  ! iact_gr_old = flag to tell where convection was active
  ! kbcon       = lfc of parcel from k22
  ! k22         = updraft originating level
  ! ichoice       = flag if only want one closure (usually set to zero!)
  !
     real(kind=kind_phys),    dimension (its:ite,1:maxens3)                            &
        ,intent (inout)                   ::                           &
        pr_ens
     real(kind=kind_phys),    dimension (its:ite,1:maxens3)                            &
        ,intent (inout  )                 ::                           &
        xf_ens
     real(kind=kind_phys),    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        zd,zu,p_cup,zdm
     real(kind=kind_phys),    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        omeg
     real(kind=kind_phys),    dimension (its:ite,1)                                    &
        ,intent (in   )                   ::                           &
        xaa0
     real(kind=kind_phys),    dimension (its:ite,4)                                    &
        ,intent (in   )                   ::                           &
       rand_clos 
     real(kind=kind_phys),    dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        aa1,edt,edtm
     real(kind=kind_phys),    dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        mconv,axx
     real(kind=kind_phys),    dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        aa0,closure_n
     real(kind=kind_phys)                                                              &
        ,intent (in   )                   ::                           &
        mbdt
     real(kind=kind_phys)                                                              &
        ,intent (in   )                   ::                           &
        dtime
     integer, dimension (its:ite)                                      &
        ,intent (inout   )                ::                           &
        k22,kbcon,ktop
     integer, dimension (its:ite)                                      &
        ,intent (in      )                ::                           &
        xland
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr,ierr2,ierr3
     integer                                                           &
        ,intent (in   )                   ::                           &
        ichoice
      integer, intent(in) :: dicycle
      real(kind=kind_phys),    intent(in)   , dimension (its:ite) :: aa1_bl,tau_ecmwf
      real(kind=kind_phys),    intent(inout), dimension (its:ite) :: xf_dicycle
      real(kind=kind_phys),    intent(inout), dimension (its:ite,10) :: forcing
      !- local var
      real(kind=kind_phys)  :: xff_dicycle
!
!  local variables in this routine
!

     real(kind=kind_phys),    dimension (1:maxens3)       ::                           &
       xff_ens3
     real(kind=kind_phys),    dimension (1)               ::                           &
       xk
     integer                              ::                           &
       kk,i,k,n,ne
!     integer, parameter :: mkxcrt=15
!     real(kind=kind_phys),    dimension(1:mkxcrt)        ::                           &
!       pcrit,acrit,acritt
     integer, dimension (its:ite)         :: kloc
     real(kind=kind_phys)                                ::                           &
       a1,a_ave,xff0,xomg!,aclim1,aclim2,aclim3,aclim4

     real(kind=kind_phys), dimension (its:ite) :: ens_adj



!
       ens_adj(:)=1.
       xff_dicycle = 0.

!--- large scale forcing
!
       do 100 i=its,itf
          kloc(i)=1
          if(ierr(i).eq.0)then
!           kloc(i)=maxloc(zu(i,:),1)
           kloc(i)=kbcon(i)
           ens_adj(i)=1.
!ss --- comment out adjustment over ocean
!ss           if(ierr2(i).gt.0.and.ierr3(i).eq.0)ens_adj(i)=0.666 ! 2./3.
!ss           if(ierr2(i).gt.0.and.ierr3(i).gt.0)ens_adj(i)=0.333
!
             a_ave=0.
             a_ave=axx(i)
             a_ave=max(0.,a_ave)
             a_ave=min(a_ave,aa1(i))
             a_ave=max(0.,a_ave)
             xff_ens3(:)=0.
             xff0= (aa1(i)-aa0(i))/dtime
             xff_ens3(1)=max(0.,(aa1(i)-aa0(i))/dtime)
             xff_ens3(2)=max(0.,(aa1(i)-aa0(i))/dtime)
             xff_ens3(3)=max(0.,(aa1(i)-aa0(i))/dtime)
             xff_ens3(16)=max(0.,(aa1(i)-aa0(i))/dtime)
             forcing(i,1)=xff_ens3(2)
!   
!--- omeg is in bar/s, mconv done with omeg in pa/s
!     more like brown (1979), or frank-cohen (199?)
!  
! average aaround kbcon
!
             xomg=0.
             kk=0
             xff_ens3(4)=0.
             xff_ens3(5)=0.
             xff_ens3(6)=0.
             do k=kbcon(i)-1,kbcon(i)+1
                     if(zu(i,k).gt.0.)then
                       xomg=xomg-omeg(i,k)/9.81/max(0.3,(1.-(edt(i)*zd(i,k)-edtm(i)*zdm(i,k))/zu(i,k)))
                       kk=kk+1
                     endif
             enddo
             if(kk.gt.0)xff_ens3(4)=xomg/float(kk)
            
!
! max below kbcon
!             xff_ens3(6)=-omeg(i,k22(i))/9.81
!             do k=k22(i),kbcon(i)
!                     xomg=-omeg(i,k)/9.81
!                     if(xomg.gt.xff_ens3(6))xff_ens3(6)=xomg
!             enddo
!
!             if(zu(i,kbcon(i)) > 0)xff_ens3(6)=betajb*xff_ens3(6)/zu(i,kbcon(i))
             xff_ens3(4)=betajb*xff_ens3(4)
             xff_ens3(5)=xff_ens3(4)
             xff_ens3(6)=xff_ens3(4)
             if(xff_ens3(4).lt.0.)xff_ens3(4)=0.
             if(xff_ens3(5).lt.0.)xff_ens3(5)=0.
             if(xff_ens3(6).lt.0.)xff_ens3(6)=0.
             xff_ens3(14)=xff_ens3(4)
             forcing(i,2)=xff_ens3(4)
!
!--- more like krishnamurti et al.; pick max and average values
!
              xff_ens3(7)= mconv(i) !/max(0.5,(1.-edt(i)*zd(i,kbcon(i))/zu(i,kloc(i))))
              xff_ens3(8)= mconv(i) !/max(0.5,(1.-edt(i)*zd(i,kbcon(i))/zu(i,kloc(i))))
              xff_ens3(9)= mconv(i) !/max(0.5,(1.-edt(i)*zd(i,kbcon(i))/zu(i,kloc(i))))
              xff_ens3(15)=mconv(i) !/max(0.5,(1.-edt(i)*zd(i,kbcon(i))/zu(i,kloc(i))))
             forcing(i,3)=xff_ens3(8)
!
!--- more like fritsch chappel or kain fritsch (plus triggers)
!
             xff_ens3(10)=aa1(i)/tau_ecmwf(i)
             xff_ens3(11)=aa1(i)/tau_ecmwf(i)
             xff_ens3(12)=aa1(i)/tau_ecmwf(i)
             xff_ens3(13)=(aa1(i))/tau_ecmwf(i) !(60.*15.) !tau_ecmwf(i)
!             forcing(i,4)=xff_ens3(10)

!!- more like bechtold et al. (jas 2014)
!!             if(dicycle == 1) xff_dicycle = max(0.,aa1_bl(i)/tau_ecmwf(i)) !(60.*30.) !tau_ecmwf(i)
!gtest
             if(ichoice.eq.0)then
                if(xff0.lt.0.)then
                     xff_ens3(1)=0.
                     xff_ens3(2)=0.
                     xff_ens3(3)=0.
                     xff_ens3(10)=0.
                     xff_ens3(11)=0.
                     xff_ens3(12)=0.
                     xff_ens3(13)= 0.
                     xff_ens3(16)= 0.
!                    closure_n(i)=12.
!                    xff_dicycle = 0.
                endif  !xff0
             endif ! ichoice

             xk(1)=(xaa0(i,1)-aa1(i))/mbdt
             forcing(i,4)=aa0(i)
             forcing(i,5)=aa1(i)
             forcing(i,6)=xaa0(i,1)
             forcing(i,7)=xk(1)
             if(xk(1).le.0.and.xk(1).gt.-.01*mbdt) &
                           xk(1)=-.01*mbdt
             if(xk(1).gt.0.and.xk(1).lt.1.e-2)     &
                           xk(1)=1.e-2
             !   enddo
!
!--- add up all ensembles
!
!
! over water, enfor!e small cap for some of the closures
!
              if(xland(i).lt.0.1)then
                 if(ierr2(i).gt.0.or.ierr3(i).gt.0)then
                      xff_ens3(1) =ens_adj(i)*xff_ens3(1)
                      xff_ens3(2) =ens_adj(i)*xff_ens3(2)
                      xff_ens3(3) =ens_adj(i)*xff_ens3(3)
                      xff_ens3(4) =ens_adj(i)*xff_ens3(4)
                      xff_ens3(5) =ens_adj(i)*xff_ens3(5)
                      xff_ens3(6) =ens_adj(i)*xff_ens3(6)
                      xff_ens3(7) =ens_adj(i)*xff_ens3(7)
                      xff_ens3(8) =ens_adj(i)*xff_ens3(8)
                      xff_ens3(9) =ens_adj(i)*xff_ens3(9)
                      xff_ens3(10) =ens_adj(i)*xff_ens3(10)
                      xff_ens3(11) =ens_adj(i)*xff_ens3(11)
                      xff_ens3(12) =ens_adj(i)*xff_ens3(12)
                      xff_ens3(13) =ens_adj(i)*xff_ens3(13)
                      xff_ens3(14) =ens_adj(i)*xff_ens3(14)
                      xff_ens3(15) =ens_adj(i)*xff_ens3(15)
                      xff_ens3(16) =ens_adj(i)*xff_ens3(16)
!!                      !srf
!!                       xff_dicycle = ens_adj(i)*xff_dicycle
!!                      !srf end
!                      xff_ens3(7) =0.
!                      xff_ens3(8) =0.
!                      xff_ens3(9) =0.
                 endif ! ierr2
              endif ! xland
!
! end water treatment
!
!

!
!--- special treatment for stability closures
!
              if(xk(1).lt.0.)then
                 if(xff_ens3(1).gt.0)xf_ens(i,1)=max(0.,-xff_ens3(1)/xk(1))
                 if(xff_ens3(2).gt.0)xf_ens(i,2)=max(0.,-xff_ens3(2)/xk(1))
                 if(xff_ens3(3).gt.0)xf_ens(i,3)=max(0.,-xff_ens3(3)/xk(1))
                 if(xff_ens3(16).gt.0)xf_ens(i,16)=max(0.,-xff_ens3(16)/xk(1))
                 xf_ens(i,1)= xf_ens(i,1)+xf_ens(i,1)*rand_clos(i,1)
                 xf_ens(i,2)= xf_ens(i,2)+xf_ens(i,2)*rand_clos(i,1)
                 xf_ens(i,3)= xf_ens(i,3)+xf_ens(i,3)*rand_clos(i,1)
                 xf_ens(i,16)=xf_ens(i,16)+xf_ens(i,16)*rand_clos(i,1)
              else
                 xff_ens3(1)= 0
                 xff_ens3(2)= 0
                 xff_ens3(3)= 0
                 xff_ens3(16)=0
              endif
!
!--- if iresult.eq.1, following independent of xff0
!
              xf_ens(i,4)=max(0.,xff_ens3(4))
              xf_ens(i,5)=max(0.,xff_ens3(5))
              xf_ens(i,6)=max(0.,xff_ens3(6))
              xf_ens(i,14)=max(0.,xff_ens3(14))
              a1=max(1.e-5,pr_ens(i,7))
              xf_ens(i,7)=max(0.,xff_ens3(7)/a1)
              a1=max(1.e-5,pr_ens(i,8))
              xf_ens(i,8)=max(0.,xff_ens3(8)/a1)
!              forcing(i,7)=xf_ens(i,8)
              a1=max(1.e-5,pr_ens(i,9))
              xf_ens(i,9)=max(0.,xff_ens3(9)/a1)
              a1=max(1.e-3,pr_ens(i,15))
              xf_ens(i,15)=max(0.,xff_ens3(15)/a1)
              xf_ens(i,4)=xf_ens(i,4)+xf_ens(i,4)*rand_clos(i,2)
              xf_ens(i,5)=xf_ens(i,5)+xf_ens(i,5)*rand_clos(i,2)
              xf_ens(i,6)=xf_ens(i,6)+xf_ens(i,6)*rand_clos(i,2)
              xf_ens(i,14)=xf_ens(i,14)+xf_ens(i,14)*rand_clos(i,2)
              xf_ens(i,7)=xf_ens(i,7)+xf_ens(i,7)*rand_clos(i,3)
              xf_ens(i,8)=xf_ens(i,8)+xf_ens(i,8)*rand_clos(i,3)
              xf_ens(i,9)=xf_ens(i,9)+xf_ens(i,9)*rand_clos(i,3)
              xf_ens(i,15)=xf_ens(i,15)+xf_ens(i,15)*rand_clos(i,3)
              if(xk(1).lt.0.)then
                 xf_ens(i,10)=max(0.,-xff_ens3(10)/xk(1))
                 xf_ens(i,11)=max(0.,-xff_ens3(11)/xk(1))
                 xf_ens(i,12)=max(0.,-xff_ens3(12)/xk(1))
                 xf_ens(i,13)=max(0.,-xff_ens3(13)/xk(1))
                 xf_ens(i,10)=xf_ens(i,10)+xf_ens(i,10)*rand_clos(i,4)
                 xf_ens(i,11)=xf_ens(i,11)+xf_ens(i,11)*rand_clos(i,4)
                 xf_ens(i,12)=xf_ens(i,12)+xf_ens(i,12)*rand_clos(i,4)
                 xf_ens(i,13)=xf_ens(i,13)+xf_ens(i,13)*rand_clos(i,4)
                 forcing(i,8)=xf_ens(i,11)
              else
                 xf_ens(i,10)=0.
                 xf_ens(i,11)=0.
                 xf_ens(i,12)=0.
                 xf_ens(i,13)=0.
                 forcing(i,8)=0.
              endif
!srf-begin
!!              if(xk(1).lt.0.)then
!!                 xf_dicycle(i)      =  max(0.,-xff_dicycle /xk(1))
!!                forcing(i,9)=xf_dicycle(i)
!!              else
!!                 xf_dicycle(i)      = 0.
!!              endif
!srf-end
              if(ichoice.ge.1)then
!                 closure_n(i)=0.
                 xf_ens(i,1)=xf_ens(i,ichoice)
                 xf_ens(i,2)=xf_ens(i,ichoice)
                 xf_ens(i,3)=xf_ens(i,ichoice)
                 xf_ens(i,4)=xf_ens(i,ichoice)
                 xf_ens(i,5)=xf_ens(i,ichoice)
                 xf_ens(i,6)=xf_ens(i,ichoice)
                 xf_ens(i,7)=xf_ens(i,ichoice)
                 xf_ens(i,8)=xf_ens(i,ichoice)
                 xf_ens(i,9)=xf_ens(i,ichoice)
                 xf_ens(i,10)=xf_ens(i,ichoice)
                 xf_ens(i,11)=xf_ens(i,ichoice)
                 xf_ens(i,12)=xf_ens(i,ichoice)
                 xf_ens(i,13)=xf_ens(i,ichoice)
                 xf_ens(i,14)=xf_ens(i,ichoice)
                 xf_ens(i,15)=xf_ens(i,ichoice)
                 xf_ens(i,16)=xf_ens(i,ichoice)
              endif
          elseif(ierr(i).ne.20.and.ierr(i).ne.0)then
              do n=1,maxens3
                 xf_ens(i,n)=0.
!!
!!                 xf_dicycle(i) = 0.
!!
             enddo
          endif ! ierror
 100   continue


!-
!- diurnal cycle mass flux
!-              
if(dicycle == 1 )then

       do i=its,itf           
          xf_dicycle(i) = 0.
          if(ierr(i) /=  0)cycle
             
            xk(1)=(xaa0(i,1)-aa1(i))/mbdt
            if(xk(1).le.0.and.xk(1).gt.-.01*mbdt) xk(1)=-.01*mbdt
            if(xk(1).gt.0.and.xk(1).lt.1.e-2)     xk(1)=1.e-2
            
            xff_dicycle  = (aa1(i)-aa1_bl(i))/tau_ecmwf(i)
            if(xk(1).lt.0) xf_dicycle(i)= max(0.,-xff_dicycle/xk(1))
 
            xf_dicycle(i)= xf_ens(i,10)-xf_dicycle(i)
       enddo
else
       xf_dicycle(:) = 0.

endif
!---------



   end subroutine cup_forcing_ens_3d

   subroutine cup_maximi(array,ks,ke,maxx,ierr,              &
              itf,ktf,                                       &
              its,ite, kts,kte                     )

   implicit none
!
!  on input
!

   ! only local wrf dimensions are need as of now in this routine

     integer                                                           &
        ,intent (in   )                   ::                           &
         itf,ktf,                                                      &
         its,ite, kts,kte
  ! array input array
  ! x output array with return values
  ! kt output array of levels
  ! ks,kend  check-range
     real(kind=kind_phys),    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
         array
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
         ierr,ke
     integer                                                           &
        ,intent (in   )                   ::                           &
         ks
     integer, dimension (its:ite)                                      &
        ,intent (out  )                   ::                           &
         maxx
     real(kind=kind_phys),    dimension (its:ite)         ::                           &
         x
     real(kind=kind_phys)                                ::                           &
         xar
     integer                              ::                           &
         i,k

       do 200 i=its,itf
       maxx(i)=ks
       if(ierr(i).eq.0)then
      x(i)=array(i,ks)
!
       do 100 k=ks,ke(i)
         xar=array(i,k)
         if(xar.ge.x(i)) then
            x(i)=xar
            maxx(i)=k
         endif
 100  continue
      endif
 200  continue

   end subroutine cup_maximi

!====================================================================
   subroutine cup_output_ens_3d(xff_mid,xf_ens,ierr,dellat,dellaq,dellaqc,  &
              outtem,outq,outqc,                                            &
              zu,pre,pw,xmb,ktop,                                           &
              edt,pwd,name,ierr2,ierr3,p_cup,pr_ens,                        &
              maxens3,                                                      &
              sig,closure_n,xland1,xmbm_in,xmbs_in,                         &
              ichoice,imid,ipr,itf,ktf,                                     &
              its,ite, kts,kte,                                             &
              dicycle,xf_dicycle )

   implicit none
!
!  on input
!
   ! only local wrf dimensions are need as of now in this routine

     integer                                                           &
        ,intent (in   )                   ::                           &
        ichoice,imid,ipr,itf,ktf,                                      &
        its,ite, kts,kte
     integer, intent (in   )              ::                           &
        maxens3
  ! xf_ens = ensemble mass fluxes
  ! pr_ens = precipitation ensembles
  ! dellat = change of temperature per unit mass flux of cloud ensemble
  ! dellaq = change of q per unit mass flux of cloud ensemble
  ! dellaqc = change of qc per unit mass flux of cloud ensemble
  ! outtem = output temp tendency (per s)
  ! outq   = output q tendency (per s)
  ! outqc  = output qc tendency (per s)
  ! pre    = output precip
  ! xmb    = total base mass flux
  ! xfac1  = correction factor
  ! pw = pw -epsilon*pd (ensemble dependent)
  ! ierr error value, maybe modified in this routine
  !
     real(kind=kind_phys),    dimension (its:ite,1:maxens3)                            &
        ,intent (inout)                   ::                           &
       xf_ens,pr_ens
     real(kind=kind_phys),    dimension (its:ite,kts:kte)                              &
        ,intent (inout  )                 ::                           &
        outtem,outq,outqc
     real(kind=kind_phys),    dimension (its:ite,kts:kte)                              &
        ,intent (in  )                    ::                           &
        zu,pwd,p_cup
     real(kind=kind_phys),   dimension (its:ite)                                       &
         ,intent (in  )                   ::                           &
        sig,xmbm_in,xmbs_in,edt
     real(kind=kind_phys),   dimension (its:ite,2)                                     &
         ,intent (in  )                   ::                           &
        xff_mid
     real(kind=kind_phys),    dimension (its:ite)                                      &
        ,intent (inout  )                 ::                           &
        pre,xmb
     real(kind=kind_phys),    dimension (its:ite)                                      &
        ,intent (inout  )                 ::                           &
        closure_n
     real(kind=kind_phys),    dimension (its:ite,kts:kte,1)                            &
        ,intent (in   )                   ::                           &
       dellat,dellaqc,dellaq,pw
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        ktop,xland1
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr,ierr2,ierr3
     integer, intent(in) :: dicycle
     real(kind=kind_phys),    intent(in), dimension (its:ite) :: xf_dicycle
!
!  local variables in this routine
!

     integer                              ::                           &
        i,k,n
     real(kind=kind_phys)                                 ::                           &
        clos_wei,dtt,dp,dtq,dtqc,dtpw,dtpwd
     real(kind=kind_phys),    dimension (its:ite)         ::                           &
       pre2,xmb_ave,pwtot
!
      character *(*), intent (in)         ::                           &
       name

!
      do k=kts,kte
      do i=its,ite
        outtem (i,k)=0.
        outq   (i,k)=0.
        outqc  (i,k)=0.
      enddo
      enddo
      do i=its,itf
        pre(i)=0.
        xmb(i)=0.
      enddo
      do i=its,itf
        if(ierr(i).eq.0)then
        do n=1,maxens3
           if(pr_ens(i,n).le.0.)then
             xf_ens(i,n)=0.
           endif
        enddo
        endif
      enddo
!
!--- calculate ensemble average mass fluxes
!
       
!
!-- now do feedback
!
!!!!! deep convection !!!!!!!!!!
      if(imid.eq.0)then
      do i=its,itf
        if(ierr(i).eq.0)then
         k=0
         xmb_ave(i)=0.
         do n=1,maxens3
          k=k+1
          xmb_ave(i)=xmb_ave(i)+xf_ens(i,n)
           
         enddo
         !print *,'xf_ens',xf_ens
         xmb_ave(i)=xmb_ave(i)/float(k)
         !print *,'k,xmb_ave',k,xmb_ave
         !srf begin
         if(dicycle == 2 )then
            xmb_ave(i)=xmb_ave(i)-max(0.,xmbs_in(i))
            xmb_ave(i)=max(0.,xmb_ave(i))
         else if (dicycle == 1) then
!           xmb_ave(i)=min(xmb_ave(i),xmb_ave(i) - xf_dicycle(i))
            xmb_ave(i)=xmb_ave(i) - xf_dicycle(i)
            xmb_ave(i)=max(0.,xmb_ave(i))
         endif
         !print *,"2 xmb_ave,xf_dicycle",xmb_ave,xf_dicycle
! --- now use proper count of how many closures were actually
!       used in cup_forcing_ens (including screening of some
!       closures over water) to properly normalize xmb
           clos_wei=16./max(1.,closure_n(i))
         xmb_ave(i)=min(xmb_ave(i),100.)
         xmb(i)=clos_wei*sig(i)*xmb_ave(i)

           if(xmb(i) < 1.e-16)then
              ierr(i)=19
           endif
!           xfac1(i)=xmb(i)
!           xfac2(i)=xmb(i)

        endif
      enddo
!!!!! not so deep convection !!!!!!!!!!
      else  ! imid == 1
         do i=its,itf
         xmb_ave(i)=0.
         if(ierr(i).eq.0)then
! ! first get xmb_ves, depend on ichoicee
!
           if(ichoice.eq.1 .or. ichoice.eq.2)then
              xmb_ave(i)=sig(i)*xff_mid(i,ichoice)
           else if(ichoice.gt.2)then
              k=0
              do n=1,maxens3
                    k=k+1
                    xmb_ave(i)=xmb_ave(i)+xf_ens(i,n)
              enddo
              xmb_ave(i)=xmb_ave(i)/float(k)
           else if(ichoice == 0)then
              xmb_ave(i)=.5*sig(i)*(xff_mid(i,1)+xff_mid(i,2))
           endif   ! ichoice gt.2
! which dicycle method
           if(dicycle == 2 )then
              xmb(i)=max(0.,xmb_ave(i)-xmbs_in(i))
           else if (dicycle == 1) then
!             xmb(i)=min(xmb_ave(i),xmb_ave(i) - xf_dicycle(i))
              xmb(i)=xmb_ave(i) - xf_dicycle(i)
              xmb(i)=max(0.,xmb_ave(i))
           else if (dicycle == 0) then
              xmb(i)=max(0.,xmb_ave(i))
           endif   ! dicycle=1,2
         endif     ! ierr >0
         enddo     ! i
      endif        ! imid=1

       do i=its,itf
         if(ierr(i).eq.0)then
             dtpw=0.
             do k=kts,ktop(i)
               dtpw=dtpw+pw(i,k,1)
               outtem(i,k)= xmb(i)* dellat  (i,k,1)
               outq  (i,k)= xmb(i)* dellaq  (i,k,1)
               outqc (i,k)= xmb(i)* dellaqc(i,k,1)
            enddo
            PRE(I)=PRE(I)+XMB(I)*dtpw
          endif
       enddo
 return

      do i=its,itf
        pwtot(i)=0.
        pre2(i)=0.
        if(ierr(i).eq.0)then
            do k=kts,ktop(i)
              pwtot(i)=pwtot(i)+pw(i,k,1)
            enddo
            do k=kts,ktop(i)
            dp=100.*(p_cup(i,k)-p_cup(i,k+1))/g
            dtt =dellat  (i,k,1)
            dtq =dellaq  (i,k,1)
! necessary to drive downdraft
            dtpwd=-pwd(i,k)*edt(i)
! take from dellaqc first
            dtqc=dellaqc (i,k,1)*dp - dtpwd
! if this is negative, use dellaqc first, rest needs to come from rain
           if(dtqc < 0.)then
             dtpwd=dtpwd-dellaqc(i,k,1)*dp
             dtqc=0.
! if this is positive, can come from clw detrainment
           else
             dtqc=dtqc/dp
             dtpwd=0.
           endif
           outtem(i,k)= xmb(i)* dtt
           outq  (i,k)= xmb(i)* dtq
           outqc (i,k)= xmb(i)* dtqc
           xf_ens(i,:)=sig(i)*xf_ens(i,:)
! what is evaporated
           pre(i)=pre(i)-xmb(i)*dtpwd
           pre2(i)=pre2(i)+xmb(i)*(pw(i,k,1)+edt(i)*pwd(i,k))
!           write(15,124)k,dellaqc(i,k,1),dtqc,-pwd(i,k)*edt(i),dtpwd
          enddo
          pre(i)=-pre(i)+xmb(i)*pwtot(i)
        endif
124     format(1x,i3,4e13.4)
125     format(1x,2e13.4)
      enddo


   end subroutine cup_output_ens_3d
!-------------------------------------------------------
!>\ingroup cu_gf_deep_group
   subroutine cup_up_moisture(name,ierr,z_cup,qc,qrc,pw,pwav,     &
              p_cup,kbcon,ktop,dby,clw_all,xland1,                &
              q,gamma_cup,zu,qes_cup,k22,qe_cup,                  &
              zqexec,ccn,rho,c1d,t,                               &
              up_massentr,up_massdetr,psum,psumh,                 &
              itest,itf,ktf,                                      &
              its,ite, kts,kte                     )

   implicit none
  real(kind=kind_phys), parameter :: bdispm = 0.366       !<berry--size dispersion (martime)
  real(kind=kind_phys), parameter :: bdispc = 0.146       !<berry--size dispersion (continental)
!
!  on input
!

   ! only local wrf dimensions are need as of now in this routine

     integer                                                      &
        ,intent (in   )                   ::                      &
                                  itest,itf,ktf,                  &
                                  its,ite, kts,kte
  ! cd= detrainment function 
  ! q = environmental q on model levels
  ! qe_cup = environmental q on model cloud levels
  ! qes_cup = saturation q on model cloud levels
  ! dby = buoancy term
  ! cd= detrainment function 
  ! zu = normalized updraft mass flux
  ! gamma_cup = gamma on model cloud levels
  !
     real(kind=kind_phys),    dimension (its:ite,kts:kte)                         &
        ,intent (in   )                   ::                      &
        p_cup,rho,q,zu,gamma_cup,qe_cup,                          &
        up_massentr,up_massdetr,dby,qes_cup,z_cup
     real(kind=kind_phys),    dimension (its:ite)                                 &
        ,intent (in   )                   ::                      &
        zqexec
  ! entr= entrainment rate 
     integer, dimension (its:ite)                                 &
        ,intent (in   )                   ::                      &
        kbcon,ktop,k22,xland1
!
! input and output
!

   ! ierr error value, maybe modified in this routine

     integer, dimension (its:ite)                                  &
        ,intent (inout)                   ::                       &
        ierr
      character *(*), intent (in)         ::                       &
       name
   ! qc = cloud q (including liquid water) after entrainment
   ! qrch = saturation q in cloud
   ! qrc = liquid water content in cloud after rainout
   ! pw = condensate that will fall out at that level
   ! pwav = totan normalized integrated condensate (i1)
   ! c0 = conversion rate (cloud to rain)

     real(kind=kind_phys),    dimension (its:ite,kts:kte)                          &
        ,intent (out  )                   ::                       &
        qc,qrc,pw,clw_all
     real(kind=kind_phys),    dimension (its:ite,kts:kte) ::                       &
        qch,qrcb,pwh,clw_allh,c1d,t
     real(kind=kind_phys),    dimension (its:ite)         ::                       &
        pwavh
     real(kind=kind_phys),    dimension (its:ite)                                  &
        ,intent (out  )                   ::                       &
        pwav,psum,psumh
     real(kind=kind_phys),    dimension (its:ite)                                  &
        ,intent (in  )                    ::                       &
        ccn
!
!  local variables in this routine
!

     integer                              ::                       &
        iprop,iall,i,k
     integer :: start_level(its:ite)
     real(kind=kind_phys)                                 ::                       &
        prop_ave,qrcb_h,bdsp,dp,rhoc,qrch,qaver,clwdet,                   &
        c0,dz,berryc0,q1,berryc
     real(kind=kind_phys)                                 ::                       &
        denom, c0t
     real(kind=kind_phys),    dimension (kts:kte)         ::                       &
        prop_b
     real(kind=kind_phys),dimension(kts:kte):: dummy1
!
        prop_b(kts:kte)=0
        iall=0
        c0=.002
        clwdet=50.
        bdsp=bdispm
!
!--- no precip for small clouds
!
!        if(name.eq.'shallow')then
!            c0=0.002
!        endif
        do i=its,itf
          pwav(i)=0.
          pwavh(i)=0.
          psum(i)=0.
          psumh(i)=0.
        enddo
        do k=kts,ktf
        do i=its,itf
          pw(i,k)=0.
          pwh(i,k)=0.
          qc(i,k)=0.
          if(ierr(i).eq.0)qc(i,k)=qe_cup(i,k)
          if(ierr(i).eq.0)qch(i,k)=qe_cup(i,k)
          clw_all(i,k)=0.
          clw_allh(i,k)=0.
          qrc(i,k)=0.
          qrcb(i,k)=0.
        enddo
        enddo
      do i=its,itf
      if(ierr(i).eq.0)then
         start_level=k22(i)
         call get_cloud_bc(kte,qe_cup (i,1:kte),qaver,k22(i))
         qaver = qaver 
         k=start_level(i)
         qc (i,k)= qaver 
         qch (i,k)= qaver
         do k=1,start_level(i)-1
           qc (i,k)= qe_cup(i,k)
           qch (i,k)= qe_cup(i,k)
         enddo
!
!  initialize below originating air
!
      endif
      enddo

       do 100 i=its,itf
        c0=.004
         if(ierr(i).eq.0)then

! below lfc, but maybe above lcl
!
!            if(name == "deep" )then
            do k=k22(i)+1,kbcon(i)
              if(t(i,k) > 273.16) then
               c0t = c0
              else
               c0t = c0 * exp(0.07 * (t(i,k) - 273.16))
              endif
              qc(i,k)=   (qc(i,k-1)*zu(i,k-1)-.5*up_massdetr(i,k-1)* qc(i,k-1)+ &
                         up_massentr(i,k-1)*q(i,k-1))   /                       &
                         (zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))
!              qrch=qes_cup(i,k)
               qrch=qes_cup(i,k)+(1./xlv)*(gamma_cup(i,k)                       &
                 /(1.+gamma_cup(i,k)))*dby(i,k)
              if(k.lt.kbcon(i))qrch=qc(i,k)
              if(qc(i,k).gt.qrch)then
                dz=z_cup(i,k)-z_cup(i,k-1)
                qrc(i,k)=(qc(i,k)-qrch)/(1.+c0t*dz)   
                pw(i,k)=c0t*dz*qrc(i,k)*zu(i,k) 
                qc(i,k)=qrch+qrc(i,k)
                clw_all(i,k)=qrc(i,k)
              endif
            enddo
 !           endif
!
!now do the rest
!
            do k=kbcon(i)+1,ktop(i)
               c0=.004
               if(t(i,k).lt.270.)c0=.002
               if(t(i,k) > 273.16) then
                  c0t = c0
               else
                  c0t = c0 * exp(0.07 * (t(i,k) - 273.16))
               endif
               denom=zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1)
               if(denom.lt.1.e-16)then
                     ierr(i)=51
                exit
               endif

   
               rhoc=.5*(rho(i,k)+rho(i,k-1))
               dz=z_cup(i,k)-z_cup(i,k-1)
               dp=p_cup(i,k)-p_cup(i,k-1)
!
!--- saturation  in cloud, this is what is allowed to be in it
!
               qrch=qes_cup(i,k)+(1./xlv)*(gamma_cup(i,k)                       &
                 /(1.+gamma_cup(i,k)))*dby(i,k)
!
!------    1. steady state plume equation, for what could
!------       be in cloud without condensation
!
!
               qc(i,k)=   (qc(i,k-1)*zu(i,k-1)-.5*up_massdetr(i,k-1)* qc(i,k-1)+ &
                         up_massentr(i,k-1)*q(i,k-1))   /                        &
                         (zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))
               qch(i,k)= (qch(i,k-1)*zu(i,k-1)-.5*up_massdetr(i,k-1)*qch(i,k-1)+ &
                         up_massentr(i,k-1)*q(i,k-1))   /                        &
                         (zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))

               if(qc(i,k).le.qrch)then
                 qc(i,k)=qrch
               endif
               if(qch(i,k).le.qrch)then
                 qch(i,k)=qrch
               endif
!
!------- total condensed water before rainout
!
               clw_all(i,k)=max(0.,qc(i,k)-qrch)
               qrc(i,k)=max(0.,(qc(i,k)-qrch)) ! /(1.+c0*dz*zu(i,k))
               clw_allh(i,k)=max(0.,qch(i,k)-qrch)
               qrcb(i,k)=max(0.,(qch(i,k)-qrch)) ! /(1.+c0*dz*zu(i,k))
               if(autoconv.eq.2) then


! 
! normalized berry
!
! first calculate for average conditions, used in cup_dd_edt!
! this will also determine proportionality constant prop_b, which, if applied,
! would give the same results as c0 under these conditions
!
                 q1=1.e3*rhoc*qrcb(i,k)  ! g/m^3 ! g[h2o]/cm^3
                 berryc0=q1*q1/(60.0*(5.0 + 0.0366*ccnclean/                           &
                    ( q1 * bdsp)  ) ) !/(
                 qrcb_h=((qch(i,k)-qrch)*zu(i,k)-qrcb(i,k-1)*(.5*up_massdetr(i,k-1)))/ &
                   (zu(i,k)+.5*up_massdetr(i,k-1)+c0t*dz*zu(i,k))
                 prop_b(k)=c0t*qrcb_h*zu(i,k)/(1.e-3*berryc0)
                 pwh(i,k)=zu(i,k)*1.e-3*berryc0*dz*prop_b(k) ! 2.
                 berryc=qrcb(i,k)
                 qrcb(i,k)=((qch(i,k)-qrch)*zu(i,k)-pwh(i,k)-qrcb(i,k-1)*(.5*up_massdetr(i,k-1)))/ &
                       (zu(i,k)+.5*up_massdetr(i,k-1))
                 if(qrcb(i,k).lt.0.)then
                   berryc0=(qrcb(i,k-1)*(.5*up_massdetr(i,k-1))-(qch(i,k)-qrch)*zu(i,k))/zu(i,k)*1.e-3*dz*prop_b(k)
                   pwh(i,k)=zu(i,k)*1.e-3*berryc0*dz*prop_b(k)
                   qrcb(i,k)=0.
                 endif
                 qch(i,k)=qrcb(i,k)+qrch
                 pwavh(i)=pwavh(i)+pwh(i,k)
                 psumh(i)=psumh(i)+clw_allh(i,k)*zu(i,k) *dz
        !
! then the real berry
!
                 q1=1.e3*rhoc*qrc(i,k)  ! g/m^3 ! g[h2o]/cm^3
                 berryc0=q1*q1/(60.0*(5.0 + 0.0366*ccn(i)/                                             &
                    ( q1 * bdsp)  ) ) !/(
                 berryc0=1.e-3*berryc0*dz*prop_b(k) ! 2.
                 berryc=qrc(i,k)
                 qrc(i,k)=((qc(i,k)-qrch)*zu(i,k)-zu(i,k)*berryc0-qrc(i,k-1)*(.5*up_massdetr(i,k-1)))/ &
                       (zu(i,k)+.5*up_massdetr(i,k-1))
                 if(qrc(i,k).lt.0.)then
                    berryc0=((qc(i,k)-qrch)*zu(i,k)-qrc(i,k-1)*(.5*up_massdetr(i,k-1)))/zu(i,k)
                    qrc(i,k)=0.
                 endif
                 pw(i,k)=berryc0*zu(i,k)
                 qc(i,k)=qrc(i,k)+qrch
!
!  if not running with berry at all, do the following
!
               else       !c0=.002
                 if(iall.eq.1)then
                   qrc(i,k)=0.
                   pw(i,k)=(qc(i,k)-qrch)*zu(i,k)
                   if(pw(i,k).lt.0.)pw(i,k)=0.
                 else
! create clw detrainment profile that depends on mass detrainment and 
! in-cloud clw/ice
!
                   c1d(i,k)=clwdet*up_massdetr(i,k-1)*qrc(i,k-1)
                   qrc(i,k)=(qc(i,k)-qrch)/(1.+(c1d(i,k)+c0t)*dz)
                   if(qrc(i,k).lt.0.)then  ! hli new test 02/12/19
                      qrc(i,k)=0.
                   endif
                   pw(i,k)=c0t*dz*qrc(i,k)*zu(i,k) 
!-----srf-08aug2017-----begin
! pw(i,k)=(c1d(i,k)+c0)*dz*max(0.,qrc(i,k) -qrc_crit)! units kg[rain]/kg[air] 
!-----srf-08aug2017-----end
                   if(qrc(i,k).lt.0)then
                     qrc(i,k)=0.
                     pw(i,k)=0.
                   endif
                 endif
                 qc(i,k)=qrc(i,k)+qrch
               endif !autoconv
               pwav(i)=pwav(i)+pw(i,k)
               psum(i)=psum(i)+clw_all(i,k)*zu(i,k) *dz
            enddo ! k=kbcon,ktop
! do not include liquid/ice in qc
       do k=k22(i)+1,ktop(i)
           qc(i,k)=qc(i,k)-qrc(i,k)
       enddo
      endif ! ierr
!
!--- integrated normalized ondensate
!
 100     continue
       prop_ave=0.
       iprop=0
       do k=kts,kte
        prop_ave=prop_ave+prop_b(k)
        if(prop_b(k).gt.0)iprop=iprop+1
       enddo
       iprop=max(iprop,1)

 end subroutine cup_up_moisture

!--------------------------------------------------------------------
  subroutine cup_up_aa1bl(aa0,t,tn,q,qo,dtime,  &
              z_cup,zu,dby,gamma_cup,t_cup,         &
              kbcon,ktop,ierr,                  &
              itf,ktf,                          &
              its,ite, kts,kte         )

   implicit none
!
!  on input
!

   ! only local wrf dimensions are need as of now in this routine

     integer                                                           &
        ,intent (in   )                   ::                           &
        itf,ktf,                                                       &
        its,ite, kts,kte
  ! aa0 cloud work function
  ! gamma_cup = gamma on model cloud levels
  ! t_cup = temperature (kelvin) on model cloud levels
  ! dby = buoancy term
  ! zu= normalized updraft mass flux
  ! z = heights of model levels 
  ! ierr error value, maybe modified in this routine
  !
     real(kind=kind_phys),    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        z_cup,zu,gamma_cup,t_cup,dby,t,tn,q,qo
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        kbcon,ktop
     real(kind=kind_phys), intent(in) :: dtime
!
! input and output
!
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr
     real(kind=kind_phys),    dimension (its:ite)                                      &
        ,intent (out  )                   ::                           &
        aa0
!
!  local variables in this routine
!
     integer                              ::                           &
        i,k
     real(kind=kind_phys)                                 ::                           &
        dz,da
!
        do i=its,itf
         aa0(i)=0.
        enddo
        do i=its,itf
          do k=kts,kbcon(i)
            if(ierr(i).ne.0 ) cycle
!           if(k.gt.kbcon(i)) cycle

            dz = (z_cup (i,k+1)-z_cup (i,k))*g
            da = dz*(tn(i,k)*(1.+0.608*qo(i,k))-t(i,k)*(1.+0.608*q(i,k)))/dtime

            aa0(i)=aa0(i)+da
          enddo
        enddo
             

 end subroutine cup_up_aa1bl
!---------------------------------------------------------------------- 
 function deriv3(xx, xi, yi, ni, m)
    !============================================================================*/
    ! evaluate first- or second-order derivatives 
    ! using three-point lagrange interpolation 
    ! written by: alex godunov (october 2009)
    ! input ...
    ! xx    - the abscissa at which the interpolation is to be evaluated
    ! xi()  - the arrays of data abscissas
    ! yi()  - the arrays of data ordinates
    ! ni - size of the arrays xi() and yi()
    ! m  - order of a derivative (1 or 2)
    ! output ...
    ! deriv3  - interpolated value
    !============================================================================*/
    
    implicit none
    integer, parameter :: n=3
    integer ni, m,i, j, k, ix
    real(kind=kind_phys):: deriv3, xx
    real(kind=kind_phys):: xi(ni), yi(ni), x(n), f(n)

    ! exit if too high-order derivative was needed,
    if (m > 2) then
      deriv3 = 0.0
      return
    end if

    ! if x is ouside the xi(1)-xi(ni) interval set deriv3=0.0
    if (xx < xi(1) .or. xx > xi(ni)) then
      deriv3 = 0.0
      stop "problems with finding the 2nd derivative"
    end if

    ! a binary (bisectional) search to find i so that xi(i-1) < x < xi(i)
    i = 1
    j = ni
    do while (j > i+1)
      k = (i+j)/2
      if (xx < xi(k)) then
        j = k
      else
        i = k
      end if
    end do

    ! shift i that will correspond to n-th order of interpolation
    ! the search point will be in the middle in x_i, x_i+1, x_i+2 ...
      i = i + 1 - n/2

    ! check boundaries: if i is ouside of the range [1, ... n] -> shift i
    if (i < 1) i=1
    if (i + n > ni) i=ni-n+1

    !  old output to test i
    !  write(*,100) xx, i
    !  100 format (f10.5, i5)

    ! just wanted to use index i
    ix = i
    ! initialization of f(n) and x(n)
    do i=1,n
      f(i) = yi(ix+i-1)
      x(i) = xi(ix+i-1)
    end do

    ! calculate the first-order derivative using lagrange interpolation
    if (m == 1) then
        deriv3 =          (2.0*xx - (x(2)+x(3)))*f(1)/((x(1)-x(2))*(x(1)-x(3)))
        deriv3 = deriv3 + (2.0*xx - (x(1)+x(3)))*f(2)/((x(2)-x(1))*(x(2)-x(3)))
        deriv3 = deriv3 + (2.0*xx - (x(1)+x(2)))*f(3)/((x(3)-x(1))*(x(3)-x(2)))
    ! calculate the second-order derivative using lagrange interpolation
      else
        deriv3 =          2.0*f(1)/((x(1)-x(2))*(x(1)-x(3)))
        deriv3 = deriv3 + 2.0*f(2)/((x(2)-x(1))*(x(2)-x(3)))
        deriv3 = deriv3 + 2.0*f(3)/((x(3)-x(1))*(x(3)-x(2)))
    end if
 end function deriv3
!=============================================================================================
!------------------------------------------------------------------------------------
!>\ingroup cu_gf_deep_group
   subroutine get_partition_liq_ice(ierr,tn,po_cup, p_liq_ice,melting_layer           & 
                                   ,itf,ktf,its,ite, kts,kte, cumulus          )
     implicit none
     character *(*), intent (in)                          :: cumulus
     integer  ,intent (in   )	                          :: itf,ktf, its,ite, kts,kte
     real(kind=kind_phys),     intent (in   ), dimension(its:ite,kts:kte) :: tn,po_cup
     real(kind=kind_phys),     intent (inout), dimension(its:ite,kts:kte) :: p_liq_ice,melting_layer
     integer  , intent (in  ), dimension(its:ite) :: ierr
     integer :: i,k
     real(kind=kind_phys)    :: dp     
     real(kind=kind_phys), dimension(its:ite) :: norm     
     real(kind=kind_phys), parameter ::  t1=276.16    
     
     ! hli initialize at the very beginning
        p_liq_ice     (:,:) = 1.
        melting_layer(:,:) = 0.
     !-- get function of t for partition of total condensate into liq and ice phases.     
     if(melt_glac .and. cumulus == 'deep') then
          do i=its,itf
           if(ierr(i).eq.0)then
           do k=kts,ktf
             
             if    (tn(i,k) <= t_ice) then

                p_liq_ice(i,k) = 0.               
             elseif(  tn(i,k) > t_ice .and. tn(i,k) < t_0) then
             
                p_liq_ice(i,k) =  ((tn(i,k)-t_ice)/(t_0-t_ice))**2        
             else             
                p_liq_ice(i,k) = 1.
             endif
             
             !melting_layer(i,k) = p_liq_ice(i,k) * (1.-p_liq_ice(i,k))
          enddo
          endif
        enddo
        !go to 655
        !-- define the melting layer (the layer will be between t_0+1 < temp < t_1
          do i=its,itf
           if(ierr(i).eq.0)then
            do k=kts,ktf
             if    (tn(i,k) <= t_0+1) then
                melting_layer(i,k) = 0.
                              
             elseif(  tn(i,k) > t_0+1 .and. tn(i,k) < t1) then             
                melting_layer(i,k) =  ((tn(i,k)-t_0+1)/(t1-t_0+1))**2        
                
             else             
                melting_layer(i,k) = 1.
             endif
             melting_layer(i,k) = melting_layer(i,k)*(1-melting_layer(i,k))
          enddo
          endif
        enddo
        655 continue
        !-normalize vertical integral of melting_layer to 1
        norm(:)=0.
        !do k=kts,ktf
          do i=its,itf
           if(ierr(i).eq.0)then
           do k=kts,ktf-1
             dp = 100.*(po_cup(i,k)-po_cup(i,k+1)) 
             norm(i) = norm(i) + melting_layer(i,k)*dp/g
          enddo
          endif
        enddo
        do i=its,itf
           if(ierr(i).eq.0)then
         !print*,"i1=",i,maxval(melting_layer(i,:)),minval(melting_layer(i,:)),norm(i)
         melting_layer(i,:)=melting_layer(i,:)/(norm(i)+1.e-6)*(100*(po_cup(i,kts)-po_cup(i,ktf))/g)
          endif
         !print*,"i2=",i,maxval(melting_layer(i,:)),minval(melting_layer(i,:)),norm(i)
        enddo
        !--check
!       norm(:)=0.
!        do k=kts,ktf-1
!          do i=its,itf
!             dp = 100.*(po_cup(i,k)-po_cup(i,k+1)) 
!             norm(i) = norm(i) + melting_layer(i,k)*dp/g/(100*(po_cup(i,kts)-po_cup(i,ktf))/g)
!             !print*,"n=",i,k,norm(i)
!          enddo
!        enddo
        
     else
        p_liq_ice     (:,:) = 1.
        melting_layer(:,:) = 0.
     endif
   end  subroutine get_partition_liq_ice

!------------------------------------------------------------------------------------
!>\ingroup cu_gf_deep_group
   subroutine get_melting_profile(ierr,tn_cup,po_cup, p_liq_ice,melting_layer,qrco    &
                                 ,pwo,edto,pwdo,melting                                &    
                                 ,itf,ktf,its,ite, kts,kte, cumulus              )
     implicit none
     character *(*), intent (in)                          :: cumulus
     integer  ,intent (in   )                                  :: itf,ktf, its,ite, kts,kte
     integer  ,intent (in   ), dimension(its:ite)         :: ierr
     real(kind=kind_phys)     ,intent (in   ), dimension(its:ite)         :: edto
     real(kind=kind_phys)     ,intent (in   ), dimension(its:ite,kts:kte) :: tn_cup,po_cup,qrco,pwo &
                                                            ,pwdo,p_liq_ice,melting_layer
     real(kind=kind_phys)     ,intent (inout), dimension(its:ite,kts:kte) :: melting
     integer :: i,k
     real(kind=kind_phys)    :: dp     
     real(kind=kind_phys), dimension(its:ite) :: norm,total_pwo_solid_phase
     real(kind=kind_phys), dimension(its:ite,kts:kte) :: pwo_solid_phase,pwo_eff
     
     if(melt_glac .and. cumulus == 'deep') then

        !-- set melting mixing ratio to zero for columns that do not have deep convection
        do i=its,itf
            if(ierr(i) > 0) melting(i,:) = 0.
        enddo
        
       !-- now, get it for columns where deep convection is activated
       total_pwo_solid_phase(:)=0.

       !do k=kts,ktf
       do k=kts,ktf-1
          do i=its,itf
           if(ierr(i) /= 0) cycle
             dp = 100.*(po_cup(i,k)-po_cup(i,k+1)) 
             
             !-- effective precip (after evaporation by downdraft)
             pwo_eff(i,k) = 0.5*(pwo(i,k)+pwo(i,k+1) + edto(i)*(pwdo(i,k)+pwdo(i,k+1)))
             
             !-- precipitation at solid phase(ice/snow)
             pwo_solid_phase(i,k) = (1.-p_liq_ice(i,k))*pwo_eff(i,k)
             
             !-- integrated precip at solid phase(ice/snow)
             total_pwo_solid_phase(i) = total_pwo_solid_phase(i)+pwo_solid_phase(i,k)*dp/g
          enddo
        enddo
        
        do k=kts,ktf
          do i=its,itf
           if(ierr(i) /= 0) cycle
             !-- melting profile (kg/kg)
             melting(i,k) = melting_layer(i,k)*(total_pwo_solid_phase(i)/(100*(po_cup(i,kts)-po_cup(i,ktf))/g))        
             !print*,"mel=",k,melting(i,k),pwo_solid_phase(i,k),po_cup(i,k)
          enddo
        enddo

!-- check conservation of total solid phase precip
!       norm(:)=0.
!        do k=kts,ktf-1
!          do i=its,itf
!             dp = 100.*(po_cup(i,k)-po_cup(i,k+1)) 
!             norm(i) = norm(i) + melting(i,k)*dp/g
!          enddo
!        enddo
!        
!       do i=its,itf
!         print*,"cons=",i,norm(i),total_pwo_solid_phase(i)
!        enddo
!--        
        
     else
        !-- no melting allowed in this run
        melting     (:,:) = 0.
     endif
   end  subroutine get_melting_profile
!---meltglac-------------------------------------------------
!-----srf-08aug2017-----begin
!>\ingroup cu_gf_deep_group
 subroutine get_cloud_top(name,ktop,ierr,p_cup,entr_rate_2d,hkbo,heo,heso_cup,z_cup, &
                         kstabi,k22,kbcon,its,ite,itf,kts,kte,ktf,zuo,kpbl,klcl,hcot)
     implicit none
     integer, intent(in) :: its,ite,itf,kts,kte,ktf
     real(kind=kind_phys), dimension (its:ite,kts:kte),intent (inout) :: entr_rate_2d,zuo
     real(kind=kind_phys), dimension (its:ite,kts:kte),intent (in) ::p_cup, heo,heso_cup,z_cup
     real(kind=kind_phys), dimension (its:ite),intent (in) :: hkbo
     integer, dimension (its:ite),intent (in) :: kstabi,k22,kbcon,kpbl,klcl
     integer, dimension (its:ite),intent (inout) :: ierr,ktop
     real(kind=kind_phys), dimension (its:ite,kts:kte) :: hcot
     character *(*), intent (in) ::    name
     real(kind=kind_phys) :: dz,dh, dbythresh
     real(kind=kind_phys) :: dby(kts:kte)
     integer :: i,k,ipr,kdefi,kstart,kbegzu,kfinalzu
     integer, dimension (its:ite) :: start_level
     integer,parameter :: find_ktop_option = 1 !0=original, 1=new
     
     dbythresh=0.8 !0.95  ! the range of this parameter is 0-1, higher => lower
                    ! overshoting (cheque aa0 calculation)
                    ! rainfall is too sensible this parameter
                    ! for now, keep =1.
     if(name == 'shallow'.or. name == 'mid')then 
         dbythresh=1.0
     endif
     !         print*,"================================cumulus=",name; call flush(6)
     do i=its,itf
       kfinalzu=ktf-2
       ktop(i)=kfinalzu
       if(ierr(i).eq.0)then
       dby (kts:kte)=0.0

       start_level(i)=kbcon(i)
       !-- hcot below kbcon
       hcot(i,kts:start_level(i))=hkbo(i)

       dz=z_cup(i,start_level(i))-z_cup(i,start_level(i)-1)
       dby(start_level(i))=(hcot(i,start_level(i))-heso_cup(i,start_level(i)))*dz
       
       !print*,'hco1=',start_level(i),kbcon(i),hcot(i,start_level(i))/heso_cup(i,start_level(i))
       
       do k=start_level(i)+1,ktf-2
           dz=z_cup(i,k)-z_cup(i,k-1)

           hcot(i,k)=( (1.-0.5*entr_rate_2d(i,k-1)*dz)*hcot(i,k-1)    &
                              +entr_rate_2d(i,k-1)*dz *heo (i,k-1) )/ &
                       (1.+0.5*entr_rate_2d(i,k-1)*dz)
           dby(k)=dby(k-1)+(hcot(i,k)-heso_cup(i,k))*dz
           !print*,'hco2=',k,hcot(i,k)/heso_cup(i,k),dby(k),entr_rate_2d(i,k-1)

       enddo
       if(find_ktop_option==0) then 
        do k=maxloc(dby(:),1),ktf-2
          !~ print*,'hco30=',k,dby(k),dbythresh*maxval(dby)
      
          if(dby(k).lt.dbythresh*maxval(dby))then
              kfinalzu = k - 1
              ktop(i)  = kfinalzu
              !print*,'hco4=',k,kfinalzu,ktop(i),kbcon(i)+1;call flush(6)
              go to 412
          endif
        enddo
        412    continue
       else
         do k=start_level(i)+1,ktf-2
          !~ print*,'hco31=',k,dby(k),dbythresh*maxval(dby)
      
          if(hcot(i,k) < heso_cup(i,k) )then
              kfinalzu = k - 1
              ktop(i)  = kfinalzu
              !print*,'hco40=',k,kfinalzu,ktop(i),kbcon(i)+1;call flush(6)
              exit
          endif
        enddo
       endif
       if(kfinalzu.le.kbcon(i)+1) ierr(i)=41
       !~ print*,'hco5=',k,kfinalzu,ktop(i),kbcon(i)+1,ierr(i);call flush(6)
      !
      endif
     enddo
  end subroutine get_cloud_top
!------------------------------------------------------------------------------------
!=================================================================================================================
 end module module_cu_gf_deep
!=================================================================================================================


