!=================================================================================================================
 module module_sf_mynn
!=================================================================================================================
 use mpas_kind_types,only: StrKIND

 use sf_mynn,only     : sf_mynn_run
 use sf_mynn_mpas,only: sf_mynn_mpas_run

 implicit none
 private
 public:: sfclay_mynn


 contains


!=================================================================================================================
 subroutine sfclay_mynn(                                         &
                   u3d,v3d,t3d,qv3d,p3d,dz8w,                    &
                   cp,g,rovcp,r,xlv,psfcpa,chs,chs2,cqs2,cpm,    &
                   znt,ust,pblh,mavail,zol,mol,regime,psim,psih, &
                   xland,hfx,qfx,lh,tsk,flhc,flqc,qgh,qsfc,rmol, &
                   u10,v10,th2,t2,q2,snowh,                      &
                   gz1oz0,wspd,br,isfflx,dx,                     &
                   svp1,svp2,svp3,svpt0,ep1,ep2,                 &
                   karman,itimestep,ch,th3d,pi3d,qc3d,rho3d,qcg, &
                   spp_pbl,pattern_spp_pbl,                      &
                   ids,ide,jds,jde,kds,kde,                      &
                   ims,ime,jms,jme,kms,kme,                      &
                   its,ite,jts,jte,kts,kte,                      &
                   ustm,ck,cka,cd,cda,isftcflx,iz0tlnd           )
!-------------------------------------------------------------------
!-- u3d         3d u-velocity interpolated to theta points (m/s)
!-- v3d         3d v-velocity interpolated to theta points (m/s)
!-- t3d         3d temperature (k)
!-- qv3d        3d water vapor mixing ratio (kg/kg)
!-- p3d         3d pressure (pa)
!-- rho3d       3d density (kg/m3) 
!-- dz8w        3d dz between full levels (m)
!-- cp          heat capacity at constant pressure for dry air (j/kg/k)
!-- g           acceleration due to gravity (m/s^2)
!-- rovcp       r/cp
!-- r           gas constant for dry air (j/kg/k)
!-- xlv         latent heat of vaporization for water (j/kg)
!-- psfcpa      surface pressure (pa)
!-- znt         roughness length (m)
!-- ust         u* in similarity theory (m/s)
!-- ustm        u* in similarity theory (m/s) w* added to wspd. this is
!               used to couple with tke scheme but not in mynn.
!               (as of now, ustm = ust in this version)
!-- pblh        pbl height from previous time (m)
!-- mavail      surface moisture availability (between 0 and 1)
!-- zol         z/l height over monin-obukhov length
!-- mol         t* (similarity theory) (k)
!-- rmol        reciprocal of m-o length (/m)
!-- regime      flag indicating pbl regime (stable, unstable, etc.)
!-- psim        similarity stability function for momentum
!-- psih        similarity stability function for heat
!-- xland       land mask (1 for land, 2 for water)
!-- hfx         upward heat flux at the surface (w/m^2)
!-- qfx         upward moisture flux at the surface (kg/m^2/s)
!-- lh          net upward latent heat flux at surface (w/m^2)
!-- tsk         surface temperature (k)
!-- flhc        exchange coefficient for heat (w/m^2/k)
!-- flqc        exchange coefficient for moisture (kg/m^2/s)
!-- chs         heat/moisture exchange coefficient for lsm (m/s)
!-- qgh         lowest-level saturated mixing ratio
!-- qsfc        qv (specific humidity) at the surface
!-- qsfcmr      qv (mixing ratio) at the surface
!-- u10         diagnostic 10m u wind
!-- v10         diagnostic 10m v wind
!-- th2         diagnostic 2m theta (k)
!-- t2          diagnostic 2m temperature (k)
!-- q2          diagnostic 2m mixing ratio (kg/kg)
!-- snowh       snow height (m)
!-- gz1oz0      log((z1+znt)/znt) where znt is roughness length 
!-- wspd        wind speed at lowest model level (m/s)
!-- br          bulk richardson number in surface layer
!-- isfflx      isfflx=1 for surface heat and moisture fluxes
!-- dx          horizontal grid size (m)
!-- svp1        constant for saturation vapor pressure (=0.6112 kpa)
!-- svp2        constant for saturation vapor pressure (=17.67 dimensionless)
!-- svp3        constant for saturation vapor pressure (=29.65 k)
!-- svpt0       constant for saturation vapor pressure (=273.15 k)
!-- ep1         constant for virtual temperature (rv/rd - 1) (dimensionless)
!-- ep2         constant for spec. hum. calc (rd/rv = 0.622) (dimensionless)
!-- ep3         constant for spec. hum. calc (1 - rd/rv = 0.378 ) (dimensionless)
!-- karman      von karman constant
!-- ck          enthalpy exchange coeff at 10 meters
!-- cd          momentum exchange coeff at 10 meters
!-- cka         enthalpy exchange coeff at the lowest model level
!-- cda         momentum exchange coeff at the lowest model level
!-- isftcflx    =0: z0, zt, and zq from coare3.0/3.5 (fairall et al 2003/edson et al 2013)
!   (water      =1: z0 from davis et al (2008), zt & zq from coare3.0/3.5
!    only)      =2: z0 from davis et al (2008), zt & zq from garratt (1992)
!               =3: z0 from taylor and yelland (2004), zt and zq from coare 3.0/3.5
!-- iz0tlnd     =0: zilitinkevich (1995) with czil=0.085, 
!   (land       =1: czil_new (modified according to chen & zhang 2008)
!    only)      =2: modified yang et al (2002, 2008) - generalized for all landuse
!               =3: constant zt = z0/7.4 (garratt 1992)
!
!-- ids         start index for i in domain
!-- ide         end index for i in domain
!-- jds         start index for j in domain
!-- jde         end index for j in domain
!-- kds         start index for k in domain
!-- kde         end index for k in domain
!-- ims         start index for i in memory
!-- ime         end index for i in memory
!-- jms         start index for j in memory
!-- jme         end index for j in memory
!-- kms         start index for k in memory
!-- kme         end index for k in memory
!-- its         start index for i in tile
!-- ite         end index for i in tile
!-- jts         start index for j in tile
!-- jte         end index for j in tile
!-- kts         start index for k in tile
!-- kte         end index for k in tile
!=================================================================
! scalars
!===================================
      integer,  intent(in)   ::        ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte
      integer,  intent(in)   ::        itimestep
      real,     intent(in)   ::        svp1,svp2,svp3,svpt0
      real,     intent(in)   ::        ep1,ep2,karman
      real,     intent(in)   ::        cp,g,rovcp,r,xlv
!namelist options:
      integer,  intent(in)   ::        isfflx
      integer,  optional,  intent(in)   ::     isftcflx, iz0tlnd
      integer,  optional,  intent(in)   ::     spp_pbl

!===================================
! 3d variables
!===================================
      real,     dimension( ims:ime, kms:kme, jms:jme )           , &
                intent(in   )   ::                           dz8w, &
                                                             qv3d, &
                                                              p3d, &
                                                              t3d, &
                                                             qc3d, &
                                                          u3d,v3d, &
                                                  rho3d,th3d,pi3d

      real, dimension( ims:ime, kms:kme, jms:jme ), optional,      &
                intent(in) ::                      pattern_spp_pbl
!===================================
! 2d variables
!===================================
      real,     dimension( ims:ime, jms:jme )                    , &
                intent(in   )               ::             mavail, &
                                                             pblh, &
                                                            xland, &
                                                              tsk, &
                                                              qcg, &
                                                           psfcpa, &
                                                            snowh, &
                                                               dx

      real,     dimension( ims:ime, jms:jme )                    , &
                intent(out  )               ::            u10,v10, &
                                                        th2,t2,q2

      real, optional, dimension( ims:ime, jms:jme )              , &
                intent(out)     ::              ck,cka,cd,cda,ustm
!
      real,     dimension( ims:ime, jms:jme )                    , &
                intent(inout)               ::             regime, &
                                                              hfx, &
                                                              qfx, &
                                                               lh, &
                                                         mol,rmol, &
                                                        qsfc, qgh, &
                                                              znt, &
                                                              zol, &
                                                              ust, &
                                                              cpm, &
                                                             chs2, &
                                                             cqs2, &
                                                              chs, &
                                                               ch, &
                                                        flhc,flqc, &
                                                   gz1oz0,wspd,br, &
                                                        psim,psih

!additional output
      real,     dimension( ims:ime, jms:jme )    ::   wstar,qstar
!===================================
! 1d local arrays
!===================================
      real,     dimension( its:ite ) ::                       u1d, &
                                                              v1d, &
                                                        u1d2,v1d2, & !level2 winds
                                                             qv1d, &
                                                              p1d, &
                                                         t1d,qc1d, &
                                                            rho1d, &
                                                           dz8w1d, & !level 1 height
                                                           dz2w1d    !level 2 height


      real,     dimension( its:ite ) ::                  rstoch1d

      integer ::  i,j,k,itf,jtf,ktf

!ccpp-physics:
 character(len=StrKIND):: errmsg
 logical:: f_spp
 integer:: errflg

 real,dimension(its:ite):: mavail_hv,pblh_hv,xland_hv,tsk_hv,psfcpa_hv, &
                           qcg_hv,snowh_hv,dx_hv
 real,dimension(its:ite):: regime_hv,hfx_hv,qfx_hv,lh_hv,mol_hv,rmol_hv,qgh_hv,  &
                           qsfc_hv,znt_hv,zol_hv,ust_hv,cpm_hv,chs2_hv,cqs2_hv,  &
                           chs_hv,ch_hv,flhc_hv,flqc_hv,gz1oz0_hv,wspd_hv,br_hv, &
                           psim_hv,psih_hv
 real,dimension(its:ite):: u10_hv,v10_hv,th2_hv,t2_hv,q2_hv,wstar_hv,qstar_hv
 real,dimension(its:ite):: cd_hv,cda_hv,ck_hv,cka_hv,ustm_hv
!ccpp-physics end.

!-----------------------------------------------------------

      itf=min0(ite,ide-1)
      jtf=min0(jte,jde-1)
      ktf=min0(kte,kde-1)

      f_spp = .false.
      if(spp_pbl==1 .and. present(pattern_spp_pbl)) f_spp = .true.

      errmsg = ' '
      errflg = 0

      do j=jts,jte

        !ccpp-physics: initialization of arrays ust,mol,qsfc,and qstar that are initialized if
        !itimestep equals 1:
        do i = its,ite
           ust_hv(i)   = ust(i,j)
           mol_hv(i)   = mol(i,j)
           qsfc_hv(i)  = qsfc(i,j)
           qstar_hv(i) = qstar(i,j)
        enddo

        call sf_mynn_mpas_run(its,ite,kte,itimestep,dz8w,u3d,v3d,p3d,t3d,rho3d,qv3d,qc3d,f_spp, &
                   pattern_spp_pbl,ust_hv,mol_hv,qsfc_hv,qstar_hv,dz8w1d,u1d,v1d,p1d,t1d,rho1d, &
                   qv1d,qc1d,rstoch1d,dz2w1d,u1d2,v1d2,errmsg,errflg)

        !ccpp-physics:
        !input arguments:
        do i = its,ite
           mavail_hv(i) = mavail(i,j)
           pblh_hv(i)   = pblh(i,j)
           xland_hv(i)  = xland(i,j)
           tsk_hv(i)    = tsk(i,j)
           psfcpa_hv(i) = psfcpa(i,j)
           qcg_hv(i)    = qcg(i,j)
           snowh_hv(i)  = snowh(i,j)
           dx_hv(i)     = dx(i,j)
        enddo

        !inout arguments:
        do i = its,ite
           regime_hv(i) = regime(i,j)
           hfx_hv(i)    = hfx(i,j)
           qfx_hv(i)    = qfx(i,j)
           lh_hv(i)     = lh(i,j)
           rmol_hv(i)   = rmol(i,j)
           qgh_hv(i)    = qgh(i,j)
           znt_hv(i)    = znt(i,j)
           zol_hv(i)    = zol(i,j)
           cpm_hv(i)    = cpm(i,j)
           chs2_hv(i)   = chs2(i,j)
           cqs2_hv(i)   = cqs2(i,j)
           chs_hv(i)    = chs(i,j)
           ch_hv(i)     = ch(i,j)
           flhc_hv(i)   = flhc(i,j)
           flqc_hv(i)   = flqc(i,j)
           gz1oz0_hv(i) = gz1oz0(i,j)
           wspd_hv(i)   = wspd(i,j)
           br_hv(i)     = br(i,j)
           psim_hv(i)   = psim(i,j)
           psih_hv(i)   = psih(i,j)
        enddo

        !output arguments:
        do i = its,ite
           u10_hv(i)    = 0.
           v10_hv(i)    = 0.
           th2_hv(i)    = 0.
           t2_hv(i)     = 0.
           q2_hv(i)     = 0.
           wstar_hv(i)  = 0.
        enddo

        !optional output arguments:
        if(present(ck) .and. present(cka) .and. present(cd) .and. present(cda)) then
           do i = its,ite
              ck_hv(i)  = 0.
              cka_hv(i) = 0.
              cd_hv(i)  = 0.
              cda_hv(i) = 0.
           enddo
        endif
        if(present(ustm)) then
           do i = its,ite
              ustm_hv(i) = ustm(i,j)
           enddo
        endif
        !ccpp-physics end.

        call sf_mynn_run( &
                u1d,v1d,t1d,qv1d,p1d,dz8w1d,rho1d,         &
                u1d2,v1d2,dz2w1d,                          &
                cp,g,rovcp,r,xlv,psfcpa_hv,chs_hv,chs2_hv, &
                cqs2_hv,cpm_hv,pblh_hv,rmol_hv,            &
                znt_hv,ust_hv,mavail_hv,zol_hv,            &
                mol_hv,regime_hv,psim_hv,psih_hv,          &
                xland_hv,hfx_hv,qfx_hv,tsk_hv,             &
                u10_hv,v10_hv,th2_hv,t2_hv,                &
                q2_hv,flhc_hv,flqc_hv,snowh_hv,            &
                qgh_hv,qsfc_hv,lh_hv,                      &
                gz1oz0_hv,wspd_hv,br_hv,isfflx,dx_hv,      &
                svp1,svp2,svp3,svpt0,ep1,ep2,karman,       &
                ch_hv,qc1d,qcg_hv,                         &
                itimestep,                                 &
                wstar_hv,qstar_hv,                         &
                spp_pbl,rstoch1d,                          &
                isftcflx,iz0tlnd,                          &
                ustm_hv,ck_hv,cka_hv,cd_hv,cda_hv,         &
                its,ite)

        !ccpp-physics:
        !inout arguments:
        do i = its,ite
           regime(i,j) = regime_hv(i)
           hfx(i,j)    = hfx_hv(i)
           qfx(i,j)    = qfx_hv(i)
           lh(i,j)     = lh_hv(i)
           mol(i,j)    = mol_hv(i)
           rmol(i,j)   = rmol_hv(i)
           qgh(i,j)    = qgh_hv(i)
           qsfc(i,j)   = qsfc_hv(i)
           znt(i,j)    = znt_hv(i)
           zol(i,j)    = zol_hv(i)
           ust(i,j)    = ust_hv(i)
           cpm(i,j)    = cpm_hv(i)
           chs2(i,j)   = chs2_hv(i)
           cqs2(i,j)   = cqs2_hv(i)
           chs(i,j)    = chs_hv(i)
           ch(i,j)     = ch_hv(i)
           flhc(i,j)   = flhc_hv(i)
           flqc(i,j)   = flqc_hv(i)
           gz1oz0(i,j) = gz1oz0_hv(i)
           wspd(i,j)   = wspd_hv(i)
           br(i,j)     = br_hv(i)
           psim(i,j)   = psim_hv(i)
           psih(i,j)   = psih_hv(i)
        enddo

        !output arguments:
        do i = its,ite
           u10(i,j)   = u10_hv(i)
           v10(i,j)   = v10_hv(i)
           th2(i,j)   = th2_hv(i)
           t2(i,j)    = t2_hv(i)
           q2(i,j)    = q2_hv(i)
           wstar(i,j) = wstar_hv(i)
           qstar(i,j) = qstar_hv(i)
        enddo

        !optional output arguments:
        if(present(ck) .and. present(cka) .and. present(cd) .and. present(cda)) then
           do i = its,ite
              ck(i,j)  = ck_hv(i)
              cka(i,j) = cka_hv(i)
              cd(i,j)  = cd_hv(i)
              cda(i,j) = cda_hv(i)
           enddo
        endif
        if(present(ustm)) then
           do i = its,ite
              ustm(i,j) = ustm_hv(i)
           enddo
        endif
        !ccpp-physics end.

      enddo

    end subroutine sfclay_mynn

!=================================================================================================================
 end module module_sf_mynn
!=================================================================================================================
