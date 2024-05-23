!=================================================================================================================
 module sf_mynn

!-------------------------------------------------------------------
!Modifications implemented by Joseph Olson NOAA/GSL
!The following overviews the current state of this scheme::
!
!   BOTH LAND AND WATER:
!1) Calculation of stability parameter (z/L) taken from Li et al. (2010 BLM)
!   for first iteration of first time step; afterwards, exact calculation
!   using a brute force iterative method described in Olson et al. (2021 NOAA
!   Tech memorandum). This method replaces the iterative technique used in 
!   module_sf_sfclayrev.F (Jimenez et al. 2013) with mods. Either technique
!   gives about the same result. The former technique is retained in this
!   module (commented out) for potential subsequent benchmarking.
!2) Fixed isflux=0 option to turn off scalar fluxes, but keep momentum
!   fluxes for idealized studies (credit: Anna Fitch).
!3) Kinematic viscosity varies with temperature according to Andreas (1989).
!4) Uses the blended Monin-Obukhov flux-profile relationships COARE (Fairall 
!   et al 2003) for the unstable regime (a blended mix of Dyer-Hicks 1974 and
!   Grachev et al (2000). Uses Cheng and Brutsaert (2005) for stable conditions.
!5) The following overviews the namelist variables that control the 
!   aerodynamic roughness lengths (over water) and the thermal and moisture
!   roughness lengths (defaults are recommended):
!
!   LAND only:
!   "iz0tlnd" namelist option is used to select the following options:
!   (default) =0: Zilitinkevich (1995); Czil now set to 0.085
!             =1: Czil_new (modified according to Chen & Zhang 2008)
!             =2: Modified Yang et al (2002, 2008) - generalized for all landuse
!             =3: constant zt = z0/7.4 (original form; Garratt 1992)
!
!   WATER only:
!   "isftcflx" namelist option is used to select the following options:
!   (default) =0: z0, zt, and zq from the COARE algorithm. Set COARE_OPT (below) to
!                 3.0 (Fairall et al. 2003, default)
!                 3.5 (Edson et al 2013) - now with bug fix (Edson et al. 2014, JPO)
!             =1: z0 from Davis et al (2008), zt & zq from COARE 3.0/3.5
!             =2: z0 from Davis et al (2008), zt & zq from Garratt (1992)
!             =3: z0 from Taylor and Yelland (2004), zt and zq from COARE 3.0/3.5
!
!   SNOW/ICE only:
!   Andreas (2002) snow/ice parameterization for thermal and
!   moisture roughness is used over all gridpoints with snow deeper than
!   0.1 m. This algorithm calculates a z0 for snow (Andreas et al. 2005, BLM), 
!   which is only used as part of the thermal and moisture roughness
!   length calculation, not to directly impact the surface winds.
!
! Misc:
!1) Added a more elaborate diagnostic for u10 & V10 for high vertical resolution
!   model configurations but for most model configurations with depth of
!   the lowest half-model level near 10 m, a neutral-log diagnostic is used.
!
!2) Option to activate stochastic parameter perturbations (SPP), which
!   perturb z0, zt, and zq, along with many other parameters in the MYNN-
!   EDMF scheme. 
!
!NOTE: This code was primarily tested in combination with the RUC LSM.
!      Performance with the Noah (or other) LSM is relatively unknown.
!-------------------------------------------------------------------
  use ccpp_kind_types,only: kind_phys
  use mynn_shared,only: esat_blend,qsat_blend,xl_blend

  implicit none
  private
  public:: sf_mynn_run,     &
           sf_mynn_init,    &
           sf_mynn_finalize


  logical,parameter:: debug_code = .false.
  integer,parameter:: psi_opt = 0    ! 0 = stable: Cheng and Brustaert
                                     !     unstable: blended COARE
                                     ! 1 = GFS
  real,parameter:: wmin      = 0.1
  real,parameter:: vconvc    = 1.25
  real,parameter:: snowz0    = 0.011
  real,parameter:: coare_opt = 3.0   ! 3.0 or 3.5
  !For debugging purposes:

  real,dimension(0:1000),save:: psim_stab,psim_unstab, &
                                psih_stab,psih_unstab


  contains


!=================================================================================================================
!>\section arg_table_sf_mynn_init
!!\html\include sf_mynn_init.html
!!
 subroutine sf_mynn_init(errmsg,errflg)
!=================================================================================================================

!--- output arguments:
 character(len=*),intent(out):: errmsg
 integer,intent(out):: errflg

!-----------------------------------------------------------------------------------------------------------------

 call psi_init(psi_opt)

 errmsg = ' '
 errflg = 0

 end subroutine sf_mynn_init

!=================================================================================================================
!>\section arg_table_sf_mynn_finalize
!!\html\include sf_mynn_finalize.html
!!
 subroutine sf_mynn_finalize(errmsg,errflg)
!=================================================================================================================

!--- output arguments:
 character(len=*),intent(out):: errmsg
 integer,intent(out):: errflg

!-----------------------------------------------------------------------------------------------------------------

  errmsg = ' '
  errflg = 0

  end subroutine sf_mynn_finalize

!=================================================================================================================
!>\section arg_table_sf_mynn_run
!!\html\include sf_mynn_run.html
!!
  subroutine sf_mynn_run(                                &
                     u1d,v1d,t1d,qv1d,p1d,dz8w1d,rho1d,  &
                     u1d2,v1d2,dz2w1d,cp,g,rovcp,r,xlv,  &
                     psfcpa,chs,chs2,cqs2,cpm,pblh,rmol, &
                     znt,ust,mavail,zol,mol,regime,psim, &
                     psih,xland,hfx,qfx,tsk,u10,v10,th2, &
                     t2,q2,flhc,flqc,snowh,qgh,qsfc,lh,  &
                     gz1oz0,wspd,br,isfflx,dx,svp1,svp2, &
                     svp3,svpt0,ep1,ep2,karman,ch,qcg,   &
                     itimestep,wstar,qstar,ustm,ck,cka,  &
                     cd,cda,spp_pbl,rstoch1d,isftcflx,   &
                     iz0tlnd,its,ite,errmsg,errflg       &
                        )
 implicit none
!=================================================================================================================

!-----------------------------
! scalars:
!-----------------------------
 integer,intent(in):: its,ite
 integer,intent(in):: itimestep

 real(kind=kind_phys),intent(in):: svp1,svp2,svp3,svpt0,ep1,ep2
 real(kind=kind_phys),intent(in):: karman,cp,g,rovcp,r,xlv

 real(kind=kind_phys),parameter:: prt=1.       !prandlt number
 real(kind=kind_phys),parameter:: xka=2.4e-5   !molecular diffusivity

 character(len=*),intent(out):: errmsg
 integer,intent(out):: errflg

!-----------------------------
! namelist options
!-----------------------------
 logical,intent(in):: spp_pbl

 integer,intent(in):: isfflx
 integer,intent(in),optional:: isftcflx,iz0tlnd

!-----------------------------
! 1d arrays
!-----------------------------
 real(kind=kind_phys),intent(in),dimension(its:ite):: mavail, &
                                                        pblh, &
                                                       xland, &
                                                         tsk, &
                                                      psfcpa, &
                                                         qcg, &
                                                       snowh, &
                                                          dx

 real(kind=kind_phys),intent(in),dimension(its:ite)::    u1d, &
                                                         v1d, &
                                                        u1d2, &
                                                        v1d2, &
                                                        qv1d, &
                                                         p1d, &
                                                         t1d, &
                                                      dz8w1d, &
                                                      dz2w1d, &
                                                       rho1d
 real(kind=kind_phys),intent(in),dimension(its:ite)::         &
                                                    rstoch1d


 real(kind=kind_phys),intent(inout),dimension(its:ite)::      &
                                                      regime, &
                                                         hfx, &
                                                         qfx, &
                                                          lh, &
                                                         mol, &
                                                        rmol, &
                                                         qgh, &
                                                        qsfc, &
                                                         znt, &
                                                         zol, &
                                                         ust, &
                                                         cpm, &
                                                        chs2, &
                                                        cqs2, &
                                                         chs, &
                                                          ch, &
                                                        flhc, &
                                                        flqc, &
                                                      gz1oz0, &
                                                        wspd, &
                                                          br, &
                                                        psim, &
                                                        psih

!-----------------------------
! diagnostic outputs:
!-----------------------------
 real(kind=kind_phys),intent(out),dimension(its:ite)::        &
                                                         u10, &
                                                         v10, &
                                                         th2, &
                                                          t2, &
                                                          q2

 real(kind=kind_phys),intent(out),dimension(its:ite)::        &
                                                       wstar, &
                                                       qstar

 real(kind=kind_phys),intent(out),dimension(its:ite),optional:: &
                                                          ck, &
                                                         cka, &
                                                          cd, &
                                                         cda, &
                                                        ustm

!-----------------------------
! local variables
!-----------------------------
 integer:: n,i,k,l,yesno

 real(kind=kind_phys):: ep3
 real(kind=kind_phys):: pl,thcon,tvcon,e1
 real(kind=kind_phys):: dthvdz,dthvm,vconv,zol2,zol10,zolza,zolz0,zolzt
 real(kind=kind_phys):: dtg,psix,dtthx,dthdz,psix10,psit,psit2, &
                        psiq,psiq2,psiq10,dzdt
 real(kind=kind_phys):: fluxc,vsgd
 real(kind=kind_phys):: restar,visc,dqg,oldust,oldtst

 real(kind=kind_phys),dimension(its:ite) :: &
                 za, &    !height of lowest 1/2 sigma level(m)
                za2, &    !height of 2nd lowest 1/2 sigma level(m)
              thv1d, &    !theta-v at lowest 1/2 sigma (K)
               th1d, &    !theta at lowest 1/2 sigma (K)
               tc1d, &    !t at lowest 1/2 sigma (Celsius)
               tv1d, &    !tv at lowest 1/2 sigma (K)
               qvsh, &    !qv at lowest 1/2 sigma (spec humidity)
              psih2, &    !m-o stability functions at z=2 m
             psim10, &    !m-o stability functions at z=10 m
             psih10, &    !m-o stability functions at z=10 m
              wspdi, &
                z_q, &    !moisture roughness length
                z_t, &    !thermalroughness length
           ZNTstoch, &
             govrth, &    !g/theta
               thgb, &    !theta at ground
              thvgb, &    !theta-v at ground
               psfc, &    !press at surface (Pa/1000)
             qsfcmr, &    !qv at surface (mixing ratio, kg/kg)
             gz2oz0, &    !log((2.0+znt(i))/znt(i))
            gz10oz0, &    !log((10.+znt(i))/znt(i))
             gz2ozt, &    !log((2.0+z_t(i))/z_t(i))
            gz10ozt, &    !log((10.+z_t(i))/z_t(i))
             gz1ozt, &    !log((za(i)+z_t(i))/z_t(i))
             zratio       !z0/zt

!-----------------------------------------------------------------------------------------------------------------

 ep3 = 1.-ep2

 do i=its,ite
    !convert ground & lowest layer temperature to potential temperature:
    !psfc cmb
    psfc(i)=psfcpa(i)/1000.
    thgb(i)=tsk(i)*(100./psfc(i))**rovcp                 !(K)
    !PL cmb
    pl=p1d(i)/1000.
    thcon=(100./pl)**rovcp
    th1d(i)=t1d(i)*thcon                                 !(Theta, K)
    tc1d(i)=t1d(i)-273.15                                !(T, Celsius)

    !convert to virtual temperature
    qvsh(i)=qv1d(i)/(1.+qv1d(i))                         !convert to spec hum (kg/kg)
    tvcon=(1.+ep1*qvsh(i))
    thv1d(i)=th1d(i)*tvcon                               !(K)
    tv1d(i)=t1d(i)*tvcon                                 !(K)

    !rho1d(i)=psfcpa(i)/(r*tv1d(i))                      !now using value calculated in sfc driver
    za(i)=0.5*dz8w1d(i)                                  !height of first half-sigma level
    za2(i)=dz8w1d(i) + 0.5*dz2w1d(i)                     !height of 2nd half-sigma level
    govrth(i)=g/th1d(i)
 enddo

 do i=its,ite
    if (tsk(i) .lt. 273.15) then
       !saturation vapor pressure wrt ice (svp1=.6112; 10*mb)
       e1=svp1*exp(4648*(1./273.15 - 1./tsk(i)) - &
       & 11.64*log(273.15/tsk(i)) + 0.02265*(273.15 - tsk(i)))
    else
       !saturation vapor pressure wrt water (Bolton 1980)
       e1=svp1*exp(svp2*(tsk(i)-svpt0)/(tsk(i)-svp3))
    endif
    !for land points, qsfc can come from lsm, only recompute over water
    if (xland(i).gt.1.5 .or. qsfc(i).le.0.0) then        !water
       qsfc(i)=ep2*e1/(psfc(i)-ep3*e1)                   !specific humidity
       qsfcmr(i)=ep2*e1/(psfc(i)-e1)                     !mixing ratio
    else                                                 !land
       qsfcmr(i)=qsfc(i)/(1.-qsfc(i))
    endif

    !qgh changed to use lowest-level air temp consistent with myjsfc change
    !q2sat = qgh in LSM
    if (tsk(i) .lt. 273.15) then
       !saturation vapor pressure wrt ice
       e1=svp1*exp(4648*(1./273.15 - 1./t1d(i)) - &
       &  11.64*log(273.15/t1d(i)) + 0.02265*(273.15 - t1d(i)))
    else
       !saturation vapor pressure wrt water (Bolton 1980)
       e1=svp1*exp(svp2*(t1d(i)-svpt0)/(t1d(i)-svp3))
    endif
    pl=p1d(i)/1000.
    !qgh(i)=ep2*e1/(pl-ep_3*e1)                          !specific humidity
    qgh(i)=ep2*e1/(pl-e1)                                !mixing ratio
    cpm(i)=cp*(1.+0.84*qv1d(i))
 enddo

 do i=its,ite
    wspd(i)=sqrt(u1d(i)*u1d(i)+v1d(i)*v1d(i))

    !tgs:thvgb(i)=thgb(i)*(1.+ep1*qsfc(i)*mavail(i))
    thvgb(i)=thgb(i)*(1.+ep1*qsfc(i))

    dthdz=(th1d(i)-thgb(i))
    dthvdz=(thv1d(i)-thvgb(i))

    !--------------------------------------------------------
    !  Calculate the convective velocity scale (WSTAR) and
    !  subgrid-scale velocity (VSGD) following Beljaars (1995, QJRMS)
    !  and Mahrt and Sun (1995, MWR), respectively
    !-------------------------------------------------------
    !Use Beljaars over land and water
    fluxc = max(hfx(i)/rho1d(i)/cp                    &
    &     + ep1*thvgb(i)*qfx(i)/rho1d(i),0.)
    !wstar(i) = vconvc*(g/tsk(i)*pblh(i)*fluxc)**.33
    if (xland(i).gt.1.5 .or. qsfc(i).le.0.0) then   !water
       wstar(i) = vconvc*(g/tsk(i)*pblh(i)*fluxc)**.33
    else                                            !land
       !increase height scale, assuming that the non-local transoport
       !from the mass-flux (plume) mixing exceedsd the pblh.
       wstar(i) = vconvc*(g/tsk(i)*min(1.5*pblh(i),4000.)*fluxc)**.33
    endif
    !--------------------------------------------------------
    ! Mahrt and Sun low-res correction
    ! (for 13 km ~ 0.37 m/s; for 3 km == 0 m/s)
    !--------------------------------------------------------
    vsgd = 0.32 * (max(dx(i)/5000.-1.,0.))**.33
    wspd(i)=sqrt(wspd(i)*wspd(i)+wstar(i)*wstar(i)+vsgd*vsgd)
    wspd(i)=max(wspd(i),wmin)

    !--------------------------------------------------------
    ! calculate the bulk richardson number of surface layer,
    ! according to Akb(1976), Eq(12).
    !--------------------------------------------------------
    br(i)=govrth(i)*za(i)*dthvdz/(wspd(i)*wspd(i))
    if (itimestep == 1) then
       !set limits according to Li et al. (2010) boundary-layer meteorol (p.158)
       br(i)=max(br(i),-2.0)
       br(i)=min(br(i),2.0)
    else
       br(i)=max(br(i),-4.0)
       br(i)=min(br(i), 4.0)
    endif

    ! if previously unstable, do not let into regimes 1 and 2 (stable)
    ! if (itimestep .gt. 1) then
    !    if(mol(i).lt.0.)br(i)=min(br(i),0.0)
    !endif
     
 enddo

 1006   format(a,f7.3,a,f9.4,a,f9.5,a,f9.4)
 1007   format(a,f2.0,a,f6.2,a,f7.3,a,f7.2)

!--------------------------------------------------------------------      
!--------------------------------------------------------------------      
!--- begin i-loop
!--------------------------------------------------------------------
!--------------------------------------------------------------------

 do i=its,ite

    !compute kinematic viscosity (m2/s) Andreas (1989) CRREL Rep. 89-11
    !valid between -173 and 277 degrees C.
    visc=1.326e-5*(1. + 6.542e-3*tc1d(i) + 8.301e-6*tc1d(i)*tc1d(i) &
                      - 4.84e-9*tc1d(i)*tc1d(i)*tc1d(i))

    if ((xland(i)-1.5).ge.0) then
       !--------------------------------------
       ! water
       !--------------------------------------
       ! calculate z0 (znt)
       !--------------------------------------
       if ( present(isftcflx) ) then
          if ( isftcflx .eq. 0 ) then
             if (coare_opt .eq. 3.0) then
                !COARE 3.0 (misleading subroutine name)
                call charnock_1955(znt(i),ust(i),wspd(i),visc,za(i))
             else
                !COARE 3.5
                call edson_etal_2013(znt(i),ust(i),wspd(i),visc,za(i))
             endif
          elseif ( isftcflx .eq. 1 .or. isftcflx .eq. 2 ) then
             call davis_etal_2008(znt(i),ust(i))
          elseif ( isftcflx .eq. 3 ) then
             call taylor_yelland_2001(znt(i),ust(i),wspd(i))
          elseif ( isftcflx .eq. 4 ) then
             if (coare_opt .eq. 3.0) then
                !COARE 3.0 (MISLEADING SUBROUTINE NAME)
                call charnock_1955(znt(i),ust(i),wspd(i),visc,za(i))
             else
                !COARE 3.5
                call edson_etal_2013(znt(i),ust(i),wspd(i),visc,za(i))
             endif
          endif
       else
          !default to COARE 3.0/3.5
          if (coare_opt .eq. 3.0) then
             !COARE 3.0
             call charnock_1955(znt(i),ust(i),wspd(i),visc,za(i))
          else
             !COARE 3.5
             call edson_etal_2013(znt(i),ust(i),wspd(i),visc,za(i))
          endif
       endif

       !add stochastic perturbaction of ZNT
       if (spp_pbl) then
          zntstoch(i)  = max(znt(i) + znt(i)*1.0*rstoch1d(i), 1e-6)
       else
          zntstoch(i)  = znt(i)
       endif

       !compute roughness reynolds number (restar) using new znt
       ! AHW: Garrattt formula: Calculate roughness Reynolds number
       !      Kinematic viscosity of air (linear approx to
       !      temp dependence at sea level)
       restar=max(ust(i)*zntstoch(i)/visc, 0.1)

       !--------------------------------------
       !calculate z_t and z_q
       !--------------------------------------
       if ( present(isftcflx) ) then
          if ( isftcflx .eq. 0 ) then
             if (coare_opt .eq. 3.0) then
                call fairall_etal_2003(z_t(i),z_q(i),restar,ust(i),visc,rstoch1d(i),spp_pbl)
             else
                !presumably, this will be published soon, but hasn't yet
                call fairall_etal_2014(z_t(i),z_q(i),restar,ust(i),visc,rstoch1d(i),spp_pbl)
             endif
          elseif ( isftcflx .eq. 1 ) then
             if (coare_opt .eq. 3.0) then
                call fairall_etal_2003(z_t(i),z_q(i),restar,ust(i),visc,rstoch1d(i),spp_pbl)
             else
                call fairall_etal_2014(z_t(i),z_q(i),restar,ust(i),visc,rstoch1d(i),spp_pbl)
             endif
          elseif ( isftcflx .eq. 2 ) then
             call garratt_1992(z_t(i),z_q(i),zntstoch(i),restar,xland(i))
          elseif ( isftcflx .eq. 3 ) then
             if (coare_opt .eq. 3.0) then
                call fairall_etal_2003(z_t(i),z_q(i),restar,ust(i),visc,rstoch1d(i),spp_pbl)
             else
                call fairall_etal_2014(z_t(i),z_q(i),restar,ust(i),visc,rstoch1d(i),spp_pbl)
             endif
          endif
       else
          !default to COARE 3.0/3.5
          if (coare_opt .eq. 3.0) then
             call fairall_etal_2003(z_t(i),z_q(i),restar,ust(i),visc,rstoch1d(i),spp_pbl)
          else
             call fairall_etal_2014(z_t(i),z_q(i),restar,ust(i),visc,rstoch1d(i),spp_pbl)
          endif
       endif
 
    else

       !add stochastic perturbaction of znt
       if (spp_pbl) then
          zntstoch(i)  = max(znt(i) + znt(i)*1.0*rstoch1d(i), 1e-6)
       else
          zntstoch(i)  = znt(i)
       endif

       !--------------------------------------
       ! land
       !--------------------------------------
       !compute roughness reynolds number (restar) using default znt
       restar=max(ust(i)*zntstoch(i)/visc, 0.1)

       !--------------------------------------
       ! get z_t and z_q
       !--------------------------------------
       !check for snow/ice points over land
       if ( snowh(i) .ge. 0.1) then
          call andreas_2002(zntstoch(i),visc,ust(i),z_t(i),z_q(i))
       else
          if ( present(iz0tlnd) ) then
             if ( iz0tlnd .le. 1 ) then
                call zilitinkevich_1995(zntstoch(i),z_t(i),z_q(i),restar,&
                           ust(i),karman,xland(i),iz0tlnd,spp_pbl,rstoch1d(i))
             elseif ( iz0tlnd .eq. 2 ) then
                call yang_2008(zntstoch(i),z_t(i),z_q(i),ust(i),mol(i),&
                               qstar(i),restar,visc,xland(i))
             elseif ( iz0tlnd .eq. 3 ) then
                !original mynn in wrf-arw used this form:
                call garratt_1992(z_t(i),z_q(i),zntstoch(i),restar,xland(i))
             endif
          else
             !default to zilitinkevich
             call zilitinkevich_1995(zntstoch(i),z_t(i),z_q(i),restar,&
                           ust(i),karman,xland(i),0,spp_pbl,rstoch1d(i))
          endif
       endif

    endif
    zratio(i)=zntstoch(i)/z_t(i)   !needed for Li et al.

    gz1oz0(i)= log((za(i)+zntstoch(i))/zntstoch(i))
    gz1ozt(i)= log((za(i)+zntstoch(i))/z_t(i))
    gz2oz0(i)= log((2.0+zntstoch(i))/zntstoch(i))
    gz2ozt(i)= log((2.0+zntstoch(i))/z_t(i))
    gz10oz0(i)=log((10.+zntstoch(i))/zntstoch(i))
    gz10ozt(i)=log((10.+zntstoch(i))/z_t(i))

    !--------------------------------------------------------------------      
    !--- DIAGNOSE BASIC PARAMETERS FOR THE APPROPRIATE STABILITY CLASS:
    !                                                                                
    !    THE STABILITY CLASSES ARE DETERMINED BY BR (BULK RICHARDSON NO.).
    !                                                                                
    !    CRITERIA FOR THE CLASSES ARE AS FOLLOWS:                                   
    !                                                                                
    !        1. BR .GE. 0.2;                                                         
    !               REPRESENTS NIGHTTIME STABLE CONDITIONS (REGIME=1),               
    !                                                                                
    !        2. BR .LT. 0.2 .AND. BR .GT. 0.0;                                       
    !               REPRESENTS DAMPED MECHANICAL TURBULENT CONDITIONS                
    !               (REGIME=2),                                                      
    !                                                                                
    !        3. BR .EQ. 0.0                                                          
    !               REPRESENTS FORCED CONVECTION CONDITIONS (REGIME=3),              
    !                                                                                
    !        4. BR .LT. 0.0                                                          
    !               REPRESENTS FREE CONVECTION CONDITIONS (REGIME=4).                
    !                                                                                
    !--------------------------------------------------------------------
    if (br(i) .gt. 0.0) then
       if (br(i) .gt. 0.2) then
          !---class 1; stable (nighttime) conditions:
          regime(i)=1.
       else
          !---class 2; damped mechanical turbulence:
          regime(i)=2.
       endif

       !compute z/l first guess:
       if (itimestep .le. 1) then
          call li_etal_2010(zol(i),br(i),za(i)/zntstoch(i),zratio(i))
       else
          zol(i)=za(i)*karman*g*mol(i)/(th1d(i)*max(ust(i)*ust(i),0.0001))
          zol(i)=max(zol(i),0.0)
          zol(i)=min(zol(i),20.)
       endif

       !Use Pedros iterative function to find z/L
       !zol(i)=zolri(br(i),za(i),zntstoch(i),z_t(i),zol(i),psi_opt)
       !Use brute-force method
       zol(i)=zolrib(br(i),za(i),zntstoch(i),z_t(i),gz1oz0(i),gz1ozt(i),zol(i),psi_opt)
       zol(i)=max(zol(i),0.0)
       zol(i)=min(zol(i),20.)

       zolzt = zol(i)*z_t(i)/za(i)               ! zt/l
       zolz0 = zol(i)*zntstoch(i)/za(i)          ! z0/l
       zolza = zol(i)*(za(i)+zntstoch(i))/za(i)  ! (z+z0/l
       zol10 = zol(i)*(10.+zntstoch(i))/za(i)    ! (10+z0)/l
       zol2  = zol(i)*(2.+zntstoch(i))/za(i)     ! (2+z0)/l

       !compute psim and psih
       if ((xland(i)-1.5).ge.0) then
          ! water
          !call psi_suselj_sood_2010(psim(i),psih(i),zol(i))
          !call psi_beljaars_holtslag_1991(psim(i),psih(i),zol(i))
          !call psi_businger_1971(psim(i),psih(i),zol(i))
          !call psi_dyerhicks(psim(i),psih(i),zol(i),z_t(i),zntstoch(i),za(i))
          !call psi_cb2005(psim(i),psih(i),zolza,zolz0)
          ! or use tables
          psim(i)=psim_stable(zolza,psi_opt)-psim_stable(zolz0,psi_opt)
          psih(i)=psih_stable(zolza,psi_opt)-psih_stable(zolzt,psi_opt)
          psim10(i)=psim_stable(zol10,psi_opt)-psim_stable(zolz0,psi_opt)
          psih10(i)=psih_stable(zol10,psi_opt)-psih_stable(zolz0,psi_opt)
          psih2(i)=psih_stable(zol2,psi_opt)-psih_stable(zolz0,psi_opt)
       else
          ! land
          !call psi_beljaars_holtslag_1991(psim(i),psih(i),zol(i))
          !call psi_businger_1971(psim(i),psih(i),zol(i))
          !call psi_zilitinkevich_esau_2007(psim(i),psih(i),zol(i))
          !call psi_dyerhicks(psim(i),psih(i),zol(i),z_t(i),zntstoch(i),za(i))
          !call psi_cb2005(psim(i),psih(i),zolza,zolz0)
          ! or use tables
          psim(i)=psim_stable(zolza,psi_opt)-psim_stable(zolz0,psi_opt)
          psih(i)=psih_stable(zolza,psi_opt)-psih_stable(zolzt,psi_opt)
          psim10(i)=psim_stable(zol10,psi_opt)-psim_stable(zolz0,psi_opt)
          psih10(i)=psih_stable(zol10,psi_opt)-psih_stable(zolz0,psi_opt)
          psih2(i)=psih_stable(zol2,psi_opt)-psih_stable(zolz0,psi_opt)
       endif

       !psim10(i)=10./za(i)*psim(i)
       !psih10(i)=10./za(i)*psih(i)
       !psim2(i)=2./za(i)*psim(i)
       !psih2(i)=2./za(i)*psih(i)

       ! 1.0 over monin-obukhov length
       rmol(i)= zol(i)/za(i)

    elseif(br(i) .eq. 0.) then
       !=========================================================  
       !-----class 3; forced convection/neutral:
       !=========================================================
       regime(i)=3.

       psim(i)=0.0
       psih(i)=psim(i)
       psim10(i)=0.
       psih10(i)=0.
       psih2(i)=0.

       zol(i)=0.
       !if (ust(i) .lt. 0.01) then
       !   zol(i)=br(i)*gz1oz0(i)
       !else
       !   zol(i)=karman*govrth(i)*za(i)*mol(i)/(max(ust(i)*ust(i),0.001))
       !endif
       rmol(i) = zol(i)/za(i)

    elseif(br(i) .lt. 0.)then
       !==========================================================
       !-----class 4; free convection:
       !==========================================================
       regime(i)=4.

       !compute z/l first guess:
       if (itimestep .le. 1) then
          call li_etal_2010(zol(i),br(i),za(i)/zntstoch(i),zratio(i))
       else
          zol(i)=za(i)*karman*g*mol(i)/(th1d(i)*max(ust(i)*ust(i),0.001))
          zol(i)=max(zol(i),-20.0)
          zol(i)=min(zol(i),0.0)
       endif

       !Use Pedros iterative function to find z/L
       !zol(I)=zolri(br(I),ZA(I),ZNTstoch(I),z_t(I),ZOL(I),psi_opt)
       !Use alternative method
       zol(i)=zolrib(br(i),za(i),zntstoch(i),z_t(i),gz1oz0(i),gz1ozt(i),zol(i),psi_opt)
       zol(i)=max(zol(i),-20.0)
       zol(i)=min(zol(i),0.0)

       zolzt = zol(i)*z_t(i)/za(i)                ! zt/l
       zolz0 = zol(i)*zntstoch(i)/za(i)           ! z0/l
       zolza = zol(i)*(za(i)+zntstoch(i))/za(i)   ! (z+z0/l
       zol10 = zol(i)*(10.+zntstoch(i))/za(i)     ! (10+z0)/l
       zol2  = zol(i)*(2.+zntstoch(i))/za(i)      ! (2+z0)/l

       !compute psim and psih
       if ((xland(i)-1.5).ge.0) then
          ! water
          !call psi_suselj_sood_2010(psim(i),psih(i),zol(i))
          !call psi_hogstrom_1996(psim(i),psih(i),zol(i), z_t(i), zntstoch(i), za(i))
          !call psi_businger_1971(psim(i),psih(i),zol(i))
          !call psi_dyerhicks(psim(i),psih(i),zol(i),z_t(i),zntstoch(i),za(i))
          ! use tables
          psim(i)=psim_unstable(zolza,psi_opt)-psim_unstable(zolz0,psi_opt)
          psih(i)=psih_unstable(zolza,psi_opt)-psih_unstable(zolzt,psi_opt)
          psim10(i)=psim_unstable(zol10,psi_opt)-psim_unstable(zolz0,psi_opt)
          psih10(i)=psih_unstable(zol10,psi_opt)-psih_unstable(zolz0,psi_opt)
          psih2(i)=psih_unstable(zol2,psi_opt)-psih_unstable(zolz0,psi_opt)
       else
          ! land
          !call psi_hogstrom_1996(psim(i),psih(i),zol(i), z_t(i), zntstoch(i), za(i))
          !call psi_businger_1971(psim(i),psih(i),zol(i))
          !call psi_dyerhicks(psim(i),psih(i),zol(i),z_t(i),zntstoch(i),za(i))
          ! use tables
          psim(i)=psim_unstable(zolza,psi_opt)-psim_unstable(zolz0,psi_opt)
          psih(i)=psih_unstable(zolza,psi_opt)-psih_unstable(zolzt,psi_opt)
          psim10(i)=psim_unstable(zol10,psi_opt)-psim_unstable(zolz0,psi_opt)
          psih10(i)=psih_unstable(zol10,psi_opt)-psih_unstable(zolz0,psi_opt)
          psih2(i)=psih_unstable(zol2,psi_opt)-psih_unstable(zolz0,psi_opt)
       endif

       !psim10(i)=10./za(i)*psim(i)
       !psih2(i)=2./za(i)*psih(i)

       !---limit psih and psim in the case of thin layers and
       !---high roughness.  this prevents denominator in fluxes
       !---from getting too small
       psih(i)=min(psih(i),0.9*gz1ozt(i))
       psim(i)=min(psim(i),0.9*gz1oz0(i))
       psih2(i)=min(psih2(i),0.9*gz2ozt(i))
       psim10(i)=min(psim10(i),0.9*gz10oz0(i))
       psih10(i)=min(psih10(i),0.9*gz10ozt(i))

       rmol(i) = zol(i)/za(i)

    endif

    !------------------------------------------------------------
    !-----compute the frictional velocity:
    !------------------------------------------------------------
    !     Za(1982) Eqs(2.60),(2.61).
    psix=gz1oz0(i)-psim(i)
    psix10=gz10oz0(i)-psim10(i)
    ! to prevent oscillations average with old value
    oldust = ust(i)
    ust(i)=0.5*ust(i)+0.5*karman*wspd(i)/psix
    !non-averaged: ust(i)=karman*wspd(i)/psix
     
    ! compute u* without vconv for use in hfx calc when isftcflx > 0
    wspdi(i)=max(sqrt(u1d(i)*u1d(i)+v1d(i)*v1d(i)), wmin)
    if ( present(ustm) ) then
       ustm(i)=0.5*ustm(i)+0.5*karman*wspdi(i)/psix
    endif

    if ((xland(i)-1.5).lt.0.) then        !land
       ust(i)=max(ust(i),0.005)  !further relaxing this limit - no need to go lower
       !keep ustm = ust over land.
       if ( present(ustm) ) ustm(i)=ust(i)
    endif

    !------------------------------------------------------------
    !-----compute the thermal and moisture resistance (psiq and psit):
    !------------------------------------------------------------
    ! lower limit added to prevent large flhc in soil model
    ! activates in unstable conditions with thin layers or high z0
    gz1ozt(i)= log((za(i)+zntstoch(i))/z_t(i))
    gz2ozt(i)= log((2.0+zntstoch(i))/z_t(i))

    psit =max(gz1ozt(i)-psih(i) ,1.)
    psit2=max(gz2ozt(i)-psih2(i),1.)

    psiq=max(log((za(i)+zntstoch(i))/z_q(i))-psih(i) ,1.0)
    psiq2=max(log((2.0+zntstoch(i))/z_q(i))-psih2(i) ,1.0)
    psiq10=max(log((10.0+zntstoch(i))/z_q(i))-psih10(i) ,1.0)
    !----------------------------------------------------
    !compute the temperature scale (or friction temperature, T*)
    !----------------------------------------------------
    dtg=thv1d(i)-thvgb(i)
    oldtst=mol(i)
    mol(i)=karman*dtg/psit/prt
    !t_star(i) = -hfx(i)/(ust(i)*cpm(i)*rho1d(i))
    !t_star(i) = mol(i)
    !----------------------------------------------------
    !compute the moisture scale (or q*)
    dqg=(qvsh(i)-qsfc(i))*1000.   !(kg/kg -> g/kg)
    qstar(i)=karman*dqg/psiq/prt

    !if () then
        !  write(*,1001)"regime:",regime(i)," z/l:",zol(i)," u*:",ust(i)," tstar:",mol(i)
        !  write(*,1002)"psim:",psim(i)," psih:",psih(i)," w*:",wstar(i)," dthv:",thv1d(i)-thvgb(i)
        !  write(*,1003)"cpm:",cpm(i)," rho1d:",rho1d(i)," l:",zol(i)/za(i)," dth:",th1d(i)-thgb(i)
        !  write(*,1004)"z0/zt:",zratio(i)," z0:",zntstoch(i)," zt:",z_t(i)," za:",za(i)
        !  write(*,1005)"re:",restar," mavail:",mavail(i)," qsfc(i):",qsfc(i)," qvsh(i):",qvsh(i)
        !  print*,"visc=",visc," z0:",zntstoch(i)," t1d(i):",t1d(i)
        !  write(*,*)"============================================="
    !endif

 enddo     ! end i-loop

 1000   format(a,f6.1, a,f6.1, a,f5.1, a,f7.1)
 1001   format(a,f2.0, a,f10.4,a,f5.3, a,f11.5)
 1002   format(a,f7.2, a,f7.2, a,f7.2, a,f10.3)
 1003   format(a,f7.2, a,f7.2, a,f10.3,a,f10.3)
 1004   format(a,f11.3,a,f9.7, a,f9.7, a,f6.2, a,f10.3)
 1005   format(a,f9.2,a,f6.4,a,f7.4,a,f7.4)

      !----------------------------------------------------------
      !  compute surface heat and moisture fluxes
      !----------------------------------------------------------
 do i=its,ite

    !For computing the diagnostics and fluxes (below), whether the fluxes
    !are turned off or on, we need the following:
    psix=gz1oz0(i)-psim(i)
    psix10=gz10oz0(i)-psim10(i)

    psit =max(gz1ozt(i)-psih(i), 1.0)
    psit2=max(gz2ozt(i)-psih2(i),1.0)
  
    psiq=max(log((za(i)+z_q(i))/z_q(i))-psih(i) ,1.0)
    psiq2=max(log((2.0+z_q(i))/z_q(i))-psih2(i) ,1.0)
    psiq10=max(log((10.0+z_q(i))/z_q(i))-psih10(i) ,1.0)

    if (isfflx .lt. 1) then                            

       qfx(i)  = 0.                                                              
       hfx(i)  = 0.    
       flhc(i) = 0.                                                             
       flqc(i) = 0.                                                             
       lh(i)   = 0.                                                             
       chs(i)  = 0.                                                             
       ch(i)   = 0.                                                             
       chs2(i) = 0.                                                              
       cqs2(i) = 0.                                                              
       if(present(ck)  .and. present(cd) .and. &
         &present(cka) .and. present(cda)) then
           ck(i) = 0.
           cd(i) = 0.
           cka(i)= 0.
           cda(i)= 0.
       endif
    else

      !------------------------------------------
      ! calculate the exchange coefficients for heat (flhc)
      ! and moisture (flqc)
      !------------------------------------------
      flqc(i)=rho1d(i)*mavail(i)*ust(i)*karman/psiq
      flhc(i)=rho1d(i)*cpm(i)*ust(i)*karman/psit

      !----------------------------------
      ! compute surface moisture flux:
      !----------------------------------
      qfx(i)=flqc(i)*(qsfcmr(i)-qv1d(i))
      !joe: qfx(i)=max(qfx(i),0.)   !originally did not allow neg qfx           
      qfx(i)=max(qfx(i),-0.02)      !allows small neg qfx, like myj
      lh(i)=xlv*qfx(i)

      !----------------------------------
      ! compute surface heat flux:
      !----------------------------------
      if(xland(i)-1.5.gt.0.)then      !water                                           
         hfx(i)=flhc(i)*(thgb(i)-th1d(i))                                
         if ( present(isftcflx) ) then
            if ( isftcflx.ne.0 ) then
               ! ahw: add dissipative heating term
               hfx(i)=hfx(i)+rho1d(i)*ustm(i)*ustm(i)*wspdi(i)
            endif
         endif
      elseif(xland(i)-1.5.lt.0.)then  !land                               
         hfx(i)=flhc(i)*(thgb(i)-th1d(i))                                
         hfx(i)=max(hfx(i),-250.)                                       
      endif

      !chs(i)=ust(i)*karman/(alog(karman*ust(i)*za(i) &
      !       /xka+za(i)/zl)-psih(i))

      chs(i)=ust(i)*karman/psit

      ! the exchange coefficient for cloud water is assumed to be the 
      ! same as that for heat. ch is multiplied by wspd.

      !ch(i)=chs(i)
      ch(i)=flhc(i)/( cpm(i)*rho1d(i) )

      !these are used for 2-m diagnostics only
      cqs2(i)=ust(i)*karman/psiq2
      chs2(i)=ust(i)*karman/psit2

      if(present(ck)  .and. present(cd) .and. &
        &present(cka) .and. present(cda)) then
         ck(i)=(karman/psix10)*(karman/psiq10)
         cd(i)=(karman/psix10)*(karman/psix10)
         cka(i)=(karman/psix)*(karman/psiq)
         cda(i)=(karman/psix)*(karman/psix)
      endif

   endif !end isfflx option

   !-----------------------------------------------------
   !compute diagnostics
   !-----------------------------------------------------
   !compute 10 m wnds
   !-----------------------------------------------------
   ! If the lowest model level is close to 10-m, use it
   ! instead of the flux-based diagnostic formula.
   if (za(i) .le. 7.0) then
      ! high vertical resolution
      if(za2(i) .gt. 7.0 .and. za2(i) .lt. 13.0) then
         !use 2nd model level
         u10(i)=u1d2(i)
         v10(i)=v1d2(i)
      else
         u10(i)=u1d(i)*log(10./zntstoch(i))/log(za(i)/zntstoch(i))
         v10(i)=v1d(i)*log(10./zntstoch(i))/log(za(i)/zntstoch(i))
      endif
   elseif(za(i) .gt. 7.0 .and. za(i) .lt. 13.0) then
      !moderate vertical resolution
      !u10(i)=u1d(i)*psix10/psix
      !v10(i)=v1d(i)*psix10/psix
      !use neutral-log:
      u10(i)=u1d(i)*log(10./zntstoch(i))/log(za(i)/zntstoch(i))
      v10(i)=v1d(i)*log(10./zntstoch(i))/log(za(i)/zntstoch(i))
   else
      ! very coarse vertical resolution
      u10(i)=u1d(i)*psix10/psix
      v10(i)=v1d(i)*psix10/psix
   endif

   !-----------------------------------------------------
   !compute 2m t, th, and q
   !these will be overwritten for land points in the lsm
   !-----------------------------------------------------
   dtg=th1d(i)-thgb(i) 
   th2(i)=thgb(i)+dtg*psit2/psit
   !***  be certain that the 2-m theta is bracketed by
   !***  the values at the surface and lowest model level.
   if ((th1d(i)>thgb(i) .and. (th2(i)<thgb(i) .or. th2(i)>th1d(i))) .or. &
       (th1d(i)<thgb(i) .and. (th2(i)>thgb(i) .or. th2(i)<th1d(i)))) then
       th2(i)=thgb(i) + 2.*(th1d(i)-thgb(i))/za(i)
   endif
   t2(i)=th2(i)*(psfc(i)/100.)**rovcp

   q2(i)=qsfcmr(i)+(qv1d(i)-qsfcmr(i))*psiq2/psiq
   q2(i)= max(q2(i), min(qsfcmr(i), qv1d(i)))
   q2(i)= min(q2(i), 1.05*qv1d(i))

   if ( debug_code ) then
      yesno = 0
      if (hfx(i) > 1200. .or. hfx(i) < -700.)then
            print*,"suspicious values in mynn sfclayer",&
            i, "hfx: ",hfx(i)
            yesno = 1
      endif
      if (lh(i)  > 1200. .or. lh(i)  < -700.)then
            print*,"suspicious values in mynn sfclayer",&
            i, "lh: ",lh(i)
            yesno = 1
      endif
      if (ust(i) < 0.0 .or. ust(i) > 4.0 )then
            print*,"suspicious values in mynn sfclayer",&
            i, "ust: ",ust(i)
            yesno = 1
      endif
      if (wstar(i)<0.0 .or. wstar(i) > 6.0)then
            print*,"suspicious values in mynn sfclayer",&
            i, "wstar: ",wstar(i)
            yesno = 1
      endif
      if (rho1d(i)<0.0 .or. rho1d(i) > 1.6 )then
            print*,"suspicious values in mynn sfclayer",&
            i, "rho: ",rho1d(i)
            yesno = 1
      endif
      if (qsfc(i)*1000. <0.0 .or. qsfc(i)*1000. >40.)then
            print*,"suspicious values in mynn sfclayer",&
            i, "qsfc: ",qsfc(i)
            yesno = 1
      endif
      if (pblh(i)<0. .or. pblh(i)>6000.)then
            print*,"suspicious values in mynn sfclayer",&
            i, "pblh: ",pblh(i)
            yesno = 1
      endif

      if (yesno == 1) then
        print*," other info:"
        write(*,1001)"regime:",regime(i)," z/l:",zol(i)," u*:",ust(i),&
              " tstar:",mol(i)
        write(*,1002)"psim:",psim(i)," psih:",psih(i)," w*:",wstar(i),&
              " dthv:",thv1d(i)-thvgb(i)
        write(*,1003)"cpm:",cpm(i)," rho1d:",rho1d(i)," l:",&
              zol(i)/za(i)," dth:",th1d(i)-thgb(i)
        write(*,*)" z0:",zntstoch(i)," zt:",z_t(i)," za:",za(i)
        write(*,1005)"re:",restar," mavail:",mavail(i)," qsfc(i):",&
              qsfc(i)," qvsh(i):",qvsh(i)
        print*,"psix=",psix," z0:",zntstoch(i)," t1d(i):",t1d(i)
        write(*,*)"============================================="
      endif
   endif

 enddo !end i-loop

 errmsg = ' '
 errflg = 0

 end subroutine sf_mynn_run

!=================================================================================================================
 subroutine zilitinkevich_1995(z_0,zt,zq,restar,ustar,karman,landsea,iz0tlnd2,spp_pbl,rstoch)
!this subroutine returns the thermal and moisture roughness lengths
!from Zilitinkevich (1995) and Zilitinkevich et al. (2001) over
!land and water, respectively.
!
!MODS:
!20120705 : added IZ0TLND option. Note: This option was designed
!           to work with the Noah LSM and may be specific for that
!           LSM only. Tests with RUC LSM showed no improvements.
 implicit none
!=================================================================================================================

!--- input arguments:
 logical,intent(in):: spp_pbl
 integer,optional,intent(in)::  iz0tlnd2

 real(kind=kind_phys),intent(in):: rstoch
 real(kind=kind_phys),intent(in):: z_0,restar,ustar,karman,landsea

!--- output arguments:
 real(kind=kind_phys),intent(out):: zt,zq

!--- local variables:
 real(kind=kind_phys):: czil  !=0.100 in Chen et al. (1997)
                              !=0.075 in Zilitinkevich (1995)
                              !=0.500 in Lemone et al. (2008)

!-----------------------------------------------------------------------------------------------------------------

 if (landsea-1.5 .gt. 0) then !water

!this is based on Zilitinkevich, Grachev, and Fairall (2001):
!their equations 15 and 16).
    if (restar .lt. 0.1) then
       zt = z_0*exp(karman*2.0)
       zt = min( zt, 6.0e-5)
       zt = max( zt, 2.0e-9)
       zq = z_0*exp(karman*3.0)
       zq = min( zq, 6.0e-5)
       zq = max( zq, 2.0e-9)
    else
       zt = z_0*exp(-karman*(4.0*sqrt(restar)-3.2))
       zt = min( zt, 6.0e-5)
       zt = max( zt, 2.0e-9)
       zq = z_0*exp(-karman*(4.0*sqrt(restar)-4.2))
       zq = min( zt, 6.0e-5)
       zq = max( zt, 2.0e-9)
    endif

 else                         !land

!option to modify czil according to Chen & Zhang (2009):
    if ( iz0tlnd2 .eq. 1 ) then
       czil = 10.0 ** ( -0.40 * ( z_0 / 0.07 ) )
    else
       czil = 0.085 !0.075 !0.10
    end if

    zt = z_0*exp(-karman*czil*sqrt(restar))
    zt = min( zt, 0.75*z_0)

    zq = z_0*exp(-karman*czil*sqrt(restar))
    zq = min( zq, 0.75*z_0)

!stochastically perturb thermal and moisture roughness length.
!currently set to half the amplitude:
    if (spp_pbl) then
       zt = zt + zt * 0.5 * rstoch
       zt = max(zt, 0.0001)
       zq = zt
    endif

 endif
                   
 end subroutine zilitinkevich_1995

!=================================================================================================================
 subroutine davis_etal_2008(Z_0,ustar)
!a.k.a. : Donelan et al. (2004)
!this formulation for roughness length was designed to match
!the labratory experiments of Donelan et al. (2004).
!this is an update version from Davis et al. 2008, which
!corrects a small-bias in Z_0 (AHW real-time 2012).
 implicit none
!=================================================================================================================

!--- input arguments:
 real(kind=kind_phys),intent(in):: ustar

!--- output arguments:
 real(kind=kind_phys),intent(out):: z_0

!--- local variables:
 real(kind=kind_phys):: zw, zn1, zn2
 real(kind=kind_phys),parameter:: g=9.81,ozo=1.59e-5

!-----------------------------------------------------------------------------------------------------------------

!old form: z_0 = 10.*exp(-10./(ustar**(1./3.)))
!new form:

 zw  = min((ustar/1.06)**(0.3),1.0)
 zn1 = 0.011*ustar*ustar/g + ozo
 zn2 = 10.*exp(-9.5*ustar**(-.3333)) + &
       0.11*1.5e-5/amax1(ustar,0.01)
 z_0 = (1.0-zw) * zn1 + zw * zn2

 z_0 = max( z_0, 1.27e-7)  !these max/mins were suggested by
 z_0 = min( z_0, 2.85e-3)  !Davis et al. (2008)
                   
 end subroutine davis_etal_2008

!=================================================================================================================
 subroutine taylor_yelland_2001(z_0,ustar,wsp10)
!this formulation for roughness length was designed account for
!wave steepness.
 implicit none
!=================================================================================================================

!--- input arguments:
 real(kind=kind_phys),intent(in):: ustar,wsp10

!--- output arguments:
 real(kind=kind_phys),intent(out):: z_0

!--- local variables:
 real(kind=kind_phys),parameter:: g=9.81, pi=3.14159265
 real(kind=kind_phys):: hs, tp, lp

!-----------------------------------------------------------------------------------------------------------------

!hs is the significant wave height
 hs = 0.0248*(wsp10**2.)
!Tp dominant wave period
 tp = 0.729*max(wsp10,0.1)
!lp is the wavelength of the dominant wave
 lp = g*tp**2/(2*pi)

 z_0 = 1200.*hs*(hs/lp)**4.5
 z_0 = max( z_0, 1.27e-7)  !these max/mins were suggested by
 z_0 = min( z_0, 2.85e-3)  !Davis et al. (2008)
                   
 end subroutine taylor_yelland_2001

!=================================================================================================================
 subroutine charnock_1955(Z_0,ustar,wsp10,visc,zu)
!This version of Charnock's relation employs a varying
!Charnock parameter, similar to COARE3.0 [Fairall et al. (2003)].
!The Charnock parameter CZC is varied from .011 to .018
!between 10-m wsp = 10 and 18.
 implicit none
!=================================================================================================================

!--- input arguments:
 real(kind=kind_phys),intent(in):: ustar,visc,wsp10,zu

!--- output arguments:
 real(kind=kind_phys),intent(out):: z_0

!--- local variables:
 real(kind=kind_phys),parameter:: G=9.81, CZO2=0.011
 real(kind=kind_phys):: czc    !variable charnock "constant"
 real(kind=kind_phys):: wsp10m ! logarithmically calculated 10 m

!-----------------------------------------------------------------------------------------------------------------

 wsp10m = wsp10*log(10./1e-4)/log(zu/1e-4)
 czc = czo2 + 0.007*min(max((wsp10m-10.)/8., 0.), 1.0)

 z_0 = czc*ustar*ustar/g + (0.11*visc/max(ustar,0.05))
 z_0 = max( z_0, 1.27e-7)  !these max/mins were suggested by
 z_0 = min( z_0, 2.85e-3)  !Davis et al. (2008)

 end subroutine charnock_1955

!=================================================================================================================
 subroutine edson_etal_2013(z_0,ustar,wsp10,visc,zu)
!This version of Charnock's relation employs a varying
!Charnock parameter, taken from COARE 3.5 [Edson et al. (2001, JPO)].
!The Charnock parameter CZC is varied from about .005 to .028
!between 10-m wind speeds of 6 and 19 m/s.
!11 Nov 2021: Note that this was finally fixed according to the
!             Edson et al (2014) corrigendum, where "m" was corrected.
 implicit none
!=================================================================================================================

!--- input arguments:
 real(kind=kind_phys),intent(in):: ustar,visc,wsp10,zu

!--- output arguments:
 real(kind=kind_phys),intent(out):: z_0

!--- local variables:
 real(kind=kind_phys),parameter:: g=9.81
 real(kind=kind_phys),parameter:: m=0.0017, b=-0.005
 real(kind=kind_phys):: czc    ! variable charnock "constant"
 real(kind=kind_phys):: wsp10m ! logarithmically calculated 10 m

!-----------------------------------------------------------------------------------------------------------------

 wsp10m = wsp10*log(10/1e-4)/log(zu/1e-4)
 wsp10m = min(19.,wsp10m)
 czc    = m*wsp10m + b
 czc    = max(czc, 0.0)

 z_0 = czc*ustar*ustar/g + (0.11*visc/max(ustar,0.07))
 z_0 = max( z_0, 1.27e-7)  !These max/mins were suggested by
 z_0 = min( z_0, 2.85e-3)  !Davis et al. (2008)

 end subroutine edson_etal_2013

!=================================================================================================================
 subroutine garratt_1992(zt,zq,z_0,ren,landsea)
!This formulation for the thermal and moisture roughness lengths
!(Zt and Zq) relates them to Z0 via the roughness Reynolds number (Ren).
!This formula comes from Fairall et al. (2003). It is modified from
!the original Garratt-Brutsaert model to better fit the COARE/HEXMAX
!data. The formula for land uses a constant ratio (Z_0/7.4) taken
!from Garratt (1992).
 implicit none
!=================================================================================================================

!--- input arguments:
 real(kind=kind_phys),intent(in):: ren, z_0,landsea

!--- output arguments:
 real(kind=kind_phys),intent(out):: zt,zq

!--- local variables:
 real(kind=kind_phys):: rq
 real(kind=kind_phys),parameter:: e=2.71828183

!-----------------------------------------------------------------------------------------------------------------

 if (landsea-1.5 .gt. 0) then !water

    zt = z_0*exp(2.0 - (2.48*(ren**0.25)))
    zq = z_0*exp(2.0 - (2.28*(ren**0.25)))

    zq = min( zq, 5.5e-5)
    zq = max( zq, 2.0e-9)
    zt = min( zt, 5.5e-5)
    zt = max( zt, 2.0e-9)     !same lower limit as ecmwf

 else                         !land

    zq = z_0/(e**2.)          !taken from Garratt (1980,1992)
    zt = zq

 endif

 end subroutine garratt_1992

!=================================================================================================================
 subroutine fairall_etal_2003(zt,zq,ren,ustar,visc,rstoch,spp_pbl)
!This formulation for thermal and moisture roughness length (Zt and Zq)
!as a function of the roughness Reynolds number (Ren) comes from the
!COARE3.0 formulation, empirically derived from COARE and HEXMAX data
![Fairall et al. (2003)]. Edson et al. (2004; JGR) suspected that this
!relationship overestimated the scalar roughness lengths for low Reynolds
!number flows, so an optional smooth flow relationship, taken from Garratt
!(1992, p. 102), is available for flows with Ren < 2.
!
!This is for use over water only.
 implicit none
!=================================================================================================================

!--- input arguments:
 logical,intent(in):: spp_pbl
 real(kind=kind_phys),intent(in):: ren,ustar,visc,rstoch

!--- output arguments:
 real(kind=kind_phys),intent(out):: zt,zq

!-----------------------------------------------------------------------------------------------------------------

 if (ren .le. 2.) then

    zt = (5.5e-5)*(ren**(-0.60))
    zq = zt
    !for smooth seas, can use Garratt
    !zq = 0.2*visc/max(ustar,0.1)
    !zq = 0.3*visc/max(ustar,0.1)

 else

    !for rough seas, use coare
    zt = (5.5e-5)*(ren**(-0.60))
    zq = zt

 endif

 if (spp_pbl) then
    zt = zt + zt * 0.5 * rstoch
    zq = zt
 endif

 zt = min(zt,1.0e-4)
 zt = max(zt,2.0e-9)

 zq = min(zt,1.0e-4)
 zq = max(zt,2.0e-9)

 end subroutine fairall_etal_2003

!=================================================================================================================
 subroutine fairall_etal_2014(zt,zq,ren,ustar,visc,rstoch,spp_pbl)
!This formulation for thermal and moisture roughness length (Zt and Zq)
!as a function of the roughness Reynolds number (Ren) comes from the
!COARE 3.5/4.0 formulation, empirically derived from COARE and HEXMAX data
![Fairall et al. (2014? coming soon, not yet published as of July 2014)].
!This is for use over water only.
 implicit none
!=================================================================================================================

!--- input arguments:
 logical,intent(in):: spp_pbl
 real(kind=kind_phys),intent(in):: ren,ustar,visc,rstoch

!--- output arguments:
 real(kind=kind_phys),intent(out):: Zt,Zq

!-----------------------------------------------------------------------------------------------------------------

!zt = (5.5e-5)*(ren**(-0.60))
 zt = min(1.6e-4, 5.8e-5/(ren**0.72))
 zq = zt

 if (spp_pbl) then
    zt = max(zt + zt*0.5*rstoch,2.0e-9)
    zq = max(zt + zt*0.5*rstoch,2.0e-9)
 else
    zt = max(zt,2.0e-9)
    zq = max(zt,2.0e-9)
 endif


 end subroutine fairall_etal_2014

!=================================================================================================================
 subroutine yang_2008(z_0,zt,zq,ustar,tstar,qst,ren,visc,landsea)
!This is a modified version of Yang et al (2002 QJRMS, 2008 JAMC)
!and Chen et al (2010, J of Hydromet). Although it was originally
!designed for arid regions with bare soil, it is modified
!here to perform over a broader spectrum of vegetation.
!
!The original formulation relates the thermal roughness length (Zt)
!to u* and T*:
!
! Zt = ht * EXP(-beta*(ustar**0.5)*(ABS(tstar)**0.25))
!
!where ht = Renc*visc/ustar and the critical Reynolds number
!(Renc) = 70. Beta was originally = 10 (2002 paper) but was revised
!to 7.2 (in 2008 paper). Their form typically varies the
!ratio Z0/Zt by a few orders of magnitude (1-1E4).
!
!This modified form uses beta = 1.5 and a variable Renc (function of Z_0),
!so zt generally varies similarly to the Zilitinkevich form (with Czil ~ 0.1)
!for very small or negative surface heat fluxes but can become close to the
!Zilitinkevich with Czil = 0.2 for very large HFX (large negative T*).
!Also, the exponent (0.25) on tstar was changed to 1.0, since we found
!Zt was reduced too much for low-moderate positive heat fluxes.
!
!This should only be used over land!
 implicit none
!=================================================================================================================

!--- input arguments:
 real(kind=kind_phys),intent(in):: z_0,ren,ustar,tstar,qst,visc,landsea

!--- output arguments:
 real(kind=kind_phys),intent(out):: zt,zq

!--- local variables:
 real(kind=kind_phys):: ht,     &! roughness height at critical Reynolds number
                        tstar2, &! bounded T*, forced to be non-positive
                        qstar2, &! bounded q*, forced to be non-positive
                        z_02,   &! bounded Z_0 for variable Renc2 calc
                        renc2    ! variable Renc, function of Z_0

 real(kind=kind_phys),parameter:: renc=300., & !old constant Renc
                                  beta=1.5,  & !important for diurnal variation
                                  m=170.,    & !slope for Renc2 function
                                  b=691.       !y-intercept for Renc2 function

!-----------------------------------------------------------------------------------------------------------------

 z_02 = min(z_0,0.5)
 z_02 = max(z_02,0.04)
 renc2= b + m*log(z_02)
 ht     = renc2*visc/max(ustar,0.01)
 tstar2 = min(tstar, 0.0)
 qstar2 = min(qst,0.0)

 zt     = ht * exp(-beta*(ustar**0.5)*(abs(tstar2)**1.0))
 zq     = ht * exp(-beta*(ustar**0.5)*(abs(qstar2)**1.0))
!zq     = zt

 zt = min(zt, z_0/2.0)
 zq = min(zq, z_0/2.0)

 end subroutine yang_2008

!=================================================================================================================
 subroutine andreas_2002(z_0,bvisc,ustar,zt,zq)
! This is taken from Andreas (2002; J. of Hydromet) and
! Andreas et al. (2005; BLM).
!
! This should only be used over snow/ice!
 implicit none
!=================================================================================================================

!--- input arguments:
 real(kind=kind_phys),intent(in):: z_0,bvisc,ustar

!--- output arguments:
 real(kind=kind_phys),intent(out):: zt, zq

!--- local variables:
 real(kind=kind_phys):: ren2,zntsno

 real(kind=kind_phys),parameter:: bt0_s=1.25,  bt0_t=0.149,  bt0_r=0.317,  &
                                  bt1_s=0.0,   bt1_t=-0.55,  bt1_r=-0.565, &
                                  bt2_s=0.0,   bt2_t=0.0,    bt2_r=-0.183

 real(kind=kind_phys),parameter:: bq0_s=1.61,  bq0_t=0.351,  bq0_r=0.396,  &
                                  bq1_s=0.0,   bq1_t=-0.628, bq1_r=-0.512, &
                                  bq2_s=0.0,   bq2_t=0.0,    bq2_r=-0.180

!-----------------------------------------------------------------------------------------------------------------

!calculate zo for snow (Andreas et al. 2005, BLM):
 zntsno = 0.135*bvisc/ustar + &
          (0.035*(ustar*ustar)/9.8) * &
          (5.*exp(-1.*(((ustar - 0.18)/0.1)*((ustar - 0.18)/0.1))) + 1.)
 ren2 = ustar*zntsno/bvisc

!Make sure that Re is not outside of the range of validity
!for using their equations
 if (ren2 .gt. 1000.) ren2 = 1000.

 if (ren2 .le. 0.135) then

    zt = zntsno*exp(bt0_s + bt1_s*log(ren2) + bt2_s*log(ren2)**2)
    zq = zntsno*exp(bq0_s + bq1_s*log(ren2) + bq2_s*log(ren2)**2)

 else if (ren2 .gt. 0.135 .and. ren2 .lt. 2.5) then

    zt = zntsno*exp(bt0_t + bt1_t*log(ren2) + bt2_t*log(ren2)**2)
    zq = zntsno*exp(bq0_t + bq1_t*log(ren2) + bq2_t*log(ren2)**2)

 else

    zt = zntsno*exp(bt0_r + bt1_r*log(ren2) + bt2_r*log(ren2)**2)
    zq = zntsno*exp(bq0_r + bq1_r*log(ren2) + bq2_r*log(ren2)**2)

 endif

 end subroutine andreas_2002

!=================================================================================================================
 subroutine psi_hogstrom_1996(psi_m,psi_h,zl,zt,z_0,za)
!this subroutine returns the stability functions based off
!of hogstrom (1996).
 implicit none
!=================================================================================================================

!--- input arguments:
 real(kind=kind_phys),intent(in):: zl,zt,z_0,za

!--- output arguments:
 real(kind=kind_phys),intent(out):: psi_m,psi_h

!--- local variables:
 real(kind=kind_phys):: x,x0,y,y0,zml,zhl

!-----------------------------------------------------------------------------------------------------------------

 zml = z_0*zl/za
 zhl = zt*zl/za

 if (zl .gt. 0.) then  !stable (not well tested - seem large)

    psi_m = -5.3*(zl - zml)
    psi_h = -8.0*(zl - zhl)
 
 else                  !unstable

    x = (1.-19.0*zl)**0.25
    x0= (1.-19.0*zml)**0.25
    y = (1.-11.6*zl)**0.5
    y0= (1.-11.6*zhl)**0.5

    psi_m = 2.*log((1.+x)/(1.+x0)) + &
            &log((1.+x**2.)/(1.+x0**2.)) - &
            &2.0*atan(x) + 2.0*atan(x0)
    psi_h = 2.*log((1.+y)/(1.+y0))

 endif
                   
 end subroutine psi_hogstrom_1996

!=================================================================================================================
 subroutine psi_dyerhicks(psi_m,psi_h,zl,zt,z_0,za)
!This subroutine returns the stability functions based off
!of Hogstrom (1996), but with different constants compatible
!with Dyer and Hicks (1970/74?). This formulation is used for
!testing/development by Nakanishi (personal communication).
 implicit none
!=================================================================================================================

!--- input arguments:
 real(kind=kind_phys),intent(in):: zl,zt,z_0,za

!--- output arguments:
 real(kind=kind_phys),intent(out):: psi_m,psi_h

!--- local variables:
 real(kind=kind_phys):: x,x0,y,y0,zml,zhl

!-----------------------------------------------------------------------------------------------------------------

 zml = z_0*zl/za  !zo/l
 zhl = zt*zl/za   !zt/l

 if (zl .gt. 0.) then  !stable

    psi_m = -5.0*(zl - zml)
    psi_h = -5.0*(zl - zhl)
 
 else                  !unstable

    x = (1.-16.*zl)**0.25
    x0= (1.-16.*zml)**0.25

    y = (1.-16.*zl)**0.5
    y0= (1.-16.*zhl)**0.5

    psi_m = 2.*log((1.+x)/(1.+x0)) + &
            &log((1.+x**2.)/(1.+x0**2.)) - &
            &2.0*atan(x) + 2.0*atan(x0)
    psi_h = 2.*log((1.+y)/(1.+y0))

 endif
                   
 end subroutine psi_dyerhicks

!=================================================================================================================
 subroutine psi_beljaars_holtslag_1991(psi_m,psi_h,zl)
!this subroutine returns the stability functions based off
!of Beljaar and Holtslag 1991, which is an extension of Holtslag
!and Debruin 1989.
 implicit none
!=================================================================================================================

!--- input arguments:
 real(kind=kind_phys),intent(in):: zl

!--- output arguments:
 real(kind=kind_phys),intent(out):: psi_m, psi_h

!--- local variables:
 real(kind=kind_phys):: a=1.,b=0.666,c=5.,d=0.35

!-----------------------------------------------------------------------------------------------------------------

 if (zl .lt. 0.) then  !unstable

    write(*,*)"WARNING: Universal stability functions from"
    write(*,*)"         Beljaars and Holtslag (1991) should only"
    write(*,*)"         be used in the stable regime!"
    psi_m = 0.
    psi_h = 0.
 
 else                  !stable

    psi_m = -(a*zl + b*(zl -(c/d))*exp(-d*zl) + (b*c/d))
    psi_h = -((1.+.666*a*zl)**1.5 + &
             b*(zl - (c/d))*exp(-d*zl) + (b*c/d) -1.)

 endif
                   
 end subroutine psi_beljaars_holtslag_1991

!=================================================================================================================
 subroutine psi_zilitinkevich_esau_2007(psi_m,psi_h,zl)
!this subroutine returns the stability functions come from
!Zilitinkevich and Esau (2007, BM), which are formulatioed from the
!"generalized similarity theory" and tuned to the LES DATABASE64
!to determine their dependence on z/L.
 IMPLICIT NONE
!=================================================================================================================

!--- input arguments:
 real(kind=kind_phys),intent(in):: zl

!--- output arguments:
 real(kind=kind_phys),intent(out):: psi_m, psi_h

!--- local variables:
 real(kind=kind_phys),parameter:: cm=3.0,ct=2.5

!-----------------------------------------------------------------------------------------------------------------

 if (zl .lt. 0.) then  !unstable

!   write(*,*)"WARNING: Universal stability function from"
!   write(*,*)"         Zilitinkevich and Esau (2007) should only"
!   write(*,*)"         be used in the stable regime!"
    psi_m = 0.
    psi_h = 0.
 
 else                  !stable

    psi_m = -cm*(zl**(5./6.))
    psi_h = -ct*(zl**(4./5.))

 endif
                   
 end subroutine psi_zilitinkevich_esau_2007

!=================================================================================================================
 subroutine psi_businger_1971(psi_m,psi_h,zl)
!this subroutine returns the flux-profile relationships
!of Businger el al. 1971.
 implicit none
!=================================================================================================================

!--- input arguments:
 real(kind=kind_phys),intent(in):: zl

!--- output arguments:
 real(kind=kind_phys),intent(out):: psi_m, psi_h

!--- local variables:
 real(kind=kind_phys):: x, y
 real(kind=kind_phys),parameter::  pi180 = 3.14159265/180.

!-----------------------------------------------------------------------------------------------------------------

 if (zl .lt. 0.) then  !unstable

    x = (1. - 15.0*zl)**0.25
    y = (1. - 9.0*zl)**0.5

    psi_m = log(((1.+x)/2.)**2.) + &
          & log((1.+x**2.)/2.) -   &
          & 2.0*atan(x) + pi180*90.
    psi_h = 2.*log((1.+y)/2.)

 else                  !stable

    psi_m = -4.7*zl
    psi_h = -(4.7/0.74)*zl

 endif

 end subroutine psi_businger_1971

!=================================================================================================================
 subroutine psi_suselj_sood_2010(psi_m,psi_h,zl)
!this subroutine returns flux-profile relatioships based off
!of Lobocki (1993), which is derived from the MY-level 2 model.
!Suselj and Sood (2010) applied the surface layer length scales
!from Nakanishi (2001) to get this new relationship. These functions
!are more agressive (larger magnitude) than most formulations. They
!showed improvement over water, but untested over land.
 implicit none
!=================================================================================================================

!--- input arguments:
 real(kind=kind_phys),intent(in):: zl

!--- output arguments:
 real(kind=kind_phys),intent(out):: psi_m, psi_h

!--- local variables:
 real(kind=kind_phys),parameter:: rfc=0.19, ric=0.183, phit=0.8

!-----------------------------------------------------------------------------------------------------------------

 if (zl .gt. 0.) then  !stable

    psi_m = -(zl/rfc + 1.1223*exp(1.-1.6666/zl))
    !psi_h = -zl*ric/((rfc**2.)*phit) + 8.209*(zl**1.1091)
    !their eq for psi_h crashes the model and does not match
    !their fig 1. this eq (below) matches their fig 1 better:
    psi_h = -(zl*ric/((rfc**2.)*5.) + 7.09*(zl**1.1091))
 
 else                  !unstable

     psi_m = 0.9904*log(1. - 14.264*zl)
     psi_h = 1.0103*log(1. - 16.3066*zl)

 endif

 end subroutine psi_suselj_sood_2010

!=================================================================================================================
 subroutine psi_cb2005(psim1,psih1,zl,z0l)
!this subroutine returns the stability functions based off
!of Cheng and Brutseart (2005, BLM), for use in stable conditions only.
!the returned values are the combination of psi((za+zo)/L) - psi(z0/L)
 implicit none
!=================================================================================================================

!--- input arguments:
 real(kind=kind_phys),intent(in):: zl,z0l

!--- output arguments:
 real(kind=kind_phys),intent(out):: psim1,psih1

!-----------------------------------------------------------------------------------------------------------------

 psim1 = -6.1*log(zl  + (1.+ zl **2.5)**0.4) &
         -6.1*log(z0l + (1.+ z0l**2.5)**0.4)
 psih1 = -5.5*log(zl  + (1.+ zl **1.1)**0.90909090909) &
         -5.5*log(z0l + (1.+ z0l**1.1)**0.90909090909)

 end subroutine psi_cb2005

!=================================================================================================================
 subroutine li_etal_2010(zl,rib,zaz0,z0zt)
!this subroutine returns a more robust z/l that best matches
!the z/l from hogstrom (1996) for unstable conditions and beljaars
!and holtslag (1991) for stable conditions.
 implicit none
!=================================================================================================================

!--- input arguments:
 real(kind=kind_phys),intent(in):: rib,zaz0,z0zt

!--- output arguments:
 real(kind=kind_phys),intent(out):: zl

!--- local variables:
 real(kind=kind_phys):: alfa,beta,zaz02,z0zt2

 real(kind=kind_phys),parameter:: au11=0.045, bu11=0.003, bu12=0.0059,    &
                                 &bu21=-0.0828, bu22=0.8845, bu31=0.1739, &
                                 &bu32=-0.9213, bu33=-0.1057
 real(kind=kind_phys),parameter:: aw11=0.5738, aw12=-0.4399, aw21=-4.901, &
                                 &aw22=52.50, bw11=-0.0539, bw12=1.540,   &
                                 &bw21=-0.669, bw22=-3.282
 real(kind=kind_phys),parameter:: as11=0.7529, as21=14.94, bs11=0.1569,   &
                                 &bs21=-0.3091, bs22=-1.303
          
!-----------------------------------------------------------------------------------------------------------------

!set limits according to Li et al (2010), p 157.
 zaz02=zaz0
 if (zaz0 .lt. 100.0) zaz02=100.
 if (zaz0 .gt. 100000.0) zaz02=100000.

!set more limits according to Li et al (2010)
 z0zt2=z0zt
 if (z0zt .lt. 0.5) z0zt2=0.5
 if (z0zt .gt. 100.0) z0zt2=100.

 alfa = log(zaz02)
 beta = log(z0zt2)

 if (rib .le. 0.0) then
    zl = au11*alfa*rib**2 + (             &
   &  (bu11*beta + bu12)*alfa**2 +        &
   &  (bu21*beta + bu22)*alfa    +        &
   &  (bu31*beta**2 + bu32*beta + bu33))*rib

    !if(zL .LT. -15 .OR. zl .GT. 0.)print*,"VIOLATION Rib<0:",zL
    zl = max(zl,-15.) !limits set according to Li et al (2010)
    zl = min(zl,0.)   !Figure 1.
 elseif (rib .gt. 0.0 .and. rib .le. 0.2) then
    zl = ((aw11*beta + aw12)*alfa +             &
   &  (aw21*beta + aw22))*rib**2 +          &
   & ((bw11*beta + bw12)*alfa +             &
   &  (bw21*beta + bw22))*rib

    !if(zl .lt. 0 .or. zl .gt. 4)print*,"violation 0<rib<0.2:",zl
    zl = min(zl,4.) !limits approx set according to Li et al (2010)
    zl = max(zl,0.) !their Figure 1b.
 else
    zl = (as11*alfa + as21)*rib + bs11*alfa +   &
   &  bs21*beta + bs22

    !if(zl .le. 1 .or. zl .gt. 23)print*,"violation rib>0.2:",zl
     zl = min(zl,20.) !limits according to Li et al (2010), their Figure 1c.
     zl = max(zl,1.)
 endif

 end subroutine li_etal_2010

!=================================================================================================================
 real(kind=kind_phys) function zolri(ri,za,z0,zt,zol1,psi_opt)
 implicit none
! This iterative algorithm is a two-point secant method taken from the revised
! surface layer scheme in WRF-ARW, written by Pedro Jimenez and Jimy Dudhia and
! summarized in Jimenez et al. (2012, MWR). This function was adapted
! to input the thermal roughness length, zt, (as well as z0) and use initial
! estimate of z/L.
!=================================================================================================================

!--- input arguments:
 integer, intent(in):: psi_opt
 real(kind=kind_phys),intent(in):: ri,za,z0,zt,zol1

!--- local variables and arrays:
 integer:: n
 integer,parameter:: nmax = 20
 real(kind=kind_phys):: x1,x2,fx1,fx2

!-----------------------------------------------------------------------------------------------------------------

 if (ri.lt.0.)then
    x1=zol1 - 0.02  !-5.
    x2=0.
 else
    x1=0.
    x2=zol1 + 0.02 !5.
 endif

 n=0
 fx1=zolri2(x1,ri,za,z0,zt,psi_opt)
 fx2=zolri2(x2,ri,za,z0,zt,psi_opt)
        
 do while (abs(x1 - x2) > 0.01 .and. n < nmax)
    if(abs(fx2) .lt. abs(fx1))then
       x1=x1-fx1/(fx2-fx1)*(x2-x1)
       fx1=zolri2(x1,ri,za,z0,zt,psi_opt)
       zolri=x1
    else
       x2=x2-fx2/(fx2-fx1)*(x2-x1)
       fx2=zolri2(x2,ri,za,z0,zt,psi_opt)
       zolri=x2
    endif
    n=n+1
 enddo

 if (n==nmax .and. abs(x1 - x2) >= 0.01) then
    !if convergence fails, use approximate values:
    call li_etal_2010(zolri, ri, za/z0, z0/zt)
    !print*,"failed, n=",n," ri=",ri," zt=",zt
 else
    !print*,"success,n=",n," ri=",ri," z/l=",zolri
 endif

 end function zolri

!=================================================================================================================
 real(kind=kind_phys) function zolri2(zol2,ri2,za,z0,zt,psi_opt)
 implicit none
! input: =================================
! zol2 - estimated z/l
! ri2  - calculated bulk richardson number
! za   - 1/2 depth of first model layer
! z0   - aerodynamic roughness length
! zt   - thermal roughness length
! output: ================================
! zolri2 - delta ri
!=================================================================================================================

!--- input arguments:
 integer,intent(in):: psi_opt
 real(kind=kind_phys),intent(in):: ri2,za,z0,zt

!--- inout arguments:
 real(kind=kind_phys),intent(inout):: zol2

!--- local variables and arrays:
 real(kind=kind_phys):: zol20,zol3,psim1,psih1,psix2,psit2,zolt

!-----------------------------------------------------------------------------------------------------------------

 if(zol2*ri2 .lt. 0.) then
    !print*,"wrong quadrants: z/l=",zol2," ri=",ri2
    zol2=0.
 endif

 zol20=zol2*z0/za ! z0/l
 zol3=zol2+zol20  ! (z+z0)/l
 zolt=zol2*zt/za  ! zt/l

 if (ri2.lt.0) then
    psit2=max(log((za+z0)/zt)-(psih_unstable(zol3,psi_opt)-psih_unstable(zolt,psi_opt)), 1.0)
    psix2=max(log((za+z0)/z0)-(psim_unstable(zol3,psi_opt)-psim_unstable(zol20,psi_opt)),1.0)
 else
    psit2=max(log((za+z0)/zt)-(psih_stable(zol3,psi_opt)-psih_stable(zolt,psi_opt)), 1.0)
    psix2=max(log((za+z0)/z0)-(psim_stable(zol3,psi_opt)-psim_stable(zol20,psi_opt)),1.0)
 endif

 zolri2=zol2*psit2/psix2**2 - ri2
!print*,"  target ri=",ri2," est ri=",zol2*psit2/psix2**2

 end function zolri2

!=================================================================================================================
 real(kind=kind_phys) function zolrib(ri,za,z0,zt,logz0,logzt,zol1,psi_opt)
 implicit none
!this iterative algorithm to compute z/L from bulk-Ri
!=================================================================================================================

!--- input arguments:
 integer,intent(in):: psi_opt
 real(kind=kind_phys),intent(in):: ri,za,z0,zt,logz0,logzt

!--- inout arguments:
 real(kind=kind_phys),intent(inout):: zol1

!--- local variables and arrays:
 integer:: n
 integer,parameter :: nmax = 20
 real(kind=kind_phys):: zol20,zol3,zolt,zolold
 real(kind=kind_phys):: psit2,psix2
!real(kind=kind_phys),dimension(nmax):: zlhux

!-----------------------------------------------------------------------------------------------------------------

 if(zol1*ri .lt. 0.) then
!   print*,"WRONG QUADRANTS: z/L=",zol1," ri=",ri
    zol1=0.
 endif

 if (ri .lt. 0.) then
    zolold=-99999.
    zolrib=-66666.
 else
    zolold=99999.
    zolrib=66666.
 endif

 n=1
 do while (abs(zolold - zolrib) > 0.01 .and. n < nmax)

    if(n==1)then
       zolold=zol1
    else
       zolold=zolrib
    endif
    zol20=zolold*z0/za ! z0/L
    zol3=zolold+zol20  ! (z+z0)/L
    zolt=zolold*zt/za  ! zt/L

    if (ri.lt.0) then
       psit2=MAX(logzt-(psih_unstable(zol3,psi_opt)-psih_unstable(zolt,psi_opt)), 1.0)
       psix2=MAX(logz0-(psim_unstable(zol3,psi_opt)-psim_unstable(zol20,psi_opt)), 1.0)
    else
       psit2=MAX(logzt-(psih_stable(zol3,psi_opt)-psih_stable(zolt,psi_opt)), 1.0)
       psix2=MAX(logz0-(psim_stable(zol3,psi_opt)-psim_stable(zol20,psi_opt)), 1.0)
    endif

    zolrib=ri*psix2**2/psit2
    !zLhux(n)=zolrib
    n=n+1
 enddo

 if (n==nmax .and. abs(zolold - zolrib) > 0.01 ) then
    !print*,"iter FAIL, n=",n," Ri=",ri," z/L=",zolri
    !if convergence fails, use approximate values:
    call li_etal_2010(zolrib,ri,za/z0,z0/zt)
    !zLhux(n)=zolri
    !print*,"FAILED, n=",n," Ri=",ri," zt=",zt
    !print*,"z/L=",zLhux(1:nmax)
 else
    !print*,"SUCCESS,n=",n," Ri=",ri," z/L=",zolrib
 endif

 end function zolrib

!=================================================================================================================
 subroutine psi_init(psi_opt)
 implicit none
!define tables from -10 <= z/L <= 10
!=================================================================================================================

 integer,intent(in):: psi_opt
 integer:: n
 real(kind=kind_phys):: zolf

!-----------------------------------------------------------------------------------------------------------------

 if (psi_opt == 0) then
    do n = 0,1000
       !stable function tables
       zolf = float(n)*0.01
       psim_stab(n)=psim_stable_full(zolf)
       psih_stab(n)=psih_stable_full(zolf)

       !unstable function tables
       zolf = -float(n)*0.01
       psim_unstab(n)=psim_unstable_full(zolf)
       psih_unstab(n)=psih_unstable_full(zolf)
    enddo
 else
    do n = 0,1000
       !stable function tables
       zolf = float(n)*0.01
       psim_stab(n)=psim_stable_full_gfs(zolf)
       psih_stab(n)=psih_stable_full_gfs(zolf)

       !unstable function tables
       zolf = -float(n)*0.01
       psim_unstab(n)=psim_unstable_full_gfs(zolf)
       psih_unstab(n)=psih_unstable_full_gfs(zolf)
    enddo
 endif

 end subroutine psi_init

!=================================================================================================================
! ... Full equations for the integrated similarity functions ...
!=================================================================================================================
 real(kind=kind_phys) function psim_stable_full(zolf)
 implicit none

 real(kind=kind_phys),intent(in):: zolf

 psim_stable_full=-6.1*log(zolf+(1+zolf**2.5)**(1./2.5))

 end function psim_stable_full

!=================================================================================================================
 real(kind=kind_phys) function psih_stable_full(zolf)
 implicit none

 real(kind=kind_phys),intent(in):: zolf

 psih_stable_full=-5.3*log(zolf+(1+zolf**1.1)**(1./1.1))

 end function psih_stable_full

!=================================================================================================================
 real(kind=kind_phys) function psim_unstable_full(zolf)
 implicit none

 real(kind=kind_phys),intent(in):: zolf
 real(kind=kind_phys):: x,ym,psimc,psimk

 x=(1.-16.*zolf)**.25
 psimk=2*ALOG(0.5*(1+X))+ALOG(0.5*(1+X*X))-2.*ATAN(X)+2.*ATAN(1.)

 ym=(1.-10.*zolf)**0.33
 psimc=(3./2.)*log((ym**2.+ym+1.)/3.)-sqrt(3.)*ATAN((2.*ym+1)/sqrt(3.))+4.*ATAN(1.)/sqrt(3.)

 psim_unstable_full=(psimk+zolf**2*(psimc))/(1+zolf**2.)

 end function psim_unstable_full

!=================================================================================================================
 real(kind=kind_phys) function psih_unstable_full(zolf)
 implicit none

 real(kind=kind_phys),intent(in):: zolf
 real(kind=kind_phys):: y,yh,psihc,psihk

 y=(1.-16.*zolf)**.5
 psihk=2.*log((1+y)/2.)

 yh=(1.-34.*zolf)**0.33
 psihc=(3./2.)*log((yh**2.+yh+1.)/3.)-sqrt(3.)*ATAN((2.*yh+1)/sqrt(3.))+4.*ATAN(1.)/sqrt(3.)

 psih_unstable_full=(psihk+zolf**2*(psihc))/(1+zolf**2.)

 end function psih_unstable_full

!=================================================================================================================
! ... integrated similarity functions from GFS...
!
!=================================================================================================================
 real(kind=kind_phys) function psim_stable_full_gfs(zolf)
 implicit none

 real(kind=kind_phys),intent(in):: zolf
 real(kind=kind_phys):: aa
 real(kind=kind_phys),parameter:: alpha4 = 20.

 aa = sqrt(1. + alpha4 * zolf)
 psim_stable_full_gfs  = -1.*aa + log(aa + 1.)

 end function psim_stable_full_gfs

!=================================================================================================================
 real(kind=kind_phys) function psih_stable_full_gfs(zolf)
 implicit none

 real(kind=kind_phys):: zolf
 real(kind=kind_phys):: bb
 real(kind=kind_phys),parameter:: alpha4 = 20.

 bb = sqrt(1. + alpha4 * zolf)
 psih_stable_full_gfs  = -1.*bb + log(bb + 1.)

 end function psih_stable_full_gfs

!=================================================================================================================
 real(kind=kind_phys) function psim_unstable_full_gfs(zolf)
 implicit none

 real(kind=kind_phys),intent(in):: zolf
 real(kind=kind_phys):: hl1,tem1
 real(kind=kind_phys),parameter:: a0=-3.975, a1=12.32,  &
                                  b1=-7.755, b2=6.041

 if (zolf .ge. -0.5) then
    hl1 = zolf
    psim_unstable_full_gfs  = (a0  + a1*hl1)  * hl1   / (1.+ (b1+b2*hl1)  *hl1)
 else
    hl1  = -zolf
    tem1 = 1.0 / sqrt(hl1)
    psim_unstable_full_gfs  = log(hl1) + 2. * sqrt(tem1) - .8776
 end if

 end function psim_unstable_full_gfs

!=================================================================================================================
 real(kind=kind_phys) function psih_unstable_full_gfs(zolf)
 implicit none

 real(kind=kind_phys),intent(in):: zolf
 real(kind=kind_phys):: hl1,tem1
 real(kind=kind_phys),parameter:: a0p=-7.941, a1p=24.75, &
                                  b1p=-8.705, b2p=7.899

 if (zolf .ge. -0.5) then
    hl1 = zolf
    psih_unstable_full_gfs  = (a0p + a1p*hl1) * hl1   / (1.+ (b1p+b2p*hl1)*hl1)
 else
    hl1 = -zolf
    tem1 = 1.0 / sqrt(hl1)
    psih_unstable_full_gfs  = log(hl1) + .5 * tem1 + 1.386
 end if

 end function psih_unstable_full_gfs

!=================================================================================================================
! These functions use the look-up table functions when |z/L| <= 10
! but default to the full equations when |z/L| > 10.  
!=================================================================================================================
 real(kind=kind_phys) function psim_stable(zolf,psi_opt)
 implicit none

 integer,intent(in):: psi_opt
 integer:: nzol
 real(kind=kind_phys),intent(in):: zolf
 real(kind=kind_phys):: rzol

!-----------------------------------------------------------------------------------------------------------------

 nzol = int(zolf*100.)
 rzol = zolf*100. - nzol
 if(nzol+1 .le. 1000)then
    psim_stable = psim_stab(nzol) + rzol*(psim_stab(nzol+1)-psim_stab(nzol))
 else
    if (psi_opt == 0) then
       psim_stable = psim_stable_full(zolf)
    else
       psim_stable = psim_stable_full_gfs(zolf)
    endif
 endif

 end function psim_stable

!=================================================================================================================
 real(kind=kind_phys) function psih_stable(zolf,psi_opt)
 implicit none

 integer,intent(in):: psi_opt
 integer:: nzol
 real(kind=kind_phys),intent(in):: zolf
 real(kind=kind_phys):: rzol

!-----------------------------------------------------------------------------------------------------------------

 nzol = int(zolf*100.)
 rzol = zolf*100. - nzol
 if(nzol+1 .le. 1000)then
    psih_stable = psih_stab(nzol) + rzol*(psih_stab(nzol+1)-psih_stab(nzol))
 else
    if (psi_opt == 0) then
       psih_stable = psih_stable_full(zolf)
    else
       psih_stable = psih_stable_full_gfs(zolf)
    endif
 endif

 end function psih_stable

!=================================================================================================================
 real(kind=kind_phys) function psim_unstable(zolf,psi_opt)
 implicit none

 integer,intent(in):: psi_opt
 integer:: nzol
 real(kind=kind_phys),intent(in):: zolf
 real(kind=kind_phys):: rzol

!-----------------------------------------------------------------------------------------------------------------

 nzol = int(-zolf*100.)
 rzol = -zolf*100. - nzol
 if(nzol+1 .le. 1000)then
    psim_unstable = psim_unstab(nzol) + rzol*(psim_unstab(nzol+1)-psim_unstab(nzol))
 else
    if (psi_opt == 0) then
       psim_unstable = psim_unstable_full(zolf)
    else
       psim_unstable = psim_unstable_full_gfs(zolf)
    endif
 endif

 end function psim_unstable

!=================================================================================================================
 real(kind=kind_phys) function psih_unstable(zolf,psi_opt)
 implicit none

 integer,intent(in):: psi_opt
 integer:: nzol
 real(kind=kind_phys),intent(in):: zolf
 real(kind=kind_phys):: rzol

!-----------------------------------------------------------------------------------------------------------------

 nzol = int(-zolf*100.)
 rzol = -zolf*100. - nzol
 if(nzol+1 .le. 1000)then
    psih_unstable = psih_unstab(nzol) + rzol*(psih_unstab(nzol+1)-psih_unstab(nzol))
 else
    if (psi_opt == 0) then
       psih_unstable = psih_unstable_full(zolf)
    else
       psih_unstable = psih_unstable_full_gfs(zolf)
    endif
 endif

 end function psih_unstable

!=================================================================================================================
 end module sf_mynn
!=================================================================================================================

