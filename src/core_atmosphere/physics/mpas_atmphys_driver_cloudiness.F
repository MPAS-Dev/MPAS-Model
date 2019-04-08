! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!=================================================================================================================
 module mpas_atmphys_driver_cloudiness
 use mpas_kind_types
 use mpas_pool_routines,only: mpas_pool_type,mpas_pool_get_array,mpas_pool_get_config
 use mpas_timer,only: mpas_timer_start,mpas_timer_stop
 use mpas_log

 use mpas_atmphys_constants, only: ep_2
 use mpas_atmphys_vars
 use module_mp_thompson_cldfra3
 
 implicit none
 private
 public:: allocate_cloudiness,   &
          deallocate_cloudiness, &
          driver_cloudiness

!MPAS driver for parameterization of the diagnostic cloud fraction.
!Laura D. Fowler (send comments to laura@ucar.edu).
!2013-05-01.
!
! subroutines in mpas_atmphys_driver_cloudiness:
! ----------------------------------------------
! allocate_cloudiness  : allocate local arrays for parameterization of diagnostic cloudiness.
! deallocate_cloudiness: deallocate local arrays for parameterization of diagnostic cloudiness.
! cloudiness_from_MPAS : initialize local arrays.
! cloudiness_to_MPAS   : copy local arrays to MPAS arrays.
! driver_cloudiness    : main driver (called from subroutine physics_driver).
! calc_cldincidence    : calculates the cloud fraction as 0 or 1, depending on cloud condensates.
! calc_cldfraction     : calculates the cloud fraction as a function of the relative humidity.
!
! add-ons and modifications to sourcecode:
! ----------------------------------------
! * throughout the sourcecode, replaced all "var_struct" defined arrays by local pointers.
!   Laura D. Fowler (laura@ucar.edu) / 2014-04-22.
! * modified sourcecode to use pools.
!   Laura D. Fowler (laura@ucar.edu) / 2014-05-15.
! * added subroutine cal_cldfra3 which contains the calculation of the radiative cloud fraction in conjunction
!   with the Thompson cloud microphysics.
!   Laura D. Fowler (laura@ucar.edu) / 2016-06-04.
! * initialize the local "radiation" water vapor, cloud water, cloud ice, and snow mixing ratios. When using the
!   option "cld_fraction_thompson", the "radiative" cloud water and ice paths are updated in conjunction with
!   cloud formation, but changes to the cloud water and cloud ice mixing ratios only affect the long wave and
!   short wave radiation codes.
!   Laura D. Fowler (laura@ucar.edu) / 2016-07-05.
! * since we removed the local variable radt_cld_scheme from mpas_atmphys_vars.F, now defines radt_cld_scheme
!   as a pointer to config_radt_cld_scheme.
!   Laura D. Fowler (laura@ucar.edu) / 2017-02-16.
! * in subroutine cloudiness_to_MPAS, added the convective cloud fraction from deep and shallow convection to
!   the grid-scale cloud fraction. added the cloud water and ice mixing ratios from convection to the grid-scale
!   cloud water and ice mixing ratios used in the long-wave and short-wave radiation codes.
!   Laura D. Fowler (laura@ucar.edu) / 2018-03-22.
! * updated the call to subroutine cal_cldfra3, now that cal_cldfra3 has been updated to WRF-4.1
!   Laura D. Fowler (laura@ucar.edu) / 2019-04-08.


 contains


!=================================================================================================================
 subroutine allocate_cloudiness
!=================================================================================================================

 if(.not.allocated(dx_p)     ) allocate(dx_p(ims:ime,jms:jme)             ) 
 if(.not.allocated(xland_p)  ) allocate(xland_p(ims:ime,jms:jme)          )
 if(.not.allocated(cldfrac_p)) allocate(cldfrac_p(ims:ime,kms:kme,jms:jme))
 if(.not.allocated(qvrad_p)  ) allocate(qvrad_p(ims:ime,kms:kme,jms:jme)  )
 if(.not.allocated(qcrad_p)  ) allocate(qcrad_p(ims:ime,kms:kme,jms:jme)  )
 if(.not.allocated(qirad_p)  ) allocate(qirad_p(ims:ime,kms:kme,jms:jme)  )
 if(.not.allocated(qsrad_p)  ) allocate(qsrad_p(ims:ime,kms:kme,jms:jme)  )

 end subroutine allocate_cloudiness

!=================================================================================================================
 subroutine deallocate_cloudiness
!=================================================================================================================

 if(allocated(dx_p)     ) deallocate(dx_p     )
 if(allocated(xland_p)  ) deallocate(xland_p  )
 if(allocated(cldfrac_p)) deallocate(cldfrac_p)
 if(allocated(qvrad_p)  ) deallocate(qvrad_p  )
 if(allocated(qcrad_p)  ) deallocate(qcrad_p  )
 if(allocated(qirad_p)  ) deallocate(qirad_p  )
 if(allocated(qsrad_p)  ) deallocate(qsrad_p  )

 end subroutine deallocate_cloudiness

!=================================================================================================================
 subroutine cloudiness_from_MPAS(configs,mesh,diag_physics,sfc_input,its,ite)
!=================================================================================================================

!input arguments:
 integer,intent(in):: its,ite

!input and inout arguments:
 type(mpas_pool_type),intent(in)   :: configs
 type(mpas_pool_type),intent(in)   :: mesh
 type(mpas_pool_type),intent(in)   :: sfc_input

!inout arguments:
 type(mpas_pool_type),intent(inout):: diag_physics

!local variables and pointers:
 integer:: i,j,k

 real(kind=RKIND),pointer:: len_disp
 real(kind=RKIND),dimension(:),pointer:: areaCell,meshDensity
 real(kind=RKIND),dimension(:),pointer:: xland

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_len_disp',len_disp)

 call mpas_pool_get_array(mesh,'areaCell'   ,areaCell   )
 call mpas_pool_get_array(mesh,'meshDensity',meshDensity)

 call mpas_pool_get_array(sfc_input,'xland',xland)

 do j = jts,jte
    do i = its,ite
       dx_p(i,j)    = len_disp / meshDensity(i)**0.25
       xland_p(i,j) = xland(i)
    enddo

    !--- initialize the radiative cloud fraction and water vapor, cloud water, cloud ice,
    !    and snow mixing ratios:
    do k = kts,kte
    do i = its,ite
       qvrad_p(i,k,j)   = qv_p(i,k,j)
       qcrad_p(i,k,j)   = qc_p(i,k,j)
       qirad_p(i,k,j)   = qi_p(i,k,j)
       qsrad_p(i,k,j)   = qs_p(i,k,j)
       cldfrac_p(i,k,j) = 0._RKIND
    enddo
    enddo
 enddo

 end subroutine cloudiness_from_MPAS

!=================================================================================================================
 subroutine cloudiness_to_MPAS(cld_scheme,diag_physics,its,ite)
!=================================================================================================================

!input arguments:
 integer,intent(in):: its,ite

!inout arguments:
 type(mpas_pool_type),intent(inout):: diag_physics

!local variables and pointers:
 character(len=StrKIND),intent(in):: cld_scheme
 integer:: i,j,k

 real(kind=RKIND),dimension(:,:),pointer:: cldfrac,cldfrac_deepc,cldfrac_shalc
 real(kind=RKIND),dimension(:,:),pointer:: qc_cu,qi_cu
 real(kind=RKIND),dimension(:,:),allocatable:: cldfrac_cu

!-----------------------------------------------------------------------------------------------------------------

 nullify(cldfrac)
 nullify(cldfrac_deepc)
 nullify(cldfrac_shalc)
 call mpas_pool_get_array(diag_physics,'cldfrac',cldfrac)
 call mpas_pool_get_array(diag_physics,'cldfrac_deepc',cldfrac_deepc)
 call mpas_pool_get_array(diag_physics,'cldfrac_shalc',cldfrac_shalc)

 nullify(qc_cu)
 nullify(qi_cu)
 call mpas_pool_get_array(diag_physics,'qc_cu',qc_cu)
 call mpas_pool_get_array(diag_physics,'qi_cu',qi_cu)

!in most cases, when we compute the cloud fraction, we want to compute the cloud fraction for grid-scale
!processes. as the cloud fraction from convective parameterizations is not always available, we query if
!it is available after computing the grid-scale cloud fraction:
 if(associated(cldfrac)) then
    do j = jts,jte
       do k = kts,kte
       do i = its,ite
          cldfrac(k,i) = cldfrac_p(i,k,j)
       enddo
       enddo
    enddo
 else
    do j = jts,jte
       do k = kts,kte
       do i = its,ite
          cldfrac(k,i) = 0._RKIND
       enddo
       enddo
    enddo
 endif

!add the convective cloud fraction to the grid-scale cloud fraction,  and the convective cloud water and ice
!mixing ratios to the grid-scale cloud water and ice mixing ratios if the option to compute the grid-scale
!cloud fraction is different than cld_incidence.
 cld_fraction_select: select case (trim(cld_scheme))

    case("cld_fraction","cld_fraction_thompson")
       if(associated(cldfrac) .and. associated(cldfrac_deepc)) then
          if(.not.allocated(cldfrac_cu)) allocate(cldfrac_cu(kms:kme,ims:ime))
          do k = kts,kte
          do i = its,ite
             cldfrac_cu(k,i) = cldfrac_deepc(k,i) + cldfrac_shalc(k,i)
             cldfrac(k,i) = (1._RKIND-cldfrac_cu(k,i))*cldfrac(k,i)
             cldfrac(k,i) = cldfrac(k,i) + cldfrac_cu(k,i)
             cldfrac(k,i) = amin1(1._RKIND,cldfrac(k,i))
          enddo
          enddo

          do j = jts,jte
             do k = kts,kte
             do i = its,ite
                qcrad_p(i,k,j) = qcrad_p(i,k,j) + qc_cu(k,i)*cldfrac_cu(k,i)
                qirad_p(i,k,j) = qirad_p(i,k,j) + qi_cu(k,i)*cldfrac_cu(k,i)
             enddo
             enddo
          enddo
          if(allocated(cldfrac_cu)) deallocate(cldfrac_cu)
       endif

    case default
 end select cld_fraction_select

 end subroutine cloudiness_to_MPAS

!=================================================================================================================
 subroutine driver_cloudiness(configs,mesh,diag_physics,sfc_input,its,ite)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in)   :: configs
 type(mpas_pool_type),intent(in)   :: mesh
 type(mpas_pool_type),intent(in)   :: sfc_input

 integer,intent(in):: its,ite

!inout arguments:
 type(mpas_pool_type),intent(inout):: diag_physics

!local variables and pointers:
 character(len=StrKIND),pointer:: cld_scheme

 integer:: i,j,k

 real(kind=RKIND):: land,gridkm
 real(kind=RKIND),dimension(:),allocatable:: p_1d,t_1d,qv_1d,qc_1d,qi_1d,qs_1d,dz_1d,cf_1d

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write('')
!call mpas_log_write('--- enter subroutine driver_cloudiness:')

 call mpas_pool_get_config(configs,'config_radt_cld_scheme',cld_scheme)

!copy MPAS arrays to local arrays:
 call cloudiness_from_MPAS(configs,mesh,diag_physics,sfc_input,its,ite)

 cld_fraction_select: select case (trim(cld_scheme))
    case("cld_incidence")
      call mpas_timer_start('calc_cldincidence')
      call calc_cldincidence(cldfrac_p,qcrad_p,qirad_p,f_qc,f_qi,its,ite)
      call mpas_timer_stop('calc_cldincidence')

    case("cld_fraction")
      call mpas_timer_start('calc_cldfraction')
      call calc_cldfraction(cldfrac_p,t_p,pres_p,qvrad_p,qcrad_p,qirad_p,qs_p,its,ite)
      call mpas_timer_stop('calc_cldfraction')

    case("cld_fraction_thompson")
      call mpas_timer_start('cal_cldfra3')

      if(.not.allocated(p_1d) ) allocate(p_1d(kts:kte) )
      if(.not.allocated(t_1d) ) allocate(t_1d(kts:kte) )
      if(.not.allocated(qv_1d)) allocate(qv_1d(kts:kte))
      if(.not.allocated(qc_1d)) allocate(qc_1d(kts:kte))
      if(.not.allocated(qi_1d)) allocate(qi_1d(kts:kte))
      if(.not.allocated(qs_1d)) allocate(qs_1d(kts:kte))
      if(.not.allocated(dz_1d)) allocate(dz_1d(kts:kte))
      if(.not.allocated(cf_1d)) allocate(cf_1d(kts:kte))

      do j = jts,jte
      do i = its,ite

         gridkm = dx_p(i,j)*0.001_RKIND
         land   = xland_p(i,j)
         do k = kts,kte
            p_1d(k)  = pres_hyd_p(i,k,j)
            t_1d(k)  = t_p(i,k,j)
            qv_1d(k) = qvrad_p(i,k,j)
            qc_1d(k) = qcrad_p(i,k,j)
            qi_1d(k) = qirad_p(i,k,j)
            qs_1d(k) = qsrad_p(i,k,j)
            dz_1d(k) = dz_p(i,k,j)
            cf_1d(k) = cldfrac_p(i,k,j)
         enddo

         call cal_cldfra3(cf_1d,qv_1d,qc_1d,qi_1d,qs_1d,dz_1d,p_1d, t_1d, &
                          land,gridkm,.false.,1.5,kts,kte,.false.)

         do k = kts,kte
            qcrad_p(i,k,j)   = qc_1d(k)
            qirad_p(i,k,j)   = qi_1d(k)
            qsrad_p(i,k,j)   = qs_1d(k)
            cldfrac_p(i,k,j) = cf_1d(k)
         enddo

      enddo
      enddo

      if(allocated(p_1d) ) deallocate(p_1d)
      if(allocated(t_1d) ) deallocate(t_1d)
      if(allocated(qv_1d)) deallocate(qv_1d)
      if(allocated(qc_1d)) deallocate(qc_1d)
      if(allocated(qi_1d)) deallocate(qi_1d)
      if(allocated(qs_1d)) deallocate(qs_1d)
      if(allocated(dz_1d)) deallocate(dz_1d)
      if(allocated(cf_1d)) deallocate(cf_1d)

      call mpas_timer_stop('cal_cldfra3')

    case default

 end select cld_fraction_select

!copy local arrays to MPAS grid:
 call cloudiness_to_MPAS(cld_scheme,diag_physics,its,ite)

!call mpas_log_write('--- end subroutine driver_cloudiness.')

 end subroutine driver_cloudiness

!=================================================================================================================
 subroutine calc_cldincidence(cldfrac,qc,qi,f_qc,f_qi,its,ite)
!=================================================================================================================

!input arguments:
 logical,intent(in):: f_qc,f_qi
 integer,intent(in):: its,ite
 real(kind=RKIND),intent(in),dimension(ims:ime,kms:kme,jms:jme):: qc,qi

!output arguments:
 real(kind=RKIND),intent(out),dimension(ims:ime,kms:kme,jms:jme):: cldfrac

!local variables:
 integer:: i,j,k

 real(kind=RKIND),parameter:: thresh = 1.e-06

!-----------------------------------------------------------------------------------------------------------------

 do j = jts,jte
 do k = kts,kte
 do i = its,ite
    cldfrac(i,k,j) = 0._RKIND
 enddo
 enddo
 enddo

 if(f_qc .and. f_qi) then
    do j = jts,jte
    do k = kts,kte
    do i = its,ite
       if(qc(i,k,j)+qi(i,k,j) .gt. thresh) cldfrac(i,k,j) = 1.0_RKIND
    enddo
    enddo
    enddo
 elseif(f_qc) then
    do j = jts,jte
    do k = kts,kte
    do i = its,ite
       if(qc(i,k,j) .gt. thresh) cldfrac(i,k,j) = 1.0_RKIND
    enddo
    enddo
    enddo
 endif
    
 end subroutine calc_cldincidence

!=================================================================================================================
 subroutine calc_cldfraction(cldfrac,t_p,pres_p,qv,qc,qi,qs,its,ite)
!=================================================================================================================

!input arguments:
 integer,intent(in):: its,ite
 real(kind=RKIND),intent(in),dimension(ims:ime,kms:kme,jms:jme):: qv,qc,qi,qs
 real(kind=RKIND),intent(in),dimension(ims:ime,kms:kme,jms:jme):: t_p,pres_p

!output arguments:
 real(kind=RKIND),intent(out),dimension(ims:ime,kms:kme,jms:jme):: cldfrac

!local variables:
 integer:: i,j,k

 real(kind=RKIND),parameter:: alpha0  = 100.
 real(kind=RKIND),parameter:: gamma   = 0.49
 real(kind=RKIND),parameter:: qcldmin = 1.e-12
 real(kind=RKIND),parameter:: pexp    = 0.25
 real(kind=RKIND),parameter:: rhgrid  = 1.0

 real(kind=RKIND),parameter:: svp1  = 0.61078
 real(kind=RKIND),parameter:: svp2  = 17.2693882
 real(kind=RKIND),parameter:: svpi2 = 21.8745584
 real(kind=RKIND),parameter:: svp3  = 35.86
 real(kind=RKIND),parameter:: svpi3 = 7.66
 real(kind=RKIND),parameter:: svpt0 = 273.15

 real(kind=RKIND):: esi,esw,qvsi,qvsw
 real(kind=RKIND):: arg,denom,qcld,qvs,rhum,subsat,weight

!-----------------------------------------------------------------------------------------------------------------

 do j = jts,jte
 do k = kts,kte
 do i = its,ite
    cldfrac(i,k,j) = 0._RKIND
 enddo
 enddo
 enddo
 
 do j = jts,jte
 do k = kts,kte
 do i = its,ite

!... calculation of the saturation mixing ratios over water and over ice (Murray, 1966):
    esw = 1000. * svp1 * exp(svp2 * (t_p(i,k,j) - svpt0) / (t_p(i,k,j) - svp3))
    esi = 1000. * svp1 * exp(svpi2 * (t_p(i,k,j) - svpt0) / (t_p(i,k,j) - svpi3))

    qvsw = ep_2 * esw / (pres_p(i,k,j) - esw)
    qvsi = ep_2 * esi / (pres_p(i,k,j) - esi)

    qcld = qc(i,k,j) + qi(i,k,j) + qs(i,k,j)
    if(qcld .lt. qcldmin) then
       weight = 0.
    else
       weight = (qi(i,k,j) + qs(i,k,j)) / qcld
    endif

    qvs = (1-weight) * qvsw + weight * qvsi
    rhum = qv(i,k,j) / qvs

    if(qcld .lt. qcldmin) then

       !assume that the cloud fraction is equal to 0. when the cloudy mixing ratio equals 0.
       cldfrac(i,k,j) = 0.

    elseif(rhum .ge. rhgrid) then
       !assume that the cloud fraction is equal to 1. when the relative humidity equal 100%.
       cldfrac(i,k,j) = 1.

    else
       !computation of the cloud fraction:
       subsat = max(1.e-10,rhgrid*qvs-qv(i,k,j))
       denom  = subsat**gamma
       arg    = max(-6.9,-alpha0*qcld/denom) ! exp(-6.9) = 0.001

       rhum = max(1.e-10,rhum)
       cldfrac(i,k,j) = (rhum/rhgrid)**pexp*(1.-exp(arg))
       if(cldfrac(i,k,j) .lt. 0.01) cldfrac(i,k,j) = 0.

    endif

 enddo
 enddo
 enddo

 end subroutine calc_cldfraction

!=================================================================================================================
 end module mpas_atmphys_driver_cloudiness
!=================================================================================================================
