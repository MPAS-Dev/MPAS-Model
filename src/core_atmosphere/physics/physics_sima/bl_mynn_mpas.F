!=================================================================================================================
 module bl_mynn_mpas
 use mpas_kind_types,only: kind_phys => RKIND

 use bl_mynn_common

 implicit none
 private
 public:: bl_mynn_mpas_run,  &
          bl_mynn_mpas_init, &
          bl_mynn_mpas_final


 contains


!=================================================================================================================
 subroutine bl_mynn_mpas_init(con_cp,con_cpv,con_cice,con_cliq,con_ep1,con_ep2,con_grav,con_karman,con_p0, &
                              con_rd,con_rv,con_svp1,con_svp2,con_svp3,con_svpt0,con_xlf,con_xls,con_xlv,  &
                              errmsg,errflg)
!=================================================================================================================

!-- input arguments:
 real(kind=kind_phys),intent(in):: &
    con_cp,     &
    con_cpv,    &
    con_cice,   &
    con_cliq

 real(kind=kind_phys),intent(in):: &
    con_ep1,    &
    con_ep2

 real(kind=kind_phys),intent(in):: &
    con_grav

 real(kind=kind_phys),intent(in):: &
    con_karman

 real(kind=kind_phys),intent(in):: &
    con_p0

 real(kind=kind_phys),intent(in):: &
    con_rd,     &
    con_rv

 real(kind=kind_phys),intent(in):: &
    con_svp1,   &
    con_svp2,   &
    con_svp3,   &
    con_svpt0

 real(kind=kind_phys),intent(in):: &
    con_xlf,    &
    con_xls,    &
    con_xlv

 character(len=*),intent(out):: errmsg
 integer,intent(out):: errflg

!-----------------------------------------------------------------------------------------------------------------
!note: this subroutine currently does nothing.

 cp      = con_cp
 cpv     = con_cpv
 cliq    = con_cliq
 cice    = con_cice

 ep_1    = con_ep1
 ep_2    = con_ep2

 grav    = con_grav

 karman  = con_karman

 p1000mb = con_p0

 rd      = con_rd
 rv      = con_rv

 svp1    = con_svp1
 svp2    = con_svp2
 svp3    = con_svp3
 svpt0   = con_svpt0

 xlf     = con_xlf
 xls     = con_xls
 xlv     = con_xlv

!---
 ep_3    = 1.-ep_2
 ev      = xlv
 gtr     = grav/tref
 p608    = ep_1
 rcp     = rd/cp
 rk      = cp/rd
 rvovrd  = rv/rd
 svp11   = svp1*1.e3
 tv0     = p608*tref
 tv1     = (1.+p608)*tref
 vk      = karman
 xlscp   = (xlv+xlf)/cp
 xlvcp   = xlv/cp


 errmsg = ' '
 errflg = 0

 end subroutine bl_mynn_mpas_init

!=================================================================================================================
 subroutine bl_mynn_mpas_run(its,ite,kte,f_qc,f_qi,qv,qc,qi,sqv,sqc,sqi,errmsg,errflg)
!=================================================================================================================

!--- input arguments:
 logical,intent(in):: &
    f_qc,      &! if true,the physics package includes the cloud liquid water mixing ratio.
    f_qi        ! if true,the physics package includes the cloud ice mixing ratio.

 integer,intent(in):: its,ite
 integer,intent(in):: kte

 real(kind=kind_phys),intent(in),dimension(its:ite,1:kte):: &
    qv          !

 real(kind=kind_phys),intent(in),dimension(its:ite,1:kte):: &
    qc,        &!
    qi          !


!--- output arguments:
 character(len=*),intent(out):: &
    errmsg      ! output error message (-).

 integer,intent(out):: &
    errflg      ! output error flag (-).

 real(kind=kind_phys),intent(out),dimension(its:ite,1:kte):: &
    sqv,       &!
    sqc,       &!
    sqi         !


!--- local variables:
 integer:: i,k,kts

!-----------------------------------------------------------------------------------------------------------------

!--- initialization:
 kts = 1
 do k = kts,kte
    do i = its,ite
       sqc(i,k) = 0._kind_phys
       sqi(i,k) = 0._kind_phys
    enddo
 enddo

!--- conversion from water vapor mixing ratio to specific humidity:
 do k = kts,kte
    do i = its,ite
       sqv(i,k) = qv(i,k)/(1.+qv(i,k))
    enddo
 enddo

!--- conversion from cloud liquid water and cloud ice mixing ratio to specific contents:
 if(f_qc) then
    do k = kts,kte
       do i = its,ite
          sqc(i,k) = qc(i,k)/(1.+qv(i,k))
       enddo
    enddo
 endif
 if(f_qi) then
    do k = kts,kte
       do i = its,ite
          sqi(i,k) = qi(i,k)/(1.+qv(i,k))
       enddo
    enddo
 endif

 end subroutine bl_mynn_mpas_run

!=================================================================================================================
 subroutine bl_mynn_mpas_final(errmsg,errflg)
!=================================================================================================================

 character(len=*),intent(out):: errmsg
 integer,intent(out):: errflg

!-----------------------------------------------------------------------------------------------------------------
!note: this subroutine currently does nothing.

 errmsg = ' '
 errflg = 0

 end subroutine bl_mynn_mpas_final

!=================================================================================================================
 end module bl_mynn_mpas
!=================================================================================================================
