!=================================================================================================================
 module bl_mynn_mpas
 use mpas_kind_types,only: kind_phys => RKIND

 implicit none
 private
 public:: bl_mynn_mpas_run,  &
          bl_mynn_mpas_init, &
          bl_mynn_mpas_final


 contains


!=================================================================================================================
 subroutine bl_mynn_mpas_init(errmsg,errflg)
!=================================================================================================================

 character(len=*),intent(out):: errmsg
 integer,intent(out):: errflg

!-----------------------------------------------------------------------------------------------------------------
!note: this subroutine currently does nothing.

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
