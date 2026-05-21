!=================================================================================================================
 module bl_mynn_pre
 use ccpp_kind_types,only: kind_phys

 implicit none
 private
 public:: bl_mynn_pre_init,     &
          bl_mynn_pre_finalize, &
          bl_mynn_pre_run


 contains


!=================================================================================================================
!>\section arg_table_bl_mynn_pre_init
!!\html\include bl_mynn_pre_init.html
!!
 subroutine bl_mynn_pre_init(errmsg,errflg)
!=================================================================================================================

!--- output arguments:
 character(len=*),intent(out):: &
    errmsg      ! output error message (-).

 integer,intent(out):: &
    errflg      ! output error flag (-).

!-----------------------------------------------------------------------------------------------------------------

!--- output error flag and message:
 errflg = 0
 errmsg = " "

 end subroutine bl_mynn_pre_init

!=================================================================================================================
!>\section arg_table_bl_mynn_pre_finalize
!!\html\include bl_mynn_pre_finalize.html
!!
 subroutine bl_mynn_pre_finalize(errmsg,errflg)
!=================================================================================================================

!--- output arguments:
 character(len=*),intent(out):: &
    errmsg      ! output error message (-).

 integer,intent(out):: &
    errflg      ! output error flag (-).

!-----------------------------------------------------------------------------------------------------------------

!--- output error flag and message:
 errflg = 0
 errmsg = " "

 end subroutine bl_mynn_pre_finalize

!=================================================================================================================
!>\section arg_table_bl_mynn_pre_run
!!\html\include bl_mynn_pre_run.html
!!
 subroutine bl_mynn_pre_run(its,ite,kte,f_qc,f_qi,f_qs,qv,qc,qi,qs,sqv,sqc,sqi,sqs,errmsg,errflg)
!=================================================================================================================

!--- input arguments:
 logical,intent(in):: &
    f_qc,      &! if true,the physics package includes the cloud liquid water mixing ratio.
    f_qi,      &! if true,the physics package includes the cloud ice mixing ratio.
    f_qs        ! if true,the physics package includes the snow mixing ratio.

 integer,intent(in):: its,ite
 integer,intent(in):: kte

 real(kind=kind_phys),intent(in),dimension(its:ite,1:kte):: &
    qv,        &!
    qc,        &!
    qi,        &!
    qs          !


!--- output arguments:
 character(len=*),intent(out):: &
    errmsg      ! output error message (-).

 integer,intent(out):: &
    errflg      ! output error flag (-).

 real(kind=kind_phys),intent(out),dimension(its:ite,1:kte):: &
    sqv,       &!
    sqc,       &!
    sqi ,      &!
    sqs         !


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

!--- conversion from cloud liquid water,cloud ice,and snow mixing ratios to specific contents:
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
 if(f_qs) then
    do k = kts,kte
       do i = its,ite
          sqs(i,k) = qs(i,k)/(1.+qs(i,k))
       enddo
    enddo
 endif

!--- output error flag and message:
 errflg = 0
 errmsg = " "

 end subroutine bl_mynn_pre_run

!=================================================================================================================
 end module bl_mynn_pre
!=================================================================================================================
