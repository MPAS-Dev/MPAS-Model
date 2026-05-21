!=================================================================================================================
 module bl_mynn_post
 use ccpp_kind_types,only: kind_phys

 implicit none
 private
 public:: bl_mynn_post_init,     &
          bl_mynn_post_finalize, &
          bl_mynn_post_run


 contains


!=================================================================================================================
!>\section arg_table_bl_mynn_post_init
!!\html\include bl_mynn_post_init.html
!!
 subroutine bl_mynn_post_init(errmsg,errflg)
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

 end subroutine bl_mynn_post_init

!=================================================================================================================
!>\section arg_table_bl_mynn_post_finalize
!!\html\include bl_mynn_post_finalize.html
!!
 subroutine bl_mynn_post_finalize(errmsg,errflg)
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

 end subroutine bl_mynn_post_finalize

!=================================================================================================================
!>\section arg_table_bl_mynn_post_run
!!\html\include bl_mynn_post_run.html
!!
 subroutine bl_mynn_post_run(its,ite,kte,f_qc,f_qi,f_qs,delt,qv,qc,qi,qs,dqv,dqc,dqi,dqs,errmsg,errflg)
!=================================================================================================================

!--- input arguments:
 logical,intent(in):: &
    f_qc, &! if true,the physics package includes the cloud liquid water mixing ratio.
    f_qi, &! if true,the physics package includes the cloud ice mixing ratio.
    f_qs   ! if true,the physics package includes the snow mixing ratio.

 integer,intent(in):: its,ite
 integer,intent(in):: kte

 real(kind=kind_phys),intent(in):: &
    delt   !

 real(kind=kind_phys),intent(in),dimension(its:ite,1:kte):: &
    qv,   &!
    qc,   &!
    qi,   &!
    qs     !


!--- inout arguments:
 real(kind=kind_phys),intent(inout),dimension(its:ite,1:kte):: &
    dqv,  &!
    dqc,  &!
    dqi,  &!
    dqs    !


!--- output arguments:
 character(len=*),intent(out):: errmsg
 integer,intent(out):: errflg


!--- local variables:
 integer:: i,k,kts
 real(kind=kind_phys):: rq,sq,tem
 real(kind=kind_phys),dimension(its:ite,1:kte):: sqv,sqc,sqi,sqs

!-----------------------------------------------------------------------------------------------------------------

!--- initialization:
 kts = 1

!---
 do i = its,ite
    do k = kts,kte
       sq = qv(i,k)/(1.+qv(i,k))      !conversion of qv at time-step n from mixing ratio to specific humidity.
       sqv(i,k) = sq + dqv(i,k)*delt  !calculation of specific humidity at time-step n+1.
       rq = sqv(i,k)/(1.-sqv(i,k))    !conversion of qv at time-step n+1 from specific humidity to mixing ratio.
       dqv(i,k) = (rq - qv(i,k))/delt !calculation of the tendency.
    enddo
 enddo

 if(f_qc) then
    do i = its,ite
       do k = kts,kte
          sq = qc(i,k)/(1.+qv(i,k))
          sqc(i,k) = sq + dqc(i,k)*delt
          rq  = sqc(i,k)*(1.+sqv(i,k))
          dqc(i,k) = (rq - qc(i,k))/delt
       enddo
    enddo
 endif

 if(f_qi) then
    do i = its,ite
       do k = kts,kte
          sq = qi(i,k)/(1.+qv(i,k))
          sqi(i,k) = sq + dqi(i,k)*delt
          rq = sqi(i,k)*(1.+sqv(i,k))
          dqi(i,k) = (rq - qi(i,k))/delt
       enddo
    enddo
 endif

 if(f_qs) then
    do i = its,ite
       do k = kts,kte
          sq = qs(i,k)/(1.+qv(i,k))
          sqs(i,k) = sq + dqs(i,k)*delt
          rq = sqs(i,k)*(1.+sqv(i,k))
          dqs(i,k) = (rq - qs(i,k))/delt
       enddo
    enddo
 endif

!--- output error flag and message:
 errmsg = " "
 errflg = 0

 end subroutine bl_mynn_post_run

!=================================================================================================================
 end module bl_mynn_post
!=================================================================================================================
