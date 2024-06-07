!=================================================================================================================
 module sf_sfclayrev_pre
 use ccpp_kind_types,only: kind_phys

 implicit none
 private
 public:: sf_sfclayrev_pre_init,     &
          sf_sfclayrev_pre_finalize, &
          sf_sfclayrev_pre_run


 contains


!=================================================================================================================
!>\section arg_table_sf_sfclayrev_pre_init
!!\html\include sf_sfclayrev_pre_init.html
!!
 subroutine sf_sfclayrev_pre_init(errmsg,errflg)
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

 end subroutine sf_sfclayrev_pre_init

!=================================================================================================================
!>\section arg_table_sf_sfclayrev_pre_finalize
!!\html\include sf_sfclayrev_pre_finalize.html
!!
 subroutine sf_sfclayrev_pre_finalize(errmsg,errflg)
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

 end subroutine sf_sfclayrev_pre_finalize

!=================================================================================================================
!>\section arg_table_sf_sfclayrev_pre_run
!!\html\include sf_sfclayrev_pre_run.html
!!
 subroutine sf_sfclayrev_pre_run(dz2d,u2d,v2d,qv2d,p2d,t2d,dz1d,u1d,v1d,qv1d,p1d,t1d, &
                                 its,ite,kts,kte,errmsg,errflg)
!=================================================================================================================

!--- input arguments:
 integer,intent(in):: its,ite,kts,kte

 real(kind=kind_phys),intent(in),dimension(its:ite,kts:kte):: &
    dz2d,u2d,v2d,qv2d,p2d,t2d

!--- output arguments:
 character(len=*),intent(out):: errmsg
 integer,intent(out):: errflg

 real(kind=kind_phys),intent(out),dimension(its:ite):: &
    dz1d,u1d,v1d,qv1d,p1d,t1d

!--- local variables:
 integer:: i

!-----------------------------------------------------------------------------------------------------------------

 do i = its,ite
    dz1d(i) = dz2d(i,kts)
    u1d(i)  = u2d(i,kts)
    v1d(i)  = v2d(i,kts)
    qv1d(i) = qv2d(i,kts)
    p1d(i)  = p2d(i,kts)
    t1d(i)  = t2d(i,kts)
 enddo

 errmsg = 'sf_sfclayrev_pre_run OK'
 errflg = 0

 end subroutine sf_sfclayrev_pre_run

!=================================================================================================================
 end module sf_sfclayrev_pre
!=================================================================================================================
