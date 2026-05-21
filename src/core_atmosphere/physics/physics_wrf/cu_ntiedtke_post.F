!=================================================================================================================
 module cu_ntiedtke_post
 use ccpp_kind_types,only: kind_phys

 implicit none
 private
 public:: cu_ntiedtke_post_init,     &
          cu_ntiedtke_post_finalize, &
          cu_ntiedtke_post_run


 contains


!=================================================================================================================
!>\section arg_table_cu_ntiedtke_post_init
!!\html\include cu_ntiedtke_post_init.html
!!
 subroutine cu_ntiedtke_post_init(errmsg,errflg)
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

 end subroutine cu_ntiedtke_post_init

!=================================================================================================================
!>\section arg_table_cu_ntiedtke_post_finalize
!!\html\include cu_ntiedtke_post_finalize.html
!!
 subroutine cu_ntiedtke_post_finalize(errmsg,errflg)
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

 end subroutine cu_ntiedtke_post_finalize

!=================================================================================================================
!>\section arg_table_cu_ntiedtke_post_run
!!\html\include cu_ntiedtke_post_run.html
!!
 subroutine cu_ntiedtke_post_run(its,ite,kts,kte,stepcu,dt,exner,qv,qc,qi,t,u,v,qvf,qcf,qif,tf,uf,vf,rn,raincv, &
                                 pratec,rthcuten,rqvcuten,rqccuten,rqicuten,rucuten,rvcuten,errmsg,errflg)
!=================================================================================================================

!--- input arguments:
 integer,intent(in):: its,ite,kts,kte
 integer,intent(in):: stepcu

 real(kind=kind_phys),intent(in):: dt
 real(kind=kind_phys),intent(in),dimension(its:ite):: rn
 real(kind=kind_phys),intent(in),dimension(its:ite,kts:kte):: exner,qv,qc,qi,t,u,v,qvf,qcf,qif,tf,uf,vf

!--- inout arguments:
 real(kind=kind_phys),intent(inout),dimension(its:ite):: raincv,pratec
 real(kind=kind_phys),intent(inout),dimension(its:ite,kts:kte):: rqvcuten,rqccuten,rqicuten
 real(kind=kind_phys),intent(inout),dimension(its:ite,kts:kte):: rthcuten,rucuten,rvcuten

!--- output arguments:
 character(len=*),intent(out):: errmsg
 integer,intent(out):: errflg

!--- local variables and arrays:
 integer:: i,k,pp,zz

 real(kind=kind_phys):: delt,rdelt

!-----------------------------------------------------------------------------------------------------------------

 delt  = dt*stepcu
 rdelt = 1./delt

 do i = its,ite
    raincv(i) = rn(i)/stepcu
    pratec(i) = rn(i)/(stepcu*dt)
 enddo

 pp = 0
 do k = kts,kte
    zz = kte - pp
    do i = its,ite
       rthcuten(i,k) = (tf(i,zz)-t(i,k))/exner(i,k)*rdelt
       rqvcuten(i,k) = (qvf(i,zz)-qv(i,k))*rdelt
       rqccuten(i,k) = (qcf(i,zz)-qc(i,k))*rdelt
       rqicuten(i,k) = (qif(i,zz)-qi(i,k))*rdelt
       rucuten(i,k)  = (uf(i,zz)-u(i,k))*rdelt
       rvcuten(i,k)  = (vf(i,zz)-v(i,k))*rdelt
    enddo
    pp = pp + 1
 enddo

 errmsg = 'cu_ntiedtke_post_run OK'
 errflg = 0

 end subroutine cu_ntiedtke_post_run

!=================================================================================================================
 end module cu_ntiedtke_post
!=================================================================================================================
