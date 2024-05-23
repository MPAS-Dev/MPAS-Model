!=================================================================================================================
 module cu_ntiedtke_pre
 use ccpp_kind_types,only: kind_phys

 implicit none
 private
 public:: cu_ntiedtke_pre_init,     &
          cu_ntiedtke_pre_finalize, &
          cu_ntiedtke_pre_run


 contains


!=================================================================================================================
!>\section arg_table_cu_ntiedtke_pre_init
!!\html\include cu_ntiedtke_pre_init.html
!!
 subroutine cu_ntiedtke_pre_init(errmsg,errflg)
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

 end subroutine cu_ntiedtke_pre_init

!=================================================================================================================
!>\section arg_table_cu_ntiedtke_pre_finalize
!!\html\include cu_ntiedtke_pre_finalize.html
!!
 subroutine cu_ntiedtke_pre_finalize(errmsg,errflg)
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

 end subroutine cu_ntiedtke_pre_finalize

!=================================================================================================================
!>\section arg_table_cu_ntiedtke_pre_run
!!\html\include cu_ntiedtke_pre_run.html
!!
 subroutine cu_ntiedtke_pre_run(its,ite,kts,kte,im,kx,kx1,itimestep,stepcu,dt,grav,xland,dz,pres,presi,   &
                                t,rho,qv,qc,qi,u,v,w,qvften,thften,qvftenz,thftenz,slimsk,delt,prsl,ghtl, &
                                tf,qvf,qcf,qif,uf,vf,prsi,ghti,omg,errmsg,errflg)
!=================================================================================================================

!--- input arguments:
 integer,intent(in):: its,ite,kts,kte
 integer,intent(in):: itimestep
 integer,intent(in):: stepcu

 real(kind=kind_phys),intent(in):: dt,grav
 real(kind=kind_phys),intent(in),dimension(its:ite):: xland
 real(kind=kind_phys),intent(in),dimension(its:ite,kts:kte):: dz,pres,t,rho,qv,qc,qi,u,v
 real(kind=kind_phys),intent(inout),dimension(its:ite,kts:kte):: qvften,thften
 real(kind=kind_phys),intent(in),dimension(its:ite,kts:kte+1):: presi,w

!--- inout arguments:
 integer,intent(inout):: im,kx,kx1
 integer,intent(inout),dimension(its:ite):: slimsk

 real(kind=kind_phys),intent(inout):: delt
 real(kind=kind_phys),intent(inout),dimension(its:ite,kts:kte):: tf,qvf,qcf,qif,uf,vf
 real(kind=kind_phys),intent(inout),dimension(its:ite,kts:kte):: ghtl,omg,prsl
 real(kind=kind_phys),intent(inout),dimension(its:ite,kts:kte):: qvftenz,thftenz
 real(kind=kind_phys),intent(inout),dimension(its:ite,kts:kte+1):: ghti,prsi

!--- output arguments:
 character(len=*),intent(out):: errmsg
 integer,intent(out):: errflg

!--- local variables and arrays:
 integer:: i,k,pp,zz

 real(kind=kind_phys),dimension(its:ite,kts:kte):: zl,dot
 real(kind=kind_phys),dimension(its:ite,kts:kte+1):: zi

!-----------------------------------------------------------------------------------------------------------------

 im  = ite-its+1
 kx  = kte-kts+1
 kx1 = kx+1

 delt  = dt*stepcu

 do i = its,ite
    slimsk(i) = (abs(xland(i)-2.))
 enddo

 k = kts
 do i = its,ite
    zi(i,k) = 0.
 enddo
 do k = kts,kte
    do i = its,ite
       zi(i,k+1) = zi(i,k)+dz(i,k)
    enddo
 enddo
 do k = kts,kte
    do i = its,ite
       zl(i,k)  = 0.5*(zi(i,k)+zi(i,k+1))
       dot(i,k) = -0.5*grav*rho(i,k)*(w(i,k)+w(i,k+1))
    enddo
 enddo

 pp = 0
 do k = kts,kte+1
    zz = kte + 1 - pp
    do i = its,ite
       ghti(i,zz) = zi(i,k)
       prsi(i,zz) = presi(i,k)
    enddo
    pp = pp + 1
 enddo
 pp = 0
 do k = kts,kte
    zz = kte-pp
    do i = its,ite
       ghtl(i,zz) = zl(i,k)
       omg(i,zz)  = dot(i,k)
       prsl(i,zz) = pres(i,k)
    enddo
    pp = pp + 1
 enddo

 pp = 0
 do k = kts,kte
    zz = kte-pp
    do i = its,ite
       tf(i,zz)  = t(i,k)
       qvf(i,zz) = qv(i,k)
       qcf(i,zz) = qc(i,k)
       qif(i,zz) = qi(i,k)
       uf(i,zz)  = u(i,k)
       vf(i,zz)  = v(i,k)
    enddo
    pp = pp + 1
 enddo

 if(itimestep == 1) then
    do k = kts,kte
       do i = its,ite
          qvftenz(i,k) = 0.
          thftenz(i,k) = 0.
       enddo
    enddo
 else
    pp = 0
    do k = kts,kte
       zz = kte-pp
       do i = its,ite
          qvftenz(i,zz) = qvften(i,k)
          thftenz(i,zz) = thften(i,k)
       enddo
       pp = pp + 1
    enddo
 endif

 errmsg = 'cu_ntiedtke_pre_run OK'
 errflg = 0

 end subroutine cu_ntiedtke_pre_run

!=================================================================================================================
 end module cu_ntiedtke_pre
!=================================================================================================================
