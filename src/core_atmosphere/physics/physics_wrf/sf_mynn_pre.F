!=================================================================================================================
 module sf_mynn_pre
 use ccpp_kind_types,only: kind_phys

 implicit none
 private
 public:: sf_mynn_pre_init,     &
          sf_mynn_pre_finalize, &
          sf_mynn_pre_run


 contains

!=================================================================================================================
!>\section arg_table_sf_mynn_pre_init
!!\html\include sf_mynn_pre_init.html
!!
 subroutine sf_mynn_pre_init(errmsg,errflg)
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

 end subroutine sf_mynn_pre_init

!=================================================================================================================
!>\section arg_table_sf_mynn_pre_finalize
!!\html\include sf_mynn_pre_finalize.html
!!
 subroutine sf_mynn_pre_finalize(errmsg,errflg)
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

 end subroutine sf_mynn_pre_finalize

!=================================================================================================================
!>\section arg_table_sf_mynn_pre_run
!!\html\include sf_mynn_pre_run.html
!!
 subroutine sf_mynn_pre_run(its,ite,kte,itimestep,dz3d,u3d,v3d,p3d,t3d,rho3d,qv3d,qc3d,f_spp,pattern_spp,   &
                            ust,mol,qsfc,qstar,dz8w1d,u1d,v1d,p1d,t1d,rho1d,qv1d,qc1d,rstoch1d,dz2w1d,u1d2, &
                            v1d2,errmsg,errflg)
!=================================================================================================================

!--- input arguments:
 logical,intent(in):: f_spp

 integer,intent(in):: its,ite
 integer,intent(in):: kte
 integer,intent(in):: itimestep

 real(kind=kind_phys),intent(in),dimension(its:ite,1:kte):: &
    dz3d,      &!
    u3d,       &!
    v3d,       &!
    qv3d,      &!
    qc3d,      &!
    p3d,       &!
    t3d,       &!
    rho3d       !

 real(kind=kind_phys),intent(in),dimension(its:ite,1:kte):: &
    pattern_spp !


!--- inout arguments:
 real(kind=kind_phys),intent(inout),dimension(its:ite):: &
    ust,       &!
    mol,       &!
    qsfc,      &!
    qstar       !


!--- output arguments:
 character(len=*),intent(out):: &
    errmsg      ! output error message (-).

 integer,intent(out):: &
    errflg      ! output error flag (-).

 real(kind=kind_phys),intent(out),dimension(its:ite):: &
    dz8w1d,    &!
    u1d,       &!
    v1d,       &!
    qv1d,      &!
    qc1d,      &!
    p1d,       &!
    t1d,       &!
    rho1d,     &!
    rstoch1d    !

 real(kind=kind_phys),intent(out),dimension(its:ite):: &
    dz2w1d,    &!
    u1d2,      &!
    v1d2        !


!--- local variables:
 integer:: i,kts

!-----------------------------------------------------------------------------------------------------------------

 kts = 1

 do i = its,ite
    dz8w1d(i) = dz3d(i,kts)
    u1d(i)    = u3d(i,kts)
    v1d(i)    = v3d(i,kts)
    qv1d(i)   = qv3d(i,kts)
    qc1d(i)   = qc3d(i,kts)
    p1d(i)    = p3d(i,kts)
    t1d(i)    = t3d(i,kts)
    rho1d(i)  = rho3d(i,kts)
    !--- 2nd model level winds - for diags with high-resolution grids:
    dz2w1d(i) = dz3d(i,kts+1)
    u1d2(i)   = u3d(i,kts+1)
    v1d2(i)   = v3d(i,kts+1)
 enddo

 if(f_spp) then
    do i = its,ite
       rstoch1d(i) = pattern_spp(i,kts)
    enddo
 else
    do i = its,ite
       rstoch1d(i)=0._kind_phys
    enddo
 endif

 if(itimestep == 1) then
    do i = its,ite
       ust(i)   = max(0.04*sqrt(u1d(i)*u1d(i) + v1d(i)*v1d(i)),0.001)
       mol(i)   = 0._kind_phys
       qsfc(i)  = qv1d(i)/(1.+qv1d(i))
       qstar(i) = 0._kind_phys
    enddo
 endif

!--- output message and error flags:
 errmsg = 'sf_mynn_mpas_run OK'
 errflg = 0

 end subroutine sf_mynn_pre_run

!=================================================================================================================
 end module sf_mynn_pre
!=================================================================================================================
