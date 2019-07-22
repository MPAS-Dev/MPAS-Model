! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!=================================================================================================================
 module mpas_atmphys_todynamics
 use mpas_pool_routines
 use mpas_dmpar
 use mpas_atm_dimensions

 use mpas_atmphys_constants, only: R_d,R_v,degrad

 implicit none
 private
 public:: physics_get_tend


!Interface between the physics parameterizations and the non-hydrostatic dynamical core.
!Laura D. Fowler (send comments to laura@ucar.edu).
!2013-05-01.


! subroutines in mpas_atmphys_todynamics:
! ---------------------------------------
! physics_get_tend     : intermediate subroutine between the dynamical core and calculation of the total
!                        physics tendencies.
! physics_get_tend_work: add and mass-weigh physics tendencies before being added to dynamics tendencies.
! tend_toEdges         : interpolate wind-tendencies from centers to edges of grid-cells.
!
! add-ons and modifications to sourcecode:
! ----------------------------------------
! * cleaned-up subroutines physics_get_tend and physics_get_tend_work.
!   Laura D. Fowler (laura@ucar.edu) / 2018-01-23.
! * removed the option bl_mynn_wrf390.
!   Laura D. Fowler (laura@ucar.edu) / 2018-01-24.
! * added the PBL tendencies for the cloud water and cloud ice number concentrations. added the PBL tendencies
!   for the "ice-friendly" and "water-friendly" aerosol number concentrations for the MYNN PBL parameterization.
!   Laura D. Fowler (laura@ucar.edu) / 2019-05-09.
! * added the convective tendencies for the cloud water and cloud ice number concentrations. added the convective
!   tendencies for the "ice-friendly" and "water-friendly" aerosol number concentrations for convective transport
!   in the GF convection scheme.
!   Laura D. Fowler (laura@ucar.edu) / 2019-05-09.


 contains

 
!=================================================================================================================
 subroutine physics_get_tend(block,mesh,state,diag,tend,tend_physics,configs,rk_step,dynamics_substep, &
                             tend_ru_physics,tend_rtheta_physics,tend_rho_physics)
!=================================================================================================================
   

!input variables:
 type(block_type),intent(in),target:: block
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: state
 type(mpas_pool_type),intent(in):: configs
 integer,intent(in):: rk_step
 integer,intent(in):: dynamics_substep

!inout variables:
 type(mpas_pool_type),intent(inout):: diag
 type(mpas_pool_type),intent(inout):: tend
 type(mpas_pool_type),intent(inout):: tend_physics

 real(kind=RKIND),intent(inout),dimension(:,:):: tend_ru_physics,tend_rtheta_physics,tend_rho_physics

!local variables:
 character(len=StrKIND),pointer:: pbl_scheme,        &
                                  convection_scheme, &
                                  radt_lw_scheme,    &
                                  radt_sw_scheme

 integer:: i,iCell,k,n
 integer,pointer:: index_qv,index_qc,index_qr,index_qi,index_qs,index_qg
 integer,pointer:: index_nc,index_ni,index_nifa,index_nwfa
 integer,pointer:: nCells,nCellsSolve,nEdges,nEdgesSolve

 real(kind=RKIND),dimension(:,:),pointer:: mass          ! time level 2 rho_zz
 real(kind=RKIND),dimension(:,:),pointer:: mass_edge     ! diag rho_edge
 real(kind=RKIND),dimension(:,:),pointer:: theta_m       ! time level 1
 real(kind=RKIND),dimension(:,:,:),pointer:: scalars

 real(kind=RKIND),dimension(:,:),pointer:: rthblten,rqvblten,rqcblten, &
                                           rqiblten,rublten ,rvblten
 real(kind=RKIND),dimension(:,:),pointer:: rncblten,rniblten,rnifablten,rnwfablten
 real(kind=RKIND),dimension(:,:),pointer:: rthcuten,rqvcuten,rqccuten, &
                                           rqrcuten,rqicuten,rqscuten, &
                                           rucuten,rvcuten
 real(kind=RKIND),dimension(:,:),pointer:: rnccuten,rnicuten,rnifacuten,rnwfacuten
 real(kind=RKIND),dimension(:,:),pointer:: rthratenlw,rthratensw                                    
 real(kind=RKIND),dimension(:,:),pointer:: rthdynten
 
 real(kind=RKIND),dimension(:,:),pointer:: tend_u_phys !nick
 real(kind=RKIND),dimension(:,:,:),pointer:: tend_scalars

 real(kind=RKIND),dimension(:,:),pointer:: rublten_Edge,rucuten_Edge

 real(kind=RKIND),dimension(:,:),allocatable:: tend_th

!=================================================================================================================

 call mpas_pool_get_dimension(mesh,'nCells',nCells)
 call mpas_pool_get_dimension(mesh,'nEdges',nEdges)
 call mpas_pool_get_dimension(mesh,'nCellsSolve',nCellsSolve)
 call mpas_pool_get_dimension(mesh,'nEdgesSolve',nEdgesSolve)

 call mpas_pool_get_config(configs,'config_pbl_scheme'       ,pbl_scheme       )
 call mpas_pool_get_config(configs,'config_convection_scheme',convection_scheme)
 call mpas_pool_get_config(configs,'config_radt_lw_scheme'   ,radt_lw_scheme   )
 call mpas_pool_get_config(configs,'config_radt_sw_scheme'   ,radt_sw_scheme   )

 call mpas_pool_get_array(state,'theta_m' ,theta_m, 1)
 call mpas_pool_get_array(state,'scalars' ,scalars, 1)
 call mpas_pool_get_array(state,'rho_zz'  ,mass,2    )
 call mpas_pool_get_array(diag ,'rho_edge',mass_edge )
 call mpas_pool_get_array(diag ,'tend_u_phys',tend_u_phys)

 call mpas_pool_get_dimension(state,'index_qv',index_qv)
 call mpas_pool_get_dimension(state,'index_qc',index_qc)
 call mpas_pool_get_dimension(state,'index_qr',index_qr)
 call mpas_pool_get_dimension(state,'index_qi',index_qi)
 call mpas_pool_get_dimension(state,'index_qs',index_qs)
 call mpas_pool_get_dimension(state,'index_qg',index_qg)
 call mpas_pool_get_dimension(state,'index_nc',index_nc)
 call mpas_pool_get_dimension(state,'index_ni',index_ni)
 call mpas_pool_get_dimension(state,'index_nifa',index_nifa)
 call mpas_pool_get_dimension(state,'index_nwfa',index_nwfa)

 call mpas_pool_get_array(tend_physics,'rublten' ,rublten)
 call mpas_pool_get_array(tend_physics,'rvblten' ,rvblten)
 call mpas_pool_get_array(tend_physics,'rthblten',rthblten)
 call mpas_pool_get_array(tend_physics,'rqvblten',rqvblten)
 call mpas_pool_get_array(tend_physics,'rqcblten',rqcblten)
 call mpas_pool_get_array(tend_physics,'rqiblten',rqiblten)
 call mpas_pool_get_array(tend_physics,'rncblten',rncblten)
 call mpas_pool_get_array(tend_physics,'rniblten',rniblten)
 call mpas_pool_get_array(tend_physics,'rnifablten',rnifablten)
 call mpas_pool_get_array(tend_physics,'rnwfablten',rnwfablten)
 call mpas_pool_get_array(tend_physics,'rublten_Edge',rublten_Edge)

 call mpas_pool_get_array(tend_physics,'rucuten' ,rucuten)
 call mpas_pool_get_array(tend_physics,'rvcuten' ,rvcuten)
 call mpas_pool_get_array(tend_physics,'rthcuten',rthcuten)
 call mpas_pool_get_array(tend_physics,'rqvcuten',rqvcuten)
 call mpas_pool_get_array(tend_physics,'rqccuten',rqccuten)
 call mpas_pool_get_array(tend_physics,'rqrcuten',rqrcuten)
 call mpas_pool_get_array(tend_physics,'rqicuten',rqicuten)
 call mpas_pool_get_array(tend_physics,'rqscuten',rqscuten)
 call mpas_pool_get_array(tend_physics,'rnccuten',rnccuten)
 call mpas_pool_get_array(tend_physics,'rnicuten',rnicuten)
 call mpas_pool_get_array(tend_physics,'rnifacuten',rnifacuten)
 call mpas_pool_get_array(tend_physics,'rnwfacuten',rnwfacuten)
 call mpas_pool_get_array(tend_physics,'rucuten_Edge',rucuten_Edge)

 call mpas_pool_get_array(tend_physics,'rthdynten',rthdynten)

 call mpas_pool_get_array(tend_physics,'rthratenlw',rthratenlw)
 call mpas_pool_get_array(tend_physics,'rthratensw',rthratensw)

 call mpas_pool_get_array(tend,'scalars_tend',tend_scalars)


!initialize the tendency for the potential temperature and all scalars due to PBL, convection,
!and longwave and shortwave radiation:
 allocate(tend_th(nVertLevels,nCellsSolve))
 tend_th = 0._RKIND

 tend_scalars(:,:,:)      = 0._RKIND
 tend_ru_physics(:,:)     = 0._RKIND
 tend_rtheta_physics(:,:) = 0._RKIND
 tend_rho_physics(:,:)    = 0._RKIND


!in case some variables are not allocated due to their associated packages. We need to make their pointers
!associated here to avoid triggering run-time. checks when calling physics_get_tend_work:
 if(.not. associated(rucuten) ) allocate(rucuten(0,0) )
 if(.not. associated(rvcuten) ) allocate(rvcuten(0,0) )
 if(.not. associated(rthcuten)) allocate(rthcuten(0,0))
 if(.not. associated(rqvcuten)) allocate(rqvcuten(0,0))
 if(.not. associated(rqccuten)) allocate(rqccuten(0,0))
 if(.not. associated(rqicuten)) allocate(rqicuten(0,0))
 if(.not. associated(rqrcuten)) allocate(rqrcuten(0,0))
 if(.not. associated(rqscuten)) allocate(rqscuten(0,0))
 if(.not. associated(rnccuten)) allocate(rnccuten(0,0))
 if(.not. associated(rnicuten)) allocate(rnicuten(0,0))
 if(.not. associated(rnifacuten)) allocate(rnifacuten(0,0))
 if(.not. associated(rnwfacuten)) allocate(rnwfacuten(0,0))

 if(.not. associated(rublten) ) allocate(rublten(0,0) )
 if(.not. associated(rvblten) ) allocate(rvblten(0,0) )
 if(.not. associated(rthblten)) allocate(rthblten(0,0))
 if(.not. associated(rqvblten)) allocate(rqvblten(0,0))
 if(.not. associated(rqcblten)) allocate(rqcblten(0,0))
 if(.not. associated(rqiblten)) allocate(rqiblten(0,0))
 if(.not. associated(rncblten)) allocate(rncblten(0,0))
 if(.not. associated(rniblten)) allocate(rniblten(0,0))
 if(.not. associated(rnifablten)) allocate(rnifablten(0,0))
 if(.not. associated(rnwfablten)) allocate(rnwfablten(0,0))

 if(.not. associated(rthdynten)) allocate(rthdynten(0,0))

 call physics_get_tend_work( &
              block,mesh,nCells,nEdges,nCellsSolve,nEdgesSolve,rk_step,dynamics_substep, &
              pbl_scheme,convection_scheme,radt_lw_scheme,radt_sw_scheme,                &
              index_qv,index_qc,index_qr,index_qi,index_qs,                              &
              index_nc,index_ni,index_nifa,index_nwfa,                                   &
              mass,mass_edge,theta_m,scalars,                                            &
              rublten,rvblten,rthblten,rqvblten,rqcblten,rqiblten,                       &
              rncblten,rniblten,rnifablten,rnwfablten,                                   &
              rucuten,rvcuten,rthcuten,rqvcuten,rqccuten,rqrcuten,rqicuten,rqscuten,     &                            
              rnccuten,rnicuten,rnifacuten,rnwfacuten,                                   &
              rthratenlw,rthratensw,rublten_Edge,rucuten_Edge,                           &
              tend_th,tend_rtheta_physics,tend_scalars,tend_ru_physics,tend_u_phys)

!clean up any pointers that were allocated with zero size before the call to physics_get_tend_work:
 if(size(rucuten) == 0 ) deallocate(rucuten )
 if(size(rvcuten) == 0 ) deallocate(rvcuten )
 if(size(rthcuten) == 0) deallocate(rthcuten)
 if(size(rqvcuten) == 0) deallocate(rqvcuten)
 if(size(rqccuten) == 0) deallocate(rqccuten)
 if(size(rqicuten) == 0) deallocate(rqicuten)
 if(size(rqrcuten) == 0) deallocate(rqrcuten)
 if(size(rqscuten) == 0) deallocate(rqscuten)
 if(size(rnccuten) == 0) deallocate(rnccuten)
 if(size(rnicuten) == 0) deallocate(rnicuten)
 if(size(rnifacuten) == 0) deallocate(rnifacuten)
 if(size(rnwfacuten) == 0) deallocate(rnwfacuten)

 if(size(rublten) == 0 ) deallocate(rublten )
 if(size(rvblten) == 0 ) deallocate(rvblten )
 if(size(rthblten) == 0) deallocate(rthblten)
 if(size(rqvblten) == 0) deallocate(rqvblten)
 if(size(rqcblten) == 0) deallocate(rqcblten)
 if(size(rqiblten) == 0) deallocate(rqiblten)
 if(size(rncblten) == 0) deallocate(rncblten)
 if(size(rniblten) == 0) deallocate(rniblten)
 if(size(rnifablten) == 0) deallocate(rnifablten)
 if(size(rnwfablten) == 0) deallocate(rnwfablten)

 if(size(rthdynten) == 0) deallocate(rthdynten)

 deallocate(tend_th)

 end subroutine physics_get_tend

!=================================================================================================================
 subroutine physics_get_tend_work( &
                    block,mesh,nCells,nEdges,nCellsSolve,nEdgesSolve,rk_step,dynamics_substep,   &
                    pbl_scheme,convection_scheme,radt_lw_scheme,radt_sw_scheme,                  &
                    index_qv,index_qc,index_qr,index_qi,index_qs,                                &
                    index_nc,index_ni,index_nifa,index_nwfa,                                     &
                    mass,mass_edge,theta_m,scalars,                                              &
                    rublten,rvblten,rthblten,rqvblten,rqcblten,rqiblten,                         &
                    rncblten,rniblten,rnifablten,rnwfablten,                                     &
                    rucuten,rvcuten,rthcuten,rqvcuten,rqccuten,rqrcuten,rqicuten,rqscuten,       &                     
                    rnccuten,rnicuten,rnifacuten,rnwfacuten,                                     &
                    rthratenlw,rthratensw,rublten_Edge,rucuten_Edge,                             &
                    tend_th,tend_theta,tend_scalars,tend_u,tend_u_phys)
!=================================================================================================================

!input arguments:
 type(block_type),intent(in)    :: block
 type(mpas_pool_type),intent(in):: mesh

 character(len=StrKIND),intent(in):: pbl_scheme
 character(len=StrKIND),intent(in):: convection_scheme
 character(len=StrKIND),intent(in):: radt_lw_scheme
 character(len=StrKIND),intent(in):: radt_sw_scheme

 integer,intent(in):: nCells,nEdges,nCellsSolve,nEdgesSolve
 integer,intent(in):: rk_step,dynamics_substep
 integer,intent(in):: index_qv,index_qc,index_qr,index_qi,index_qs
 integer,intent(in):: index_nc,index_ni,index_nifa,index_nwfa

 real(kind=RKIND),intent(in),dimension(nVertLevels,nCells+1):: mass
 real(kind=RKIND),intent(in),dimension(nVertLevels,nEdges+1):: mass_edge
 real(kind=RKIND),intent(in),dimension(nVertLevels,nCells+1):: theta_m
 real(kind=RKIND),intent(in),dimension(num_scalars,nVertLevels,nCells+1):: scalars

 real(kind=RKIND),intent(in),dimension(nVertLevels,nCells+1):: rublten
 real(kind=RKIND),intent(in),dimension(nVertLevels,nCells+1):: rvblten
 real(kind=RKIND),intent(in),dimension(nVertLevels,nCells+1):: rthblten
 real(kind=RKIND),intent(in),dimension(nVertLevels,nCells+1):: rqvblten
 real(kind=RKIND),intent(in),dimension(nVertLevels,nCells+1):: rqcblten
 real(kind=RKIND),intent(in),dimension(nVertLevels,nCells+1):: rqiblten
 real(kind=RKIND),intent(in),dimension(nVertLevels,nCells+1):: rncblten
 real(kind=RKIND),intent(in),dimension(nVertLevels,nCells+1):: rniblten
 real(kind=RKIND),intent(in),dimension(nVertLevels,nCells+1):: rnifablten
 real(kind=RKIND),intent(in),dimension(nVertLevels,nCells+1):: rnwfablten

 real(kind=RKIND),intent(in),dimension(nVertLevels,nCells+1):: rucuten
 real(kind=RKIND),intent(in),dimension(nVertLevels,nCells+1):: rvcuten
 real(kind=RKIND),intent(in),dimension(nVertLevels,nCells+1):: rthcuten
 real(kind=RKIND),intent(in),dimension(nVertLevels,nCells+1):: rqvcuten
 real(kind=RKIND),intent(in),dimension(nVertLevels,nCells+1):: rqccuten
 real(kind=RKIND),intent(in),dimension(nVertLevels,nCells+1):: rqrcuten
 real(kind=RKIND),intent(in),dimension(nVertLevels,nCells+1):: rqicuten
 real(kind=RKIND),intent(in),dimension(nVertLevels,nCells+1):: rqscuten
 real(kind=RKIND),intent(in),dimension(nVertLevels,nCells+1):: rnccuten
 real(kind=RKIND),intent(in),dimension(nVertLevels,nCells+1):: rnicuten
 real(kind=RKIND),intent(in),dimension(nVertLevels,nCells+1):: rnifacuten
 real(kind=RKIND),intent(in),dimension(nVertLevels,nCells+1):: rnwfacuten

 real(kind=RKIND),intent(in),dimension(nVertLevels,nCells+1):: rthratenlw
 real(kind=RKIND),intent(in),dimension(nVertLevels,nCells+1):: rthratensw

!inout arguments:
 real(kind=RKIND),intent(inout),dimension(nVertLevels,nEdges+1):: rublten_Edge
 real(kind=RKIND),intent(inout),dimension(nVertLevels,nEdges+1):: rucuten_Edge
 real(kind=RKIND),intent(inout),dimension(nVertLevels,nEdges+1):: tend_u
 real(kind=RKIND),intent(inout),dimension(nVertLevels,nEdges+1):: tend_u_phys

 real(kind=RKIND),intent(inout),dimension(nVertLevels,nCells+1):: tend_th
 real(kind=RKIND),intent(inout),dimension(nVertLevels,nCells+1):: tend_theta

 real(kind=RKIND),dimension(num_scalars,nVertLevels,nCells+1), intent(inout) :: tend_scalars

!local variables and arrays:
 integer:: i,k
 real(kind=RKIND):: coeff

!-----------------------------------------------------------------------------------------------------------------

!add coupled tendencies due to PBL processes:
 if(pbl_scheme .ne. 'off') then
    if(rk_step == 1 .and. dynamics_substep == 1) then
       call tend_toEdges(block,mesh,rublten,rvblten,rublten_Edge)

       tend_u_phys(1:nVertLevels,1:nEdges) = rublten_Edge(1:nVertLevels,1:nEdges)
    end if

    do i = 1, nEdgesSolve
    do k = 1, nVertLevels
       tend_u(k,i)=tend_u(k,i)+rublten_Edge(k,i)*mass_edge(k,i)
    enddo
    enddo

    do i = 1, nCellsSolve
    do k = 1, nVertLevels
       tend_th(k,i) = tend_th(k,i) + rthblten(k,i)*mass(k,i)
       tend_scalars(index_qv,k,i) = tend_scalars(index_qv,k,i) + rqvblten(k,i)*mass(k,i)
       tend_scalars(index_qc,k,i) = tend_scalars(index_qc,k,i) + rqcblten(k,i)*mass(k,i)
       tend_scalars(index_qi,k,i) = tend_scalars(index_qi,k,i) + rqiblten(k,i)*mass(k,i)
    enddo
    enddo

    pbl_select: select case (trim(pbl_scheme))
       case('bl_mynn')

          do i = 1, nCellsSolve
          do k = 1, nVertLevels
             tend_scalars(index_nc,k,i)   = tend_scalars(index_nc,k,i) + rncblten(k,i)*mass(k,i)
             tend_scalars(index_ni,k,i)   = tend_scalars(index_ni,k,i) + rniblten(k,i)*mass(k,i)
             tend_scalars(index_nifa,k,i) = tend_scalars(index_nifa,k,i) + rnifablten(k,i)*mass(k,i)
             tend_scalars(index_nwfa,k,i) = tend_scalars(index_nwfa,k,i) + rnwfablten(k,i)*mass(k,i)
          enddo
          enddo

       case default
    end select pbl_select
 endif


!add coupled tendencies due to convection:
 if(convection_scheme .ne. 'off') then
    do i = 1, nCellsSolve
    do k = 1, nVertLevels
       tend_th(k,i) = tend_th(k,i) + rthcuten(k,i)*mass(k,i)
       tend_scalars(index_qv,k,i) = tend_scalars(index_qv,k,i) + rqvcuten(k,i)*mass(k,i)
       tend_scalars(index_qc,k,i) = tend_scalars(index_qc,k,i) + rqccuten(k,i)*mass(k,i)
       tend_scalars(index_qi,k,i) = tend_scalars(index_qi,k,i) + rqicuten(k,i)*mass(k,i)
    enddo
    enddo

    cu_select: select case(convection_scheme)
       case('cu_kain_fritsch','cu_kain_fritsch_sca')
          do i = 1, nCellsSolve
          do k = 1, nVertLevels
             tend_scalars(index_qr,k,i) = tend_scalars(index_qr,k,i) + rqrcuten(k,i)*mass(k,i)
             tend_scalars(index_qs,k,i) = tend_scalars(index_qs,k,i) + rqscuten(k,i)*mass(k,i)
          enddo
          enddo
    
       case('cu_tiedtke','cu_ntiedtke','cu_grell_freitas','cu_grell_freitas_wrf390')
          if(rk_step == 1 .and. dynamics_substep == 1) then
             call tend_toEdges(block,mesh,rucuten,rvcuten,rucuten_Edge)
                 
             tend_u_phys(1:nVertLevels,1:nEdges) = tend_u_phys(1:nVertLevels,1:nEdges) &
                                                 + rucuten_Edge(1:nVertLevels,1:nEdges)
          end if

          do i = 1, nEdgesSolve
          do k  = 1, nVertLevels
             tend_u(k,i)=tend_u(k,i)+rucuten_Edge(k,i)*mass_edge(k,i)
          enddo
          enddo

       cu2_select: select case(convection_scheme)
          case('cu_grell_freitas','cu_grell_freitas_wrf390')
             do i = 1, nCellsSolve
             do k = 1, nVertLevels
                tend_scalars(index_nc,k,i)   = tend_scalars(index_nc,k,i) + rnccuten(k,i)*mass(k,i)
                tend_scalars(index_ni,k,i)   = tend_scalars(index_ni,k,i) + rnicuten(k,i)*mass(k,i)
                tend_scalars(index_nifa,k,i) = tend_scalars(index_nifa,k,i) + rnifacuten(k,i)*mass(k,i)
                tend_scalars(index_nwfa,k,i) = tend_scalars(index_nwfa,k,i) + rnwfacuten(k,i)*mass(k,i)
             enddo
             enddo

          case default
       end select cu2_select

       case default
    end select cu_select
 endif


!add coupled tendencies due to longwave radiation:
 if(radt_lw_scheme .ne. 'off') then
    do i = 1, nCellsSolve
    do k = 1, nVertLevels
       tend_th(k,i) = tend_th(k,i) + rthratenlw(k,i)*mass(k,i)
    enddo
    enddo
 endif

!add coupled tendencies due to shortwave radiation:
 if(radt_sw_scheme .ne. 'off') then
    do i = 1, nCellsSolve
    do k = 1, nVertLevels
       tend_th(k,i) = tend_th(k,i) + rthratensw(k,i)*mass(k,i)
    enddo
    enddo
 endif


!convert the tendency for the potential temperature to tendency for the modified potential temperature:
 do i = 1, nCellsSolve
 do k = 1, nVertLevels
    coeff = (1. + R_v/R_d * scalars(index_qv,k,i))
    tend_th(k,i) = coeff * tend_th(k,i) + R_v/R_d * theta_m(k,i) * tend_scalars(index_qv,k,i) / coeff
    tend_theta(k,i) = tend_theta(k,i) + tend_th(k,i)
 enddo
 enddo


 end subroutine physics_get_tend_work

!=================================================================================================================
 subroutine tend_toEdges(block,mesh,Ux_tend,Uy_tend,U_tend)
!=================================================================================================================

 use mpas_atm_dimensions

!input arguments:
 type(block_type),intent(in),target:: block
 type(mpas_pool_type),intent(in):: mesh
 real(kind=RKIND),intent(in),dimension(:,:),target:: Ux_tend,Uy_tend 

!output arguments:
 real(kind=RKIND),intent(out),dimension(:,:):: U_tend

!local variables:
 type (field2DReal),pointer :: tempField
 type (field2DReal),target :: tempFieldTarget
 integer:: iCell,iEdge,k,j
 integer:: cell1, cell2
 integer,pointer:: nCells,nCellsSolve,nEdges
 integer,dimension(:,:),pointer:: cellsOnEdge

 real(kind=RKIND),dimension(:,:),pointer:: Ux_tend_halo,Uy_tend_halo
 real(kind=RKIND),dimension(:,:),pointer:: east, north, edgeNormalVectors

 
!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_dimension(mesh,'nCells',nCells)
 call mpas_pool_get_dimension(mesh,'nCellsSolve',nCellsSolve)
 call mpas_pool_get_dimension(mesh,'nEdges',nEdges)

 call mpas_pool_get_array(mesh,'east',east)
 call mpas_pool_get_array(mesh,'north',north)
 call mpas_pool_get_array(mesh,'edgeNormalVectors',edgeNormalVectors)

 call mpas_pool_get_array(mesh,'cellsOnEdge',cellsOnEdge)

 Ux_tend_halo => Ux_tend
 Uy_tend_halo => Uy_tend

 tempField => tempFieldTarget 
 tempField % block => block
 tempField % dimSizes(1) = nVertLevels
 tempField % dimSizes(2) = nCellsSolve
 tempField % sendList => block % parinfo % cellsToSend
 tempField % recvList => block % parinfo % cellsToRecv
 tempField % copyList => block % parinfo % cellsToCopy
 tempField % prev => null()
 tempField % next => null()
 tempField % isActive = .true.

 tempField % array => Ux_tend_halo
 call mpas_dmpar_exch_halo_field(tempField)
 
 tempField % array => Uy_tend_halo
 call mpas_dmpar_exch_halo_field(tempField)

 do iEdge = 1, nEdges
    cell1 = cellsOnEdge(1,iEdge)
    cell2 = cellsOnEdge(2,iEdge)
                           
    U_tend(:,iEdge) =  Ux_tend_halo(:,cell1) * 0.5 * (edgeNormalVectors(1,iEdge) * east(1,cell1)   &
                                                   +  edgeNormalVectors(2,iEdge) * east(2,cell1)   &
                                                   +  edgeNormalVectors(3,iEdge) * east(3,cell1))  &
                     + Uy_tend_halo(:,cell1) * 0.5 * (edgeNormalVectors(1,iEdge) * north(1,cell1)  &
                                                   +  edgeNormalVectors(2,iEdge) * north(2,cell1)  &
                                                   +  edgeNormalVectors(3,iEdge) * north(3,cell1)) &
                     + Ux_tend_halo(:,cell2) * 0.5 * (edgeNormalVectors(1,iEdge) * east(1,cell2)   &
                                                   +  edgeNormalVectors(2,iEdge) * east(2,cell2)   &
                                                   +  edgeNormalVectors(3,iEdge) * east(3,cell2))  &
                     + Uy_tend_halo(:,cell2) * 0.5 * (edgeNormalVectors(1,iEdge) * north(1,cell2)  &
                                                   +  edgeNormalVectors(2,iEdge) * north(2,cell2)  &
                                                   +  edgeNormalVectors(3,iEdge) * north(3,cell2))
 end do
 
 end subroutine tend_toEdges

!=================================================================================================================
 end module mpas_atmphys_todynamics
!=================================================================================================================
