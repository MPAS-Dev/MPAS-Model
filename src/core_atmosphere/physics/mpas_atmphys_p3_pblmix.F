! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!=================================================================================================================
 module mpas_atmphys_p3_pblmix
 use mpas_kind_types
 use mpas_pool_routines
 use mpas_atmphys_vars,only: ims,ime,jms,jme,kms,kme,jts,jte,kts,kte,dz_p,rho_p,kzh_p

 implicit none
 private
 public:: p3mix_p,rp3blten_p,nmix_max,     &
          p3_scalar_indices,               &
          allocate_p3pbl,                  &
          deallocate_p3pbl,                &
          p3pbl_from_MPAS,                 &
          p3pbl_mix_mynn,                  &
          p3pbl_to_MPAS,                   &
          p3pbl_add_tend

!mpas_atmphys_p3_pblmix couples the P3 cloud microphysics with the PBL schemes. P3 predicts several
!prognostic scalars per ice category (p3_qit, p3_qni, p3_qir, p3_qib, and p3_qzi, p3_qli when
!config_p3_trplMomI, config_p3_liqfrac are on) and no bulk qi or ni, so the PBL schemes mix the P3
!scalars directly, all with the same diffusivity:
! * bl_ysu : the P3 scalars are passed to the YSU tracer mixing (nmix,qmix), which uses the same implicit
!            solver as for cloud water and cloud ice.
! * bl_mynn: p3pbl_mix_mynn repeats the MYNN implicit vertical diffusion of cloud ice (mynn_tendencies,
!            "MIX CLOUD ICE") for each P3 scalar, using the MYNN heat exchange coefficient (exch_h, kzh_p).
!            As for cloud ice, the P3 scalars are mixed only if config_mynn_mixclouds = 1.
!The PBL tendencies of the P3 scalars are stored in rp3blten (tend_physics) and added to the scalar
!tendencies in p3pbl_add_tend. The order of the P3 scalars, n = 1,nmix, is: p3_qit (categories 1 to nCat),
!p3_qni, p3_qir, p3_qib, then p3_qzi and p3_qli if active (p3_scalar_indices, also used by
!mpas_atmphys_p3_cumix), then nc, nwfa and nifa with bl_ysu (p3pbl_mix_indices).
!
!The other microphysics scalars of P3 are mixed by the PBL schemes as follows:
! * bl_ysu : qc (as for all schemes). nc (config_p3_predictNc), and nwfa and nifa (config_p3_aerosols), are
!            mixed with the P3 scalars above, so that nc is mixed together with qc and the aerosols are mixed
!            in the PBL.
! * bl_mynn: qc and nc, and nwfa and nifa with config_p3_aerosols (as for mp_thompson_aerosols).
! * qr and nr are not mixed by the PBL schemes (as for all schemes).

!maximum number of scalars mixed by the PBL: 6 P3 scalars per ice category (3 categories), plus nc, nwfa and
!nifa with bl_ysu. nmix_max must match the Registry dimension nP3pblmax.
 integer,parameter:: nmix_max = 6*3 + 3

!PBL input (P3 scalars) and output (tendencies) arrays, allocated only for mp_p3 with bl_ysu or bl_mynn:
 real(kind=RKIND),dimension(:,:,:,:),allocatable:: p3mix_p,rp3blten_p


 contains


!=================================================================================================================
 subroutine p3_scalar_indices(configs,state,nmix,idx)
!=================================================================================================================
!returns the number nmix of P3 ice-category scalars and, if state and idx are present, their indices in
!scalars, in the order p3_qit (categories 1 to nCat), p3_qni, p3_qir, p3_qib, then p3_qzi and p3_qli if
!active: the index of scalar n (n = 1 for p3_qit, ...) of category c is idx(c+(n-1)*nCat).

!input arguments:
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(in),optional:: state

!output arguments:
 integer,intent(out):: nmix
 integer,intent(out),dimension(nmix_max),optional:: idx

!local variables and pointers:
 integer,pointer:: p3_nCat
 logical,pointer:: p3_trplMomI,p3_liqfrac
 integer,pointer:: index_p3_qit,index_p3_qni,index_p3_qir,index_p3_qib,index_p3_qzi,index_p3_qli
 integer:: c,n

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_p3_nCat'    ,p3_nCat    )
 call mpas_pool_get_config(configs,'config_p3_trplMomI',p3_trplMomI)
 call mpas_pool_get_config(configs,'config_p3_liqfrac' ,p3_liqfrac )

 nmix = 4*p3_nCat
 if(p3_trplMomI) nmix = nmix + p3_nCat
 if(p3_liqfrac ) nmix = nmix + p3_nCat

 if(.not.(present(idx) .and. present(state))) return

 call mpas_pool_get_dimension(state,'index_p3_qit',index_p3_qit)
 call mpas_pool_get_dimension(state,'index_p3_qni',index_p3_qni)
 call mpas_pool_get_dimension(state,'index_p3_qir',index_p3_qir)
 call mpas_pool_get_dimension(state,'index_p3_qib',index_p3_qib)
 call mpas_pool_get_dimension(state,'index_p3_qzi',index_p3_qzi)
 call mpas_pool_get_dimension(state,'index_p3_qli',index_p3_qli)
 idx(:) = -1
 do c = 1, p3_nCat
    idx(c)           = index_p3_qit + c - 1
    idx(c+  p3_nCat) = index_p3_qni + c - 1
    idx(c+2*p3_nCat) = index_p3_qir + c - 1
    idx(c+3*p3_nCat) = index_p3_qib + c - 1
    n = 4
    if(p3_trplMomI) then
       n = n + 1
       idx(c+(n-1)*p3_nCat) = index_p3_qzi + c - 1
    endif
    if(p3_liqfrac) then
       n = n + 1
       idx(c+(n-1)*p3_nCat) = index_p3_qli + c - 1
    endif
 enddo

 end subroutine p3_scalar_indices

!=================================================================================================================
 subroutine p3pbl_mix_indices(configs,state,nmix,idx)
!=================================================================================================================
!returns the number nmix of scalars mixed by the PBL for mp_p3 and, if state and idx are present, their
!indices in scalars: the P3 ice-category scalars (p3_scalar_indices) then, with bl_ysu only, nc
!(config_p3_predictNc), and nwfa and nifa (config_p3_aerosols). bl_mynn mixes nc, nwfa and nifa itself.

!input arguments:
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(in),optional:: state

!output arguments:
 integer,intent(out):: nmix
 integer,intent(out),dimension(nmix_max),optional:: idx

!local variables and pointers:
 character(len=StrKIND),pointer:: pbl_scheme
 logical,pointer:: p3_predictNc,p3_aerosols
 integer,pointer:: index_nc,index_nwfa,index_nifa
 logical:: get_idx

!-----------------------------------------------------------------------------------------------------------------

 call p3_scalar_indices(configs,state,nmix,idx)
 get_idx = present(idx) .and. present(state)

 call mpas_pool_get_config(configs,'config_pbl_scheme',pbl_scheme)
 if(trim(pbl_scheme) /= 'bl_ysu') return

 call mpas_pool_get_config(configs,'config_p3_predictNc',p3_predictNc)
 call mpas_pool_get_config(configs,'config_p3_aerosols' ,p3_aerosols )

 if(p3_predictNc) then
    nmix = nmix + 1
    if(get_idx) then
       call mpas_pool_get_dimension(state,'index_nc',index_nc)
       idx(nmix) = index_nc
    endif
 endif
 if(p3_aerosols) then
    nmix = nmix + 2
    if(get_idx) then
       call mpas_pool_get_dimension(state,'index_nwfa',index_nwfa)
       call mpas_pool_get_dimension(state,'index_nifa',index_nifa)
       idx(nmix-1) = index_nwfa
       idx(nmix)   = index_nifa
    endif
 endif

 end subroutine p3pbl_mix_indices

!=================================================================================================================
 subroutine allocate_p3pbl(configs)
!=================================================================================================================
!allocates p3mix_p and rp3blten_p for mp_p3 when the YSU or MYNN PBL scheme is called. These arrays must be
!allocated before MPAS_to_physics, where p3mix_p is filled.

!input arguments:
 type(mpas_pool_type),intent(in):: configs

!local variables and pointers:
 character(len=StrKIND),pointer:: microp_scheme,pbl_scheme,sfclayer_scheme
 integer:: nmix

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_microp_scheme'  ,microp_scheme  )
 call mpas_pool_get_config(configs,'config_pbl_scheme'     ,pbl_scheme     )
 call mpas_pool_get_config(configs,'config_sfclayer_scheme',sfclayer_scheme)

 if(trim(microp_scheme) /= 'mp_p3') return
 if(trim(pbl_scheme) /= 'bl_ysu' .and. trim(pbl_scheme) /= 'bl_mynn') return
 if(trim(sfclayer_scheme) == 'off') return

 call p3pbl_mix_indices(configs,nmix=nmix)
 if(.not.allocated(p3mix_p)   ) allocate(p3mix_p(ims:ime,kms:kme,jms:jme,nmix)   )
 if(.not.allocated(rp3blten_p)) allocate(rp3blten_p(ims:ime,kms:kme,jms:jme,nmix))
 p3mix_p(:,:,:,:)    = 0._RKIND
 rp3blten_p(:,:,:,:) = 0._RKIND

 end subroutine allocate_p3pbl

!=================================================================================================================
 subroutine deallocate_p3pbl
!=================================================================================================================

 if(allocated(p3mix_p)   ) deallocate(p3mix_p   )
 if(allocated(rp3blten_p)) deallocate(rp3blten_p)

 end subroutine deallocate_p3pbl

!=================================================================================================================
 subroutine p3pbl_from_MPAS(configs,state,scalars,its,ite)
!=================================================================================================================
!copies the P3 scalars to p3mix_p, the input to the PBL schemes.

!input arguments:
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(in):: state
 integer,intent(in):: its,ite
 real(kind=RKIND),intent(in),dimension(:,:,:):: scalars

!local variables:
 integer:: i,j,k,n,nmix
 integer,dimension(nmix_max):: idx

!-----------------------------------------------------------------------------------------------------------------

 if(.not.allocated(p3mix_p)) return

 call p3pbl_mix_indices(configs,state,nmix,idx)
 do n = 1, nmix
    do j = jts,jte
    do k = kts,kte
    do i = its,ite
       p3mix_p(i,k,j,n) = max(0._RKIND,scalars(idx(n),k,i))
    enddo
    enddo
    enddo
 enddo

 end subroutine p3pbl_from_MPAS

!=================================================================================================================
 subroutine p3pbl_mix_mynn(dt,its,ite)
!=================================================================================================================
!implicit vertical diffusion of the P3 scalars with the MYNN heat exchange coefficient. This is the MYNN
!cloud-ice mixing in mynn_tendencies: khdz = rho_interface*K_h/dz_interface, no flux at the surface
!(K_h = 0 at the lowest interface), and the value at the top level is held fixed. The tridiagonal solver of
!MYNN (tridiag2) is private to bl_mynn_subroutines (MMM-physics), hence p3pbl_tridiag.

!input arguments:
 integer,intent(in):: its,ite
 real(kind=RKIND),intent(in):: dt

!local variables:
 integer:: i,j,k,n,nmix
 real(kind=RKIND):: dzk
 real(kind=RKIND),dimension(kts:kte):: dtz,rhoinv,khdz,a,b,c,d,x

!-----------------------------------------------------------------------------------------------------------------

 if(.not.allocated(p3mix_p)) return
 nmix = size(p3mix_p,4)

 do j = jts,jte
 do i = its,ite

    khdz(kts) = 0._RKIND
    do k = kts,kte
       dtz(k)    = dt/dz_p(i,k,j)
       rhoinv(k) = 1._RKIND/max(rho_p(i,k,j),1.e-4_RKIND)
    enddo
    do k = kts+1,kte
       dzk     = 0.5_RKIND*(dz_p(i,k,j)+dz_p(i,k-1,j))
       khdz(k) = max((rho_p(i,k,j)*dz_p(i,k-1,j)+rho_p(i,k-1,j)*dz_p(i,k,j))/(dz_p(i,k-1,j)+dz_p(i,k,j)),1.e-4_RKIND) &
               * kzh_p(i,k,j)/dzk
    enddo

    do k = kts,kte-1
       a(k) =         -dtz(k)*khdz(k)*rhoinv(k)
       b(k) = 1._RKIND+dtz(k)*(khdz(k)+khdz(k+1))*rhoinv(k)
       c(k) =         -dtz(k)*khdz(k+1)*rhoinv(k)
    enddo
    a(kte) = 0._RKIND
    b(kte) = 1._RKIND
    c(kte) = 0._RKIND

    do n = 1, nmix
       do k = kts,kte
          d(k) = p3mix_p(i,k,j,n)
       enddo
       call p3pbl_tridiag(kts,kte,a,b,c,d,x)
       do k = kts,kte
          rp3blten_p(i,k,j,n) = (x(k)-p3mix_p(i,k,j,n))/dt
       enddo
    enddo

 enddo
 enddo

 end subroutine p3pbl_mix_mynn

!=================================================================================================================
 subroutine p3pbl_tridiag(kts,kte,a,b,c,d,x)
!=================================================================================================================
!solves a(k)*x(k-1) + b(k)*x(k) + c(k)*x(k+1) = d(k), k = kts,kte (a(kts) and c(kte) are not used).

 integer,intent(in):: kts,kte
 real(kind=RKIND),intent(in),dimension(kts:kte):: a,b,c,d
 real(kind=RKIND),intent(out),dimension(kts:kte):: x

 integer:: k
 real(kind=RKIND):: m
 real(kind=RKIND),dimension(kts:kte):: cp,dp

 cp(kts) = c(kts)/b(kts)
 dp(kts) = d(kts)/b(kts)
 do k = kts+1,kte
    m     = 1._RKIND/(b(k)-cp(k-1)*a(k))
    cp(k) = c(k)*m
    dp(k) = (d(k)-dp(k-1)*a(k))*m
 enddo
 x(kte) = dp(kte)
 do k = kte-1,kts,-1
    x(k) = dp(k)-cp(k)*x(k+1)
 enddo

 end subroutine p3pbl_tridiag

!=================================================================================================================
 subroutine p3pbl_to_MPAS(tend_physics,its,ite)
!=================================================================================================================
!copies the PBL tendencies of the P3 scalars to rp3blten.

!input arguments:
 integer,intent(in):: its,ite

!inout arguments:
 type(mpas_pool_type),intent(inout):: tend_physics

!local variables and pointers:
 integer:: i,j,k,n
 real(kind=RKIND),dimension(:,:,:),pointer:: rp3blten

!-----------------------------------------------------------------------------------------------------------------

 if(.not.allocated(rp3blten_p)) return

 call mpas_pool_get_array(tend_physics,'rp3blten',rp3blten)
 do j = jts,jte
 do i = its,ite
    do k = kts,kte
       do n = 1, size(rp3blten_p,4)
          rp3blten(n,k,i) = rp3blten_p(i,k,j,n)
       enddo
    enddo
 enddo
 enddo

 end subroutine p3pbl_to_MPAS

!=================================================================================================================
 subroutine p3pbl_add_tend(configs,state,tend_physics,mass,tend_scalars,nCellsSolve,nVertLevels)
!=================================================================================================================
!adds the PBL tendencies of the P3 scalars to the scalar tendencies.

!input arguments:
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(in):: state
 type(mpas_pool_type),intent(in):: tend_physics
 integer,intent(in):: nCellsSolve,nVertLevels
 real(kind=RKIND),intent(in),dimension(:,:):: mass

!inout arguments:
 real(kind=RKIND),intent(inout),dimension(:,:,:):: tend_scalars

!local variables and pointers:
 integer:: i,k,n,nmix
 integer,dimension(nmix_max):: idx
 real(kind=RKIND),dimension(:,:,:),pointer:: rp3blten

!-----------------------------------------------------------------------------------------------------------------

 nullify(rp3blten)
 call mpas_pool_get_array(tend_physics,'rp3blten',rp3blten)
 if(.not.associated(rp3blten)) return

 call p3pbl_mix_indices(configs,state,nmix,idx)
 do i = 1, nCellsSolve
 do k = 1, nVertLevels
    do n = 1, nmix
       tend_scalars(idx(n),k,i) = tend_scalars(idx(n),k,i) + rp3blten(n,k,i)*mass(k,i)
    enddo
 enddo
 enddo

 end subroutine p3pbl_add_tend

!=================================================================================================================
 end module mpas_atmphys_p3_pblmix
!=================================================================================================================
