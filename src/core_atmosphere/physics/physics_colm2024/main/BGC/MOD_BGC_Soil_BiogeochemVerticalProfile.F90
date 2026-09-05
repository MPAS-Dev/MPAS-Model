#include <define.h>
#ifdef BGC
MODULE MOD_BGC_Soil_BiogeochemVerticalProfile

!------------------------------------------------------------------------------------
! !DESCRIPTION:
! This MODULE calculate soil vertical profile of different C and N inputs, including:
! nitrogen fixation, nitrogen deposition, fine root litter, coarse root litter,
! leaf litter and stem litter.
!
! !ORIGINAL:
! The Community Land Model version 5.0 (CLM5)
!
! !REVISION:
! Xingjie Lu, 2021, revised the CLM5 code to be compatible with CoLM code structure.


   USE MOD_Precision
   USE MOD_MPAS_MPI, only: CoLM_stop
   USE MOD_Const_PFT, only: rootfr_p
   USE MOD_BGC_Vars_TimeVariables, only: &
       nfixation_prof, ndep_prof, altmax_lastyear_indx, w_scalar
   USE MOD_Vars_TimeVariables, only: t_soisno
   USE MOD_BGC_Vars_PFTimeVariables, only: &
       leaf_prof_p, froot_prof_p, croot_prof_p, stem_prof_p, cinput_rootfr_p
   USE MOD_Vars_PFTimeInvariants, only: &
       pftclass, pftfrac
   IMPLICIT NONE

   PUBLIC SoilBiogeochemVerticalProfile

   real(r8), PUBLIC :: surfprof_exp  = 10. ! how steep profile is for surface components (1/ e_folding depth) (1/m)

CONTAINS

   SUBROUTINE SoilBiogeochemVerticalProfile(i,ps,pe,nl_soil,nl_soil_full,nbedrock,zmin_bedrock,z_soi,dz_soi)

   integer ,intent(in) :: i                      ! patch index
   integer ,intent(in) :: ps                     ! start pft index
   integer ,intent(in) :: pe                     ! END pft index
   integer ,intent(in) :: nl_soil                ! number of total soil layers
   integer ,intent(in) :: nl_soil_full           ! number of total soil layers plus bedrock layers
   integer ,intent(in) :: nbedrock               ! where bedrock layer starts (ith soil layer)
   real(r8),intent(in) :: zmin_bedrock           ! depth where bedrock layer starts (m)
   real(r8),intent(in) :: z_soi (1:nl_soil_full) ! depth of each soil layer (m)
   real(r8),intent(in) :: dz_soi(1:nl_soil_full) ! thicknesses of each soil layer (m)

   ! !LOCAL VARIABLES:
   real(r8) :: surface_prof(1:nl_soil)
   real(r8) :: surface_prof_tot
   real(r8) :: rootfr_tot
   real(r8) :: col_cinput_rootfr(1:nl_soil_full)
   integer  :: ivt, m
   integer  :: j
   ! debugging temp variables
   real(r8) :: froot_prof_sum
   real(r8) :: croot_prof_sum
   real(r8) :: leaf_prof_sum
   real(r8) :: stem_prof_sum
   real(r8) :: ndep_prof_sum
   real(r8) :: nfixation_prof_sum
   real(r8) :: delta = 1.e-10
   real(r8) :: sumprof

      surface_prof(:) = 0._r8
      DO j = 1, nl_soil
         surface_prof(j) = exp(-surfprof_exp * z_soi(j)) / dz_soi(j)
         IF (z_soi(j) > zmin_bedrock) THEN
            surface_prof(j) = 0._r8
         ENDIF
      ENDDO

      ! initialize profiles to zero
      col_cinput_rootfr(:)   = 0._r8
      nfixation_prof   (:,i) = 0._r8
      ndep_prof        (:,i) = 0._r8
      DO m = ps , pe
         ivt = pftclass(m)
         leaf_prof_p (:,m)     = 0._r8
         froot_prof_p(:,m)     = 0._r8
         croot_prof_p(:,m)     = 0._r8
         stem_prof_p (:,m)     = 0._r8

         cinput_rootfr_p(:,m)    = 0._r8

         IF (ivt /= 0) THEN
            DO j = 1, nl_soil
               cinput_rootfr_p(j,m) = rootfr_p(j,ivt) / dz_soi(j)
            ENDDO

         ELSE
            cinput_rootfr_p(1,m) = 0.
         ENDIF
      ENDDO

      DO m = ps , pe
      ! integrate rootfr over active layer of soil column
         rootfr_tot = 0._r8
         surface_prof_tot = 0._r8
         DO j = 1, min(max(altmax_lastyear_indx(i), 1), nl_soil)
            rootfr_tot = rootfr_tot + cinput_rootfr_p(j,m) * dz_soi(j)
            surface_prof_tot = surface_prof_tot + surface_prof(j)  * dz_soi(j)
         ENDDO
         IF ( (altmax_lastyear_indx(i) > 0) .and. (rootfr_tot > 0._r8) .and. (surface_prof_tot > 0._r8) ) THEN
         ! WHERE there is not permafrost extending to the surface, integrate the profiles over the active layer
         ! this is equivalnet to integrating over all soil layers outside of permafrost regions
            DO j = 1, min(max(altmax_lastyear_indx(i), 1), nl_soil)
               froot_prof_p(j,m) = cinput_rootfr_p(j,m) / rootfr_tot
               croot_prof_p(j,m) = cinput_rootfr_p(j,m) / rootfr_tot

               IF (j > nbedrock .and. cinput_rootfr_p(j,m) > 0._r8) THEN
                  write(*,*) 'ERROR: cinput_rootfr_p > 0 in bedrock'
               ENDIF
            ! set all surface processes to shallower profile
               leaf_prof_p(j,m) = surface_prof(j)/ surface_prof_tot
               stem_prof_p(j,m) = surface_prof(j)/ surface_prof_tot
            ENDDO
         ELSE
            ! IF fully frozen, or no roots, put everything in the top layer
            froot_prof_p(1,m) = 1./dz_soi(1)
            croot_prof_p(1,m) = 1./dz_soi(1)
            leaf_prof_p(1,m)  = 1./dz_soi(1)
            stem_prof_p(1,m)  = 1./dz_soi(1)
         ENDIF
         DO j = 1, nl_soil
            IF(w_scalar(j,i) .eq. 0._r8 .and. t_soisno(j,i) < 273.15_r8)THEN
               froot_prof_p(j,m) = 0._r8
               croot_prof_p(j,m) = 0._r8
               stem_prof_p (j,m) = 0._r8
               leaf_prof_p (j,m) = 0._r8
            ENDIF
         ENDDO
         sumprof = sum(froot_prof_p(1:nl_soil,m)*dz_soi(1:nl_soil))
         IF(sumprof .ne. 0)THEN
            DO j = 1,nl_soil
               froot_prof_p(j,m) = froot_prof_p(j,m) / sumprof
            ENDDO
         ELSE
            froot_prof_p(1,m) = 1./dz_soi(1)
         ENDIF
         sumprof = sum(croot_prof_p(1:nl_soil,m)*dz_soi(1:nl_soil))
         IF(sumprof .ne. 0)THEN
            DO j = 1,nl_soil
               croot_prof_p(j,m) = croot_prof_p(j,m) / sumprof
            ENDDO
         ELSE
            croot_prof_p(1,m) = 1./dz_soi(1)
         ENDIF
         sumprof = sum(stem_prof_p(1:nl_soil,m)*dz_soi(1:nl_soil))
         IF(sumprof .ne. 0)THEN
            DO j = 1,nl_soil
               stem_prof_p(j,m) = stem_prof_p(j,m) / sumprof
            ENDDO
         ELSE
            stem_prof_p(1,m) = 1./dz_soi(1)
         ENDIF
         sumprof = sum(leaf_prof_p(1:nl_soil,m)*dz_soi(1:nl_soil))
         IF(sumprof .ne. 0)THEN
            DO j = 1,nl_soil
               leaf_prof_p(j,m) = leaf_prof_p(j,m) / sumprof
            ENDDO
         ELSE
            leaf_prof_p(1,m) = 1./dz_soi(1)
         ENDIF
      ENDDO



      !! aggregate root profile to column
      DO m = ps , pe
         DO j = 1,nl_soil
            col_cinput_rootfr(j) = col_cinput_rootfr(j) + cinput_rootfr_p(j,m) * pftfrac(m)
         ENDDO
      ENDDO

      ! repeat for column-native profiles: Ndep and Nfix
      rootfr_tot = 0._r8
      surface_prof_tot = 0._r8
      ! redo column ntegration over active layer for column-native profiles
      DO j = 1, min(max(altmax_lastyear_indx(i), 1), nl_soil)
         rootfr_tot = rootfr_tot + col_cinput_rootfr(j) * dz_soi(j)
         surface_prof_tot = surface_prof_tot + surface_prof(j) * dz_soi(j)
      ENDDO
      IF ( (altmax_lastyear_indx(i) > 0) .and. (rootfr_tot > 0._r8) .and. (surface_prof_tot > 0._r8) ) THEN
         DO j = 1,  min(max(altmax_lastyear_indx(i), 1), nl_soil)
            nfixation_prof(j,i) = col_cinput_rootfr(j) / rootfr_tot
            ndep_prof(j,i) = surface_prof(j)/ surface_prof_tot
         ENDDO
      ELSE
         nfixation_prof(1,i) = 1./dz_soi(1)
         ndep_prof(1,i) = 1./dz_soi(1)
      ENDIF

    ! check to make sure integral of all profiles = 1.
      ndep_prof_sum = 0.
      nfixation_prof_sum = 0.
      DO j = 1, nl_soil
         ndep_prof_sum = ndep_prof_sum + ndep_prof(j,i) *  dz_soi(j)
         nfixation_prof_sum = nfixation_prof_sum + nfixation_prof(j,i) *  dz_soi(j)
      ENDDO
      IF ( ( abs(ndep_prof_sum - 1._r8) > delta ) .or.  ( abs(nfixation_prof_sum - 1._r8) > delta ) ) THEN
         print*,'i',i,delta
         write(*,*) 'profile sums:',ndep_prof_sum-1._r8,nfixation_prof_sum-1._r8
         write(*,*) 'altmax_lastyear_indx: ', altmax_lastyear_indx(i)
         write(*,*) 'nfixation_prof: ', nfixation_prof(:,i)
         write(*,*) 'ndep_prof: ', ndep_prof(:,i)
         write(*,*) 'cinput_rootfr: ', cinput_rootfr_p(:,ps:pe)
         write(*,*) 'dz_soi: ', dz_soi(:)
         write(*,*) 'surface_prof: ', surface_prof(:)
         write(*,*) 'p, itype(p) : ', i, pftclass(ps:pe)
         write(*,*) 'cinput_rootfr(p,:): ', cinput_rootfr_p(:,ps:pe)
         write(*,*) 'ERROR: _prof_sum-1>delta'
         CALL CoLM_stop('BGC soil input profiles do not sum to one.')
      ENDIF

      DO m = ps , pe
         froot_prof_sum = 0.
         croot_prof_sum = 0.
         leaf_prof_sum = 0.
         stem_prof_sum = 0.
         DO j = 1, nl_soil
            froot_prof_sum = froot_prof_sum + froot_prof_p(j,m) *  dz_soi(j)
            croot_prof_sum = croot_prof_sum + croot_prof_p(j,m) *  dz_soi(j)
            leaf_prof_sum = leaf_prof_sum + leaf_prof_p(j,m) *  dz_soi(j)
            stem_prof_sum = stem_prof_sum + stem_prof_p(j,m) *  dz_soi(j)
         ENDDO
         IF ( ( abs(froot_prof_sum - 1._r8) > delta ) .or.  ( abs(croot_prof_sum - 1._r8) > delta ) .or. &
              ( abs(stem_prof_sum - 1._r8) > delta ) .or.  ( abs(leaf_prof_sum - 1._r8) > delta ) ) THEN
            write(*,*) 'profile sums: ', froot_prof_sum, croot_prof_sum, leaf_prof_sum, stem_prof_sum
            write(*,*) ' ERROR: sum-1 > delta'
            CALL CoLM_stop('BGC vegetation input profiles do not sum to one.')
         ENDIF
      ENDDO

   END SUBROUTINE SoilBiogeochemVerticalProfile

END MODULE MOD_BGC_Soil_BiogeochemVerticalProfile
#endif
