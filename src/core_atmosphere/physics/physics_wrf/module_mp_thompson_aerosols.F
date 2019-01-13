!=================================================================================================================
!module_mp_thompson_aerosols includes subroutine gt_aod. gt_aod is called from subroutine radiation_sw_from_MPAS
!in mpas_atmphys_driver_radiation_sw.F. gt_aod calculates the 550 nm aerosol optical depth of "water-friendly"
!and "ice-friendly" aerosols from the Thompson cloud microphysics scheme. gt_aod was copied from WRF-4.0.2 (see
!module_radiation_driver.F).
!Laura D. Fowler (laura@ucar.edu) / 2019-01-13.

 module module_mp_thompson_aerosols
 use mpas_atmphys_functions,only: rslf
 use mpas_atmphys_utilities, only: physics_error_fatal,physics_message
#define FATAL_ERROR(M) call physics_error_fatal(M)
#define WRITE_MESSAGE(M) call physics_message(M)

 implicit none
 private
 public:: gt_aod


 contains


!=================================================================================================================
      SUBROUTINE gt_aod(p_phy,DZ8W,t_phy,qvapor, nwfa,nifa, taod5503d,  &
     &             ims,ime, jms,jme, kms,kme, its,ite, jts,jte, kts,kte)

!     USE module_mp_thompson, only: RSLF

!     IMPLICIT NONE

      INTEGER,  INTENT(IN):: ims,ime, jms,jme, kms,kme,                 &
     &                       its,ite, jts,jte, kts,kte

      REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) ::           &
     &                                            t_phy,p_phy, DZ8W,    &
     &                                            qvapor, nifa, nwfa
      REAL,DIMENSION(ims:ime,kms:kme,jms:jme),INTENT(INOUT):: taod5503d

      !..Local variables. 

      REAL, DIMENSION(its:ite,kts:kte,jts:jte):: AOD_wfa, AOD_ifa
      REAL:: RH, a_RH,b_RH, rh_d,rh_f, rhoa,qvsat, unit_bext1,unit_bext3
      REAL:: ntemp
      INTEGER :: i, k, j, RH_idx, RH_idx1, RH_idx2, t_idx
      INTEGER, PARAMETER:: rind=8
      REAL, DIMENSION(rind), PARAMETER:: rh_arr =                       &
     &                      (/10., 60., 70., 80., 85., 90., 95., 99.8/)
      REAL, DIMENSION(rind,4,2) :: lookup_tabl                           ! RH, temp, water-friendly, ice-friendly

      lookup_tabl(1,1,1) =  5.73936E-15
      lookup_tabl(1,1,2) =  2.63577E-12
      lookup_tabl(1,2,1) =  5.73936E-15
      lookup_tabl(1,2,2) =  2.63577E-12
      lookup_tabl(1,3,1) =  5.73936E-15
      lookup_tabl(1,3,2) =  2.63577E-12
      lookup_tabl(1,4,1) =  5.73936E-15
      lookup_tabl(1,4,2) =  2.63577E-12

      lookup_tabl(2,1,1) = 6.93515E-15
      lookup_tabl(2,1,2) = 2.72095E-12
      lookup_tabl(2,2,1) = 6.93168E-15
      lookup_tabl(2,2,2) = 2.72092E-12
      lookup_tabl(2,3,1) = 6.92570E-15
      lookup_tabl(2,3,2) = 2.72091E-12
      lookup_tabl(2,4,1) = 6.91833E-15
      lookup_tabl(2,4,2) = 2.72087E-12

      lookup_tabl(3,1,1) = 7.24707E-15
      lookup_tabl(3,1,2) = 2.77219E-12
      lookup_tabl(3,2,1) = 7.23809E-15
      lookup_tabl(3,2,2) = 2.77222E-12
      lookup_tabl(3,3,1) = 7.23108E-15
      lookup_tabl(3,3,2) = 2.77201E-12
      lookup_tabl(3,4,1) = 7.21800E-15
      lookup_tabl(3,4,2) = 2.77111E-12

      lookup_tabl(4,1,1) = 8.95130E-15
      lookup_tabl(4,1,2) = 2.87263E-12
      lookup_tabl(4,2,1) = 9.01582E-15
      lookup_tabl(4,2,2) = 2.87252E-12
      lookup_tabl(4,3,1) = 9.13216E-15
      lookup_tabl(4,3,2) = 2.87241E-12
      lookup_tabl(4,4,1) = 9.16219E-15
      lookup_tabl(4,4,2) = 2.87211E-12

      lookup_tabl(5,1,1) = 1.06695E-14
      lookup_tabl(5,1,2) = 2.96752E-12
      lookup_tabl(5,2,1) = 1.06370E-14
      lookup_tabl(5,2,2) = 2.96726E-12
      lookup_tabl(5,3,1) = 1.05999E-14
      lookup_tabl(5,3,2) = 2.96702E-12
      lookup_tabl(5,4,1) = 1.05443E-14
      lookup_tabl(5,4,2) = 2.96603E-12

      lookup_tabl(6,1,1) = 1.37908E-14
      lookup_tabl(6,1,2) = 3.15081E-12
      lookup_tabl(6,2,1) = 1.37172E-14
      lookup_tabl(6,2,2) = 3.15020E-12
      lookup_tabl(6,3,1) = 1.36362E-14
      lookup_tabl(6,3,2) = 3.14927E-12
      lookup_tabl(6,4,1) = 1.35287E-14
      lookup_tabl(6,4,2) = 3.14817E-12

      lookup_tabl(7,1,1) = 2.26019E-14
      lookup_tabl(7,1,2) = 3.66798E-12
      lookup_tabl(7,2,1) = 2.24435E-14
      lookup_tabl(7,2,2) = 3.66540E-12
      lookup_tabl(7,3,1) = 2.23254E-14
      lookup_tabl(7,3,2) = 3.66173E-12
      lookup_tabl(7,4,1) = 2.20496E-14
      lookup_tabl(7,4,2) = 3.65796E-12

      lookup_tabl(8,1,1) = 4.41983E-13
      lookup_tabl(8,1,2) = 7.50091E-11
      lookup_tabl(8,2,1) = 3.93335E-13
      lookup_tabl(8,2,2) = 6.79097E-11
      lookup_tabl(8,3,1) = 3.45569E-13
      lookup_tabl(8,3,2) = 6.07845E-11
      lookup_tabl(8,4,1) = 2.96971E-13
      lookup_tabl(8,4,2) = 5.36085E-11

      DO j=jts,jte
         DO k=kts,kte
            DO i=its,ite
               AOD_wfa(i,k,j) = 0.
               AOD_ifa(i,k,j) = 0.
            END DO
         END DO
      END DO

      DO j=jts,jte
         DO k=kts,kte
            DO i=its,ite
               rhoa = p_phy(i,k,j)/(287.*t_phy(i,k,j))
               t_idx = MAX(1, MIN(nint(10.999-0.0333*t_phy(i,k,j)),4))
               qvsat = rslf(p_phy(i,k,j),t_phy(i,k,j))
               RH = MIN(98., MAX(10.1, qvapor(i,k,j)/qvsat*100.))

               !..Get the index for the RH array element

               if (RH .lt. 60) then
                  RH_idx1 = 1
                  RH_idx2 = 2
               elseif (RH .ge. 60 .AND. RH.lt.80) then
                  a_RH = 0.1
                  b_RH = -4
                  RH_idx = nint(a_RH*RH+b_RH)
                  rh_d = rh-rh_arr(rh_idx)
                  if (rh_d .lt. 0) then
                     RH_idx1 = RH_idx-1
                     RH_idx2 = RH_idx
                  else
                     RH_idx1 = RH_idx
                     RH_idx2 = RH_idx+1
                     if (RH_idx2.gt.rind) then
                        RH_idx2 = rind
                        RH_idx1 = rind-1
                     endif
                  endif
               else
                  a_RH = 0.2
                  b_RH = -12.
                  RH_idx = MIN(rind, nint(a_RH*RH+b_RH))
                  rh_d = rh-rh_arr(rh_idx)
                  if (rh_d .lt. 0) then
                     RH_idx1 = RH_idx-1
                     RH_idx2 = RH_idx
                  else
                     RH_idx1 = RH_idx
                     RH_idx2 = RH_idx+1
                     if (RH_idx2.gt.rind) then
                        RH_idx2 = rind
                        RH_idx1 = rind-1
                     endif
                  endif
               endif

               !..RH fraction to be used

               rh_f = MAX(0., MIN(1.0, (rh/(100-rh)-rh_arr(rh_idx1)     &
     &                                  /(100-rh_arr(rh_idx1)))         &
     &                        /(rh_arr(rh_idx2)/(100-rh_arr(rh_idx2))   &
     &                        -rh_arr(rh_idx1)/(100-rh_arr(rh_idx1))) ))


               unit_bext1 = lookup_tabl(RH_idx1,t_idx,1)                &
     &                    + (lookup_tabl(RH_idx2,t_idx,1)               &
     &                    - lookup_tabl(RH_idx1,t_idx,1))*rh_f
               unit_bext3 = lookup_tabl(RH_idx1,t_idx,2)                &
     &                    + (lookup_tabl(RH_idx2,t_idx,2)               &
     &                    - lookup_tabl(RH_idx1,t_idx,2))*rh_f

               ntemp = MAX(1., MIN(99999.E6, nwfa(i,k,j)))
               AOD_wfa(i,k,j) = unit_bext1*ntemp*dz8w(i,k,j)*rhoa

               ntemp = MAX(0.01, MIN(9999.E6, nifa(i,k,j)))
               AOD_ifa(i,k,j) = unit_bext3*ntemp*dz8w(i,k,j)*rhoa

            END DO
         END DO
      END DO

      DO j=jts,jte
         DO k=kts,kte
            DO i=its,ite
               taod5503d(i,k,j) = aod_wfa(i,k,j) + aod_ifa(i,k,j)
            END DO
         END DO
      END DO

      END SUBROUTINE gt_aod

!=================================================================================================================
 end module module_mp_thompson_aerosols
!=================================================================================================================
