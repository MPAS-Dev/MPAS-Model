#include <define.h>

MODULE MOD_LAIReadin

!-----------------------------------------------------------------------
   USE MOD_Precision
   IMPLICIT NONE
   SAVE

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: LAI_readin


!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------


   SUBROUTINE LAI_readin (year, time, dir_landdata)
!=======================================================================
!  Read in the LAI, the LAI dataset was created by Yuan et al. (2011)
!  http://globalchange.bnu.edu.cn
!
!  Created by Yongjiu Dai, March, 2014
!=======================================================================

   USE MOD_Precision
   USE MOD_Namelist
   USE MOD_MPAS_MPI
   USE MOD_UserDefFun
   USE MOD_NetCDFVector
   USE MOD_LandPatch
   USE MOD_Vars_TimeInvariants
   USE MOD_Vars_TimeVariables

   USE MOD_Vars_Global
   USE MOD_Const_LC
#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
   USE MOD_LandPFT
   USE MOD_Vars_PFTimeVariables
#endif
#ifdef SinglePoint
   USE MOD_SingleSrfdata
#endif

   IMPLICIT NONE

   integer, intent(in) :: year, time
   character(len=256), intent(in) :: dir_landdata

   ! Local variables
   integer :: iyear, itime
   character(len=256) :: cyear, ctime
   character(len=256) :: landdir, lndname
   integer :: m, npatch, pc

#ifdef LULC_USGS
   real(r8), dimension(24), parameter :: &   ! Maximum fractional cover of vegetation [-]
      vegc=(/1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
             1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, &
             1.0, 1.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0 /)
#endif

      ! READ in Leaf area index and stem area index

      landdir = trim(dir_landdata) // '/LAI'

#ifdef SinglePoint
#ifndef URBAN_MODEL
      IF (USE_SITE_LAI) THEN
         iyear = minloc(abs(SITE_LAI_year-year), dim=1)
      ELSE
         iyear = findloc_ud(SITE_LAI_year == min(DEF_LAI_END_YEAR, max(DEF_LAI_START_YEAR,year)))
      ENDIF

      IF (.not. DEF_LAI_MONTHLY) THEN
         itime = (time-1)/8 + 1
      ENDIF
#endif
#endif

#if (defined LULC_USGS || defined LULC_IGBP)

#ifdef SinglePoint
#ifndef URBAN_MODEL
      IF (DEF_LAI_MONTHLY) THEN
         tlai(:) = SITE_LAI_monthly(time,iyear)
         tsai(:) = SITE_SAI_monthly(time,iyear)
      ELSE
         tlai(:) = SITE_LAI_8day(itime,iyear)
      ENDIF
#endif
#else
      IF (DEF_LAI_MONTHLY) THEN
         write(cyear,'(i4.4)') min(DEF_LAI_END_YEAR, max(DEF_LAI_START_YEAR,year) )
         write(ctime,'(i2.2)') time

         lndname = trim(landdir)//'/'//trim(cyear)//'/LAI_patches'//trim(ctime)//'.nc'
         CALL ncio_read_vector (lndname, 'LAI_patches',  landpatch, tlai)

         lndname = trim(landdir)//'/'//trim(cyear)//'/SAI_patches'//trim(ctime)//'.nc'
         CALL ncio_read_vector (lndname, 'SAI_patches',  landpatch, tsai)
      ELSE
         write(cyear,'(i4.4)') min(DEF_LAI_END_YEAR, max(DEF_LAI_START_YEAR,year) )
         write(ctime,'(i3.3)') time
         lndname = trim(landdir)//'/'//trim(cyear)//'/LAI_patches'//trim(ctime)//'.nc'
         CALL ncio_read_vector (lndname, 'LAI_patches',  landpatch, tlai)
      ENDIF
#endif

      IF (.true.) THEN
         IF (numpatch > 0) THEN

            DO npatch = 1, numpatch
               m = patchclass(npatch)
#ifdef URBAN_MODEL
               IF(m == URBAN) CYCLE
#endif
               IF(m == 0 .or. m == WATERBODY)THEN
                  fveg(npatch)  = 0.
                  tlai(npatch)  = 0.
                  tsai(npatch)  = 0.
                  green(npatch) = 0.
               ELSE
                  fveg(npatch)  = fveg0(m)     !fraction of veg. cover
                  IF (fveg0(m) > 0) THEN
                     tlai(npatch)  = tlai(npatch)/fveg0(m)   !leaf area index
                     IF (DEF_LAI_MONTHLY) THEN
                        tsai(npatch) = tsai(npatch)/fveg0(m) !stem are index
                     ELSE
                        tsai(npatch) = sai0(m) !stem are index
                     ENDIF
                     green(npatch) = 1.        !fraction of green leaf
                  ELSE
                     tlai(npatch)  = 0.
                     tsai(npatch)  = 0.
                     green(npatch) = 0.
                  ENDIF
               ENDIF
            ENDDO

         ENDIF
      ENDIF

#endif

#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)

#ifdef SinglePoint

#ifndef URBAN_MODEL
      IF (.not. DEF_USE_LAIFEEDBACK)THEN
         IF (patchtypes(SITE_landtype) == 0) THEN
            tlai_p(:) = pack(SITE_LAI_pfts_monthly(:,time,iyear), SITE_pctpfts > 0.)
            tsai_p(:) = pack(SITE_SAI_pfts_monthly(:,time,iyear), SITE_pctpfts > 0.)
            tlai(:)   = sum (SITE_LAI_pfts_monthly(:,time,iyear) * SITE_pctpfts)
            tsai(:)   = sum (SITE_SAI_pfts_monthly(:,time,iyear) * SITE_pctpfts)
         ELSE
            tlai(:) = SITE_LAI_monthly(time,iyear)
            tsai(:) = SITE_SAI_monthly(time,iyear)
         ENDIF
      ELSE
         IF (patchtypes(SITE_landtype) == 0) THEN
            tsai_p(:) = pack(SITE_SAI_pfts_monthly(:,time,iyear), SITE_pctpfts > 0.)
            tsai(:)   = sum (SITE_SAI_pfts_monthly(:,time,iyear) * SITE_pctpfts)
         ELSE
            tsai(:) = SITE_SAI_monthly(time,iyear)
         ENDIF
      ENDIF
#endif
#else

      write(cyear,'(i4.4)') min(DEF_LAI_END_YEAR, max(DEF_LAI_START_YEAR,year) )
      write(ctime,'(i2.2)') time
      IF (.not. DEF_USE_LAIFEEDBACK)THEN
         lndname = trim(landdir)//'/'//trim(cyear)//'/LAI_patches'//trim(ctime)//'.nc'
         CALL ncio_read_vector (lndname, 'LAI_patches',  landpatch, tlai )
      ENDIF
      lndname = trim(landdir)//'/'//trim(cyear)//'/SAI_patches'//trim(ctime)//'.nc'
      CALL ncio_read_vector (lndname, 'SAI_patches',  landpatch, tsai )
      IF (.not. DEF_USE_LAIFEEDBACK)THEN
         lndname = trim(landdir)//'/'//trim(cyear)//'/LAI_pfts'//trim(ctime)//'.nc'
         CALL ncio_read_vector (lndname, 'LAI_pfts', landpft, tlai_p )
      ENDIF
      lndname = trim(landdir)//'/'//trim(cyear)//'/SAI_pfts'//trim(ctime)//'.nc'
      CALL ncio_read_vector (lndname, 'SAI_pfts', landpft, tsai_p )

#endif

      IF (.true.) THEN
         IF (numpatch > 0) THEN
            DO npatch = 1, numpatch
               m = patchclass(npatch)

#ifdef URBAN_MODEL
               IF (m == URBAN) CYCLE
#endif
               !TODO@yuan: may need to revise patch LAI/SAI
               green(npatch) = 1.
               fveg (npatch) = fveg0(m)

               IF (m == WATERBODY) THEN
                  fveg(npatch)  = 0.
                  tlai(npatch)  = 0.
                  tsai(npatch)  = 0.
                  green(npatch) = 0.
               ENDIF

            ENDDO
         ENDIF
      ENDIF

#endif

   END SUBROUTINE LAI_readin

END MODULE MOD_LAIReadin
