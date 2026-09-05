#include <define.h>

#ifdef URBAN_MODEL
MODULE MOD_Urban_LAIReadin

   USE MOD_Precision
   IMPLICIT NONE
   SAVE

   PUBLIC :: UrbanLAI_readin

CONTAINS

   SUBROUTINE UrbanLAI_readin (year, time, dir_landdata)
!-----------------------------------------------------------------------
!
! !DESCRIPTION:
!  Read in urban LAI, SAI and urban tree cover data.
!
!  Create by Hua Yuan, 11/2021
!
!
! !REVISIONS:
!  08/2023, Wenzong Dong: add codes to read urban tree LAI.
!
!-----------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_Namelist
   USE MOD_MPAS_MPI
   USE MOD_LandUrban
   USE MOD_Vars_Global
   USE MOD_Const_LC
   USE MOD_Vars_TimeVariables
   USE MOD_Vars_TimeInvariants
   USE MOD_Urban_Vars_TimeInvariants
   USE MOD_NetCDFVector
   USE MOD_UserDefFun
#ifdef SinglePoint
   USE MOD_SingleSrfdata
#endif

   IMPLICIT NONE

   integer, intent(in) :: year
   integer, intent(in) :: time
   character(len=256), intent(in) :: dir_landdata

   character(len=256) :: lndname
   character(len=256) :: cyear, ctime
   integer :: u, npatch, iyear

      ! READ in Leaf area index and stem area index
      write(ctime,'(i2.2)') time
      write(cyear,'(i4.4)') min(DEF_LAI_END_YEAR, max(DEF_LAI_START_YEAR,year) )

#ifdef SinglePoint
      iyear = findloc_ud(SITE_LAI_year == min(DEF_LAI_END_YEAR, max(DEF_LAI_START_YEAR,year)) )
      urb_lai(:) = SITE_LAI_monthly(time,iyear)
      urb_sai(:) = SITE_SAI_monthly(time,iyear)
#else
      lndname = trim(dir_landdata)//'/urban/'//trim(cyear)//'/LAI/urban_LAI_'//trim(ctime)//'.nc'
      CALL ncio_read_vector (lndname, 'TREE_LAI',  landurban, urb_lai)

      lndname = trim(dir_landdata)//'/urban/'//trim(cyear)//'/LAI/urban_SAI_'//trim(ctime)//'.nc'
      CALL ncio_read_vector (lndname, 'TREE_SAI',  landurban, urb_sai)
#endif
      ! loop for urban patch to assign fraction of green leaf
      IF (.true.) THEN
         DO u = 1, numurban
            npatch = urban2patch(u)
            tlai(npatch) = urb_lai(u)
            tsai(npatch) = urb_sai(u)
            urb_green(u) = 1.              !TODO: usage? fraction of green leaf
            green(npatch)= 1.              !fraction of green leaf
         ENDDO
      ENDIF

   END SUBROUTINE UrbanLAI_readin

END MODULE MOD_Urban_LAIReadin
#endif
! ---------- EOP ------------
