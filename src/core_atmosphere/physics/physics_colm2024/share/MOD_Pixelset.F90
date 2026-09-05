#include <define.h>

MODULE MOD_Pixelset

!------------------------------------------------------------------------------------
! !DESCRIPTION:
!
!    Pixelset refers to a set of pixels in CoLM.
!
!    In CoLM, the global/regional area is divided into a hierarchical structure:
!    1. If GRIDBASED or UNSTRUCTURED is defined, it is
!       ELEMENT >>> PATCH
!    2. If CATCHMENT is defined, it is
!       ELEMENT >>> HRU >>> PATCH
!    If Plant FUNCTION Type classification is used, PATCH is further divided into PFT.
!    If Plant Community classification is used,     PATCH is further divided into PC.
!
!    In CoLM, the land surface is first divided into pixels, which are rasterized
!    points defined by fine-resolution data. Then ELEMENT, PATCH, HRU, PFT, PC
!    are all consists of pixels, and hence they are all pixelsets.
!
!    The highest level pixelset in CoLM is ELEMENT, all other pixelsets are subsets
!    of ELEMENTs.
!    In a pixelset, pixels are sorted to make pixels in its subsets consecutive.
!    Thus a subset can be represented by starting pixel index and ending pixel index
!    in an ELEMENT.
!
!                Example of hierarchical pixelsets
!        ************************************************ <-- pixels in an ELEMENT
!        |<------------------- ELEMENT ---------------->| <-- level 1
!        |   subset 1  |       subset 2      | subset 3 | <-- level 2
!        |s11|   s12   | s21 |   s22   | s23 |    s31   | <-- level 3
!
!    "Vector" is a collection of data when each pixelset in a given level is associated
!    with a value, representing its averaged physical, chemical or biological state.
!
!    MPAS-embedded CoLM keeps land vectors on MPAS-owned cell subsets.
!
!  Created by Shupeng Zhang, May 2023
!------------------------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_DataType
   IMPLICIT NONE

   ! ---- data types ----
   type :: vec_gather_scatter_type

      ! rank-local vector layout
      integer, allocatable :: vlen(:,:)

      ! local vector offsets
      integer, allocatable :: vstt(:,:)
      integer, allocatable :: vend(:,:)

   CONTAINS
      final  :: vec_gather_scatter_free_mem

   END type vec_gather_scatter_type

   ! ---- data types ----
   type :: pixelset_type

      integer :: nset

      integer*8, allocatable :: eindex(:)  ! global index of element to which pixelset belongs

      integer, allocatable :: ipxstt(:)    ! start local index of pixel in the element
      integer, allocatable :: ipxend(:)    ! end   local index of pixel in the element
      integer, allocatable :: settyp(:)    ! type of pixelset

      integer, allocatable :: ielm(:)      ! local index of element to which pixelset belongs
      integer, allocatable :: srcpos(:)    ! original block-vector position for packed subsets

      integer :: nblkgrp                   ! number of blocks for this process's working group
      integer, allocatable :: xblkgrp (:)  ! block index in longitude for this process's group
      integer, allocatable :: yblkgrp (:)  ! block index in latitude  for this process's group

      type(vec_gather_scatter_type) :: vecgs ! for vector gathering and scattering

      logical :: has_shared = .false.
      real(r8), allocatable :: pctshared (:)

   CONTAINS
      procedure, PUBLIC :: set_vecgs         => vec_gather_scatter_set
      procedure, PUBLIC :: get_lonlat_radian => pixelset_get_lonlat_radian
      procedure, PUBLIC :: pset_pack         => pixelset_pack
      procedure, PUBLIC :: forc_free_mem     => pixelset_forc_free_mem
      final :: pixelset_free_mem

   END type pixelset_type

   ! ---- data types ----
   type :: subset_type

      integer,  allocatable :: substt(:)
      integer,  allocatable :: subend(:)
      real(r8), allocatable :: subfrc(:)

   CONTAINS
      procedure, PUBLIC :: build => subset_build
      final :: subset_free_mem

   END type subset_type

   ! ---- data types ----
   type :: superset_type

      integer,  allocatable :: sup(:)

   CONTAINS
      procedure, PUBLIC :: build => superset_build
      final :: superset_free_mem

   END type superset_type

CONTAINS

   ! --------------------------------
   SUBROUTINE pixelset_get_lonlat_radian (this, rlon, rlat)

   USE MOD_Precision
   USE MOD_Utils
   USE MOD_Pixel
   USE MOD_Mesh

   IMPLICIT NONE
   CLASS(pixelset_type) :: this

   real(r8), intent(inout) :: rlon(:), rlat(:)

   ! Local Variables
   integer :: iset, ie, ipxstt, ipxend, npxl, ipxl
   real(r8), allocatable :: area(:)

      DO iset = 1, this%nset

         ie = this%ielm(iset)

         ipxstt = this%ipxstt (iset)
         ipxend = this%ipxend (iset)

         ! for 2m WMO patch, use all pixels
         IF (ipxstt == -1) THEN
            ipxstt = 1
            ipxend = mesh(ie)%npxl
         ENDIF

         allocate (area (ipxstt:ipxend))
         DO ipxl = ipxstt, ipxend
            area(ipxl) = areaquad (&
               pixel%lat_s(mesh(ie)%ilat(ipxl)), &
               pixel%lat_n(mesh(ie)%ilat(ipxl)), &
               pixel%lon_w(mesh(ie)%ilon(ipxl)), &
               pixel%lon_e(mesh(ie)%ilon(ipxl)) )
         ENDDO

         npxl = ipxend - ipxstt + 1
         rlat(iset) = get_pixelset_rlat ( &
            npxl, mesh(ie)%ilat(ipxstt:ipxend), area)
         rlon(iset) = get_pixelset_rlon ( &
            npxl, mesh(ie)%ilon(ipxstt:ipxend), area)

         deallocate (area)

      ENDDO

   END SUBROUTINE pixelset_get_lonlat_radian

   ! --------------------------------
   FUNCTION get_pixelset_rlat (npxl, ilat, area) result(rlat)

   USE MOD_Precision
   USE MOD_Vars_Global, only: pi
   USE MOD_Pixel
   IMPLICIT NONE

   real(r8) :: rlat

   integer,  intent(in) :: npxl
   integer,  intent(in) :: ilat(npxl)
   real(r8), intent(in) :: area(npxl)

   ! Local variables
   integer :: ipxl

      rlat = 0.0
      DO ipxl = 1, npxl
         rlat = rlat + (pixel%lat_s(ilat(ipxl)) + pixel%lat_n(ilat(ipxl))) * 0.5 * area(ipxl)
      ENDDO
      rlat = rlat / sum(area) * pi/180.0

   END FUNCTION get_pixelset_rlat

   ! --------------------------------
   FUNCTION get_pixelset_rlon (npxl, ilon, area) result(rlon)

   USE MOD_Precision
   USE MOD_Utils
   USE MOD_Vars_Global, only: pi
   USE MOD_Pixel
   IMPLICIT NONE

   real(r8) :: rlon

   integer,  intent(in) :: npxl
   integer,  intent(in) :: ilon(npxl)
   real(r8), intent(in) :: area(npxl)

   ! Local variables
   integer  :: ipxl
   real(r8) :: lon, lon0, area_done

      lon = 0.0
      area_done = 0.0
      DO ipxl = 1, npxl

         IF (pixel%lon_w(ilon(ipxl)) > pixel%lon_e(ilon(ipxl))) THEN
            lon0 = (pixel%lon_w(ilon(ipxl)) + pixel%lon_e(ilon(ipxl)) + 360.0) * 0.5
         ELSE
            lon0 = (pixel%lon_w(ilon(ipxl)) + pixel%lon_e(ilon(ipxl))) * 0.5
         ENDIF

         CALL normalize_longitude (lon0)

         IF (lon - lon0 > 180._r8) THEN
            lon = lon * area_done + (lon0 + 360._r8) * area(ipxl)
         ELSEIF (lon - lon0 < -180._r8) THEN
            lon = lon * area_done + (lon0 - 360._r8) * area(ipxl)
         ELSE
            lon = lon * area_done + lon0 * area(ipxl)
         ENDIF

         area_done = area_done + area(ipxl)
         lon = lon / area_done

         CALL normalize_longitude(lon)

      ENDDO

      rlon = lon * pi/180.0

   END FUNCTION get_pixelset_rlon

   ! --------------------------------
   SUBROUTINE pixelset_free_mem (this)

   IMPLICIT NONE
   type (pixelset_type) :: this

      IF (allocated(this%eindex)) deallocate(this%eindex)
      IF (allocated(this%ipxstt)) deallocate(this%ipxstt)
      IF (allocated(this%ipxend)) deallocate(this%ipxend)
      IF (allocated(this%settyp)) deallocate(this%settyp)

      IF (allocated(this%ielm  )) deallocate(this%ielm  )
      IF (allocated(this%srcpos)) deallocate(this%srcpos)

      IF (allocated(this%xblkgrp)) deallocate(this%xblkgrp)
      IF (allocated(this%yblkgrp)) deallocate(this%yblkgrp)

      CALL vec_gather_scatter_free_mem(this%vecgs)

      IF (allocated(this%pctshared)) deallocate(this%pctshared)

      this%nset = 0
      this%nblkgrp = 0
      this%has_shared = .false.

   END SUBROUTINE pixelset_free_mem

   ! --------------------------------
   SUBROUTINE pixelset_forc_free_mem (this)

   IMPLICIT NONE

   class(pixelset_type) :: this

      IF (allocated(this%eindex )) deallocate(this%eindex )
      IF (allocated(this%ipxstt )) deallocate(this%ipxstt )
      IF (allocated(this%ipxend )) deallocate(this%ipxend )
      IF (allocated(this%settyp )) deallocate(this%settyp )

      IF (allocated(this%ielm   )) deallocate(this%ielm   )
      IF (allocated(this%srcpos )) deallocate(this%srcpos )

      IF (allocated(this%xblkgrp)) deallocate(this%xblkgrp)
      IF (allocated(this%yblkgrp)) deallocate(this%yblkgrp)

      CALL vec_gather_scatter_free_mem(this%vecgs)

      IF (allocated(this%pctshared)) deallocate(this%pctshared)

      this%nset = 0
      this%nblkgrp = 0
      this%has_shared = .false.

   END SUBROUTINE pixelset_forc_free_mem

   ! --------------------------------
   SUBROUTINE copy_pixelset(pixel_from, pixel_to)

   IMPLICIT NONE

   type(pixelset_type), intent(in)  :: pixel_from
   type(pixelset_type), intent(out) :: pixel_to

      pixel_to%nset    = pixel_from%nset
      pixel_to%eindex  = pixel_from%eindex
      pixel_to%ipxstt  = pixel_from%ipxstt
      pixel_to%ipxend  = pixel_from%ipxend
      pixel_to%settyp  = pixel_from%settyp
      pixel_to%ielm    = pixel_from%ielm
      IF (allocated(pixel_from%srcpos)) THEN
         pixel_to%srcpos = pixel_from%srcpos
      ENDIF

      pixel_to%nblkgrp = pixel_from%nblkgrp
      pixel_to%xblkgrp = pixel_from%xblkgrp
      pixel_to%yblkgrp = pixel_from%yblkgrp

      IF (pixel_from%has_shared) THEN
         pixel_to%pctshared = pixel_from%pctshared
      ENDIF

   END SUBROUTINE

   ! --------------------------------
   SUBROUTINE vec_gather_scatter_set (this)

   USE MOD_Block
   USE MOD_MPAS_MPI
   USE MOD_Mesh
   IMPLICIT NONE

   class(pixelset_type)  :: this

   ! Local variables
   integer :: iset, ie, xblk, yblk, iblk, jblk, iblkgrp
   logical, allocatable :: nonzero(:,:)


      IF (.not. allocated (this%vecgs%vlen)) THEN
         allocate (this%vecgs%vlen (gblock%nxblk, gblock%nyblk))
         this%vecgs%vlen(:,:) = 0
      ENDIF

      IF (.true.) THEN

         IF (this%nset > 0) THEN
            IF (.not. allocated(this%eindex)) THEN
               CALL CoLM_stop('Invalid pixelset element-index vector in set_vecgs.')
            ENDIF
            IF (size(this%eindex) /= this%nset) THEN
               CALL CoLM_stop('Invalid pixelset element-index vector in set_vecgs.')
            ENDIF
            IF (.not. allocated(this%ielm)) THEN
               CALL CoLM_stop('Pixelset element mapping must be established before set_vecgs.')
            ENDIF
            IF (size(this%ielm) /= this%nset) THEN
               CALL CoLM_stop('Pixelset element mapping must be established before set_vecgs.')
            ENDIF
            IF (.not. allocated(mesh)) THEN
               CALL CoLM_stop('Pixelset vector layout requires an allocated local element mesh.')
            ENDIF
         ENDIF

         IF (.not. allocated (this%vecgs%vstt)) THEN
            allocate (this%vecgs%vstt (gblock%nxblk, gblock%nyblk))
            allocate (this%vecgs%vend (gblock%nxblk, gblock%nyblk))
         ENDIF

         this%vecgs%vstt(:,:) = 0
         this%vecgs%vend(:,:) = -1

         xblk = 0
         yblk = 0
         DO iset = 1, this%nset
            ie = this%ielm(iset)
            IF (ie < 1 .or. ie > size(mesh)) THEN
               CALL CoLM_stop('Pixelset contains an out-of-range local element index in set_vecgs.')
            ENDIF
            IF (this%eindex(iset) /= mesh(ie)%indx) THEN
               CALL CoLM_stop('Pixelset element ID does not match its local CoLM element in set_vecgs.')
            ENDIF
            IF (mesh(ie)%xblk < 1 .or. mesh(ie)%xblk > gblock%nxblk .or. &
                mesh(ie)%yblk < 1 .or. mesh(ie)%yblk > gblock%nyblk) THEN
               CALL CoLM_stop('Pixelset references an element with an invalid CoLM block index.')
            ENDIF

            IF ((mesh(ie)%xblk /= xblk) .or. (mesh(ie)%yblk /= yblk)) THEN
               xblk = mesh(ie)%xblk
               yblk = mesh(ie)%yblk
               this%vecgs%vstt(xblk,yblk) = iset
            ENDIF

            this%vecgs%vend(xblk,yblk) = iset
         ENDDO

         this%vecgs%vlen = this%vecgs%vend - this%vecgs%vstt + 1

      ENDIF


      IF (.true.) THEN
         allocate (nonzero (gblock%nxblk,gblock%nyblk))

         nonzero = this%vecgs%vlen > 0

         this%nblkgrp = count(nonzero)
         IF (allocated(this%xblkgrp)) deallocate(this%xblkgrp)
         IF (allocated(this%yblkgrp)) deallocate(this%yblkgrp)
         allocate (this%xblkgrp (this%nblkgrp))
         allocate (this%yblkgrp (this%nblkgrp))

         iblkgrp = 0
         DO jblk = 1, gblock%nyblk
            DO iblk = 1, gblock%nxblk
               IF (nonzero(iblk,jblk)) THEN
                  iblkgrp = iblkgrp + 1
                  this%xblkgrp(iblkgrp) = iblk
                  this%yblkgrp(iblkgrp) = jblk
               ENDIF
            ENDDO
         ENDDO

         deallocate(nonzero)
      ENDIF

   END SUBROUTINE vec_gather_scatter_set

   ! --------------------------------
   SUBROUTINE pixelset_pack (this, mask, nset_packed)

   USE MOD_MPAS_MPI
   IMPLICIT NONE
   class(pixelset_type) :: this
   logical, intent(in)  :: mask(:)
   integer, intent(out) :: nset_packed

   integer*8, allocatable :: eindex_(:)
   integer,   allocatable :: ipxstt_(:)
   integer,   allocatable :: ipxend_(:)
   integer,   allocatable :: settyp_(:)
   integer,   allocatable :: ielm_  (:)
   integer,   allocatable :: srcpos_(:)

   real(r8),  allocatable :: pctshared_(:)
   integer :: s, e

      IF (.true.) THEN

         IF (this%nset > 0) THEN
            IF (count(mask) < this%nset) THEN

               allocate (eindex_(this%nset))
               allocate (ipxstt_(this%nset))
	               allocate (ipxend_(this%nset))
	               allocate (settyp_(this%nset))
	               allocate (ielm_  (this%nset))
	               IF (allocated(this%srcpos)) allocate (srcpos_(this%nset))

	               eindex_ = this%eindex
	               ipxstt_ = this%ipxstt
	               ipxend_ = this%ipxend
	               settyp_ = this%settyp
	               ielm_   = this%ielm
	               IF (allocated(this%srcpos)) srcpos_ = this%srcpos

	               deallocate (this%eindex)
	               deallocate (this%ipxstt)
	               deallocate (this%ipxend)
	               deallocate (this%settyp)
	               deallocate (this%ielm  )
	               IF (allocated(this%srcpos)) deallocate (this%srcpos)

               IF (this%has_shared) THEN
                  allocate   (pctshared_(this%nset))
                  pctshared_ = this%pctshared
                  deallocate (this%pctshared)
               ENDIF

               this%nset = count(mask)

               IF (this%nset > 0) THEN

                  allocate (this%eindex(this%nset))
                  allocate (this%ipxstt(this%nset))
	                  allocate (this%ipxend(this%nset))
	                  allocate (this%settyp(this%nset))
	                  allocate (this%ielm  (this%nset))
	                  IF (allocated(srcpos_)) allocate (this%srcpos(this%nset))

	                  this%eindex = pack(eindex_, mask)
	                  this%ipxstt = pack(ipxstt_, mask)
	                  this%ipxend = pack(ipxend_, mask)
	                  this%settyp = pack(settyp_, mask)
	                  this%ielm   = pack(ielm_  , mask)
	                  IF (allocated(srcpos_)) this%srcpos = pack(srcpos_, mask)

                  IF (this%has_shared) THEN

                     this%pctshared = pack(pctshared_, mask)

                     s = 1
                     DO WHILE (s < this%nset)
                        e = s
                        DO WHILE (e < this%nset)
                           IF ((this%ielm(e+1) == this%ielm(s)) &
                              .and. (this%ipxstt(e+1) == this%ipxstt(s))) THEN
                              e = e + 1
                           ELSE
                              EXIT
                           ENDIF
                        ENDDO

                        IF (e > s) THEN
                           this%pctshared(s:e) = this%pctshared(s:e)/sum(this%pctshared(s:e))
                        ENDIF

                        s = e + 1
                     ENDDO

                  ENDIF

               ENDIF

               deallocate (eindex_)
               deallocate (ipxstt_)
	               deallocate (ipxend_)
	               deallocate (settyp_)
	               deallocate (ielm_  )
	               IF (allocated(srcpos_)) deallocate (srcpos_)

               IF (this%has_shared) THEN
                  deallocate (pctshared_)
               ENDIF

            ENDIF
         ENDIF

      ENDIF

      CALL this%set_vecgs

      nset_packed = this%nset

   END SUBROUTINE pixelset_pack

   ! --------------------------------
   SUBROUTINE vec_gather_scatter_free_mem (this)

   IMPLICIT NONE
   type (vec_gather_scatter_type) :: this

      IF (allocated(this%vlen))  deallocate (this%vlen)
      IF (allocated(this%vstt))  deallocate (this%vstt)
      IF (allocated(this%vend))  deallocate (this%vend)
   END SUBROUTINE vec_gather_scatter_free_mem

   ! --------------------------------
   SUBROUTINE subset_build (this, superset, subset, use_frac)

   USE MOD_Mesh
   USE MOD_Pixel
   USE MOD_Utils
   USE MOD_MPAS_MPI, only: CoLM_stop
   USE, INTRINSIC :: ieee_arithmetic, only: ieee_is_finite
   IMPLICIT NONE

   CLASS(subset_type) :: this

   type (pixelset_type), intent(in) :: superset
   type (pixelset_type), intent(in) :: subset
   logical, intent(in) :: use_frac

   ! Local Variables
   integer :: isuperset, isubset, ielm, ipxl, istt, iend
   real(r8) :: subset_area

      IF (superset%has_shared) THEN
         write(*,*) 'Warning: superset has shared area.'
      ENDIF

      IF (allocated(this%substt)) deallocate(this%substt)
      IF (allocated(this%subend)) deallocate(this%subend)

      allocate (this%substt (superset%nset))
      allocate (this%subend (superset%nset))

      this%substt =  0
      this%subend = -1

      IF (use_frac) THEN
         IF (allocated(this%subfrc)) deallocate(this%subfrc)
         allocate (this%subfrc (subset%nset))
      ENDIF

      IF (superset%nset <= 0 .or. subset%nset <= 0) RETURN

      isuperset = 1
      isubset   = 1
      DO WHILE (isubset <= subset%nset)
         IF (isuperset > superset%nset) THEN
            CALL CoLM_stop('A CoLM subset does not belong to any loaded element.')
         ENDIF
         IF (     (subset%eindex(isubset) == superset%eindex(isuperset)) &
            .and. (subset%ipxstt(isubset) >= superset%ipxstt(isuperset) .or. &
                   subset%ipxstt(isubset) == -1 ) &
            .and. (subset%ipxend(isubset) <= superset%ipxend(isuperset) .or. &
                   subset%ipxend(isubset) == -1 ) ) THEN

            IF (this%substt(isuperset) == 0) THEN
               this%substt(isuperset) = isubset
            ENDIF

            this%subend(isuperset) = isubset

            isubset = isubset + 1
         ELSE
            isuperset = isuperset + 1
         ENDIF
      ENDDO

      IF (use_frac) THEN

         DO isubset = 1, subset%nset
            ielm = subset%ielm(isubset)
            IF (ielm < 1 .or. ielm > size(mesh)) THEN
               CALL CoLM_stop('A CoLM subset references an invalid local element.')
            ENDIF
            IF (subset%ipxstt(isubset) /= -1) THEN
               IF (subset%ipxstt(isubset) < 1 .or. subset%ipxend(isubset) < subset%ipxstt(isubset) .or. &
                   subset%ipxend(isubset) > mesh(ielm)%npxl) THEN
                  CALL CoLM_stop('A CoLM subset references pixels outside its element.')
               ENDIF
            ELSEIF (subset%ipxend(isubset) /= -1) THEN
               CALL CoLM_stop('A CoLM virtual subset has inconsistent pixel bounds.')
            ENDIF
            this%subfrc(isubset) = 0
            DO ipxl = subset%ipxstt(isubset), subset%ipxend(isubset)
               IF (ipxl == -1) CYCLE
               this%subfrc(isubset) = this%subfrc(isubset) &
                  + areaquad (&
                  pixel%lat_s(mesh(ielm)%ilat(ipxl)), &
                  pixel%lat_n(mesh(ielm)%ilat(ipxl)), &
                  pixel%lon_w(mesh(ielm)%ilon(ipxl)), &
                  pixel%lon_e(mesh(ielm)%ilon(ipxl)) )
            ENDDO
            IF (subset%has_shared) THEN
               this%subfrc(isubset) = this%subfrc(isubset) * subset%pctshared(isubset)
            ENDIF
         ENDDO

         DO isuperset = 1, superset%nset
            IF (this%substt(isuperset) /= 0) THEN
               istt = this%substt(isuperset)
               iend = this%subend(isuperset)
               subset_area = sum(this%subfrc(istt:iend))
               IF (.not. ieee_is_finite(subset_area) .or. subset_area <= 0._r8) THEN
                  CALL CoLM_stop('A CoLM element has no positive finite subset area.')
               ENDIF
               this%subfrc(istt:iend) = this%subfrc(istt:iend) / subset_area
            ENDIF
         ENDDO

      ENDIF

   END SUBROUTINE subset_build

   ! --------------------------------
   SUBROUTINE subset_free_mem (this)

   IMPLICIT NONE
   type (subset_type) :: this

      IF (allocated(this%substt))  deallocate (this%substt)
      IF (allocated(this%subend))  deallocate (this%subend)
      IF (allocated(this%subfrc))  deallocate (this%subfrc)

   END SUBROUTINE subset_free_mem

   ! --------------------------------
   SUBROUTINE superset_build (this, superset, subset)

   IMPLICIT NONE

   CLASS(superset_type) :: this

   type (pixelset_type), intent(in) :: superset
   type (pixelset_type), intent(in) :: subset

   ! Local Variables
   integer :: isuperset, isubset

      IF (subset%nset <= 0) RETURN

      IF (allocated(this%sup)) deallocate(this%sup)

      allocate (this%sup (subset%nset))

      isuperset = 1
      isubset   = 1
      DO WHILE (isubset <= subset%nset)
         IF (     (subset%eindex(isubset) == superset%eindex(isuperset)) &
            .and. (subset%ipxstt(isubset) >= superset%ipxstt(isuperset)) &
            .and. (subset%ipxend(isubset) <= superset%ipxend(isuperset))) THEN

            this%sup(isubset) = isuperset

            isubset = isubset + 1
         ELSE
            isuperset = isuperset + 1
         ENDIF
      ENDDO

   END SUBROUTINE superset_build

   ! --------------------------------
   SUBROUTINE superset_free_mem (this)

   IMPLICIT NONE
   type (superset_type) :: this

      IF (allocated(this%sup))  deallocate (this%sup)

   END SUBROUTINE superset_free_mem

END MODULE MOD_Pixelset
