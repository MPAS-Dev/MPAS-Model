#include <define.h>

MODULE MOD_SrfdataRestart
!-----------------------------------------------------------------------
! !DESCRIPTION:
!
!    This module includes subroutines to read/write data of mesh and pixelsets.
!
!  Created by Shupeng Zhang, May 2023
!-----------------------------------------------------------------------

   IMPLICIT NONE

   ! ----- subroutines -----
   PUBLIC :: mesh_load_from_file

   PUBLIC :: pixelset_load_from_file

CONTAINS

   !------------------------------------
   SUBROUTINE prepare_subset_eindex(subset_eindex, sorted_subset)

   USE MOD_Utils, only: quicksort
   USE MOD_MPAS_MPI, only: CoLM_stop
   IMPLICIT NONE

   integer*8, intent(in) :: subset_eindex(:)
   integer*8, allocatable, intent(out) :: sorted_subset(:)

   integer, allocatable :: order(:)
   integer :: i

      allocate(sorted_subset(size(subset_eindex)))
      sorted_subset = subset_eindex

      IF (size(sorted_subset) > 1) THEN
         allocate(order(size(sorted_subset)))
         order = (/ (i, i = 1, size(sorted_subset)) /)
         CALL quicksort(size(sorted_subset), sorted_subset, order)
         deallocate(order)

         IF (any(sorted_subset(2:) == sorted_subset(:size(sorted_subset)-1))) THEN
            CALL CoLM_stop('MPAS supplied duplicate owned cell IDs to embedded CoLM.')
         ENDIF
      ENDIF

   END SUBROUTINE prepare_subset_eindex

   !------------------------------------
   logical FUNCTION eindex_in_subset(eindex, sorted_subset) result(keep)

   USE MOD_Utils, only: find_in_sorted_list1
   IMPLICIT NONE

   integer*8, intent(in) :: eindex
   integer*8, allocatable, intent(in) :: sorted_subset(:)

      IF (.not. allocated(sorted_subset)) THEN
         keep = .true.
      ELSEIF (size(sorted_subset) < 1) THEN
         keep = .false.
      ELSE
         keep = find_in_sorted_list1(eindex, size(sorted_subset), sorted_subset) > 0
      ENDIF

   END FUNCTION eindex_in_subset

   !------------------------------------
   integer FUNCTION count_subset_eindex(eindex, sorted_subset) result(nkeep)

   IMPLICIT NONE

   integer*8, intent(in) :: eindex(:)
   integer*8, allocatable, intent(in) :: sorted_subset(:)

   integer :: i

      nkeep = 0
      DO i = 1, size(eindex)
         IF (eindex_in_subset(eindex(i), sorted_subset)) nkeep = nkeep + 1
      ENDDO

   END FUNCTION count_subset_eindex

   !------------------------------------
   SUBROUTINE mesh_load_from_file (dir_landdata, lc_year, subset_eindex)

   USE MOD_MPAS_MPI
   USE MOD_Namelist
   USE MOD_Block
   USE MOD_NetCDFSerial
   USE MOD_Mesh
   IMPLICIT NONE

   integer         , intent(in) :: lc_year
   character(len=*), intent(in) :: dir_landdata
   integer*8, optional, intent(in) :: subset_eindex(:)

   ! Local variables
   character(len=256) :: filename, fileblock, cyear
   integer :: iblkme, iblk, jblk, ie, nelm, ndsp, pdsp
   integer*8, allocatable :: elmindx(:)
   integer*8, allocatable :: subset_sorted(:)
   integer,   allocatable :: datasize(:)
   integer,   allocatable :: npxl(:), pixels(:,:), pixels2d(:,:,:)
   logical,   allocatable :: keep_elm(:)
   logical :: use_subset

#ifdef MPAS_MPI
      CALL mpi_barrier (mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('mesh-load entry synchronization')
#endif

      IF (mpas_is_root) THEN
         write(*,*) 'Loading land elements ...'
      ENDIF

      ! add parameter input for time year
      write(cyear,'(i4.4)') lc_year
      filename = trim(dir_landdata) // '/mesh/' // trim(cyear) // '/mesh.nc'
      IF (.true.) CALL mesh_free_mem()
      CALL ncio_read_bcast_serial (filename, 'nelm_blk', nelm_blk)
      use_subset = present(subset_eindex)
      IF (use_subset) CALL prepare_subset_eindex(subset_eindex, subset_sorted)

      IF (.true.) THEN
         IF (use_subset) THEN
            numelm = size(subset_eindex)
         ELSE
            numelm = sum(nelm_blk, mask = gblock%owner_rank == mpas_rank)
         ENDIF

         IF (numelm > 0) THEN

            IF (allocated(mesh)) deallocate(mesh)
            allocate (mesh (numelm))

            ndsp = 0
            DO iblkme = 1, gblock%nblkme
               iblk = gblock%xblkme(iblkme)
               jblk = gblock%yblkme(iblkme)

               nelm = nelm_blk(iblk,jblk)

               IF (nelm > 0) THEN

                  CALL get_filename_block (filename, iblk, jblk, fileblock)
                  CALL ncio_read_serial (fileblock, 'elmindex',  elmindx)
                  CALL ncio_read_serial (fileblock, 'elmnpxl',   npxl   )

                  IF (size(elmindx) /= nelm .or. size(npxl) /= nelm) THEN
                     CALL CoLM_stop('CoLM mesh block metadata lengths disagree with nelm_blk: '//trim(fileblock))
                  ENDIF
                  IF (any(npxl < 1)) THEN
                     CALL CoLM_stop('CoLM mesh block contains an element with no pixels: '//trim(fileblock))
                  ENDIF

                  IF (use_subset) THEN
                     allocate (keep_elm(nelm))
                     DO ie = 1, nelm
                        keep_elm(ie) = eindex_in_subset(elmindx(ie), subset_sorted)
                     ENDDO
                     IF (count(keep_elm) < 1) THEN
                        deallocate(keep_elm)
                        IF (allocated(elmindx)) deallocate(elmindx)
                        IF (allocated(npxl)) deallocate(npxl)
                        CYCLE
                     ENDIF
                  ENDIF

                  CALL ncio_inquire_varsize (fileblock, 'elmpixels', datasize)
                  IF (size(datasize) == 3) THEN
                     CALL ncio_read_serial (fileblock, 'elmpixels', pixels2d)
                     IF (size(pixels2d,1) < 2 .or. size(pixels2d,3) < nelm .or. &
                         size(pixels2d,2) < maxval(npxl)) THEN
                        CALL CoLM_stop('CoLM mesh block has inconsistent 3-D elmpixels dimensions: '//trim(fileblock))
                     ENDIF
                  ELSEIF (size(datasize) == 2) THEN
                     CALL ncio_read_serial (fileblock, 'elmpixels', pixels)
                     IF (size(pixels,1) < 2 .or. size(pixels,2) < sum(npxl)) THEN
                        CALL CoLM_stop('CoLM mesh block has inconsistent 2-D elmpixels dimensions: '//trim(fileblock))
                     ENDIF
                  ELSE
                     CALL CoLM_stop('CoLM mesh elmpixels must be a two- or three-dimensional variable: '//trim(fileblock))
                  ENDIF

                  pdsp = 0
                  DO ie = 1, nelm
                     IF (use_subset) THEN
                        IF (.not. keep_elm(ie)) THEN
                           IF (size(datasize) /= 3) pdsp = pdsp + npxl(ie)
                           CYCLE
                        ENDIF
                     ENDIF

                     ndsp = ndsp + 1
                     IF (ndsp > size(mesh)) THEN
                        CALL CoLM_stop('MPAS embedded CoLM mesh contains duplicate element IDs for an owned cell subset.')
                     ENDIF
                     mesh(ndsp)%indx = elmindx(ie)
                     mesh(ndsp)%npxl = npxl(ie)
                     mesh(ndsp)%xblk = iblk
                     mesh(ndsp)%yblk = jblk

                     allocate (mesh(ndsp)%ilon (npxl(ie)))
                     allocate (mesh(ndsp)%ilat (npxl(ie)))

                     IF (size(datasize) == 3) THEN
                        mesh(ndsp)%ilon = pixels2d(1,1:npxl(ie),ie)
                        mesh(ndsp)%ilat = pixels2d(2,1:npxl(ie),ie)
                     ELSE
                        mesh(ndsp)%ilon = pixels(1,pdsp+1:pdsp+npxl(ie))
                        mesh(ndsp)%ilat = pixels(2,pdsp+1:pdsp+npxl(ie))
                        pdsp = pdsp + npxl(ie)
                     ENDIF
                  ENDDO

                  IF (allocated(keep_elm)) deallocate(keep_elm)
                  IF (allocated(elmindx)) deallocate(elmindx)
                  IF (allocated(npxl)) deallocate(npxl)
                  IF (allocated(datasize)) deallocate(datasize)
                  IF (allocated(pixels)) deallocate(pixels)
                  IF (allocated(pixels2d)) deallocate(pixels2d)
               ENDIF
            ENDDO

            IF (ndsp /= size(mesh)) THEN
               CALL CoLM_stop('MPAS embedded CoLM mesh does not contain exactly one element per owned MPAS cell.')
            ENDIF
            numelm = ndsp
            IF (numelm == 0) CALL mesh_free_mem()
         ENDIF

         IF (use_subset .and. allocated(nelm_blk)) THEN
            nelm_blk(:,:) = 0
            DO ie = 1, numelm
               nelm_blk(mesh(ie)%xblk, mesh(ie)%yblk) = &
                  nelm_blk(mesh(ie)%xblk, mesh(ie)%yblk) + 1
            ENDDO
         ENDIF

         IF (allocated(elmindx ))  deallocate(elmindx )
         IF (allocated(npxl    ))  deallocate(npxl    )
         IF (allocated(datasize))  deallocate(datasize)
         IF (allocated(pixels  ))  deallocate(pixels  )
         IF (allocated(pixels2d))  deallocate(pixels2d)
         IF (allocated(subset_sorted)) deallocate(subset_sorted)

      ENDIF

#ifdef CoLMDEBUG
      IF (.true.) write(*,'(I10,A,I4)') numelm, ' elements on group ', mpas_rank
#endif

#ifdef MPAS_MPI
      CALL mpi_barrier (mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('mesh-load completion synchronization')
#endif

      IF (mpas_is_root) THEN
         write(*,*) 'Loading land elements done.'
      ENDIF

   END SUBROUTINE mesh_load_from_file

   !---------------------------
   SUBROUTINE pixelset_load_from_file (dir_landdata, psetname, pixelset, numset, lc_year, subset_eindex)

   USE MOD_MPAS_MPI
   USE MOD_Block
   USE MOD_NetCDFSerial
   USE MOD_NetCDFVector
   USE MOD_Mesh
   USE MOD_Pixelset
   USE MOD_Utils, only: quicksort, find_in_sorted_list1
   IMPLICIT NONE

   integer         ,    intent(in) :: lc_year
   character(len=*),    intent(in) :: dir_landdata
   character(len=*),    intent(in) :: psetname
   type(pixelset_type), intent(inout) :: pixelset
   integer, intent(out) :: numset
   integer*8, optional, intent(in) :: subset_eindex(:)

   ! Local variables
	   character(len=256) :: filename, fileblock, cyear
#if (defined VectorInOneFileS || defined VectorInOneFileP)
	   character(len=256) :: blockname
#endif
   integer :: iset, nset, nset_file, ndsp, iblkme, iblk, jblk, ie, ipos
   integer :: match
   integer*8, allocatable :: rbuff(:), mesh_sorted(:)
   integer*8, allocatable :: subset_sorted(:)
   integer,   allocatable :: mesh_order(:)
   logical,   allocatable :: keep_set(:)
   logical :: fexists, fexists_any
   logical :: use_subset

      write(cyear,'(i4.4)') lc_year
#ifdef MPAS_MPI
      CALL mpi_barrier (mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('pixelset-load entry synchronization')
#endif

      use_subset = present(subset_eindex)
      IF (use_subset) CALL prepare_subset_eindex(subset_eindex, subset_sorted)

      IF (mpas_is_root) THEN
         write(*,*) 'Loading Pixel Sets ' // trim(psetname) // ' ...'
      ENDIF

      filename = trim(dir_landdata) // '/' // trim(psetname) // '/' // trim(cyear) // '/' // trim(psetname) // '.nc'

      CALL pixelset%forc_free_mem()

      IF (.true.) THEN

         pixelset%nset = 0

         fexists_any = .false.

         DO iblkme = 1, gblock%nblkme
            iblk = gblock%xblkme(iblkme)
            jblk = gblock%yblkme(iblkme)

#if (defined VectorInOneFileS || defined VectorInOneFileP)
            CALL get_blockname (iblk, jblk, blockname)
            CALL ncio_inquire_length_grp (filename, 'eindex', &
               trim(psetname)//'_'//trim(blockname), nset)
            IF (use_subset .and. nset > 0) THEN
               CALL ncio_read_serial_grp_int64_1d (filename, 'eindex', &
                  trim(psetname)//'_'//trim(blockname), rbuff)
               nset = count_subset_eindex(rbuff, subset_sorted)
               deallocate(rbuff)
            ENDIF
            pixelset%nset = pixelset%nset + nset
#else
            CALL get_filename_block (filename, iblk, jblk, fileblock)

            inquire (file=trim(fileblock), exist=fexists)
            IF (fexists) THEN
               IF (use_subset) THEN
                  CALL ncio_read_serial (fileblock, 'eindex', rbuff)
                  nset = count_subset_eindex(rbuff, subset_sorted)
                  deallocate(rbuff)
               ELSE
                  CALL ncio_inquire_length (fileblock, 'eindex', nset)
               ENDIF
               pixelset%nset = pixelset%nset + nset
            ENDIF

            fexists_any = fexists_any .or. fexists
#endif
         ENDDO

#if (defined VectorInOneFileS || defined VectorInOneFileP)
         inquire(file=trim(filename), exist=fexists_any)
#endif

#ifdef MPAS_MPI
         CALL mpi_allreduce (MPI_IN_PLACE, fexists_any, 1, MPI_LOGICAL, MPI_LOR, mpas_comm, mpas_mpi_ierr)
         CALL mpas_mpi_check('pixelset file-presence reduction')
#endif
         IF (.not. fexists_any) THEN
            write(*,*) 'Warning : restart file ' //trim(filename)// ' not found.'
            CALL CoLM_stop ()
         ENDIF

         IF (pixelset%nset > 0) THEN

	            allocate (pixelset%eindex (pixelset%nset))
	            allocate (pixelset%srcpos (pixelset%nset))

            ndsp = 0
            DO iblkme = 1, gblock%nblkme
               iblk = gblock%xblkme(iblkme)
               jblk = gblock%yblkme(iblkme)

#if (defined VectorInOneFileS || defined VectorInOneFileP)
               CALL get_blockname (iblk, jblk, blockname)
               CALL ncio_inquire_length_grp (filename, 'eindex', &
                  trim(psetname)//'_'//trim(blockname), nset)

               IF (nset > 0) THEN

                  CALL ncio_read_serial_grp_int64_1d (filename, 'eindex', &
                     trim(psetname)//'_'//trim(blockname), rbuff)

                  nset_file = size(rbuff)
                  IF (use_subset) THEN
                     allocate(keep_set(nset_file))
                     DO iset = 1, nset_file
                        keep_set(iset) = eindex_in_subset(rbuff(iset), subset_sorted)
                     ENDDO
                     nset = count(keep_set)
                  ELSE
                     nset = nset_file
                  ENDIF

                  IF (nset > 0) THEN
	                     IF (ndsp + nset > pixelset%nset) THEN
	                        CALL CoLM_stop('CoLM pixelset count changed while loading '//trim(psetname)//'.')
	                     ENDIF
	                     IF (use_subset) THEN
	                        pixelset%eindex(ndsp+1:ndsp+nset) = pack(rbuff, keep_set)
	                        pixelset%srcpos(ndsp+1:ndsp+nset) = &
	                           pack((/ (ipos, ipos = 1, nset_file) /), keep_set)
	                     ELSE
	                        pixelset%eindex(ndsp+1:ndsp+nset) = rbuff
	                        pixelset%srcpos(ndsp+1:ndsp+nset) = (/ (ipos, ipos = 1, nset) /)
	                     ENDIF

	                     ndsp = ndsp + nset
                  ENDIF

                  IF (allocated(keep_set)) deallocate(keep_set)
                  deallocate(rbuff)
               ENDIF
#else
               CALL get_filename_block (filename, iblk, jblk, fileblock)
               inquire (file=trim(fileblock), exist=fexists)
               IF (fexists) THEN

                  CALL ncio_read_serial (fileblock, 'eindex', rbuff)

	                  nset_file = size(rbuff)
	                  IF (use_subset) THEN
	                     allocate(keep_set(nset_file))
	                     DO iset = 1, nset_file
	                        keep_set(iset) = eindex_in_subset(rbuff(iset), subset_sorted)
	                     ENDDO
	                     nset = count(keep_set)
	                  ELSE
	                     nset = nset_file
	                  ENDIF

	                  IF (nset > 0) THEN
	                     IF (ndsp + nset > pixelset%nset) THEN
	                        CALL CoLM_stop('CoLM pixelset count changed while loading '//trim(psetname)//'.')
	                     ENDIF
	                     IF (use_subset) THEN
	                        pixelset%eindex(ndsp+1:ndsp+nset) = pack(rbuff, keep_set)
	                        pixelset%srcpos(ndsp+1:ndsp+nset) = &
	                           pack((/ (ipos, ipos = 1, nset_file) /), keep_set)
	                     ELSE
	                        pixelset%eindex(ndsp+1:ndsp+nset) = rbuff
	                        pixelset%srcpos(ndsp+1:ndsp+nset) = (/ (ipos, ipos = 1, nset) /)
	                     ENDIF

	                     ndsp = ndsp + nset
	                  ENDIF

	                  IF (allocated(keep_set)) deallocate(keep_set)
	                  deallocate(rbuff)
               ENDIF
#endif

            ENDDO

            IF (ndsp /= pixelset%nset) THEN
               CALL CoLM_stop('CoLM pixelset count changed between metadata and data reads for '//trim(psetname)//'.')
            ENDIF
         ENDIF
      ENDIF


      IF (.true. .and. pixelset%nset > 0) THEN
         IF (.not. allocated(mesh)) THEN
            CALL CoLM_stop('Cannot map '//trim(psetname)//' because the local CoLM element mesh is empty or inconsistent.')
         ENDIF
         IF (numelm < 1 .or. size(mesh) /= numelm) THEN
            CALL CoLM_stop('Cannot map '//trim(psetname)//' because the local CoLM element mesh is empty or inconsistent.')
         ENDIF
         IF (.not. allocated(pixelset%eindex)) THEN
            CALL CoLM_stop('Invalid element-index vector while loading '//trim(psetname)//'.')
         ENDIF
         IF (size(pixelset%eindex) /= pixelset%nset) THEN
            CALL CoLM_stop('Invalid element-index vector while loading '//trim(psetname)//'.')
         ENDIF

         allocate (mesh_sorted(numelm))
         allocate (mesh_order (numelm))
         DO ie = 1, numelm
            mesh_sorted(ie) = mesh(ie)%indx
            mesh_order(ie) = ie
         ENDDO
         IF (numelm > 1) CALL quicksort(numelm, mesh_sorted, mesh_order)
         IF (numelm > 1) THEN
            IF (any(mesh_sorted(2:numelm) == mesh_sorted(1:numelm-1))) THEN
               CALL CoLM_stop('The local CoLM mesh contains duplicate element IDs while loading '//trim(psetname)//'.')
            ENDIF
         ENDIF

         allocate (pixelset%ielm(pixelset%nset))
         DO iset = 1, pixelset%nset
            match = find_in_sorted_list1(pixelset%eindex(iset), numelm, mesh_sorted)
            IF (match < 1) THEN
               write(*,'(A,A,A,I0,A,I0)') 'CoLM ', trim(psetname), ' entry ', iset, &
                  ' references missing element ', pixelset%eindex(iset)
               CALL CoLM_stop('CoLM pixelset and MPAS-owned element mesh are inconsistent.')
            ENDIF
            pixelset%ielm(iset) = mesh_order(match)
         ENDDO

         deallocate (mesh_sorted)
         deallocate (mesh_order)
      ENDIF

      CALL pixelset%set_vecgs

      CALL ncio_read_vector (filename, 'ipxstt', pixelset, pixelset%ipxstt)
      CALL ncio_read_vector (filename, 'ipxend', pixelset, pixelset%ipxend)
      CALL ncio_read_vector (filename, 'settyp', pixelset, pixelset%settyp)

      IF (.true. .and. pixelset%nset > 0) THEN
         IF (.not. allocated(pixelset%ipxstt) .or. .not. allocated(pixelset%ipxend) .or. &
             .not. allocated(pixelset%settyp)) THEN
            CALL CoLM_stop('CoLM pixelset identity vectors are incomplete for '//trim(psetname)//'.')
         ENDIF
         IF (size(pixelset%ipxstt) /= pixelset%nset .or. size(pixelset%ipxend) /= pixelset%nset .or. &
             size(pixelset%settyp) /= pixelset%nset) THEN
            CALL CoLM_stop('CoLM pixelset identity vector lengths are inconsistent for '//trim(psetname)//'.')
         ENDIF
         DO iset = 1, pixelset%nset
            ie = pixelset%ielm(iset)
            IF (ie < 1 .or. ie > numelm) THEN
               CALL CoLM_stop('CoLM pixelset references an invalid local element for '//trim(psetname)//'.')
            ENDIF
            IF (pixelset%ipxstt(iset) == -1) THEN
               IF (pixelset%ipxend(iset) /= -1) THEN
                  CALL CoLM_stop('CoLM virtual pixelset has inconsistent pixel bounds for '//trim(psetname)//'.')
               ENDIF
            ELSEIF (pixelset%ipxstt(iset) < 1 .or. pixelset%ipxend(iset) < pixelset%ipxstt(iset) .or. &
                    pixelset%ipxend(iset) > mesh(ie)%npxl) THEN
               CALL CoLM_stop('CoLM pixelset references pixels outside its element for '//trim(psetname)//'.')
            ENDIF
         ENDDO
      ENDIF

      IF (.true.) THEN
         IF (pixelset%nset == 0) THEN
            write(*,*) 'Warning: 0 ',trim(psetname), ' on rank :', mpas_rank
         ENDIF
      ENDIF

      numset = pixelset%nset

      pixelset%has_shared = .false.
      IF (.true.) THEN
         DO iset = 1, pixelset%nset-1
            IF ((pixelset%ielm(iset) == pixelset%ielm(iset+1)) &
               .and. (pixelset%ipxstt(iset) == pixelset%ipxstt(iset+1))) THEN
               pixelset%has_shared = .true.
               exit
            ENDIF
         ENDDO
      ENDIF

#ifdef MPAS_MPI
      CALL mpi_allreduce (MPI_IN_PLACE, pixelset%has_shared, 1, MPI_LOGICAL, &
         MPI_LOR, mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('shared-pixelset presence reduction')
#endif

      IF (pixelset%has_shared) THEN
         CALL ncio_read_vector (filename, 'pctshared', pixelset, pixelset%pctshared)
      ENDIF

#ifdef CoLMDEBUG
      IF (.true.)  write(*,*) numset, trim(psetname), ' on group', mpas_rank
#endif

      IF (allocated(subset_sorted)) deallocate(subset_sorted)

   END SUBROUTINE pixelset_load_from_file

END MODULE MOD_SrfdataRestart
