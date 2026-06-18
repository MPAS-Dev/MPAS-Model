! Copyright (c) 2026,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!-----------------------------------------------------------------------
!  mpas_compute_dim
!
!> \brief MPAS Compute Dimensions
!> \author Anthony Islas
!> \date   02/17/2026
!> \details
!>  This module will contain the mpas_compute_dim_list type, along with routines
!>  to register and access a computed dimension function.
!
!-----------------------------------------------------------------------

module mpas_compute_dim

   use mpas_kind_types
   use mpas_derived_types
   use mpas_stream_manager
   use mpas_log

   implicit none

   contains


!-----------------------------------------------------------------------
!  routine mpas_compute_dim_create_compute_dim_list
!
!> \brief MPAS compute_dim list create routine
!> \author Anthony Islas
!> \date   02/17/2026
!> \details
!>  This routine creates a computed dimension list.
!
!-----------------------------------------------------------------------
   subroutine mpas_compute_dim_create_compute_dim_list(computeDimList)!{{{
      type (mpas_compute_dim_list), pointer :: computeDimList

      integer :: errLocal

      if ( .not. associated(computeDimList) ) then
         allocate(computeDimList)
         computeDimList % dimName = ''
         computeDimList % nameLen = -1
         nullify(computeDimList % compDimFunc)
         nullify(computeDimList % next)
      end if

   end subroutine!}}}


!-----------------------------------------------------------------------
!  routine mpas_compute_dim_destroy_compute_dim_list
!
!> \brief MPAS compute_dim list destruction routine
!> \author Anthony Islas
!> \date   02/17/2026
!> \details
!>  This routine destroys a computed dimension list.
!
!-----------------------------------------------------------------------
   subroutine mpas_compute_dim_destroy_compute_dim_list(computeDimList)!{{{
      type (mpas_compute_dim_list), pointer :: computeDimList

      type (mpas_compute_dim_list), pointer :: cursor

      do while (associated(computeDimList % next))
         cursor => computeDimList % next

         if ( associated(cursor % next) ) then
            computeDimList % next => cursor % next
         else
            nullify(computeDimList % next)
         end if

         deallocate(cursor)
      end do

      deallocate(computeDimList)
   end subroutine mpas_compute_dim_destroy_compute_dim_list!}}}


!-----------------------------------------------------------------------
!  routine mpas_compute_dim_register_method
!
!> \brief MPAS compute_dim list register routine
!> \author Anthony Islas
!> \date   02/17/2026
!> \details
!>  This routine registers a computed dimension function in a computed dimension list.
!>  If present, the optional argument iErr is set to 1 if 
!>  the function is already registered and to 0 otherwise.
!
!-----------------------------------------------------------------------
   subroutine mpas_compute_dim_register_method(computeDimList, dimName, compDimFunc, iErr)!{{{
      type (mpas_compute_dim_list), pointer :: computeDimList
      character (len=*), intent(in) :: dimName
      procedure (mpas_compute_dim_function), pointer :: compDimFunc
      integer, intent(out) :: iErr

      type (mpas_compute_dim_list), pointer :: cursor
      type (mpas_compute_dim_list), pointer :: cursorPrev
      integer :: nameLen

      iErr = 0

      nameLen = len_trim(dimName)

      cursor => computeDimList
      nullify(cursorPrev)
      do while (associated(cursor))
         if ( nameLen == cursor % nameLen ) then
            if ( trim(dimName) == trim(cursor % dimName) ) then
               call mpas_log_write('Computed dimension ' // trim(dimName) // ' is already registered.')
               iErr = 1
               return
            end if
         end if
         cursorPrev => cursor
         cursor => cursor % next
      end do

      cursor => cursorPrev

      allocate(cursor % next)
      cursor => cursor % next
      cursor % dimName = dimName
      cursor % compDimFunc => compDimFunc
      cursor % nameLen = nameLen
      nullify(cursor % next)
   end subroutine mpas_compute_dim_register_method!}}}


!-----------------------------------------------------------------------
!  routine mpas_compute_dim_get_method
!
!> \brief MPAS compute_dim list query routine
!> \author Anthony Islas
!> \date   02/17/2026
!> \details
!>  This routine querys a computed dimension list for a specific computed dimension function.
!>  If present, the optional argument iErr is set to 1 if 
!>  the requested function is not found and to 0 otherwise.
!
!-----------------------------------------------------------------------
   subroutine mpas_compute_dim_get_method(computeDimList, dimName, compDimFunc, iErr)!{{{
      type (mpas_compute_dim_list), pointer :: computeDimList
      character (len=*), intent(in) :: dimName
      procedure (mpas_compute_dim_function), pointer :: compDimFunc
      integer, intent(out) :: iErr

      type (mpas_compute_dim_list), pointer :: cursor
      integer :: nameLen

      iErr = 0

      nameLen = len_trim(dimName)

      cursor => computeDimList
      do while (associated(cursor))
         if ( nameLen == cursor % nameLen ) then
            if ( trim(cursor % dimName) == trim(dimName) ) then
               compDimFunc => cursor % compDimFunc
               return
            end if
         end if
         cursor => cursor % next
      end do

      iErr = 1
      call mpas_log_write('Computed dimension '//trim(dimName)//' not found in computed dimension list.')

   end subroutine mpas_compute_dim_get_method!}}}


!-----------------------------------------------------------------------
!  routine mpas_compute_dim_remove_method
!
!> \brief MPAS compute_dim list removal routine
!> \author Anthony Islas
!> \date   02/17/2026
!> \details
!>  This routine removes a specific computed dimension function from a  computed dimension list.
!
!-----------------------------------------------------------------------
   subroutine mpas_compute_dim_remove_method(computeDimList, dimName, iErr)!{{{
      type (mpas_compute_dim_list), pointer :: computeDimList
      character (len=*), intent(in) :: dimName
      integer, intent(out) :: iErr

      integer :: nameLen
      type (mpas_compute_dim_list), pointer :: cursor, cursorPrev

      iErr = 0

      nameLen = len_trim(dimName)

      cursor => computeDimList
      nullify(cursorPrev)
      do while (associated(cursor))
         if ( nameLen == cursor % nameLen ) then
            if ( trim(cursor % dimName) == trim(dimName) ) then
               if ( associated(cursor % next) ) then
                  cursorPrev % next => cursor % next
               else
                  nullify(cursorPrev % next)
               end if

               deallocate(cursor)
               return
            end if
         end if
         cursorPrev => cursor
         cursor => cursor % next
      end do
   end subroutine mpas_compute_dim_remove_method!}}}

end module mpas_compute_dim
