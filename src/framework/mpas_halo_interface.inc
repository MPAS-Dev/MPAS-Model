   !
   ! Abstract interface for routine used to communicate halos of fields
   ! in a named group
   !
   abstract interface
      subroutine halo_exchange_routine(domain, halo_group, ierr)

         use mpas_derived_types, only : domain_type

         type (domain_type), intent(inout) :: domain
         character(len=*), intent(in) :: halo_group
         integer, intent(out), optional :: ierr

      end subroutine halo_exchange_routine
   end interface
