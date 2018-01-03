    type mpas_coupler_type
        integer :: intracomm
        integer :: intercomm
        logical :: initialized_mpi
        integer :: intercomm_rank
        integer :: intercomm_size
        integer :: thread_id
        procedure(mpas_coupler_role_function), pointer, pass(self) :: role
        procedure(mpas_coupler_roleleader_function), pointer, pass(self) :: role_leader
    end type mpas_coupler_type

    abstract interface
        integer function mpas_coupler_role_function(self)
            import mpas_coupler_type
            class (mpas_coupler_type), intent(in) :: self
        end function mpas_coupler_role_function
    end interface

    abstract interface
        integer function mpas_coupler_roleleader_function(self, role)
            import mpas_coupler_type
            class (mpas_coupler_type), intent(in) :: self
            integer, intent(in) :: role
        end function mpas_coupler_roleleader_function
    end interface
