    type mpas_coupler_type
        integer :: intracomm
        integer :: intercomm
        logical :: initialized_mpi
        integer :: mpi_rank
        integer :: thread_id
        procedure(mpas_coupler_role_function), pointer, pass(self) :: role
    end type mpas_coupler_type

    abstract interface
        integer function mpas_coupler_role_function(self)
            import mpas_coupler_type
            class (mpas_coupler_type), intent(in) :: self
        end function mpas_coupler_role_function
    end interface
