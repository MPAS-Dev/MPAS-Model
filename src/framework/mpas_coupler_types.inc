    type mpas_coupler_type
        integer :: role
        integer :: intracomm
        integer :: intercomm
        logical :: initialized_mpi
    end type mpas_coupler_type
