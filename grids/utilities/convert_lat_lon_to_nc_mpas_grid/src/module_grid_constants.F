module grid_constants

   real, parameter :: radius = 1.
   real, parameter :: pii = 3.141592653589793
   real, parameter :: rtod = 180./pii
   real, parameter :: s_area = 4.*pii*radius*radius

   integer, parameter :: nicos_vertices = 12
   integer, parameter :: nicos_neighbors = 5

end module grid_constants
