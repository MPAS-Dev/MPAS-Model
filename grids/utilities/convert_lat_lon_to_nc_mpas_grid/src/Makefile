.SUFFIXES: .F .o

OBJS = grid_gen.o STRIPACK.o module_grid_params.o module_grid_constants.o \
       module_data_types.o module_sphere_utilities.o module_voronoi_utils.o \
       module_grid_gen_utils.o module_scvt.o module_write_netcdf.o module_grid_meta.o

all: $(OBJS)
	$(FC) $(PROMOTION) $(LDFLAGS) -o grid_gen $(OBJS) $(NCLIBS)

#$(FC) $(PROMOTION) $(LDFLAGS) -o grid_gen $(OBJS) -L$(NETCDF)/lib $(NCLIBS)

grid_gen.o: module_grid_params.o module_grid_constants.o module_data_types.o module_grid_gen_utils.o module_voronoi_utils.o STRIPACK.o module_scvt.o module_grid_meta.o

module_grid_gen_utils.o: module_sphere_utilities.o module_data_types.o module_grid_params.o module_grid_constants.o

module_scvt.o: module_data_types.o module_sphere_utilities.o module_voronoi_utils.o module_grid_constants.o module_grid_params.o

module_write_netcdf.o: module_grid_params.o

module_data_types.o:

module_grid_meta.o: module_data_types.o module_grid_constants.o module_sphere_utilities.o module_write_netcdf.o module_scvt.o

module_sphere_utilities.o: module_data_types.o

module_grid_constants.o:

module_grid_params.o:

module_voronoi_utils.o: module_grid_constants.o STRIPACK.o

STRIPACK.o: STRIPACK.f
	$(FC) $(F77FLAGS) $(PROMOTION) -c STRIPACK.f

.F.o:
	$(FC) $(CPPFLAGS) $(FFLAGS) $(PROMOTION) -c $*.F -I$(NETCDF)/include

clean:
	rm -f *.o *.mod grid_gen
