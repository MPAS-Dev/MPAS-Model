FC = $(shell nc-config --fc)
FFLAGS = -O3
FCINCLUDES = $(shell nc-config --fflags)
FCLIBS = $(shell nc-config --flibs)

all: grid_rotate

grid_rotate: grid_rotate.f90
	$(FC) grid_rotate.f90 -o grid_rotate $(FFLAGS) $(FCINCLUDES) $(FCLIBS)

clean:
	rm grid_rotate
