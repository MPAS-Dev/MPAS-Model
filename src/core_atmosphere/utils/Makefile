.SUFFIXES: .F .o

ifdef PHYSICS
    UTILS = build_tables tempo_build_tables
endif

all: $(UTILS)

build_tables: build_tables.o atmphys_build_tables_thompson.o
	$(LINKER) $(LDFLAGS) -o build_tables build_tables.o atmphys_build_tables_thompson.o -L../../framework -L../physics -lphys -lframework $(LIBS) $(MPAS_ESMF_LIB)
	mv build_tables ../../..

build_tables.o: \
	atmphys_build_tables_thompson.o

atmphys_build_tables_thompson.o: \
	../physics/physics_wrf/module_mp_thompson.o

tempo_build_tables: module_mp_tempo_tables.o tempo_build_tables.o
	$(LINKER) $(LDFLAGS) -o tempo_build_tables tempo_build_tables.o module_mp_tempo_tables.o -L../../framework -L../physics -lphys -lframework $(LIBS) $(MPAS_ESMF_LIB)
	mv tempo_build_tables ../../..

module_mp_tempo_tables.o: ../physics/physics_noaa/TEMPO/src/module_mp_tempo_tables.F90
	$(FC) $(CPPFLAGS) $(PHYSICS) $(FFLAGS) -c ../physics/physics_noaa/TEMPO/src/module_mp_tempo_tables.F90 $(CPPINCLUDES) $(FCINCLUDES) -I../../framework -I../../operators -I../physics -I../physics/physics_mmm -I../physics/physics_wrf -I../physics/physics_noaa/TEMPO $(MPAS_ESMF_INC)

tempo_build_tables.o: module_mp_tempo_tables.o ../physics/physics_noaa/TEMPO/tools/build_tables.F90
	$(FC) $(CPPFLAGS) $(PHYSICS) $(FFLAGS) -o tempo_build_tables.o -c ../physics/physics_noaa/TEMPO/tools/build_tables.F90 $(CPPINCLUDES) $(FCINCLUDES) -I../../framework -I../../operators -I../physics -I../physics/physics_mmm -I../physics/physics_wrf -I../physics/physics_noaa/TEMPO $(MPAS_ESMF_INC)

clean:
	$(RM) ../../../build_tables
	$(RM) ../../../tempo_build_tables
	$(RM) *.o *.mod *.f90
	@# Some Intel compilers generate *.i files; clean them up, too
	$(RM) *.i

.F.o:
	$(RM) $@ $*.mod
ifeq "$(GEN_F90)" "true"
	$(CPP) $(CPPFLAGS) $(PHYSICS) $(CPPINCLUDES) $< > $*.f90
	$(FC) $(FFLAGS) -c $*.f90 $(FCINCLUDES) -I../../framework -I../../operators -I../physics -I../physics/physics_mmm -I../physics/physics_wrf $(MPAS_ESMF_INC)
else
	$(FC) $(CPPFLAGS) $(PHYSICS) $(FFLAGS) -c $*.F $(CPPINCLUDES) $(FCINCLUDES) -I../../framework -I../../operators -I../physics -I../physics/physics_mmm -I../physics/physics_wrf $(MPAS_ESMF_INC)
endif
