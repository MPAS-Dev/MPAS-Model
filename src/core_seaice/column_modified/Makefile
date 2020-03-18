.SUFFIXES: .F90 .o

OBJS =  ice_colpkg_modified.o \
	ice_kinds_mod_modified.o \
	ice_warnings_modified.o \
	ice_colpkg_shared_modified.o \
	ice_constants_colpkg_modified.o \
	ice_therm_shared_modified.o \
	ice_orbital_modified.o \
	ice_mushy_physics_modified.o \
	ice_therm_mushy_modified.o \
	ice_atmo_modified.o \
	ice_age_modified.o \
	ice_firstyear_modified.o \
	ice_flux_colpkg_modified.o \
	ice_meltpond_cesm_modified.o \
	ice_meltpond_lvl_modified.o \
	ice_meltpond_topo_modified.o \
	ice_therm_vertical_modified.o \
	ice_therm_bl99_modified.o \
	ice_therm_0layer_modified.o \
	ice_itd_modified.o \
	ice_colpkg_tracers_modified.o \
	ice_therm_itd_modified.o \
	ice_shortwave_modified.o \
	ice_mechred_modified.o \
        ice_aerosol_modified.o \
        ice_brine_modified.o \
        ice_algae_modified.o \
        ice_zbgc_modified.o \
        ice_zbgc_shared_modified.o \
	ice_zsalinity_modified.o \
	ice_snow_modified.o

all: $(OBJS)

ifneq "$(ESM)" ""
ice_constants_colpkg_modified.F90:
	cp constants/cesm/ice_constants_colpkg_modified.F90 .
else
ice_constants_colpkg_modified.F90:
	cp constants/cice/ice_constants_colpkg_modified.F90 .
endif

ice_zsalinity_modified.o: ice_kinds_mod_modified.o ice_constants_colpkg_modified.o ice_zbgc_shared_modified.o ice_warnings_modified.o ice_colpkg_tracers_modified.o ice_colpkg_shared_modified.o ice_therm_shared_modified.o ice_brine_modified.o ice_colpkg_shared_modified.o

ice_colpkg_modified.o: ice_kinds_mod_modified.o ice_constants_colpkg_modified.o ice_warnings_modified.o ice_colpkg_shared_modified.o ice_therm_shared_modified.o ice_orbital_modified.o ice_atmo_modified.o ice_age_modified.o ice_firstyear_modified.o ice_flux_colpkg_modified.o ice_meltpond_cesm_modified.o ice_meltpond_lvl_modified.o ice_meltpond_topo_modified.o ice_therm_vertical_modified.o ice_itd_modified.o ice_therm_itd_modified.o ice_shortwave_modified.o ice_mechred_modified.o ice_colpkg_tracers_modified.o ice_atmo_modified.o ice_mushy_physics_modified.o ice_zbgc_modified.o ice_zbgc_shared_modified.o  ice_aerosol_modified.o ice_algae_modified.o ice_brine_modified.o ice_zsalinity_modified.o ice_snow_modified.o

ice_kinds_mod_modified.o:

ice_warnings_modified.o: ice_kinds_mod_modified.o

ice_constants_colpkg_modified.o: ice_constants_colpkg_modified.F90 ice_kinds_mod_modified.o

ice_colpkg_shared_modified.o: ice_kinds_mod_modified.o ice_constants_colpkg_modified.o

ice_mushy_physics_modified.o: ice_kinds_mod_modified.o ice_constants_colpkg_modified.o

ice_therm_shared_modified.o: ice_kinds_mod_modified.o ice_constants_colpkg_modified.o

ice_orbital_modified.o: ice_kinds_mod_modified.o ice_constants_colpkg_modified.o

ice_therm_mushy_modified.o: ice_kinds_mod_modified.o ice_constants_colpkg_modified.o ice_colpkg_shared_modified.o ice_therm_shared_modified.o ice_mushy_physics_modified.o ice_colpkg_tracers_modified.o

ice_atmo_modified.o: ice_kinds_mod_modified.o ice_constants_colpkg_modified.o ice_colpkg_tracers_modified.o

ice_age_modified.o: ice_kinds_mod_modified.o

ice_firstyear_modified.o: ice_kinds_mod_modified.o ice_constants_colpkg_modified.o

ice_flux_colpkg_modified.o: ice_kinds_mod_modified.o ice_constants_colpkg_modified.o

ice_meltpond_cesm_modified.o: ice_kinds_mod_modified.o ice_constants_colpkg_modified.o

ice_meltpond_lvl_modified.o: ice_kinds_mod_modified.o ice_constants_colpkg_modified.o ice_therm_shared_modified.o

ice_meltpond_topo_modified.o: ice_kinds_mod_modified.o ice_constants_colpkg_modified.o ice_therm_shared_modified.o

ice_therm_vertical_modified.o: ice_kinds_mod_modified.o ice_constants_colpkg_modified.o ice_colpkg_shared_modified.o ice_therm_shared_modified.o ice_therm_bl99_modified.o ice_therm_0layer_modified.o ice_therm_mushy_modified.o ice_mushy_physics_modified.o

ice_therm_bl99_modified.o: ice_kinds_mod_modified.o ice_constants_colpkg_modified.o ice_colpkg_shared_modified.o ice_therm_shared_modified.o

ice_therm_0layer_modified.o: ice_kinds_mod_modified.o ice_constants_colpkg_modified.o ice_therm_bl99_modified.o

ice_itd_modified.o: ice_kinds_mod_modified.o ice_constants_colpkg_modified.o ice_colpkg_tracers_modified.o ice_therm_shared_modified.o ice_zbgc_shared_modified.o

ice_colpkg_tracers_modified.o: ice_kinds_mod_modified.o ice_constants_colpkg_modified.o 

ice_therm_itd_modified.o: ice_kinds_mod_modified.o ice_constants_colpkg_modified.o ice_itd_modified.o ice_colpkg_tracers_modified.o ice_therm_shared_modified.o ice_therm_shared_modified.o  ice_mushy_physics_modified.o ice_zbgc_modified.o ice_zbgc_shared_modified.o

ice_shortwave_modified.o: ice_kinds_mod_modified.o ice_constants_colpkg_modified.o ice_orbital_modified.o

ice_mechred_modified.o: ice_kinds_mod_modified.o ice_constants_colpkg_modified.o ice_itd_modified.o ice_colpkg_tracers_modified.o

ice_aerosol_modified.o: ice_kinds_mod_modified.o ice_constants_colpkg_modified.o ice_colpkg_shared_modified.o ice_colpkg_tracers_modified.o ice_zbgc_shared_modified.o 

ice_algae_modified.o: ice_kinds_mod_modified.o ice_constants_colpkg_modified.o ice_colpkg_shared_modified.o ice_colpkg_tracers_modified.o ice_zbgc_shared_modified.o ice_aerosol_modified.o ice_warnings_modified.o

ice_brine_modified.o: ice_kinds_mod_modified.o ice_constants_colpkg_modified.o ice_colpkg_shared_modified.o ice_colpkg_tracers_modified.o ice_zbgc_shared_modified.o ice_therm_mushy_modified.o ice_mushy_physics_modified.o ice_therm_shared_modified.o

ice_zbgc_modified.o: ice_kinds_mod_modified.o ice_constants_colpkg_modified.o ice_colpkg_shared_modified.o ice_colpkg_tracers_modified.o ice_zbgc_shared_modified.o ice_therm_shared_modified.o ice_itd_modified.o 

ice_zbgc_shared_modified.o: ice_kinds_mod_modified.o ice_constants_colpkg_modified.o ice_colpkg_shared_modified.o 

ice_snow_modified.o: ice_kinds_mod_modified.o ice_constants_colpkg_modified.o ice_warnings_modified.o  ice_therm_vertical_modified.o ice_colpkg_shared_modified.o


.F90.o:
	$(RM) $@ $*.mod
ifeq "$(GEN_F90)" "true"
	$(CPP) $(CPPFLAGS) $(CPPINCLUDES) $< > $*.f90

	$(FC) $(FFLAGS) -c $*.f90 $(FCINCLUDES)
else
	$(FC) $(CPPFLAGS) $(FFLAGS) -c $*.F90 $(CPPINCLUDES) $(FCINCLUDES)
endif

clean:
	$(RM) *.o *.mod *.i90
	rm -f ice_constants_colpkg_modified.F90
