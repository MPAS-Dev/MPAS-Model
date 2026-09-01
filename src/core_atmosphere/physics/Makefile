.SUFFIXES: .F .o .F90

ifeq ($(CORE),atmosphere)
COREDEF = -Dmpas
endif

all:
	./../tools/manage_externals/checkout_externals --externals ./../Externals.cfg
	$(MAKE) lookup_tables core_physics_init core_physics_mmm core_UGWP core_mynnedmf core_mynnsfc core_physics_wrf core_physics_noahmp core_physics

dummy:
	echo "****** compiling physics ******"

OBJS_init = \
	ccpp_kind_types.o        \
	mpas_atmphys_constants.o \
	mpas_atmphys_date_time.o \
	mpas_atmphys_functions.o \
	mpas_atmphys_utilities.o

OBJS = \
	mpas_atmphys_camrad_init.o         \
	mpas_atmphys_control.o             \
	mpas_atmphys_driver.o              \
	mpas_atmphys_driver_cloudiness.o   \
	mpas_atmphys_driver_convection.o   \
	mpas_atmphys_driver_gwdo.o         \
	mpas_atmphys_driver_lsm.o          \
	mpas_atmphys_driver_lsm_noahmp.o   \
	mpas_atmphys_driver_microphysics.o \
	mpas_atmphys_driver_oml.o          \
	mpas_atmphys_driver_pbl.o          \
	mpas_atmphys_driver_radiation_lw.o \
	mpas_atmphys_driver_radiation_sw.o \
	mpas_atmphys_driver_seaice.o       \
	mpas_atmphys_driver_sfclayer.o     \
	mpas_atmphys_finalize.o            \
	mpas_atmphys_init.o                \
	mpas_atmphys_init_microphysics.o   \
	mpas_atmphys_interface.o           \
	mpas_atmphys_landuse.o             \
	mpas_atmphys_lsm_noahinit.o        \
	mpas_atmphys_lsm_noahmpinit.o      \
	mpas_atmphys_lsm_noahmpfinalize.o  \
	mpas_atmphys_lsm_shared.o          \
	mpas_atmphys_manager.o             \
	mpas_atmphys_o3climatology.o       \
	mpas_atmphys_packages.o            \
	mpas_atmphys_rrtmg_lwinit.o        \
	mpas_atmphys_rrtmg_swinit.o        \
	mpas_atmphys_sfc_diagnostics.o     \
	mpas_atmphys_todynamics.o          \
	mpas_atmphys_update_surface.o      \
	mpas_atmphys_update.o              \
	mpas_atmphys_vars.o

lookup_tables:
	./checkout_data_files.sh

core_physics_mmm: core_physics_init
	(cd physics_mmm; $(MAKE) -f Makefile.mpas all)

core_UGWP: core_physics_init
	(cd physics_noaa/UGWP; $(MAKE) all)

core_mynnedmf: core_physics_init
	(cd physics_noaa/MYNN-EDMF/src; ln -sf ./MPAS/Makefile Makefile; ln -sf ./MPAS/module_bl_mynnedmf_common.F90 module_bl_mynnedmf_common.F90; $(MAKE) all COREDEF="$(COREDEF)")

core_mynnsfc: core_physics_init
	(cd physics_noaa/MYNN-SFC; cp ./MPAS/Makefile .; cp ./MPAS/module_sf_mynnsfc_driver.F90 .; cp ./MPAS/module_sf_mynnsfc_common.F90 .; $(MAKE) all COREDEF="$(COREDEF)")

core_physics_wrf: core_physics_init core_physics_mmm core_UGWP
	(cd physics_wrf; $(MAKE) all COREDEF="$(COREDEF)")

core_physics_noahmp:
	(cd physics_noahmp/utility; $(MAKE) all COREDEF="$(COREDEF)")
	(cd physics_noahmp/src; $(MAKE) all COREDEF="$(COREDEF)")
	(cd physics_noahmp/drivers/mpas; $(MAKE) all COREDEF="$(COREDEF)")

core_physics_init: $(OBJS_init)

core_physics: core_physics_wrf core_physics_noahmp core_mynnedmf core_mynnsfc
	($(MAKE) phys_interface COREDEF="$(COREDEF)")
	ar -ru libphys.a $(OBJS_init) $(OBJS)
	($(MAKE) -C ./physics_mmm -f Makefile.mpas physics_mmm_lib)
	($(MAKE) -C ./physics_wrf physics_wrf_lib)
	($(MAKE) -C ./physics_noahmp/drivers/mpas driver_lib)
	($(MAKE) -C ./physics_noahmp/src src_lib)
	($(MAKE) -C ./physics_noahmp/utility utility_lib)

phys_interface: $(OBJS)

# DEPENDENCIES:
mpas_atmphys_camrad_init.o: \
	mpas_atmphys_constants.o \
	mpas_atmphys_utilities.o

mpas_atmphys_control.o: \
	mpas_atmphys_utilities.o \
	mpas_atmphys_vars.o

mpas_atmphys_driver.o: \
	mpas_atmphys_driver_cloudiness.o \
	mpas_atmphys_driver_convection.o \
	mpas_atmphys_driver_gwdo.o \
	mpas_atmphys_driver_lsm.o \
	mpas_atmphys_driver_lsm_noahmp.o \
	mpas_atmphys_driver_pbl.o \
	mpas_atmphys_driver_radiation_lw.o \
	mpas_atmphys_driver_radiation_sw.o \
	mpas_atmphys_driver_seaice.o \
	mpas_atmphys_driver_sfclayer.o \
	mpas_atmphys_driver_oml.o \
	mpas_atmphys_constants.o \
	mpas_atmphys_interface.o \
	mpas_atmphys_sfc_diagnostics.o \
	mpas_atmphys_update.o \
	mpas_atmphys_vars.o

mpas_atmphys_driver_cloudiness.o: \
	mpas_atmphys_constants.o \
	mpas_atmphys_vars.o

mpas_atmphys_driver_convection.o: \
	mpas_atmphys_constants.o \
	mpas_atmphys_utilities.o \
	mpas_atmphys_vars.o

mpas_atmphys_driver_gwdo.o: \
	mpas_atmphys_vars.o \
	mpas_atmphys_manager.o

mpas_atmphys_driver_lsm.o: \
	mpas_atmphys_constants.o \
	mpas_atmphys_landuse.o \
	mpas_atmphys_lsm_noahinit.o \
	mpas_atmphys_vars.o

mpas_atmphys_driver_lsm_noahmp.o: \
	mpas_atmphys_constants.o \
	mpas_atmphys_manager.o \
	mpas_atmphys_vars.o

mpas_atmphys_driver_microphysics.o: \
	mpas_atmphys_constants.o \
	mpas_atmphys_init_microphysics.o \
	mpas_atmphys_interface.o \
	mpas_atmphys_vars.o

mpas_atmphys_driver_oml.o: \
	mpas_atmphys_constants.o \
	mpas_atmphys_landuse.o \
	mpas_atmphys_vars.o

mpas_atmphys_driver_pbl.o: \
	mpas_atmphys_constants.o \
	mpas_atmphys_vars.o

mpas_atmphys_driver_radiation_lw.o: \
	mpas_atmphys_camrad_init.o \
	mpas_atmphys_constants.o \
	mpas_atmphys_driver_radiation_sw.o \
	mpas_atmphys_manager.o \
	mpas_atmphys_rrtmg_lwinit.o \
	mpas_atmphys_vars.o

mpas_atmphys_driver_radiation_sw.o: \
	mpas_atmphys_camrad_init.o \
	mpas_atmphys_constants.o \
	mpas_atmphys_manager.o \
	mpas_atmphys_rrtmg_swinit.o \
	mpas_atmphys_vars.o

mpas_atmphys_driver_seaice.o: \
	mpas_atmphys_constants.o \
	mpas_atmphys_lsm_shared.o \
	mpas_atmphys_vars.o

mpas_atmphys_driver_sfclayer.o: \
	mpas_atmphys_constants.o \
	mpas_atmphys_vars.o

mpas_atmphys_finalize.o: \
	mpas_atmphys_lsm_noahmpfinalize.o

mpas_atmphys_init.o: \
	mpas_atmphys_driver_convection.o \
	mpas_atmphys_driver_lsm.o \
	mpas_atmphys_driver_microphysics.o \
	mpas_atmphys_driver_pbl.o \
	mpas_atmphys_driver_radiation_lw.o \
	mpas_atmphys_driver_radiation_sw.o \
	mpas_atmphys_driver_sfclayer.o \
	mpas_atmphys_lsm_noahmpinit.o \
	mpas_atmphys_landuse.o \
	mpas_atmphys_o3climatology.o \
	mpas_atmphys_vars.o

mpas_atmphys_interface.o: \
	mpas_atmphys_constants.o \
	mpas_atmphys_vars.o

mpas_atmphys_landuse.o: \
	mpas_atmphys_utilities.o \
	mpas_atmphys_vars.o

mpas_atmphys_lsm_noahinit.o: \
	mpas_atmphys_constants.o \
	mpas_atmphys_utilities.o

mpas_atmphys_lsm_noahmpinit.o: \
	mpas_atmphys_utilities.o \
	mpas_atmphys_vars.o

mpas_atmphys_lsm_noahmpfinalize.o : \
	mpas_atmphys_vars.o

mpas_atmphys_manager.o: \
	mpas_atmphys_constants.o \
	mpas_atmphys_o3climatology.o \
	mpas_atmphys_update_surface.o \
	mpas_atmphys_utilities.o \
	mpas_atmphys_vars.o

mpas_atmphys_o3climatology.o: \
	mpas_atmphys_date_time.o \
	mpas_atmphys_constants.o \
	mpas_atmphys_utilities.o

mpas_atmphys_rrtmg_lwinit.o: \
	mpas_atmphys_constants.o \
	mpas_atmphys_utilities.o

mpas_atmphys_rrtmg_swinit.o: \
	mpas_atmphys_constants.o \
	mpas_atmphys_utilities.o

mpas_atmphys_sfc_diagnostics.o: \
	mpas_atmphys_constants.o \
	mpas_atmphys_vars.o

mpas_atmphys_todynamics.o: \
	mpas_atmphys_constants.o \
	mpas_atmphys_vars.o

mpas_atmphys_update_surface.o: \
	mpas_atmphys_date_time.o \
	mpas_atmphys_constants.o \
	mpas_atmphys_vars.o

mpas_atmphys_update.o: \
	mpas_atmphys_driver_convection.o \
	mpas_atmphys_vars.o

clean:
	$(RM) *.o *.mod *.f90 libphys.a
	( cd physics_wrf; $(MAKE) clean )
	( if [ -d physics_mmm ]; then cd physics_mmm; $(MAKE) -f Makefile.mpas clean; fi; )
	( cd physics_noahmp/drivers/mpas; $(MAKE) clean )
	( cd physics_noahmp/src; $(MAKE) clean )
	( cd physics_noahmp/utility; $(MAKE) clean )
	( if [ -d physics_noaa/UGWP ]; then cd physics_noaa/UGWP; $(MAKE) clean; fi )
	( if [ -d physics_noaa/MYNN-EDMF ]; then cd physics_noaa/MYNN-EDMF/src; $(MAKE) -f ./MPAS/Makefile clean; fi )
	( if [ -d physics_noaa/MYNN-SFC ]; then cd physics_noaa/MYNN-SFC; $(MAKE) clean; fi )
	@# Certain systems with intel compilers generate *.i files
	@# This removes them during the clean process
	$(RM) *.i

.F.o:
	$(RM) $@ $*.mod
ifeq "$(GEN_F90)" "true"
	$(CPP) $(CPPFLAGS) $(COREDEF) $(HYDROSTATIC) $(CPPINCLUDES) -I../../framework $< > $*.f90
	$(FC) $(FFLAGS) -c $*.f90 $(FCINCLUDES) -I./physics_mmm -I./physics_wrf -I./physics_noahmp -I./physics_noahmp/utility -I./physics_noahmp/drivers/mpas -I./physics_noahmp/src -I./physics_noaa/UGWP -I.. -I../../framework $(MPAS_ESMF_INC)
else
	$(FC) $(CPPFLAGS) $(COREDEF) $(HYDROSATIC) $(FFLAGS) -c $*.F $(CPPINCLUDES) $(FCINCLUDES) -I./physics_mmm -I./physics_wrf -I./physics_noahmp -I./physics_noahmp/utility -I./physics_noahmp/drivers/mpas -I./physics_noahmp/src -I./physics_noaa/UGWP -I./physics_noaa/MYNN-EDMF/src -I./physics_noaa/MYNN-SFC -I.. -I../../framework $(MPAS_ESMF_INC)
endif

.F90.o:
	$(RM) $@ $*.mod
ifeq "$(GEN_F90)" "true"
        $(CPP) $(CPPFLAGS) $(COREDEF) $(HYDROSTATIC) $(CPPINCLUDES) $< > $*.f90
	$(FC) $(FFLAGS) -c $*.f90 $(FCINCLUDES) -I./physics_mmm -I./physics_wrf -I./physics_noaa/UGWP -I./physics_noaa/MYNN-EDMF/src -I./physics_noaa/MYNN-SFC  -I.. -I../../framework $(MPAS_ESMF_INC)
else
	$(FC) $(CPPFLAGS) $(COREDEF) $(HYDROSATIC) $(FFLAGS) -c $*.F90 $(CPPINCLUDES) $(FCINCLUDES) -I./physics_mmm -I./physics_wrf -I./physics_noaa/TEMPO -I./physics_noaa/UGWP -I./physics_noaa/MYNN-EDMF/src -I./physics_noaa/MYNN-SFC -I.. -I../../framework $(MPAS_ESMF_INC)
endif
