.SUFFIXES: .F .c .o

ifneq "$(ESM)" ""

include Makefile.in.$(ESM)

else

BUILD_ALL = mpas
ifeq "$(NUOPC)" "true"
BUILD_ALL = nuopc_lib
endif

all: $(BUILD_ALL)

mpas: $(AUTOCLEAN_DEPS) externals frame ops dycore drver
	$(LINKER) $(LDFLAGS) -o $(EXE_NAME) driver/*.o -L. -ldycore -lops -lframework $(LIBS) $(MPAS_ESMF_INC) $(MPAS_ESMF_LIB)

externals: $(AUTOCLEAN_DEPS)
	( cd external; $(MAKE) FC="$(FC)" SFC="$(SFC)" CC="$(CC)" SCC="$(SCC)" FFLAGS="$(FFLAGS)" CFLAGS="$(CFLAGS)" CPP="$(CPP)" NETCDF="$(NETCDF)" CORE="$(CORE)" all )

drver:  $(AUTOCLEAN_DEPS) externals frame ops dycore
	( cd driver; $(MAKE) CPPFLAGS="$(CPPFLAGS)" CPPINCLUDES="$(CPPINCLUDES)" all ) 
endif

build_tools: externals
	(cd tools; $(MAKE) CPPFLAGS="$(CPPFLAGS)" CC="$(SCC)" CFLAGS="$(CFLAGS)")

frame: $(AUTOCLEAN_DEPS) externals
	( cd framework; $(MAKE) CPPFLAGS="$(CPPFLAGS)" CPPINCLUDES="$(CPPINCLUDES)" all ) 
	ln -sf framework/libframework.a libframework.a

ops: $(AUTOCLEAN_DEPS) externals frame
	( cd operators; $(MAKE) CPPFLAGS="$(CPPFLAGS)" CPPINCLUDES="$(CPPINCLUDES)" all ) 
	ln -sf operators/libops.a libops.a

dycore: $(AUTOCLEAN_DEPS) build_tools externals frame ops
	( cd core_$(CORE); $(MAKE) CPPFLAGS="$(CPPFLAGS)" CPPINCLUDES="$(CPPINCLUDES)" REG_PARSE="$(PWD)/tools/registry/parse" gen_includes )
	( cd core_$(CORE); $(MAKE) CPPFLAGS="$(CPPFLAGS)" CPPINCLUDES="$(CPPINCLUDES)" NL_GEN="$(PWD)/tools/input_gen/namelist_gen" ST_GEN="$(PWD)/tools/input_gen/streams_gen" core_input_gen )
	( cd core_$(CORE); $(MAKE) CPPFLAGS="$(CPPFLAGS)" CPPINCLUDES="$(CPPINCLUDES)" all ) 
	ln -sf core_$(CORE)/libdycore.a libdycore.a

nuopc_lib: $(AUTOCLEAN_DEPS) externals frame ops dycore drver
	cp framework/*.mod $(MPAS_MODDIR)/.
	( cd core_$(CORE); $(MAKE) CPPFLAGS="$(CPPFLAGS)" CPPINCLUDES="$(CPPINCLUDES)" FFLAGS="$(FFLAGS)" FCINCLUDES="$(FCINCLUDES)" nuopc )
	ar rcs $(MPAS_LIBDIR)/libmpas_nuopc.a \
		core_$(CORE)/nuopc/*.o \
		core_$(CORE)/libphys/*.o \
		core_$(CORE)/*.o \
		core_$(CORE)/diagnostics/*.o \
		core_$(CORE)/libchem/*.o \
		core_$(CORE)/dynamics/*.o \
		driver/*.o \
		framework/*.o \
		operators/*.o \
		external/ezxml/*.o \
		external/SMIOL/*.o
	ranlib $(MPAS_LIBDIR)/libmpas_nuopc.a
	cp core_$(CORE)/nuopc/*.mod $(MPAS_MODDIR)/.

CLEAN_ALL = clean_shared clean_core
ifeq "$(NUOPC)" "true"
CLEAN_ALL += clean_nuopc
endif

clean: $(CLEAN_ALL)

clean_core:
ifeq "$(AUTOCLEAN)" "true"
	$(info )
	$(info *********************************************************************************************)
	$(info The $(CORE) core will be cleaned and re-compiled.)
	$(info *********************************************************************************************)
	$(info )
endif
	if [ -d core_$(CORE) ] ; then \
	   ( cd core_$(CORE); $(MAKE) clean ) \
	fi;

clean_shared:
ifeq "$(AUTOCLEAN)" "true"
	$(info )
	$(info *********************************************************************************************)
	$(info The infrastructure will be cleaned and re-compiled.)
	$(info *********************************************************************************************)
	$(info )
endif
	$(RM) libframework.a libops.a libdycore.a lib$(CORE).a *.o
	( cd tools; $(MAKE) clean )
	( cd external; $(MAKE) clean )
	( cd framework; $(MAKE) clean )
	( cd operators; $(MAKE) clean )
	( cd driver; $(MAKE) clean )

clean_nuopc:
	$(info )
	$(info *********************************************************************************************)
	$(info The NUOPC library will be cleaned.)
	$(info *********************************************************************************************)
	$(info )
	$(RM) $(MPAS_LIBDIR)/libmpas_nuopc.a
	$(RM) $(MPAS_MODDIR)/*.mod
