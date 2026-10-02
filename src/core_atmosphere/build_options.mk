PWD=$(shell pwd)
EXE_NAME=atmosphere_model
NAMELIST_SUFFIX=atmosphere
override CPPFLAGS += -DCORE_ATMOSPHERE
override CPPFLAGS += -DNOAHMP_MPAS
FCINCLUDES += -I$(PWD)/src/core_atmosphere/physics/physics_noahmp/drivers/mpas \
              -I$(PWD)/src/core_atmosphere/physics/physics_noahmp/utility \
              -I$(PWD)/src/core_atmosphere/physics/physics_noahmp/src
# NetCDF-Fortran is required by Noah-MP (SnowInputSnicarMod.F90)
NF_CONFIG := $(shell command -v nf-config 2>/dev/null)
ifneq ($(strip $(NF_CONFIG)),)
FCINCLUDES += $(shell $(NF_CONFIG) --fflags)
LIBS       += $(shell $(NF_CONFIG) --flibs)
else
$(error NetCDF-Fortran is required for MPAS-NoahMP, but nf-config was not found)
endif

report_builds:
	@echo "CORE=atmosphere"
