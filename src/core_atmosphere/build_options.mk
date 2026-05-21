PWD=$(shell pwd)
EXE_NAME=atmosphere_model
NAMELIST_SUFFIX=atmosphere
override CPPFLAGS += -DCORE_ATMOSPHERE
FCINCLUDES += -I$(PWD)/src/core_atmosphere/physics/physics_noahmp/drivers/mpas \
              -I$(PWD)/src/core_atmosphere/physics/physics_noahmp/utility \
              -I$(PWD)/src/core_atmosphere/physics/physics_noahmp/src

report_builds:
	@echo "CORE=atmosphere"
