
FC := gfortran
FFLAGS := -O -Wall -Wconversion-extra
LDFLAGS := 

SCRIPTS_DIR ?= ../scripts
MODULES_DIR ?= ../modules
TOOLBOX_DIR ?= ../toolbox

PROG1 = gsw_check
SRCS1 = gsw_check_functions.f90

PROG2 = poly_check
SRCS2 = gsw_poly_check.f90

MOD_TOOLBOX = $(MODULES_DIR)/gsw_mod_toolbox.mod
MOD_KINDS = $(MODULES_DIR)/gsw_mod_kinds.mod

MOD_TOOLBOX_SRC = $(MOD_TOOLBOX:.mod=.f90)

OBJS1 := $(SRCS1:.f90=.o)
OBJS2 := $(SRCS2:.f90=.o)

TOOLBOX_SRCS := $(wildcard $(TOOLBOX_DIR)/*.f90)
TOOLBOX_OBJS := $(TOOLBOX_SRCS:.f90=.o)

MODULES_SRCS := $(wildcard $(MODULES_DIR)/*.f90)
MODULES_OBJS := $(MODULES_SRCS:.f90=.o)
MODULES_MODS := $(MODULES_SRCS:.f90=.mod)

%.o : %.f90
	$(FC) -I$(MODULES_DIR) $(FFLAGS) -c $< -o $@

%.mod : %.f90
	cd $(MODULES_DIR); $(FC) $(FFLAGS) -c $(<F)
	touch $@

.PHONY:	all
all:	$(PROG1) $(PROG2)

$(PROG1): $(OBJS1) $(TOOLBOX_OBJS)
	$(FC) $(MODULES_DIR)/*.o $(TOOLBOX_DIR)/*.o $(LDFLAGS) $(LIBS) $< -o $@

$(OBJS1): $(SRCS1) $(MOD_TOOLBOX) $(MODULES_MODS)
	$(FC) -I$(MODULES_DIR) $(FFLAGS) -c $< -o $@

$(PROG2): $(OBJS2) $(TOOLBOX_OBJS)
	$(FC) $(MODULES_DIR)/*.o $(TOOLBOX_DIR)/*.o $(LDFLAGS) $(LIBS) $< -o $@

$(OBJS2): $(SRCS2) $(MOD_TOOLBOX) $(MODULES_MODS)
	$(FC) -I$(MODULES_DIR) $(FFLAGS) -c $< -o $@

$(MOD_TOOLBOX): $(MOD_TOOLBOX_SRC) $(MOD_KINDS)

$(MOD_TOOLBOX_SRC):
	cd $(MODULES_DIR); $(SCRIPTS_DIR)/make_mod.sh $(TOOLBOX_DIR)/*.f90

.PHONY:	toolbox
toolbox:
	rm $(MOD_TOOLBOX_SRC)
	$(MAKE)

.PHONY:	clean
clean:
	rm $(MODULES_DIR)/*.o $(MODULES_DIR)/*.mod $(TOOLBOX_DIR)/*.o
	rm $(PROG1) $(OBJS1)
