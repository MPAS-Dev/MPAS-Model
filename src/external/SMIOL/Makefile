override CPPINCLUDES += -DSMIOL_PNETCDF

all: libsmiol.a libsmiolf.a

libsmiol.a: smiol.o smiol_utils.o
	ar -cr libsmiol.a smiol.o smiol_utils.o

libsmiolf.a: smiolf.o
	ar -cr libsmiolf.a smiolf.o

clean:
	$(RM) -f smiol.o smiol_utils.o libsmiol.a
	$(RM) -f smiolf.o smiolf.mod libsmiolf.a

# Cancel the built-in implicit rule for Modula-2 files (.mod) to avoid having 'make'
# try to create .o files from Fortran .mod files
%.o : %.mod

%.o : %.F90
	$(FC) $(CPPINCLUDES) $(FFLAGS) -c $<

%.o : %.c
	$(CC) $(CPPINCLUDES) $(CFLAGS) -c $<
