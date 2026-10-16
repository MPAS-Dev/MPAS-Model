#!/usr/bin/env bash
# Print the tool versions and exact commands for an MPAS build.
# Called by action.yml; the output goes to the job log and the build log.
#
# Environment variables set by the action:
#   COMPILER_FAMILY  gcc, oneapi, nvhpc, ...
#   MAKE_TARGET      make target
#   CORE             atmosphere or init_atmosphere
#   USE_PIO          true or false
#   PRECISION        single or double
#   ARCH             NVHPC target arch flag that was applied (empty = none)
#   CLEAN_CMD        clean command run before the build (empty = none)
#   MAKE_CMD         the build command
# NETCDF, PNETCDF and PIO_ROOT come from the container environment.

# First non-empty line of a command's output, or "not found"
v() { command -v "$1" >/dev/null 2>&1 || { echo "not found"; return 0; }; { "$@" 2>&1 || true; } | grep -m1 . || true; }
case "${COMPILER_FAMILY}" in
  gcc)    FC_SER=gfortran;  CC_SER=gcc;    FC_MPI=mpif90 ;;
  oneapi) FC_SER=ifx;       CC_SER=icx;    FC_MPI=mpifort ;;
  nvhpc)  FC_SER=nvfortran; CC_SER=nvc;    FC_MPI=mpifort ;;
  *)      FC_SER=mpifort;   CC_SER=mpicc;  FC_MPI=mpifort ;;
esac
PATH="${NETCDF:+${NETCDF}/bin:}${PNETCDF:+${PNETCDF}/bin:}${PATH}"
# MPI implementation: MPICH or Open MPI
if command -v mpichversion >/dev/null 2>&1; then
  MPI_VERSION="$(v mpichversion)"; MPI_SHOW="-show"
elif command -v ompi_info >/dev/null 2>&1; then
  MPI_VERSION="$(v ompi_info --version)"; MPI_SHOW="--showme"
else
  MPI_VERSION="not found"; MPI_SHOW="-show"
fi
echo "==== Build environment ===="
echo "Core:            ${CORE} (${COMPILER_FAMILY}, make target ${MAKE_TARGET})"
echo "Fortran:         $(v ${FC_SER} --version)"
echo "C:               $(v ${CC_SER} --version)"
echo "MPI:             ${MPI_VERSION}"
echo "MPI wrapper:     $(v ${FC_MPI} ${MPI_SHOW} | cut -c1-160)"
echo "NetCDF-C:        $(v nc-config --version)"
echo "NetCDF-Fortran:  $(v nf-config --version)"
echo "PnetCDF:         $(v pnetcdf-config --version)"
if [ "${USE_PIO}" = "true" ]; then
  echo "I/O layer:       PIO (${PIO_ROOT})"
else
  echo "I/O layer:       SMIOL (in-tree)"
fi
echo "Precision:       ${PRECISION}"
echo "Container OS:    $( (. /etc/os-release && echo "${PRETTY_NAME}") 2>/dev/null || echo unknown)"
if [ "${COMPILER_FAMILY}" = "nvhpc" ]; then
  if [ -n "${ARCH}" ]; then
    echo "Makefile edit:   added ${ARCH} to FFLAGS_OPT, CFLAGS_OPT, CXXFLAGS_OPT, LDFLAGS_OPT"
  else
    echo "Makefile edit:   none (use-target-arch is false)"
  fi
fi
if [ -n "${CLEAN_CMD}" ]; then
  echo "Clean command:   ${CLEAN_CMD}"
fi
echo "Build command:   ${MAKE_CMD}"
echo "==== End build environment ===="
