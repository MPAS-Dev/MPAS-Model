MPAS-v8.4.2
====

## CI Status

Each test builds MPAS-Atmosphere in double precision, runs 3 perturbed ensemble
members (4 MPI ranks), and validates with
[PyCECT](https://github.com/NCAR/PyCECT)
([Price-Broncucia et al. 2025](https://doi.org/10.5194/gmd-18-2349-2025)).
All testing subsets run automatically on every push and PR to the
master and develop branches. Reminder: Feature PRs should be opened
against `develop`.

| Compiler | MPI | Status |
|----------|-----|--------|
| GNU | MPICH | [![GNU+MPICH (CPU)](https://github.com/MPAS-Dev/MPAS-Model/actions/workflows/test-gcc-mpich.yml/badge.svg)](https://github.com/MPAS-Dev/MPAS-Model/actions/workflows/test-gcc-mpich.yml) |
| GNU | OpenMPI | [![GNU+OpenMPI (CPU)](https://github.com/MPAS-Dev/MPAS-Model/actions/workflows/test-gcc-openmpi.yml/badge.svg)](https://github.com/MPAS-Dev/MPAS-Model/actions/workflows/test-gcc-openmpi.yml) |
| Intel | MPICH | [![Intel+MPICH (CPU)](https://github.com/MPAS-Dev/MPAS-Model/actions/workflows/test-intel-mpich.yml/badge.svg)](https://github.com/MPAS-Dev/MPAS-Model/actions/workflows/test-intel-mpich.yml) |
| Intel | OpenMPI | [![Intel+OpenMPI (CPU)](https://github.com/MPAS-Dev/MPAS-Model/actions/workflows/test-intel-openmpi.yml/badge.svg)](https://github.com/MPAS-Dev/MPAS-Model/actions/workflows/test-intel-openmpi.yml) |
| NVHPC | MPICH | [![NVHPC+MPICH (CPU)](https://github.com/MPAS-Dev/MPAS-Model/actions/workflows/test-nvhpc-mpich.yml/badge.svg)](https://github.com/MPAS-Dev/MPAS-Model/actions/workflows/test-nvhpc-mpich.yml) |
| NVHPC | OpenMPI | [![NVHPC+OpenMPI (CPU)](https://github.com/MPAS-Dev/MPAS-Model/actions/workflows/test-nvhpc-openmpi.yml/badge.svg)](https://github.com/MPAS-Dev/MPAS-Model/actions/workflows/test-nvhpc-openmpi.yml) |

The Model for Prediction Across Scales (MPAS) is a collaborative project for
developing atmosphere, ocean, and other earth-system simulation components for
use in climate, regional climate, and weather studies. The primary development
partners are the climate modeling group at Los Alamos National Laboratory
(COSIM) and the National Center for Atmospheric Research. Both primary
partners are responsible for the MPAS framework, operators, and tools common to
the applications; LANL has primary responsibility for the ocean model, and NCAR
has primary responsibility for the atmospheric model.

The MPAS framework facilitates the rapid development and prototyping of models
by providing infrastructure typically required by model developers, including
high-level data types, communication routines, and I/O routines. By using MPAS,
developers can leverage pre-existing code and focus more on development of
their model.

BUILDING
========

This README is provided as a brief introduction to the MPAS framework. It does
not provide details about each specific model, nor does it provide building
instructions.

For information about building and running each core, please refer to each
core's user's guide, which can be found at the following web sites:

[MPAS-Atmosphere](http://mpas-dev.github.io/atmosphere/atmosphere_download.html)

[MPAS-Albany Land Ice](http://mpas-dev.github.io/land_ice/download.html)

[MPAS-Ocean](http://mpas-dev.github.io/ocean/releases.html)

[MPAS-Seaice](http://mpas-dev.github.io/sea_ice/releases.html)


Code Layout
----------

Within the MPAS repository, code is laid out as follows. Sub-directories are
only described below the src directory.

	MPAS-Model
	├── src
	│   ├── driver -- Main driver for MPAS in stand-alone mode (Shared)
	│   ├── external -- External software for MPAS (Shared)
	│   ├── framework -- MPAS Framework (Includes DDT Descriptions, and shared routines. Shared)
	│   ├── operators -- MPAS Opeartors (Includes Operators for MPAS meshes. Shared)
	│   ├── tools -- Empty directory for include files that Registry generates (Shared)
	│   │   ├── registry -- Code for building Registry.xml parser (Shared)
	│   │   └── input_gen -- Code for generating streams and namelist files (Shared)
	│   └── core_* -- Individual model cores.
	│       └── inc -- Empty directory for include files that Registry generates
	├── testing_and_setup -- Tools for setting up configurations and test cases (Shared)
	└── default_inputs -- Copies of default stream and namelists files (Shared)

Model cores are typically developed independently. For information about
building and running a particular core, please refer to that core's user's
guide.
