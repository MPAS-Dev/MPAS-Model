! Copyright (c) 2022, Advanced Micro Devices, Inc. All rights reserved
!
! Unless noted otherwise source code is licensed under the BSD license.
#ifdef MPAS_AMD_OFFLOAD
#define GPUOMP
#define TMPOMP !
#define GPUACC !
#ifdef MPAS_OPENMP
#define CPUOMP
#else
#define CPUOMP !
#endif
#else
#define GPUOMP !
#define TMPOMP !
#define GPUACC
#define CPUOMP
#endif
#ifdef _GPUMPI_OFF
#define MPIGPU !
#define MPICPU
#else
#define MPIGPU 
#define MPICPU !
#endif
