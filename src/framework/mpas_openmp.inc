#ifdef MPAS_AMD_OFFLOAD
#define GPUOMP
#ifdef MPAS_OPENMP
#define CPUOMP
#else
#define CPUOMP !
#endif
#else
#define GPUOMP !
#define CPUOMP
#endif
