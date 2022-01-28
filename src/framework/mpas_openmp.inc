#ifdef MPAS_AMD_OFFLOAD
#define GPUOMP
#define TMPOMP !
#ifdef MPAS_OPENMP
#define CPUOMP
#else
#define CPUOMP !
#endif
#else
#define GPUOMP !
#define TMPOMP !
#define CPUOMP
#endif
