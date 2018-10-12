/* In Fortran, use the following as an interfaces:
    use iso_c_binding, only : c_int
 
    interface
        subroutine timer_start(timer_id) bind(C)
           use iso_c_binding, only : c_int
           integer (c_int), intent(in), value :: timer_id
        end subroutine timer_start

        subroutine timer_stop(timer_id, sec, nsec) bind(C)
           use iso_c_binding, only : c_int
           integer (c_int), intent(in), value :: timer_id
           integer (c_int), intent(out) :: sec, nsec
        end subroutine timer_stop

        subroutine timer_set_base() bind(C)
        end subroutine timer_set_base

        subroutine timer_time(sec, nsec) bind(C)
           use iso_c_binding, only : c_int
           integer (c_int), intent(out) :: sec, nsec
        end subroutine timer_time
    end interface
 
    integer (c_int) :: timer_id, sec, nsec 
*/

#include <stdlib.h>

#define MAX_TIMERS 10

#ifdef GETTIMEOFDAY
#include <sys/time.h>
#endif

#ifdef DARWIN
#include <mach/mach.h>
#include <mach/mach_time.h>
#include <unistd.h>
#endif

#ifdef LINUX
#include <time.h>
#endif

#ifdef GETTIMEOFDAY
struct timeval start_time[MAX_TIMERS];
struct timeval end_time[MAX_TIMERS];
#endif

#ifdef DARWIN
uint64_t start_time[MAX_TIMERS];
uint64_t end_time[MAX_TIMERS];
#endif

#ifdef AIX
timebasestruct_t start_time[MAX_TIMERS];
timebasestruct_t end_time[MAX_TIMERS];
#endif

#ifdef LINUX
struct timespec start_time[MAX_TIMERS];
struct timespec end_time[MAX_TIMERS];

int secs_base;
int n_secs_base;
#endif

void timer_start(int n)
{
#ifdef GETTIMEOFDAY
   gettimeofday(&start_time[n], NULL);
#endif

#ifdef DARWIN
   start_time[n] = mach_absolute_time();
#endif

#ifdef AIX
   read_real_time(&start_time[n], TIMEBASE_SZ);
#endif

#ifdef LINUX
   clock_gettime(CLOCK_MONOTONIC_RAW, &start_time[n]);
#endif
}

void timer_stop(int n, int *secs, int *n_secs)
{
#ifdef GETTIMEOFDAY
   gettimeofday(&end_time[n], NULL);
  
   *secs   = (int)(end_time[n].tv_sec - start_time[n].tv_sec);
   *n_secs = (int)(end_time[n].tv_usec - start_time[n].tv_usec) * 1000;

   if (*n_secs < 0)  {
      *secs   -= 1;
      *n_secs += 1000000000;
   }
#endif

#ifdef DARWIN
   uint64_t elapsed, elapsedNano;
   static mach_timebase_info_data_t sTimebaseInfo;

   end_time[n] = mach_absolute_time();

   elapsed = end_time[n] - start_time[n];

    if ( sTimebaseInfo.denom == 0 ) {
        (void) mach_timebase_info(&sTimebaseInfo);
    }

    // Do the maths. We hope that the multiplication doesn't 
    // overflow; the price you pay for working in fixed point.

    elapsedNano = elapsed * sTimebaseInfo.numer / sTimebaseInfo.denom;


   *secs   = (int)(elapsedNano / 1000000000);
   *n_secs = (int)(elapsedNano % 1000000000);
#endif

#ifdef AIX
   read_real_time(&end_time[n], TIMEBASE_SZ);
   time_base_to_time(&start_time[n], TIMEBASE_SZ);
   time_base_to_time(&end_time[n], TIMEBASE_SZ);

   *secs = end_time[n].tb_high - start_time[n].tb_high;
   *n_secs = end_time[n].tb_low - start_time[n].tb_low;

   if (*n_secs < 0)  {
      *secs   -= 1;
      *n_secs += 1000000000;
   }
#endif

#ifdef LINUX
   clock_gettime(CLOCK_MONOTONIC_RAW, &end_time[n]);

   *secs = (int)(end_time[n].tv_sec - start_time[n].tv_sec);
   *n_secs = (int)(end_time[n].tv_nsec - start_time[n].tv_nsec);

   if (*n_secs < 0)  {
      *secs   -= 1;
      *n_secs += 1000000000;
   }
#endif
}


void timer_set_base()
{
#ifdef LINUX
   struct timespec now;

   clock_gettime(CLOCK_MONOTONIC_RAW, &now);

   secs_base = (int)now.tv_sec;
   n_secs_base = (int)now.tv_nsec;
#endif
}


void timer_time(int *secs, int *n_secs)
{
#ifdef LINUX
   struct timespec now;

   clock_gettime(CLOCK_MONOTONIC_RAW, &now);

   *secs = (int)now.tv_sec - secs_base;
   *n_secs = (int)now.tv_nsec - n_secs_base;

   if (*n_secs < 0)  {
      *secs   -= 1;
      *n_secs += 1000000000;
   }
#endif
}
