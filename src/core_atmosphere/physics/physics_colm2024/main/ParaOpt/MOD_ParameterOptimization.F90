#include <define.h>

MODULE MOD_ParameterOptimization

   USE MOD_Precision
   USE MOD_Opt_Baseflow
   IMPLICIT NONE
   SAVE

CONTAINS

   ! -----
   SUBROUTINE ParaOpt_init (ref_date_in, ref_lc_year_in)

   IMPLICIT NONE
   integer :: ref_date_in(3), ref_lc_year_in

      CALL Opt_Baseflow_init ()

   END SUBROUTINE ParaOpt_init

   ! -----
   SUBROUTINE ParameterOptimization (idate, deltim, is_spinup)

   IMPLICIT NONE

   integer,  intent(in) :: idate(3)
   real(r8), intent(in) :: deltim
   logical,  intent(in) :: is_spinup

      CALL BaseFlow_Optimize (idate, deltim, is_spinup)

   END SUBROUTINE ParameterOptimization

   ! -----
   SUBROUTINE ParaOpt_final ()

   IMPLICIT NONE

      CALL Opt_Baseflow_final ()

   END SUBROUTINE ParaOpt_final

END MODULE MOD_ParameterOptimization
