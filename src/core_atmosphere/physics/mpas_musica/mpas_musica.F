! Copyright (c) 2025 University Corporation for Atmospheric Research
!
! Unless noted otherwise, this software is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!=================================================================================================================
module mpas_musica
#ifdef USE_MUSICA
  use musica_micm, only : get_micm_version
  use musica_util, only : string_t
#endif
use mpas_derived_types, only: MPAS_LOG_CRIT
use mpas_log, only: mpas_log_write

implicit none
private
public:: init_musica

!MPAS initialization of MUSICA library components. To use MUSICA components. the
!`config_musica_scheme` option must be set to the top-level configuration file for MUSICA.
!The MUSICA library must also be compiled and linked to the MPAS executable, by setting the
!environment variable `MPAS_EXTERNAL_CPPFLAGS` to `-DUSE_MUSICA` and ensuring that the
!appropriate include and library paths are included in `MPAS_EXTERNAL_INCLUDES` and
!`MPAS_EXTERNAL_LIBS`.
!ACOM Software Engineering Team 2025


 contains


!=================================================================================================================
 subroutine init_musica()
!=================================================================================================================
#ifdef USE_MUSICA

!local variables
  type(string_t) :: version_string

  version_string = get_micm_version()
  call mpas_log_write("You are NOT using MICM for chemistry, but if you were, "// &
                      "it would be with version: "//version_string%value_)
#else
  call mpas_log_write("MUSICA Support not available. Ensure the environment "// &
                      "variable MPAS_EXTERNAL_CPPFLAGS includes -DUSE_MUSICA", messageType=MPAS_LOG_CRIT)
#endif
 end subroutine init_musica

!=================================================================================================================
end module mpas_musica
!=================================================================================================================

