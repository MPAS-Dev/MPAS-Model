! Copyright (c) 2025 University Corporation for Atmospheric Research
!
! Unless noted otherwise, this software is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!=================================================================================================================
module mpas_musica

implicit none
private
public:: init_musica

!MPAS initialization of MUSICA library components. In order to use MUSICA components. the
!`config_musica_scheme' option must be set to the top-level configuration file for MUSICA.
!The MUSICA library must also be compiled and linked to the MPAS executable, by setting the
!environment variable `MPAS_USE_MUSICA' to `TRUE' and ensuring that the appropriate include
!and library paths are included in `LD_LIBRARY_PATH' and `CPATH'.
!ACOM Software Engineering Team 2025


 contains


!=================================================================================================================
 subroutine init_musica()
!=================================================================================================================
  use mpas_log
#ifdef USE_MUSICA
  use musica_micm, only : get_micm_version
  use musica_util, only : string_t

  type(string_t) :: version_string

  version_string = get_micm_version()
  call mpas_log_write("You are NOT using MICM for chemistry, but if you were, "// &
                      "it would be with version: "//version_string%value_)
#else
  call mpas_log_write("MUSICA Support not available. Ensure the environment "// &
                      "variable MPAS_USE_MUSICA is set to TRUE", messageType=MPAS_LOG_CRIT)
#endif
 end subroutine physics_init

!=================================================================================================================
end module mpas_musica
!=================================================================================================================

