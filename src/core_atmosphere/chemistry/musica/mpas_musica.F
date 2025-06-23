! Copyright (c) 2025 University Corporation for Atmospheric Research
!
! Unless noted otherwise, this software is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!=================================================================================================================
module mpas_musica
#ifdef MPAS_USE_MUSICA
  use musica_micm, only : get_micm_version
  use musica_util, only : string_t
#endif
use mpas_log, only: mpas_log_write

implicit none
private
public:: log_musica_support

!The MUSICA library must also be compiled and linked to the MPAS executable, This is
!typically done by first installing the MUSICA and then including the `MUSICA=true` option
!when building the MPAS atmosphere model.
!
!CURRENTLY UNDER DEVELOPMENT
!
!ACOM Software Engineering Team 2025


 contains


!=================================================================================================================
 subroutine log_musica_support()
!=================================================================================================================
#ifdef MPAS_USE_MUSICA

!local variables
  type(string_t) :: version_string

  version_string = get_micm_version()
  call mpas_log_write("MUSICA support is available. MICM version: "//version_string%value_)
#else
  call mpas_log_write("MUSICA support not available.")
#endif
 end subroutine log_musica_support

!=================================================================================================================
end module mpas_musica
!=================================================================================================================

