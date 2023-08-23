! Copyright (c) 2023 The University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at https://mpas-dev.github.io/license.html .
!
!-----------------------------------------------------------------------
!  mpas_string_utils
!
!> \brief Collection of functions used for string manipulation
!> \author Matthew Dimond
!> \date   25 July 2023
!> \details
!>  This module provides functions and subroutines used for string
!>  manipulations and utilities.
!
!-----------------------------------------------------------------------
module mpas_string_utils

    contains

    !-----------------------------------------------------------------------
    !  routine mpas_split_string
    !
    !> \brief This routine splits a string on a specified delimiting character
    !> \author Michael Duda, Doug Jacobsen
    !> \date   07/23/2014
    !> \details This routine splits the given "string" on the delimiter
    !>          character, and returns an array of pointers to the substrings
    !>          between the delimiting characters. Strings are trimmed before
    !>          splitting such that all trailing whitespace is ignored.
    !
    !-----------------------------------------------------------------------
    subroutine mpas_split_string(string, delimiter, subStrings)

        implicit none

        ! Arguments
        character(len=*), intent(in) :: string
        character, intent(in) :: delimiter
        character(len=*), pointer, dimension(:) :: subStrings

        ! Local variables
        character(len=len_trim(string)) :: trimString
        integer :: i, start, index

        trimString = trim(string)
        index = 1

        do i = 1, len(trimString)
            if (trimString(i:i) == delimiter) then
                index = index + 1
            end if
        end do

        allocate(subStrings(1:index))

        start = 1
        index = 1
        do i = 1, len(trimString)
            if (trimString(i:i) == delimiter) then
                subStrings(index) = trimString(start:i-1)
                index = index + 1
                start = i + 1
            end if
        end do
        subStrings(index) = trimString(start:len(trimString))

    end subroutine mpas_split_string

    !-----------------------------------------------------------------------
    !  routine mpas_string_replace
    !
    !> \brief Returns string with charToReplace replaced with targetChar
    !> \author Matthew Dimond
    !> \date   07/26/2023
    !> \details This function replaces all characters matching charToReplace in
    !>          "string" with the char "targetChar" after trimming "string"
    !
    !-----------------------------------------------------------------------
    function mpas_string_replace(string, charToReplace, targetChar) result(stringOut)

        implicit none

        ! Arguments
        character(len=*), intent(in) :: string 
        character, intent(in) :: targetChar, charToReplace

        ! Local variables
        integer :: i

        ! Result
        character(len=len_trim(string)) :: stringOut

        stringOut = trim(string)

        do i = 1, len_trim(string)
            if (string(i:i) == charToReplace) then
                stringOut(i:i) = targetChar
            end if
        end do

    end function mpas_string_replace

end module mpas_string_utils

