! Copyright (c) 2023, University Corporation for Atmospheric Research (UCAR)
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the 
! LICENSE file distributed with this code, or at 
! http://mpas-dev.github.com/license.html .
!
module test_core_string_utils

    use mpas_derived_types
    use mpas_log

    private

    public :: mpas_test_string_utils

    contains

    subroutine mpas_test_string_replace(err)

        use mpas_string_utils, only : mpas_string_replace

        implicit none

        ! Arguments
        integer, intent(out) :: err

        ! Local variables
        character(len=StrKIND) :: testString
        character(len=StrKIND) :: outString
        character :: targetCharacter, toReplace

        err = 0

        ! Basic functionality
        testString = 'Test_String'
        targetCharacter = '-'
        toReplace = '_'
        outString = mpas_string_replace(testString, toReplace, targetCharacter)
        if (trim(outString) /= 'Test-String') then
            err = err + 1
            call mpas_log_write('FAILED TO REPLACE STRING #1 CORRECTLY', &
                                MPAS_LOG_ERR)
        end if

        ! Whitespace replacement
        testString = 'Test String'
        targetCharacter = '-'
        toReplace = ' '
        outString = mpas_string_replace(testString, toReplace, targetCharacter)
        if (trim(outString) /= 'Test-String') then
            err = err + 1
            call mpas_log_write('FAILED TO REPLACE STRING #2 CORRECTLY', &
                                MPAS_LOG_ERR)
        end if

        ! Consecutive charcters
        testString = 'Test__String'
        toReplace = '_'
        outString = mpas_string_replace(testString, toReplace, targetCharacter)
        if (trim(outString) /= 'Test--String') then
            err = err + 1
            call mpas_log_write('FAILED TO REPLACE STRING #3 CORRECTLY', &
                                MPAS_LOG_ERR)
        end if

        ! No Replacement
        testString = 'Test String'
        toReplace = '-'
        outString = mpas_string_replace(testString, toReplace, targetCharacter)
        if (trim(outString) /= 'Test String') then
            err = err + 1
            call mpas_log_write('FAILED TO REPLACE STRING #4 CORRECTLY', &
                                MPAS_LOG_ERR)
        end if

    end subroutine mpas_test_string_replace

    subroutine mpas_test_split_string(err)

        use mpas_string_utils, only : mpas_split_string

        implicit none

        character(len=StrKIND) :: testString
        character :: delimiter
        character(len=StrKIND), pointer, dimension(:) :: splitStrings
        integer, intent(out) :: err
        integer :: i

        err = 0

        ! Test a basic case
        delimiter = ' '
        testString = 'This is a basic test'
        call mpas_split_string(testString, delimiter, splitStrings)
        
        if (size(splitStrings) /= 5) then
            err = err + 1
            call mpas_log_write('FAILED TO SPLIT STRING #1 CORRECTLY: WRONG'//&
            ' SUBSTRING COUNT', MPAS_LOG_ERR)
            return
        end if

        if (trim(splitStrings(1)) /= 'This' .or. &
                 trim(splitStrings(2)) /= 'is' .or. &
                 trim(splitStrings(3)) /= 'a' .or. &
                 trim(splitStrings(4)) /= 'basic' .or. &
                 trim(splitStrings(5)) /= 'test') then
             err = err + 1
             call mpas_log_write('FAILED TO SPLIT STRING #1 CORRECTLY', &
                                 MPAS_LOG_ERR)
        end if

        ! Test a string without delimiters 
        testString = 'This-is-a-test'
        call mpas_split_string(testString, delimiter, splitStrings)
        
        if (size(splitStrings) /= 1) then
            err = err + 1
            call mpas_log_write('FAILED TO SPLIT STRING #2 CORRECTLY: WRONG'//&
            ' SUBSTRING COUNT', MPAS_LOG_ERR)
            return
        end if
        
        if (trim(splitStrings(1)) /= 'This-is-a-test') then
            err = err + 1
            call mpas_log_write('FAILED TO SPLIT STRING #2 CORRECTLY', &
                                MPAS_LOG_ERR)
        end if
        
        ! Test a string with consecutive delimiters
        testString = 'This--is-a-test'
        delimiter = '-'
        call mpas_split_string(testString, delimiter, splitStrings)
        
        if (size(splitStrings) /= 5) then
            err = err + 1
            call mpas_log_write('FAILED TO SPLIT STRING #3 CORRECTLY: WRONG'//&
            ' SUBSTRING COUNT', MPAS_LOG_ERR)
            return
        end if
        
        if (trim(splitStrings(1)) /= 'This' .or. &
            trim(splitStrings(2)) /= '' .or. &
            trim(splitStrings(3)) /= 'is' .or. &
            trim(splitStrings(4)) /= 'a' .or. & 
            trim(splitStrings(5)) /= 'test') then
            err = err + 1
            call mpas_log_write('FAILED TO SPLIT STRING #3 CORRECTLY', &
                                MPAS_LOG_ERR)
        end if

    end subroutine mpas_test_split_string

    subroutine mpas_test_string_utils(err)

        implicit none

        integer, intent(out) :: err

        err = 0

        call mpas_log_write('String Utils Tests')

        call mpas_test_split_string(err)
        if (err == 0) then
            call mpas_log_write('   mpas_split_string: SUCCESS')
        else
            call mpas_log_write('   mpas_split_string: FAILURE', MPAS_LOG_ERR)
        end if

        call mpas_test_string_replace(err)
        if (err == 0) then
            call mpas_log_write('   mpas_string_replace: SUCCESS')
        else
            call mpas_log_write('   mpas_string_replace: FAILURE', &
                                MPAS_LOG_ERR)
        end if

    end subroutine mpas_test_string_utils

end module test_core_string_utils
