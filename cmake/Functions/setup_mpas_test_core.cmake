function(download_mpas_test_data dst_dir)
    set(url "https://www2.mmm.ucar.edu/mpas_test_data/mpas_test_data.tar.gz")
    set(output_file "${dst_dir}/mpas_test_data.tar.gz")
    message(STATUS "dst_dir: ${dst_dir}")
    message(STATUS "Downloading MPAS test data from ${url}")
    execute_process(
            COMMAND wget -O ${output_file} ${url}
            WORKING_DIRECTORY ${dst_dir}
            RESULT_VARIABLE result
    )

    if(result EQUAL 0)
        message(STATUS "MPAS test data saved to: ${output_file}")
    else()
        message(FATAL_ERROR "Failed to download MPAS test data (wget exit code: ${result})")
    endif()
endfunction()

function(untar_tarball tarball_path dest_dir)
    if(NOT EXISTS "${tarball_path}")
        message(FATAL_ERROR "Tarball not found: ${tarball_path}")
    endif()

    # Ensure the destination directory exists
    file(MAKE_DIRECTORY "${dest_dir}")

    message(STATUS "Extracting ${tarball_path} → ${dest_dir}")
    execute_process(
            COMMAND ${CMAKE_COMMAND} -E tar xzf "${tarball_path}"
            WORKING_DIRECTORY "${dest_dir}"
            RESULT_VARIABLE untar_result
    )

    if(NOT untar_result EQUAL 0)
        message(FATAL_ERROR "Failed to extract ${tarball_path} (exit code ${untar_result})")
    else()
        message(STATUS "Successfully extracted ${tarball_path} to ${dest_dir}")
    endif()
endfunction()


#------------------------------------------------------------------------------
# mpas_link_file_force(src dst)
#
# Create a symbolic link from `src` to `dst`, overwriting `dst` if it exists.
#
# Arguments:
#   src - Path to the source file to link to (must exist).
#   dst - Path where the symbolic link should be created.
#
# Behavior:
#   - Removes any existing file/symlink at `dst`.
#   - Fails with FATAL_ERROR if `src` does not exist.
#------------------------------------------------------------------------------
function(mpas_link_file_force src dst)
    if(NOT EXISTS "${src}")
        message(FATAL_ERROR "Link source does not exist: ${src}")
    endif()
    if(EXISTS "${dst}" OR IS_SYMLINK "${dst}")
        file(REMOVE "${dst}")
    endif()
    file(CREATE_LINK "${src}" "${dst}" SYMBOLIC)
endfunction()

#------------------------------------------------------------------------------
# mpas_link_directory(src_dir dst_dir)
#
# Create symbolic links in `dst_dir` for all files in `src_dir`.
#
# Arguments:
#   src_dir - Directory containing files to link from.
#   dst_dir - Directory where symlinks will be created.
#
# Behavior:
#   - Ensures `dst_dir` exists.
#   - Links every file in `src_dir` (non-recursive).
#   - Uses mpas_link_file_force to overwrite existing symlinks.
#------------------------------------------------------------------------------
function(mpas_link_directory src_dir dst_dir)
    if(NOT IS_DIRECTORY "${src_dir}")
        message(FATAL_ERROR "mpas_link_directory: src_dir is not a directory: ${src_dir}")
    endif()

    file(MAKE_DIRECTORY "${dst_dir}")

    file(GLOB files CONFIGURE_DEPENDS LIST_DIRECTORIES false "${src_dir}/*")

    foreach(file_path IN LISTS files)
        get_filename_component(filename "${file_path}" NAME)
        mpas_link_file_force("${file_path}" "${dst_dir}/${filename}")
    endforeach()
endfunction()

#------------------------------------------------------------------------------
# mpas_setup_test_core([dst_dir])
#
# Prepare the MPAS test core directory with symlinks to required files.
#
# Arguments:
#   dst_dir - Optional destination directory for test core setup.
#             Defaults to `${CMAKE_BINARY_DIR}/test`.
#
# Behavior:
#   - Creates `dst_dir` if it doesn’t exist.
#   - Symlinks all files from:
#       * `${CMAKE_BINARY_DIR}/MPAS/core_atmosphere`
#       * `${CMAKE_BINARY_DIR}/MPAS/core_test`
#       * `${MPAS_TEST_DATA_DIR}` (if defined and valid)
#   - Ensures `${dst_dir}/grid.nc` exists via mpas_link_grid.
#------------------------------------------------------------------------------
function(mpas_setup_test_core dst_dir)
    # Default destination directory if not provided
    if(ARGC LESS 1 OR "${dst_dir}" STREQUAL "")
        set(dst_dir "${CMAKE_BINARY_DIR}/test")
    endif()

    file(MAKE_DIRECTORY "${dst_dir}")
    message(STATUS "Setting up MPAS test core in: ${dst_dir}")

    # Symlink core directories
    mpas_link_directory("${CMAKE_BINARY_DIR}/MPAS/core_atmosphere" "${dst_dir}")
    mpas_link_directory("${CMAKE_BINARY_DIR}/MPAS/core_test" "${dst_dir}")

    # Download tarball only if missing
    set(tarball_path "${dst_dir}/mpas_test_data.tar.gz")
    if(EXISTS "${tarball_path}")
        message(STATUS "MPAS test data tarball already exists: ${tarball_path}")
    else()
        message(STATUS "Downloading MPAS test data to: ${tarball_path}")
        file(DOWNLOAD
                "https://www2.mmm.ucar.edu/mpas_test_data/mpas_test_data.tar.gz"
                "${tarball_path}"
                SHOW_PROGRESS
                STATUS status
                LOG log
        )
        list(GET status 0 status_code)
        if(NOT status_code EQUAL 0)
            message(FATAL_ERROR "Failed to download MPAS test data: ${log}")
        endif()
    endif()

    # Extract the tarball
    message(STATUS "Extracting MPAS test data...")
    untar_tarball("${tarball_path}" "${dst_dir}")

    # Create symlink to extracted data
    set(extracted_dir "${dst_dir}/mpas_test_data")
    if(EXISTS "${dst_dir}/test_data")
        file(REMOVE "${dst_dir}/test_data")
    endif()

    mpas_link_directory("${extracted_dir}" "${dst_dir}")
    message(STATUS "MPAS test core setup complete.")
endfunction()

