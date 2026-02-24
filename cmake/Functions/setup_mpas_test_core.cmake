#------------------------------------------------------------------------------
# download_mpas_test_data(dst_dir)
#
# Downloads the MPAS test data tarball to the specified destination directory.
# If the tarball already exists, it is not re-downloaded.
#------------------------------------------------------------------------------
function(download_mpas_test_data dst_dir)
    set(url "https://www2.mmm.ucar.edu/mpas_test_data/mpas_test_data.tar.gz")
    set(output_file "${dst_dir}/mpas_test_data.tar.gz")

    file(MAKE_DIRECTORY "${dst_dir}")

    message(STATUS "Downloading MPAS test data from ${url}")

    execute_process(
            COMMAND wget -O "${output_file}" "${url}"
            WORKING_DIRECTORY "${dst_dir}"
            RESULT_VARIABLE result
    )

    if(result EQUAL 0)
        message(STATUS "MPAS test data saved to: ${output_file}")
    else()
        message(FATAL_ERROR "Failed to download MPAS test data (wget exit code: ${result})")
    endif()
endfunction()


#------------------------------------------------------------------------------
# untar_tarball(tarball_path dest_dir)
#
# Extracts a tarball into the specified destination directory.
#------------------------------------------------------------------------------
function(untar_tarball tarball_path dest_dir)
    if(NOT EXISTS "${tarball_path}")
        message(FATAL_ERROR "Tarball not found: ${tarball_path}")
    endif()

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
# Creates a symbolic link from `src` to `dst`, overwriting if it exists.
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
# Creates symbolic links in `dst_dir` for all files in `src_dir` (non-recursive).
#------------------------------------------------------------------------------
function(mpas_link_directory src_dir dst_dir)
    if(NOT IS_DIRECTORY "${src_dir}")
        message(FATAL_ERROR "mpas_link_directory: Source is not a directory: ${src_dir}")
    endif()

    file(MAKE_DIRECTORY "${dst_dir}")
    file(GLOB files CONFIGURE_DEPENDS LIST_DIRECTORIES false "${src_dir}/*")

    foreach(file_path IN LISTS files)
        get_filename_component(filename "${file_path}" NAME)
        mpas_link_file_force("${file_path}" "${dst_dir}/${filename}")
    endforeach()
endfunction()


#------------------------------------------------------------------------------
# setup_mpas_test_core()
#
# Prepares the MPAS test core environment:
#   - Downloads and extracts MPAS test data if missing.
#   - Symlinks all extracted test data into the test directory.
#------------------------------------------------------------------------------
function(setup_mpas_test_core)
    set(dst_dir "${CMAKE_BINARY_DIR}/test")

    file(MAKE_DIRECTORY "${dst_dir}")
    message(STATUS "Setting up MPAS test core in: ${dst_dir}")

    #--------------------------------------------------------------------------
    # Download test data tarball (if missing)
    #--------------------------------------------------------------------------
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

    #--------------------------------------------------------------------------
    # Extract the test data
    #--------------------------------------------------------------------------
    message(STATUS "Extracting MPAS test data...")
    untar_tarball("${tarball_path}" "${dst_dir}")

    #--------------------------------------------------------------------------
    # Link extracted data into the test directory
    #--------------------------------------------------------------------------
    set(extracted_dir "${dst_dir}/mpas_test_data")
    if(EXISTS "${extracted_dir}")
        mpas_link_directory("${extracted_dir}" "${dst_dir}")
    endif()

    message(STATUS "MPAS test core setup complete.")
endfunction()
