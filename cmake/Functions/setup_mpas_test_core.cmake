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
# mpas_link_grid(dir)
#
# Ensure that `${dir}/grid.nc` exists. If missing, create a symlink to a file
# in `dir` matching `*grid.nc`.
#
# Arguments:
#   dir - Directory to check for grid files.
#
# Behavior:
#   - If `grid.nc` already exists, nothing is changed.
#   - If multiple *grid.nc files exist, the first is used (warning issued).
#   - Fails with FATAL_ERROR if no grid file is found.
#------------------------------------------------------------------------------
function(mpas_link_grid dir)
    if(NOT IS_DIRECTORY "${dir}")
        message(FATAL_ERROR "mpas_link_grid: dir is not a directory: ${dir}")
    endif()

    set(grid_exact "${dir}/grid.nc")
    if(EXISTS "${grid_exact}" OR IS_SYMLINK "${grid_exact}")
        message(STATUS "Found existing grid.nc at ${grid_exact}")
        return()
    endif()

    file(GLOB grid_files CONFIGURE_DEPENDS LIST_DIRECTORIES false "${dir}/*grid.nc")

    list(LENGTH grid_files num_files)
    if(num_files EQUAL 0)
        message(FATAL_ERROR "No file ending in 'grid.nc' found in ${dir}")
    endif()

    list(GET grid_files 0 target_file)
    if(num_files GREATER 1)
        message(WARNING "Multiple *grid.nc files found in ${dir}; using: ${target_file}")
    endif()

    message(STATUS "Linking grid.nc -> ${target_file}")
    mpas_link_file_force("${target_file}" "${grid_exact}")
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
    if(ARGC LESS 1 OR "${dst_dir}" STREQUAL "")
        set(dst_dir "${CMAKE_BINARY_DIR}/test")
    endif()

    file(MAKE_DIRECTORY "${dst_dir}")

    mpas_link_directory("${CMAKE_BINARY_DIR}/MPAS/core_atmosphere" "${dst_dir}")
    mpas_link_directory("${CMAKE_BINARY_DIR}/MPAS/core_test"       "${dst_dir}")

    if(DEFINED MPAS_TEST_DATA_DIR AND IS_DIRECTORY "${MPAS_TEST_DATA_DIR}")
        mpas_link_directory("${MPAS_TEST_DATA_DIR}" "${dst_dir}")
    else()
        message(WARNING "MPAS_TEST_DATA_DIR not set or not a directory; skipping.")
    endif()

    mpas_link_grid("${dst_dir}")
endfunction()
