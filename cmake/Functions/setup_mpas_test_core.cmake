# Create a symbolic link (overwrites an existing link/file if needed).
function(_create_symlink src dst)
    if(NOT EXISTS "${src}")
        message(FATAL_ERROR "Symlink source does not exist: ${src}")
    endif()
    # Remove an existing path so CREATE_LINK won’t fail on reconfigure
    if(EXISTS "${dst}" OR IS_SYMLINK "${dst}")
        file(REMOVE "${dst}")
    endif()
    file(CREATE_LINK "${src}" "${dst}" SYMBOLIC)
endfunction()

# Symlink all files (non-directories) from src_dir into dst_dir.
function(symlink_all_files src_dir dst_dir)
    if(NOT IS_DIRECTORY "${src_dir}")
        message(FATAL_ERROR "symlink_all_files: src_dir is not a directory: ${src_dir}")
    endif()

    file(MAKE_DIRECTORY "${dst_dir}")

    file(GLOB files
            CONFIGURE_DEPENDS
            LIST_DIRECTORIES false
            "${src_dir}/*")

    foreach(file_path IN LISTS files)
        get_filename_component(filename "${file_path}" NAME)
        _create_symlink("${file_path}" "${dst_dir}/${filename}")
    endforeach()
endfunction()

# Ensure ${dir}/grid.nc exists; if a different *grid.nc exists, link it to grid.nc.
function(ensure_grid_symlink dir)
    if(NOT IS_DIRECTORY "${dir}")
        message(FATAL_ERROR "ensure_grid_symlink: dir is not a directory: ${dir}")
    endif()

    set(grid_exact "${dir}/grid.nc")
    if(EXISTS "${grid_exact}" OR IS_SYMLINK "${grid_exact}")
        message(STATUS "Found existing grid.nc at ${grid_exact}")
        return()
    endif()

    file(GLOB grid_files
            CONFIGURE_DEPENDS
            LIST_DIRECTORIES false
            "${dir}/*grid.nc")

    list(LENGTH grid_files num_files)
    if(num_files EQUAL 0)
        message(FATAL_ERROR "No file ending in 'grid.nc' found in ${dir}")
    endif()

    # Prefer the first match; warn if there are multiple.
    list(GET grid_files 0 target_file)
    if(num_files GREATER 1)
        message(WARNING
                "Multiple *grid.nc files found in ${dir}; using: ${target_file}")
    endif()

    message(STATUS "Creating symlink: ${grid_exact} -> ${target_file}")
    _create_symlink("${target_file}" "${grid_exact}")
endfunction()

# Set up a test directory by symlinking needed files, then ensure grid.nc exists.
# Accepts an optional destination directory; defaults to ${CMAKE_BINARY_DIR}/test.
function(setup_mpas_test_core dst_dir)
    if(ARGC LESS 1 OR "${dst_dir}" STREQUAL "")
        set(dst_dir "${CMAKE_BINARY_DIR}/test")
    endif()

    file(MAKE_DIRECTORY "${dst_dir}")

    # If you really want to set the runtime dir for tests, do it explicitly here:
    # set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${dst_dir}" PARENT_SCOPE)

    symlink_all_files("${CMAKE_BINARY_DIR}/MPAS/core_atmosphere" "${dst_dir}")
    symlink_all_files("${CMAKE_BINARY_DIR}/MPAS/core_test"       "${dst_dir}")

    if(DEFINED MPAS_TEST_DATA_DIR AND IS_DIRECTORY "${MPAS_TEST_DATA_DIR}")
        symlink_all_files("${MPAS_TEST_DATA_DIR}"                  "${dst_dir}")
    else()
        message(WARNING "MPAS_TEST_DATA_DIR not set or not a directory; skipping.")
    endif()

    ensure_grid_symlink("${dst_dir}")
endfunction()
