cmake_minimum_required(VERSION 3.15)

#----- Append EC specific module path
list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake_rpn)
list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake/modules)

include(ec_init)           # Initialise compilers and ec specific functions
ec_git_version()           # Get version from git state
ec_parse_manifest()        # Parse MANIFEST file

project(${NAME} DESCRIPTION "${DESCRIPTION}")
set(PROJECT_VERSION ${VERSION} CACHE STRING "Define the version of VGRID that will be baked into the various artifacts of this build")

ec_build_info()            # Generate build information
ec_install_prefix()        # Define install prefix  

enable_language(C Fortran)


# configure_file(version.in.inc version.inc @ONLY)
include(CTest)
add_custom_target(check COMMAND CTEST_OUTPUT_ON_FAILURE=TRUE ${CMAKE_CTEST_COMMAND})

include(ec_compiler_presets)

include(orgmanpages)
orgmanpages_add_man_target()

find_package(RMN)

add_subdirectory(bemol)
add_subdirectory(editfst)
add_subdirectory(fstcomp)
add_subdirectory(fstcompress)
add_subdirectory(fstinfo)
add_subdirectory(fststat)
add_subdirectory(fstxml)
add_subdirectory(pgsm)
add_subdirectory(reflex)
add_subdirectory(voir)

ec_build_config()      # Create build configuration script
ec_prepare_ssm()       # Prepare ssm packaging files

set(CPACK_GENERATOR "TGZ")
set(CPACK_PACKAGE_VENDOR "ECCC")
set(CPACK_PACKAGE_CONTACT "${MAINTAINER}")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE.txt")
set(CPACK_RESOURCE_FILE_README "${CMAKE_CURRENT_SOURCE_DIR}/README.org")
set(CPACK_OUTPUT_FILE_PREFIX "${CMAKE_CURRENT_BINARY_DIR}/package")
set(CPACK_PACKAGE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
set(CPACK_PACKAGE_FILE_NAME "${CMAKE_INSTALL_PREFIX}")
set(CPACK_SOURCE_PACKAGE_FILE_NAME "${CMAKE_INSTALL_PREFIX}")
include(CPack)
