cmake_minimum_required(VERSION 3.15)

#----- Append EC specific module path
list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake/cmake_rpn)

include(ec_init)           # Initialise compilers and ec specific functions
ec_git_version()           # Get version from git state

project(pgsm C Fortran)
set(PROJECT_VERSION ${GIT_VERSION} CACHE STRING "Define the version of PGSM that will be baked into the various artifacts of this build")

if("${CMAKE_INSTALL_PREFIX}" STREQUAL "/usr/local")
    set(CMAKE_INSTALL_PREFIX ${CMAKE_BINARY_DIR}/install-site CACHE FILEPATH "CMake Installation prefix for ${PROJECT_NAME}" FORCE)
    message(STATUS "Setting CMAKE_INSTALL_PREFIX to ${CMAKE_INSTALL_PREFIX}")
endif()

# configure_file(version.in.inc version.inc @ONLY)

include(ec_compiler_presets)

find_package(RMN)

add_subdirectory(editfst)
add_subdirectory(fstcomp)
add_subdirectory(fstcompress)
add_subdirectory(fstinfo)
add_subdirectory(fststat)
