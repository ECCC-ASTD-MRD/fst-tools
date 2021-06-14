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

add_library(editfstlib STATIC
    configuration.f90
    copystx.f90
    # create_test_file.f90
    critsup.f90
    desire.f90
    dmpdes.f90
    editfst.f90
    excdes_new.c
    fermes.f90
    fstnol.f90
    holacar.f90
    ouvred.f90
    ouvres.f90
    rewinds.f90
    sautsqi.f90
    sauvdez.f90
    select.f90
    setper.f90
    sqicopi.f90
    stdcopi.f90
    # stubs.f90
    weofile.f90
    zap.f90
    # main.F90
)

include_directories(${CMAKE_SOURCE_DIR})

##################### NORMAL TARGETS ##############################

target_link_libraries(editfstlib PUBLIC ${RMN_LIBRAIRES})

# Needed for version.inc
target_include_directories(editfstlib PRIVATE ${CMAKE_BINARY_DIR})

add_executable(editfst main.F90)
target_link_libraries(editfst PRIVATE editfstlib ${RMN_LIBRARIES})


install(TARGETS editfst)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake/modules)
include(orgmanpages)
orgmanpages_add_man_target()

######################## TESTING ###################################
# Boilerplate
include(CTest)
add_custom_target(check COMMAND CTEST_OUTPUT_ON_FAILURE=1 ${CMAKE_CTEST_COMMAND})
