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
target_include_directories(editfstlib PRIVATE ${CMAKE_BINARY_DIR} ${RMN_INCLUDE_DIRS})

add_executable(editfst main.F90)
target_link_libraries(editfst PRIVATE editfstlib ${RMN_LIBRARIES})

install(TARGETS editfst)
