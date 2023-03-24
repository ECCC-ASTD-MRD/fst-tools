add_executable(editfst
    configuration.f90
    copystx.f90
    # create_test_file.f90
    critsup.f90
    desire.f90
    dmpdes.f90
    editfst.f90
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
    main.F90
)
add_dependencies(editfst fst-tools_build_info)
target_link_libraries(editfst PRIVATE rmn::rmn)
set_target_properties(editfst PROPERTIES 
   OUTPUT_NAME editfst-${EDITFST_VERSION}
   COMPILE_DEFINITIONS EDITFST_VERSION="${EDITFST_VERSION}")

# Symbolic links to latest build
add_custom_command(TARGET editfst POST_BUILD
   COMMAND ln -sfv $<TARGET_FILE_NAME:editfst> editfst)

# Force the use of RPATH instead of RUNPATH to revert to the
# old behaviour which inherits the search paths in the dependency chain
target_link_libraries(editfst PUBLIC "-Wl,--disable-new-dtags")

install(TARGETS editfst)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/editfst DESTINATION bin)
