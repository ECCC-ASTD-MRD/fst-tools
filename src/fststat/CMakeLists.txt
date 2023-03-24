add_executable(fststat fststat.F90 loop_fields.F90 statfld4.F90)
add_dependencies(fststat fst-tools_build_info)
target_link_libraries(fststat PRIVATE rmn::rmn)
set_target_properties(fststat PROPERTIES 
   OUTPUT_NAME fststat-${FSTSTAT_VERSION}
   COMPILE_DEFINITIONS FSTSTAT_VERSION="${FSTSTAT_VERSION}")

# Symbolic links to latest build
add_custom_command(TARGET fststat POST_BUILD
   COMMAND ln -sfv $<TARGET_FILE_NAME:fststat> fststat)

# Force the use of RPATH instead of RUNPATH to revert to the
# old behaviour which inherits the search paths in the dependency chain
target_link_libraries(fststat PUBLIC "-Wl,--disable-new-dtags")

install(TARGETS fststat)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/fststat DESTINATION bin)
