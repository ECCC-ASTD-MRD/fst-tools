add_executable(fstcomp fstcomp.F90 statfldx.F90)
add_dependencies(fstcomp fst-tools_build_info)
target_link_libraries(fstcomp PRIVATE rmn::rmn)
set_target_properties(fstcomp PROPERTIES 
   OUTPUT_NAME fstcomp-${FSTCOMP_VERSION}
   COMPILE_DEFINITIONS FSTCOMP_VERSION="${FSTCOMP_VERSION}")

# Symbolic links to latest build
add_custom_command(TARGET fstcomp POST_BUILD
   COMMAND ln -sfv $<TARGET_FILE_NAME:fstcomp> fstcomp)

# Force the use of RPATH instead of RUNPATH to revert to the
# old behaviour which inherits the search paths in the dependency chain
target_link_libraries(fstcomp PUBLIC "-Wl,--disable-new-dtags")

install(TARGETS fstcomp)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/fstcomp DESTINATION bin)
