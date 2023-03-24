add_executable(fstcompress fstcompress.F90)
add_dependencies(fstcompress fst-tools_build_info)
target_link_libraries(fstcompress PRIVATE rmn::rmn)
set_target_properties(fstcompress PROPERTIES 
   OUTPUT_NAME fstcompress-${FSTCOMPRESS_VERSION}
   COMPILE_DEFINITIONS FSTCOMPRESS_VERSION="${FSTCOMPRESS_VERSION}")

# Symbolic links to latest build
add_custom_command(TARGET fstcompress POST_BUILD
   COMMAND ln -sfv $<TARGET_FILE_NAME:fstcompress> fstcompress)

add_executable(fstuncompress fstuncompress.F90)
add_dependencies(fstuncompress fst-tools_build_info)
target_link_libraries(fstuncompress PRIVATE rmn::rmn)
set_target_properties(fstuncompress PROPERTIES 
   OUTPUT_NAME fstuncompress-${FSTCOMPRESS_VERSION}
   COMPILE_DEFINITIONS FSTCOMPRESS_VERSION="${FSTCOMPRESS_VERSION}")

# Symbolic links to latest build
add_custom_command(TARGET fstuncompress POST_BUILD
   COMMAND ln -sfv $<TARGET_FILE_NAME:fstuncompress> fstuncompress)

# Force the use of RPATH instead of RUNPATH to revert to the
# old behaviour which inherits the search paths in the dependency chain
target_link_libraries(fstcompress PUBLIC "-Wl,--disable-new-dtags")
target_link_libraries(fstuncompress PUBLIC "-Wl,--disable-new-dtags")

install(TARGETS fstcompress fstuncompress)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/fstcompress ${CMAKE_CURRENT_BINARY_DIR}/fstuncompress DESTINATION bin)
