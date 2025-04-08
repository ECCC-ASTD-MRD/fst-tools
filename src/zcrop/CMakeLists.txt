add_executable(zcrop zcrop.F90)
add_dependencies(zcrop fst-tools_build_info)
target_link_libraries(zcrop PRIVATE rmn::rmn)

# Force the use of RPATH instead of RUNPATH to revert to the
# old behaviour which inherits the search paths in the dependency chain
target_link_libraries(zcrop PUBLIC "-Wl,--disable-new-dtags")
set_target_properties(zcrop PROPERTIES 
   OUTPUT_NAME zcrop-${EDITFST_VERSION}
   COMPILE_DEFINITIONS ZCROP_VERSION="${ZCROP_VERSION}")

# Symbolic links to latest build
add_custom_command(TARGET zcrop POST_BUILD
   COMMAND ln -sfv $<TARGET_FILE_NAME:zcrop> zcrop)

# Force the use of RPATH instead of RUNPATH to revert to the
# old behaviour which inherits the search paths in the dependency chain
target_link_libraries(editfst PUBLIC "-Wl,--disable-new-dtags")

install(TARGETS zcrop)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/zcrop DESTINATION bin)
