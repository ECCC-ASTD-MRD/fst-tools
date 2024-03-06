add_executable(voir voir.F90)
add_dependencies(voir fst-tools_build_info)
target_link_libraries(voir PRIVATE rmn::rmn)
set_target_properties(voir PROPERTIES 
   OUTPUT_NAME voir-${VOIR_VERSION}
   COMPILE_DEFINITIONS VOIR_VERSION="${VOIR_VERSION}")

add_executable(voir24 voir24.F90)
add_dependencies(voir24 fst-tools_build_info)
target_link_libraries(voir24 PRIVATE rmn::rmn)
set_target_properties(voir24 PROPERTIES 
   OUTPUT_NAME voir24-${VOIR_VERSION}
   COMPILE_DEFINITIONS VOIR_VERSION="${VOIR_VERSION}")
   
# Symbolic links to latest build
add_custom_command(TARGET voir POST_BUILD
   COMMAND ln -sfv $<TARGET_FILE_NAME:voir> voir)
add_custom_command(TARGET voir24 POST_BUILD
   COMMAND ln -sfv $<TARGET_FILE_NAME:voir24> voir24)
   
install(TARGETS voir voir24)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/voir DESTINATION bin)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/voir24 DESTINATION bin)

# Force the use of RPATH instead of RUNPATH to revert to the
# old behaviour which inherits the search paths in the dependency chain
target_link_libraries(voir PUBLIC "-Wl,--disable-new-dtags")
