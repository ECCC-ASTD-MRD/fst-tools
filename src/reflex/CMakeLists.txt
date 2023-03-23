##################### NORMAL TARGETS ##############################

add_executable(reflex reflex.F90)
add_dependencies(reflex fst-tools_build_info)
target_link_libraries(reflex PRIVATE rmn::rmn)
target_include_directories(reflex PRIVATE ${CMAKE_CURRENT_BINARY_DIR})
set_target_properties(reflex PROPERTIES 
   OUTPUT_NAME reflex-${REFLEX_VERSION}
   COMPILE_DEFINITIONS REFLEX_VERSION="${REFLEX_VERSION}")

# Symbolic links to latest build
add_custom_command(TARGET reflex POST_BUILD
   COMMAND ln -sfv $<TARGET_FILE_NAME:reflex> reflex)


# Force the use of RPATH instead of RUNPATH to revert to the
# old behaviour which inherits the search paths in the dependency chain
target_link_libraries(reflex PUBLIC "-Wl,--disable-new-dtags")

install(TARGETS reflex)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/reflex DESTINATION bin)

######################## TESTING ###################################
# Lets not forget to add tests later
# Unit test executable
# add_executable(test_reflex test.f90)
# target_compile_options(test_reflex PRIVATE -DTEST_DIR=\"${CMAKE_CURRENT_SOURCE_DIR}/tests\")
# target_link_libraries(test_reflex PRIVATE pgsmlib rmn::rmn)
# add_test(NAME test_reflex COMMAND test_reflex)
# add_dependencies(check test_reflex)
