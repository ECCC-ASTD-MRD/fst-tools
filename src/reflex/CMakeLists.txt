##################### NORMAL TARGETS ##############################

add_executable(reflex reflex.f90)
target_link_libraries(reflex PRIVATE rmn)
target_include_directories(reflex PRIVATE ${CMAKE_CURRENT_BINARY_DIR})
add_dependencies(reflex ${PROJECT_NAME}_build_info)

# Force the use of RPATH instead of RUNPATH to revert to the
# old behaviour which inherits the search paths in the dependency chain
target_link_libraries(reflex PUBLIC "-Wl,--disable-new-dtags")

install(TARGETS reflex)

######################## TESTING ###################################
# Lets not forget to add tests later
# Unit test executable
# add_executable(test_reflex test.f90)
# target_compile_options(test_reflex PRIVATE -DTEST_DIR=\"${CMAKE_CURRENT_SOURCE_DIR}/tests\")
# target_link_libraries(test_reflex PRIVATE pgsmlib rmn)
# add_test(NAME test_reflex COMMAND test_reflex)
# add_dependencies(check test_reflex)
