include_directories(${CMAKE_CURRENT_SOURCE_DIR})

##################### NORMAL TARGETS ##############################
configure_file(version.in.inc version.inc @ONLY)

add_executable(reflex reflex.f)
target_link_libraries(reflex PRIVATE ${RMN_LIBRARIES})
target_include_directories(reflex PRIVATE ${CMAKE_CURRENT_BINARY_DIR} ${RMN_INCLUDE_DIRS})

install(TARGETS reflex)

######################## TESTING ###################################
# Lets not forget to add tests later
# Unit test executable
# add_executable(test_reflex test.f90)
# target_compile_options(test_reflex PRIVATE -DTEST_DIR=\"${CMAKE_CURRENT_SOURCE_DIR}/tests\")
# target_link_libraries(test_reflex PRIVATE pgsmlib ${RMN_LIBRARIES})
# add_test(NAME test_reflex COMMAND test_reflex)
# add_dependencies(check test_reflex)
