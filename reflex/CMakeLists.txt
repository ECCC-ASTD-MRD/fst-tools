include_directories(${CMAKE_CURRENT_SOURCE_DIR})

##################### NORMAL TARGETS ##############################
configure_file(version.in.inc version.inc @ONLY)

add_library(reflexlib STATIC reflex.f)
target_link_libraries(reflexlib PUBLIC ${RMN_LIBRAIRES})

# Needed for version.inc
target_include_directories(reflexlib PRIVATE ${CMAKE_CURRENT_BINARY_DIR} ${RMN_INCLUDE_DIRS})

add_executable(reflex main.F90)
target_link_libraries(reflex PRIVATE reflexlib ${RMN_LIBRARIES})


install(TARGETS reflex)

######################## TESTING ###################################

# Unit test executable
# add_executable(test_pgsm test.f90)
# target_compile_options(test_pgsm PRIVATE -DTEST_DIR=\"${CMAKE_CURRENT_SOURCE_DIR}/tests\")
# target_link_libraries(test_pgsm PRIVATE pgsmlib ${RMN_LIBRARIES})
# add_test(NAME test_pgsm COMMAND test_pgsm)
# add_dependencies(check test_pgsm)
