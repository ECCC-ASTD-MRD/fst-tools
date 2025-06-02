
add_executable(generate_fst EXCLUDE_FROM_ALL generate_fst.c)
target_link_libraries(generate_fst PRIVATE rmn::rmn)

if (EC_INIT_DONE LESS 2)
   if(BUILD_TESTING)
        add_test(NAME generate_fst COMMAND $<TARGET_FILE_NAME:generate_fst>)
        add_dependencies(check generate_fst)
        set_tests_properties(generate_fst PROPERTIES FIXTURES_SETUP GEN_FST)

        add_test(
            NAME 01_editfst_basic
            COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/editfst/01_editfst_basic.sh $<TARGET_FILE:editfst> $<TARGET_FILE:fstcomp>
        )
        set_tests_properties(01_editfst_basic PROPERTIES FIXTURES_REQUIRED GEN_FST)
    endif()
endif()
