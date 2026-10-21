
if (EC_INIT_DONE LESS 2)
   if(BUILD_TESTING)
        add_dependencies(test_execs editfst fstcomp voir)

        find_package(Python3 REQUIRED COMPONENTS Interpreter)
        add_test(NAME generate_fst COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/generate_fst.py)
        set_tests_properties(generate_fst PROPERTIES
            FIXTURES_SETUP GEN_FST
            ENVIRONMENT "PYTHONPATH=${rmn_PYTHONPATH};LD_LIBRARY_PATH=${rmn_LIBRARY_PATH}")

        add_test(
            NAME 01_editfst_basic
            COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/editfst/01_editfst_basic.sh
                    $<TARGET_FILE:editfst> $<TARGET_FILE:fstcomp>
        )
        set_tests_properties(01_editfst_basic PROPERTIES FIXTURES_REQUIRED GEN_FST)

        add_test(
            NAME 02_editfst_exclure
            COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/editfst/02_editfst_exclure.sh
                    $<TARGET_FILE:editfst> $<TARGET_FILE:fstcomp>
        )
        set_tests_properties(02_editfst_exclure PROPERTIES FIXTURES_REQUIRED GEN_FST)

        add_test(
            NAME 03_voir_basic
            COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/voir/01_voir_basic.sh
                    $<TARGET_FILE:voir>
        )
        set_tests_properties(03_voir_basic PROPERTIES FIXTURES_REQUIRED GEN_FST)

        add_test(
            NAME 04_editfst_ds
            COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/editfst/04_editfst_ds.sh
                    $<TARGET_FILE:editfst> $<TARGET_FILE:fstcomp>
        )
        set_tests_properties(04_editfst_ds PROPERTIES FIXTURES_REQUIRED GEN_FST TIMEOUT 10)

        add_test(
            NAME 04_voir_style
            COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/voir/02_voir_style.sh
                    $<TARGET_FILE:voir>
        )
        set_tests_properties(04_voir_style PROPERTIES FIXTURES_REQUIRED GEN_FST)
    endif()
endif()
