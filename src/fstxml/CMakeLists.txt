add_executable(fst2xml fst2xml.c fst2xml_main.f90)
target_link_libraries(fst2xml PRIVATE rmn)

add_executable(xml2fst xml2fst.c xml2fst_main.f90)
target_link_libraries(xml2fst PRIVATE rmn)

# Force the use of RPATH instead of RUNPATH to revert to the
# old behaviour which inherits the search paths in the dependency chain
target_link_libraries(fst2xml PUBLIC "-Wl,--disable-new-dtags")
target_link_libraries(xml2fst PUBLIC "-Wl,--disable-new-dtags")
add_dependencies(fst2xml ${PROJECT_NAME}_build_info)
add_dependencies(xml2fst ${PROJECT_NAME}_build_info)

install(TARGETS fst2xml xml2fst)
