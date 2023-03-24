add_executable(fst2xml fst2xml.c fst2xml_main.f90)
add_dependencies(fst2xml fst-tools_build_info)
target_link_libraries(fst2xml PRIVATE rmn::rmn)

add_executable(xml2fst xml2fst.c xml2fst_main.f90)
add_dependencies(xml2fst fst-tools_build_info)
target_link_libraries(xml2fst PRIVATE rmn::rmn)

# Force the use of RPATH instead of RUNPATH to revert to the
# old behaviour which inherits the search paths in the dependency chain
target_link_libraries(fst2xml PUBLIC "-Wl,--disable-new-dtags")
target_link_libraries(xml2fst PUBLIC "-Wl,--disable-new-dtags")

install(TARGETS fst2xml xml2fst)
