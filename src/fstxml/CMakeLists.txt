add_executable(fst2xml fst2xml.c fst2xml_main.f90)
target_link_libraries(fst2xml PRIVATE rmn)

add_executable(xml2fst xml2fst.c xml2fst_main.f90)
target_link_libraries(xml2fst PRIVATE rmn)

install(TARGETS fst2xml xml2fst)
