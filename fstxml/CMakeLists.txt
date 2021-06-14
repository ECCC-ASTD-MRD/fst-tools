
add_executable(fst2xml fst2xml.c fst2xml_main.f90)
target_link_libraries(fst2xml PRIVATE ${RMN_LIBRARIES})
target_include_directories(fst2xml PRIVATE ${RMN_INCLUDE_DIRS})

add_executable(xml2fst xml2fst.c xml2fst_main.f90)
target_link_libraries(xml2fst PRIVATE ${RMN_LIBRARIES})
target_include_directories(xml2fst PRIVATE ${RMN_INCLUDE_DIRS})

install(TARGETS fst2xml xml2fst RUNTIME DESTINATION bin)
