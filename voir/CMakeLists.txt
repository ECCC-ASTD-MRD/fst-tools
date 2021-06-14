
add_executable(voir voir.f90)
target_link_libraries(voir PRIVATE ${RMN_LIBRARIES})
install(TARGETS voir RUNTIME DESTINATION bin)

