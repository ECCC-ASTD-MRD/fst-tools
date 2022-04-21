add_executable(voir voir.f90)
target_link_libraries(voir PRIVATE rmn)
install(TARGETS voir)

