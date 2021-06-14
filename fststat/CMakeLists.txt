
add_executable(fststat fststat.f fststatm.f statfld4.f)
target_link_libraries(fststat PRIVATE ${RMN_LIBRARIES})

install(TARGETS fststat RUNTIME DESTINATION bin)

