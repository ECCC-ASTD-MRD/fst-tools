
add_executable(fstinfo fstinfo.f)
target_link_libraries(fstinfo PRIVATE ${RMN_LIBRARIES})

install(TARGETS fstinfo RUNTIME DESTINATION bin)

