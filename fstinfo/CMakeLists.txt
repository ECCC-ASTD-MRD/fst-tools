add_executable(fstinfo fstinfo.f)
target_link_libraries(fstinfo PRIVATE rmn)
install(TARGETS fstinfo)
