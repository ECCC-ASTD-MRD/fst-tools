add_executable(fstdumpfld fstdumpfld.c)
target_link_libraries(fstdumpfld PRIVATE rmn)
install(TARGETS fstdumpfld)

