add_executable(fststat fststat.F90 loop_fields.F90 statfld4.F90)
target_link_libraries(fststat PRIVATE rmn)
install(TARGETS fststat)

