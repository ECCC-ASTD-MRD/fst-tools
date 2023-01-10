add_executable(fststat fststat.F90 loop_fields.F90 statfld4.F90)
add_dependencies(fststat fst-tools_build_info)
target_link_libraries(fststat PRIVATE rmn)

# Force the use of RPATH instead of RUNPATH to revert to the
# old behaviour which inherits the search paths in the dependency chain
target_link_libraries(fststat PUBLIC "-Wl,--disable-new-dtags")

install(TARGETS fststat)
