add_executable(fststat fststat.F90 loop_fields.F90 statfld4.F90)
target_link_libraries(fststat PRIVATE rmn)

# Force the use of RPATH instead of RUNPATH to revert to the
# old behaviour which inherits the search paths in the dependency chain
target_link_libraries(fststat PUBLIC "-Wl,--disable-new-dtags")
add_dependencies(fststat ${PROJECT_NAME}_build_info)

install(TARGETS fststat)
