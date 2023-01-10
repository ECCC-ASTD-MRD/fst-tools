add_executable(fstcomp fstcomp.f90 statfldx.F90)
add_dependencies(fstcomp fst-tools_build_info)
target_link_libraries(fstcomp PRIVATE rmn)

# Force the use of RPATH instead of RUNPATH to revert to the
# old behaviour which inherits the search paths in the dependency chain
target_link_libraries(fstcomp PUBLIC "-Wl,--disable-new-dtags")

install(TARGETS fstcomp)
