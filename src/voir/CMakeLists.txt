add_executable(voir voir.f90)
target_link_libraries(voir PRIVATE rmn)
install(TARGETS voir)

# Force the use of RPATH instead of RUNPATH to revert to the
# old behaviour which inherits the search paths in the dependency chain
target_link_libraries(voir PUBLIC "-Wl,--disable-new-dtags")
