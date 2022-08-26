add_executable(fstinfo fstinfo.f)
target_link_libraries(fstinfo PRIVATE rmn)

# Force the use of RPATH instead of RUNPATH to revert to the
# old behaviour which inherits the search paths in the dependency chain
target_link_libraries(fstinfo PUBLIC "-Wl,--disable-new-dtags")

install(TARGETS fstinfo)
