add_executable(r.fstinfo fstinfo.F90)
target_link_libraries(r.fstinfo PRIVATE rmn)

# Force the use of RPATH instead of RUNPATH to revert to the
# old behaviour which inherits the search paths in the dependency chain
target_link_libraries(r.fstinfo PUBLIC "-Wl,--disable-new-dtags")

install(TARGETS r.fstinfo)

#add_custom_command(TARGET fstinfo POST_BUILD
#    COMMAND ln -sfv fstinfo r.fstinfo)
#install(FILES ${CMAKE_CURRENT_BINARY_DIR}/r.fstinfo DESTINATION bin)
