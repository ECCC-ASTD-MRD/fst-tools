add_executable(dbzono 
    bsort.f
    dbzono.f90
    debalzn.f90
    inctcon.f90
    litzon.f90
    outfld.f90
    setuvd0.f90
    writlzn.f90
)
add_dependencies(dbzono fst-tools_build_info)
target_link_libraries(dbzono PRIVATE rmn::rmn)

# Force the use of RPATH instead of RUNPATH to revert to the
# old behaviour which inherits the search paths in the dependency chain
target_link_libraries(dbzono PUBLIC "-Wl,--disable-new-dtags")

install(TARGETS dbzono)
