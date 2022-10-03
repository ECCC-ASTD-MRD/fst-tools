add_executable(dbzono 
    bsort.f
    dbzono.f
    debalzn.f
    inctcon.f
    litzon.f
    outfld.f
    setuvd0.f
    writlzn.f
)
target_link_libraries(dbzono PRIVATE rmn)

# Force the use of RPATH instead of RUNPATH to revert to the
# old behaviour which inherits the search paths in the dependency chain
target_link_libraries(dbzono PUBLIC "-Wl,--disable-new-dtags")

install(TARGETS dbzono)
