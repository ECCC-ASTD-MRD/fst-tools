set(FICHIERS_F  
    bsort.f       dbzono.f       debalzn.f       inctcon.f
    litzon.f      outfld.f       setuvd0.f       writlzn.f)

add_executable(dbzono ${FICHIERS_F})
target_link_libraries(dbzono PRIVATE rmn)
install(TARGETS dbzono)

