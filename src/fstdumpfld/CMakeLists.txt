add_executable(fstdumpfld fstdumpfld.c)
add_dependencies(fstdumpfld fst-tools_build_info)
target_link_libraries(fstdumpfld PRIVATE rmn::rmn)

# Force the use of RPATH instead of RUNPATH to revert to the
# old behaviour which inherits the search paths in the dependency chain
target_link_libraries(fstdumpfld PUBLIC "-Wl,--disable-new-dtags")

install(TARGETS fstdumpfld)

if(COMPILER_SUITE STREQUAL "intel")
   target_link_libraries(fstdumpfld PUBLIC -lintlc -lifcore -lifport -limf -lsvml -liomp5 -lifcoremt)
endif()
