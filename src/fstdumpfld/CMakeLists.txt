add_executable(fstdumpfld fstdumpfld.c)
target_link_libraries(fstdumpfld PRIVATE rmn)
install(TARGETS fstdumpfld)

if(COMPILER_SUITE STREQUAL "intel")
   target_link_libraries(fstdumpfld PUBLIC -lintlc -lifcore -lifport -limf -lsvml -liomp5 -lifcoremt)
endif()
   