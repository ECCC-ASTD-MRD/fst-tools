add_executable(fstcompress fstcompress.f90)
target_link_libraries(fstcompress PRIVATE rmn)

add_executable(fstuncompress fstuncompress.f90)
target_link_libraries(fstuncompress PRIVATE rmn)

# Force the use of RPATH instead of RUNPATH to revert to the
# old behaviour which inherits the search paths in the dependency chain
target_link_libraries(fstcompress PUBLIC "-Wl,--disable-new-dtags")
target_link_libraries(fstuncompress PUBLIC "-Wl,--disable-new-dtags")
add_dependencies(fstcompress ${PROJECT_NAME}_build_info)
add_dependencies(fstuncompress ${PROJECT_NAME}_build_info)

install(TARGETS fstcompress fstuncompress)
