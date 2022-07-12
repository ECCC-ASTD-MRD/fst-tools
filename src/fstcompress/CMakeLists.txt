add_executable(fstcompress fstcompress.f90)
target_link_libraries(fstcompress PRIVATE rmn)

add_executable(fstuncompress fstuncompress.f90)
target_link_libraries(fstuncompress PRIVATE rmn)

install(TARGETS fstcompress fstuncompress)
