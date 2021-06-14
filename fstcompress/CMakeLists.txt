
add_executable(fstcompress fstcompress.f90)
target_link_libraries(fstcompress PRIVATE ${RMN_LIBRARIES})

add_executable(fstuncompress fstuncompress.f90)
target_link_libraries(fstuncompress PRIVATE ${RMN_LIBRARIES})
install(TARGETS fstcompress fstuncompress  RUNTIME DESTINATION bin)
