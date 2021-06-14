add_executable(fstcomp fstcomp.f statfldx.F90)
target_link_libraries(fstcomp PRIVATE ${RMN_LIBRARIES})
install(TARGETS fstcomp RUNTIME DESTINATION bin)

