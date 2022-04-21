add_executable(fstcomp fstcomp.f statfldx.F90)
target_link_libraries(fstcomp PRIVATE rmn)
install(TARGETS fstcomp)

