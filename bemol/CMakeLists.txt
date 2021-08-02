
add_library(bemollib
    bemol_opt.c
    bm_utils.c
    c_dies_process_flds.c
    c_inventaire_tuiles.c
    dies.c
    diese_var_g.c
    # ftnstrclean.c
    bm_closefiles.F90
    bm_coarse_wrt.F90
    bm_core_wrt.F90
    bm_openfiles.F90
    bm_std_wrt.F90
    bm_vanilla_wrt.F90
    bm_wrt_axay.F90
    fillgrid.F90
    statfld.F90
)

target_include_directories(bemollib PUBLIC ${CMAKE_CURRENT_SOURCE_DIR} ${RMN_INCLUDE_DIRS})
target_link_libraries(bemollib PRIVATE ${RMN_LIBRARIES})

add_executable(bemol bemol.F90)
target_link_libraries(bemol PRIVATE bemollib ${RMN_LIBRARIES})

add_executable(zcrop utils/zcrop/zcrop.F90)
target_link_libraries(zcrop PRIVATE bemollib ${RMN_LIBRARIES})

install(TARGETS bemol zcrop RUNTIME DESTINATION bin)
