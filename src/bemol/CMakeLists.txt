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

#target_include_directories(bemollib PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(bemollib PRIVATE rmn::rmn)

add_executable(bemol bemol.F90)
target_link_libraries(bemol PRIVATE bemollib rmn::rmn)
add_dependencies(bemol fst-tools_build_info)

add_executable(zcrop utils/zcrop/zcrop.F90)
target_link_libraries(zcrop PRIVATE bemollib rmn::rmn)
add_dependencies(zcrop fst-tools_build_info)

# Force the use of RPATH instead of RUNPATH to revert to the
# old behaviour which inherits the search paths in the dependency chain
target_link_libraries(bemol PUBLIC "-Wl,--disable-new-dtags")
target_link_libraries(zcrop PUBLIC "-Wl,--disable-new-dtags")

install(TARGETS bemol zcrop)
