set(FICHIERS_C chk_tmpdir.c c_pgsm_utils.c)
set(FICHIERS_FTN90
    # This might be worth a glob but that can wait till after we make this work
    # NOPE: It's not all the files, but even if we delete the unused files
    # we still have to remove main.F90 and test_f_pgsm.F90 which isn't much
    # better than doing this.
    calcul.F90 champ.F90 champ_seq.F90 chk_extrap.F90
    chk_hy.F90 chk_toctoc.F90 chkenrpos.F90 chk_userdate.F90
    chmpdif.F90 comme.F90 conlalo.F90 conver.F90 convs.F90
    coord.F90 coupe.F90 coupzm.F90 ecrits.F90 ecritur.F90
    epais.F90 fst_get_mask_key.F90 fillcoord.F90 grigaus.F90
    grigef.F90 grigrib.F90 grille2.F90 grillps.F90
    grilstd.F90 griltp4.F90 gristdb.F90 gristereo.F90
    gritp12.F90 grlalon.F90 heure.F90 imprime.F90
    initid.F90 initseq.F90 itrouve.F90 lastcol.F90
    legvar.F90 liraxez.F90 liren.F90 lopascm.F90
    loupmir.F90 lrsmdes.F90 macpcp.F90 messags.F90
    metsym.F90 operat.F90 outlalo.F90 pairvct.F90 pgsm2.F90
    pgsmabt.F90 ipgsmlic.F90 pgsmlic.F90 ipgsmlir.F90
    pgsmlir.F90 ipgsmluk.F90 pgsmluk.F90 plmnmod.F90
    prefiltre.F90 putfld.F90 qaaqr.F90 qqqecho.F90
    qqqfilt.F90 qqqform.F90 qqqident.F90 routines.F90
    scalair.F90 scalair_msk.F90 setintx.F90 setxtrap.F90
    sorti.F90 stenfilt.F90 symetri.F90 testseq.F90
    uvect.F90 uvecteur_masque.F90 vdauv.F90 verlalo.F90
)

##################### NORMAL TARGETS ##############################

add_library(pgsmlib STATIC ${FICHIERS_FTN90} ${FICHIERS_C})
add_dependencies(pgsmlib fst-tools_build_info)
target_link_libraries(pgsmlib PRIVATE rmn::rmn)
set_target_properties(pgsmlib PROPERTIES 
   COMPILE_DEFINITIONS PGSM_VERSION="${PGSM_VERSION}")

add_executable(pgsm main.F90)
target_link_libraries(pgsm PRIVATE pgsmlib rmn::rmn)
set_target_properties(pgsm PROPERTIES 
   OUTPUT_NAME pgsm-${PGSM_VERSION})

# Symbolic links to latest build
add_custom_command(TARGET pgsm POST_BUILD
   COMMAND ln -sfv $<TARGET_FILE_NAME:pgsm> pgsm)

# Force the use of RPATH instead of RUNPATH to revert to the
# old behaviour which inherits the search paths in the dependency chain
target_link_libraries(pgsm PUBLIC "-Wl,--disable-new-dtags")

install(TARGETS pgsm)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/pgsm DESTINATION bin)

######################## TESTING ###################################

# Unit test executable
add_executable(test_pgsm test.f90)
target_compile_options(test_pgsm PRIVATE -DTEST_DIR=\"${CMAKE_CURRENT_SOURCE_DIR}/tests\")
target_link_libraries(test_pgsm PRIVATE pgsmlib rmn::rmn)

if (EC_INIT_DONE LESS 2)
   if(BUILD_TESTING)
      add_test(NAME test_pgsm COMMAND test_pgsm)
      add_dependencies(check test_pgsm)
    endif()
endif()