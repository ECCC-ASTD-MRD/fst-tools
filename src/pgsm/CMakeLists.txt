set(FICHIERS_C chk_tmpdir.c c_pgsm_utils.c)
set(FICHIERS_FTN90
    accum.F90
    cfldinf.F90
    champs.F90
    chck.F90
    convers.F90
    ecrires.F90
    files.F90
    grilles.F90
    heuress.F90
    nivos.F90
    packing.F90
    pairs.F90
    param.F90
    pgsm_mod.F90
    plmnmod.F90
    symetry.F90
    conver.F90 calcul.F90 champ.F90 champ_seq.F90 chk_extrap.F90
    chkenrpos.F90 chk_userdate.F90
    chmpdif.F90 comme.F90 conlalo.F90 conver.F90 convs.F90
    coord.F90 coupe.F90 coupzm.F90 ecrits.F90 ecritur.F90
    epais.F90 fillcoord.F90 grigaus.F90
    grigef.F90 grigrib.F90 grille2.F90 grillps.F90
    grilstd.F90 griltp4.F90 gristdb.F90 gristereo.F90
    gritp12.F90 grlalon.F90 heure.F90 imprime.F90
    initid.F90 initseq.F90 itrouve.F90 lastcol.F90
    legvar.F90 liren.F90 lopascm.F90
    loupmir.F90 lrsmdes.F90 macpcp.F90 messags.F90
    operat.F90 outlalo.F90 pairvct.F90
    pgsmabt.F90 plmnmod.F90
    prefiltre.F90 tourbillon_relatif.F90 qqqecho.F90
    qqqfilt.F90 qqqform.F90 qqqident.F90
    scalair.F90 scalair_msk.F90 setintx.F90 setxtrap.F90
    sorti.F90 filtre.F90 testseq.F90
    uvect.F90 uvecteur_masque.F90 vdauv.F90 verlalo.F90
    pgsm.F90
)

##################### NORMAL TARGETS ##############################

add_library(pgsmlib STATIC ${FICHIERS_FTN90} ${FICHIERS_C})
add_dependencies(pgsmlib fst-tools_build_info)
target_link_libraries(pgsmlib PRIVATE rmn::rmn)

add_executable(pgsm pgsm.F90)
target_link_libraries(pgsm PRIVATE pgsmlib rmn::rmn)
set_target_properties(pgsm PROPERTIES 
   COMPILE_DEFINITIONS PGSM_VERSION="${PGSM_VERSION}"
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