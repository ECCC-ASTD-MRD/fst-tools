# Compiles every ${CMAKE_SOURCE_DIR}/${base_dir}/man/man<n>/*.org
# to ${CMAKE_BINARY_DIR}/${base_dir}/man/man<n>/%.<n>
# where <n> is a number from 1 to 8 corresponding to the standard
# manpage sections.
# MANUAL SECTIONS
#        The standard sections of the manual include:
#
#        1      User Commands
#
#        2      System Calls
#
#        3      C Library Functions
#
#        4      Devices and Special Files
#
#        5      File Formats and Conventions
#
#        6      Games et. Al.
#
#        7      Miscellanea
#
#        8      System Administration tools and Deamons
#
#        Distributions customize the manual section to their specifics, which
#        often include additional sections.0
#
macro(orgmanpages_add_man_target)

    add_custom_target(man ALL)

    file(GLOB_RECURSE org_files
        RELATIVE ${CMAKE_SOURCE_DIR}
        share/man/*.org
    )

    foreach(rel_org_file ${org_files})
        get_filename_component(org_base ${rel_org_file} NAME)
        get_filename_component(rel_dir ${rel_org_file} DIRECTORY)

        set(source_dir  ${CMAKE_SOURCE_DIR}/${rel_dir})
        set(target_dir  ${CMAKE_BINARY_DIR}/${rel_dir})

        set(org_file ${CMAKE_SOURCE_DIR}/${rel_org_file})

        string(REGEX REPLACE ".org$" ".man" man_file  ${org_file})

        string(REGEX REPLACE ".*man([1-9]).*" "\\1" man_section_number ${rel_org_file})
        string(REGEX REPLACE ".org$" ".${man_section_number}" target_base ${org_base})
        set(target_file ${target_dir}/${target_base})

        add_custom_command(
            OUTPUT ${target_file}
            DEPENDS ${rel_org_file}

            # Emacs version
            # COMMAND emacs --batch -l ox-man ${org_file} -f org-man-export-to-man
            #     && mkdir -p ${target_dir}
            #     && mv ${man_file} ${target_file}
            # BYPRODUCTS ${man_file}~

            # The '-s' is really important!
            COMMAND mkdir -p ${target_dir} && pandoc -s -f org -t man ${org_file} -o ${target_file}
        )
        add_custom_target(${target_base} DEPENDS ${target_file})

        add_dependencies(man ${target_base})
    endforeach()
    install(DIRECTORY ${CMAKE_BINARY_DIR}/share DESTINATION ${CMAKE_INSTALL_PREFIX})
endmacro()
