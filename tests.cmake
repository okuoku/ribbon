enable_testing()

set(tests ${WITH_YUNI}/tests)

set(testsrcs
    ${tests}/scheme/core0.sps
    ${tests}/scheme/core1.sps
    ${tests}/scheme/core2.sps
    ${tests}/scheme/exp0.sps
    ${tests}/scheme/exp1.sps
    ${tests}/scheme/exp2.sps
    ${tests}/scheme/vectors0.sps
    ${tests}/scheme/bytevectors0.sps
    ${tests}/scheme/exact0.sps
    ${tests}/scheme/inexact0.sps
    ${tests}/scheme/inexact1.sps
    ${tests}/scheme/inexact2.sps
    ${tests}/scheme/iter0.sps
    ${tests}/scheme/qq1.sps
    ${tests}/scheme/stx0.sps
    ${tests}/scheme/strings0.sps
    ${tests}/scheme/values0.sps
    ${tests}/scheme/values1.sps
    ${tests}/scheme/values2.sps
    ${tests}/scheme/values3.sps
    ${tests}/lib/hashtables0.sps
    ${tests}/lib/hashtables1.sps
    ${tests}/lib/lighteval0.sps
    ${tests}/lib/miniread0.sps
    ${tests}/lib/minitest0.sps
    ${tests}/lib/yuniio0.sps
    ${tests}/lib/yuniio1.sps
    ${tests}/sibr/err-sibr0012string.sps
    ${tests}/sibr/err-sibr0014.sps
    )

set(negativetestsrcs
    # Run phase
    ${tests}/scheme/inexact3.sps # SIBR0013
    ${tests}/scheme/qq0.sps # ???

    # Expand phase
    ${tests}/app/basic/app.sps # lib ?

    # Not ported yet
    ${tests}/sibr/sibr0010string.sps
    ${tests}/sibr/sibr0010vector.sps
    ${tests}/sibr/sibr0011.sps
    ${tests}/sibr/sibr0012gen.sps
    ${tests}/sibr/sibr0013.sps
    ${tests}/sibr/sibr0014.sps

    # fake syntax-rules
    ${tests}/scheme/synrule0.sps
    ${tests}/scheme/synrule1.sps

    # Failure tests
    ${tests}/err/fail0.sps
    ${tests}/err/fail1.sps
    ${tests}/err/fail2.sps
    ${tests}/err/fail3.sps
    ${tests}/err/fail4.sps
    ${tests}/err/fail5.sps
    ${tests}/err/fail6.sps
    ${tests}/err/fail7.sps
    ${tests}/err/fail8.sps
    )

set(excludes 
    # FIXME: Currently, C/CXX would abort() for any error. Skip these tests.
    qq0 app sibr0010string sibr0010vector sibr0011 sibr0012gen
    sibr0013 sibr0014
    fail1 fail3 fail4 fail5 fail6 fail8)

foreach(e ${excludes})
    set(exclude_C-${e} ON)
    set(exclude_CXX-${e} ON)
endforeach()

function(add_c_test nam fil turnfail)
    foreach(lang C CXX)
        if(NOT exclude_${lang}-${nam})
            add_test(NAME ${lang}-${nam}
                COMMAND proto
                ${fil})
            set_tests_properties(${lang}-${nam}
                PROPERTIES WILL_FAIL ${turnfail})
        endif()
    endforeach()
endfunction()

function(add_scm_test nam fil turnfail)
    # Base
    add_test(NAME BASE-${nam}
        COMMAND ${CMAKE_COMMAND}
        -DROOT=${WITH_YUNI}
        -DYUNIBUILD=${CMAKE_CURRENT_BINARY_DIR}/yuni
        -DIMPL=gauche
        -Dinput=${f}
        -P ${WITH_RUNTIME}/_testrun.cmake)

    ## interp
    #add_test(NAME EMUL-${nam}
    #    COMMAND ${CMAKE_COMMAND}
    #    -DROOT=${WITH_YUNI}
    #    -DYUNIBUILD=${CMAKE_CURRENT_BINARY_DIR}/yuni
    #    -DIMPL=gauche
    #    -Dinterp=emul
    #    -Dinput=${f}
    #    -P ${WITH_RUNTIME}/_testrun.cmake)

    set_tests_properties(BASE-${nam}
        PROPERTIES WILL_FAIL ${turnfail})
    #set_tests_properties(BASE-${nam} EMUL-${nam}
    #    PROPERTIES WILL_FAIL ${turnfail})
endfunction()

function(add_any_test nam fil turnfail)
    add_scm_test(${nam} ${fil} ${turnfail})
    add_c_test(${nam} ${fil} ${turnfail})
endfunction()

foreach(f ${testsrcs})
    get_filename_component(nam ${f} NAME_WE)
    add_any_test(${nam} ${f} FALSE)
endforeach()

foreach(f ${negativetestsrcs})
    get_filename_component(nam ${f} NAME_WE)
    add_any_test(${nam} ${f} TRUE)
endforeach()

