cmake_minimum_required(VERSION 3.12) # CONFIGURE_DEPENDS
project(ribbon C CXX)

if(USE_FULL_BOOTSTRAP)
    if(NOT YUNI_WITH_YUNIBASE)
        message(FATAL_ERROR "Ribbon requires yunibase to generate bootstrap.")
    endif()
endif()

if(NOT WITH_YUNI)
    message(FATAL_ERROR "Ribbon requires yuni(https://github.com/okuoku/yuni/) to build.")
endif()
if(NOT WITH_RUNTIME)
    message(FATAL_ERROR "Ribbon requires WITH_RUNTIME to build.")
endif()

if(RIBBON_SNAPSHOT_BUILD)
    message(STATUS "(This is RIBBON_SNAPSHOT_BUILD)")
    set(RIBBON_SKIP_TESTS ON)
    set(USE_SNAPSHOT_BOOTSTRAP OFF)
endif()

file(TO_CMAKE_PATH "${WITH_YUNI}" WITH_YUNI)
file(TO_CMAKE_PATH "${WITH_RUNTIME}" WITH_RUNTIME)

set(yuni_runtimepath external lib lib-compat lib-r7c)
set(runtime_runtimepath
    runtime
    # Followings are not registered as libpath but we need these for now...
    emul ribbon yuniribbit)

set(yunifiles)
foreach(e ${yuni_runtimepath})
    file(GLOB_RECURSE globfiles LIST_DIRECTORIES false 
        CONFIGURE_DEPENDS
        ${WITH_YUNI}/${e}/*.sls)
    list(APPEND yunifiles ${globfiles})
endforeach()

set(runtimefiles)
foreach(e ${runtime_runtimepath})
    file(GLOB_RECURSE globfiles LIST_DIRECTORIES false 
        CONFIGURE_DEPENDS
        ${WITH_RUNTIME}/${e}/*.sls)
    list(APPEND runtimefiles ${globfiles})
endforeach()

# Interpreter(C, C++)
function(maxwarn tgt)
    if(MSVC)
        #target_compile_options(${tgt} PRIVATE /W4 /WX)
        target_compile_options(${tgt} PRIVATE /W4)
    else()
        target_compile_options(${tgt} PRIVATE -Wall -Wextra -Wpedantic -Werror)
    endif()
endfunction()

function(brand tgt)
    if(UNIX)
        target_link_libraries(${tgt} m)
    endif()
    if(COVERAGE)
        if(NOT MSVC)
            target_compile_options(${tgt} PRIVATE --coverage)
            target_link_options(${tgt} PRIVATE --coverage)
        endif()
    endif()
endfunction()

# Global definitions
set(ribbondefs
    -DYUNIROOT=\"${WITH_YUNI}\"
    -DRUNTIMEROOT=\"${WITH_RUNTIME}\"
    -DBUILDROOT=\"${CMAKE_CURRENT_BINARY_DIR}\")

function(add_ribbon_c tgt)
    add_executable(${tgt}
        c-proto/nccc-dummy.c
        c-proto/c-proto.c)
    maxwarn(${tgt})
    brand(${tgt})
endfunction()

function(add_ribbon_cxx tgt)
    add_executable(${tgt}
        c-proto/nccc-dummy.c
        cxx-proto/cxx-proto.cpp)
    maxwarn(${tgt})
    brand(${tgt})
endfunction()

# bootstrap-ribbon{-cxx} : Ribbon => Ribbon bootstrap (uses dump.bin)
# ribbon{-cxx} : Final executable (uses riboot.bin)

if(USE_FULL_BOOTSTRAP)
    add_ribbon_c(bootstrap-ribbon)
    target_compile_definitions(bootstrap-ribbon
        PRIVATE
        ${ribbondefs}
        -DBOOTNAME=\"dump.bin\")
endif()

if(RIBBON_SNAPSHOT_BUILD)
    set(ribbontgt snapshot-ribbon)
    set(ribboncxxtgt)
else()
    set(ribbontgt ribbon)
    set(ribboncxxtgt ribbon-cxx)
endif()

add_ribbon_c(${ribbontgt})
target_compile_definitions(${ribbontgt}
    PRIVATE
    ${ribbondefs}
    -DBOOTNAME=\"riboot.bin\")

if(ribboncxxtgt)
    add_ribbon_cxx(${ribboncxxtgt})
    target_compile_definitions(ribbon-cxx
        PRIVATE
        ${ribbondefs}
        -DBOOTNAME=\"riboot.bin\")
endif()

# Runtime BLOB
set(bootstrapfile0 ${CMAKE_CURRENT_BINARY_DIR}/dump.bin)
set(bootstrapfile ${CMAKE_CURRENT_BINARY_DIR}/riboot.bin)
if(WITH_RIBBON_PREBOOT)
    # Use PRE_BUILD so we can ensure built ${ribbontgt} properly bootstrapped
    # when it used on secondary bootstrap
    add_custom_command(
        TARGET ${ribbontgt} PRE_BUILD
        BYPRODUCTS ${bootstrapfile}
        COMMAND
        ${CMAKE_COMMAND} -E copy
        ${WITH_RIBBON_PREBOOT} ${bootstrapfile}
        DEPENDS ${WITH_RIBBON_PREBOOT})
elseif(USE_SNAPSHOT_BOOTSTRAP)
    set(WITH_RIBBON_PREBOOT ${WITH_RIBBON_SNAPSHOT}/riboot.bin)
    set(RIBBON_SNAPSHOT_BUILD ON)
    add_subdirectory(${WITH_RIBBON_SNAPSHOT}/ribbon snapshot-build)

    # Bootstrap
    add_custom_command(OUTPUT ${bootstrapfile}
        COMMAND snapshot-ribbon
        ${CMAKE_CURRENT_LIST_DIR}/boot/bootstrap.sps
        -bootstrapsave ${bootstrapfile}
        -bootstrapfile ${CMAKE_CURRENT_LIST_DIR}/boot/start.sps
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        DEPENDS ${yunifiles} ${runtimefiles}
        ${WITH_RIBBON_PREBOOT} 
        ${CMAKE_CURRENT_LIST_DIR}/boot/start.sps
        ${CMAKE_CURRENT_LIST_DIR}/boot/bootstrap.sps)
elseif(USE_FULL_BOOTSTRAP)
    add_subdirectory(${WITH_YUNI} yuni)

    # Bootstrap0
    add_custom_command(OUTPUT ${bootstrapfile0}
        COMMAND
        ${CMAKE_COMMAND} 
        -DROOT=${WITH_YUNI} 
        -DYUNIBUILD=${CMAKE_CURRENT_BINARY_DIR}/yuni 
        -DIMPL=gauche 
        -P ${WITH_RUNTIME}/_run.cmake 
        COMPILE
        ${CMAKE_CURRENT_LIST_DIR}/boot/start.sps
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        DEPENDS ${yunifiles} ${runtimefiles}
        ${WITH_RUNTIME}/_run.cmake
        ${WITH_RUNTIME}/_immvm.sps
        ${CMAKE_CURRENT_LIST_DIR}/boot/start.sps)

    # Bootstrap
    add_custom_command(OUTPUT ${bootstrapfile}
        COMMAND
        bootstrap-ribbon
        ${CMAKE_CURRENT_LIST_DIR}/boot/bootstrap.sps
        -bootstrapsave ${bootstrapfile}
        -bootstrapfile ${CMAKE_CURRENT_LIST_DIR}/boot/start.sps
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        DEPENDS ${yunifiles} ${runtimefiles}
        ${bootstrapfile0} 
        ${CMAKE_CURRENT_LIST_DIR}/boot/start.sps
        ${CMAKE_CURRENT_LIST_DIR}/boot/bootstrap.sps)
else()
    message(FATAL_ERROR "Unimplemented.")
endif()

add_custom_target(${ribbontgt}-bootfile ALL DEPENDS ${bootstrapfile})

# Tests

if(NOT RIBBON_SKIP_TESTS)
    include(${CMAKE_CURRENT_LIST_DIR}/tests.cmake)
endif()

# Integration helpers
if(NOT RIBBON_SNAPSHOT_BUILD)
    if(RIBBON_INTEG_BUILD_DIR)
        message(STATUS "Generating bootstrap files on ${RIBBON_INTEG_BUILD_DIR}")
        # Dep list
        file(WRITE ${RIBBON_INTEG_BUILD_DIR}/deps.cmake "set(yunideps_srcs ${yunifiles};${runtimefiles})\n")
        file(WRITE ${RIBBON_INTEG_BUILD_DIR}/bootstrap_params.cmake
            "set(WITH_RUNTIME \"${WITH_RUNTIME}\")\nset(ROOT \"${WITH_YUNI}\")\nset(IMPL gauche)\n\nset(WITH_YUNI \"${WITH_YUNI}\")\nset(BUILDROOT \"${CMAKE_CURRENT_BINARY_DIR}\")\n")
    endif()
endif()

