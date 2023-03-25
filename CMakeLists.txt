cmake_minimum_required(VERSION 3.12) # CONFIGURE_DEPENDS
project(ribbon C CXX)

if(NOT YUNI_WITH_YUNIBASE)
    message(FATAL_ERROR "Ribbon requires yunibase to generate bootstrap.")
endif()

if(NOT WITH_YUNI)
    message(FATAL_ERROR "Ribbon requires yuni(https://github.com/okuoku/yuni/) to build.")
endif()
if(NOT WITH_RUNTIME)
    message(FATAL_ERROR "Ribbon requires WITH_RUNTIME to build.")
endif()

file(TO_CMAKE_PATH "${WITH_YUNI}" WITH_YUNI)
file(TO_CMAKE_PATH "${WITH_RUNTIME}" WITH_RUNTIME)

set(yuni_runtimepath external lib lib-compat lib-r7c)
set(runtime_runtimepath
    runtime
    # Followings are not registered as libpath but we need these for now...
    emul ribbon yuniribbit)

foreach(e ${yuni_runtimepath})
    file(GLOB_RECURSE yunifiles LIST_DIRECTORIES false 
        CONFIGURE_DEPENDS
        ${WITH_YUNI}/${e}/*.sls)
endforeach()

foreach(e ${runtime_runtimepath})
    file(GLOB_RECURSE runtimefiles LIST_DIRECTORIES false 
        CONFIGURE_DEPENDS
        ${WITH_RUNTIME}/${e}/*.sls)
endforeach()

# Runtime BLOB
add_subdirectory(${WITH_YUNI} yuni)

add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/dump.bin
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
    ${CMAKE_CURRENT_LIST_DIR}/boot/start.sps
    )

add_custom_target(bootstrapblob DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/dump.bin)

# Interpreter(C, C++)

function(maxwarn tgt)
    if(MSVC)
        #target_compile_options(${tgt} PRIVATE /W4 /WX)
        target_compile_options(${tgt} PRIVATE /W4)
    else()
        target_compile_options(${tgt} PRIVATE -Wall -Wextra -Wpedantic -Werror)
    endif()
endfunction()

add_definitions(
    -DYUNIROOT=\"${WITH_YUNI}\"
    -DRUNTIMEROOT=\"${WITH_RUNTIME}\"
    -DBUILDROOT=\"${CMAKE_CURRENT_BINARY_DIR}\")

add_executable(proto
    c-proto/c-proto.c)

add_executable(protoxx
    cxx-proto/cxx-proto.cpp)

maxwarn(proto)
maxwarn(protoxx)

# Tests

include(${CMAKE_CURRENT_LIST_DIR}/tests.cmake)
