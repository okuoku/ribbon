cmake_minimum_required(VERSION 3.0)
project(ribbon C CXX)

function(maxwarn tgt)
    if(MSVC)
        target_compile_options(${tgt} PRIVATE /W4 /WX)
    else()
        target_compile_options(${tgt} PRIVATE -Wall -Wextra -Wpedantic -Werror)
    endif()
endfunction()

add_executable(proto
    c-proto/c-proto.c)

add_executable(protoxx
    cxx-proto/cxx-proto.cpp)

maxwarn(proto)
maxwarn(protoxx)
