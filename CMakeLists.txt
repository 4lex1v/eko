cmake_minimum_required(VERSION 3.15)

project(Eko VERSION 0.0.1 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(LLVM REQUIRED CONFIG)
llvm_map_components_to_libnames(llvm_libs core)

add_executable(eko ./code/main.cpp)
target_include_directories(eko SYSTEM PRIVATE ${LLVM_INCLUDE_DIRS})
target_compile_definitions(eko PRIVATE ${LLVM_DEFINITIONS})
target_link_libraries(eko PRIVATE ${llvm_libs})

if (MSVC)
    target_compile_options(eko PRIVATE /MD)
endif (MSVC)