cmake_minimum_required(VERSION 3.15)

project(Eko VERSION 0.0.1 LANGUAGES CXX)

if (MSVC)
    message(FATAL_ERROR "MSVC Toolchain is not supported")
endif ()

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_compile_definitions(PLATFORM_WIN32 CPU_ARCH_X64)

add_executable(eko
        ./code/driver.cpp
        ./code/tokenizer.cpp
        ./code/parser.cpp
        ./code/typer.cpp
        ./code/llvm_codegen.cpp)

target_include_directories(eko PRIVATE libs libs/llvm/include)
target_compile_definitions(eko PRIVATE $<$<CONFIG:Debug>:DEV_BUILD>)

set_target_properties(eko PROPERTIES 
    VS_DEBUGGER_WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    VS_STARTUP_PROJECT eko
)
