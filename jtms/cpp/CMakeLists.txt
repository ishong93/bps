cmake_minimum_required(VERSION 3.14)
project(jtms_cpp LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Core JTMS library
add_library(jtms_core
    jtms.cpp
    jtre.cpp
    match.cpp
    simplify.cpp
    jsaint.cpp
)
target_include_directories(jtms_core PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

# Main test executable
add_executable(jtms_test
    main.cpp
    jtms_ex.cpp
    jqueens.cpp
)
target_link_libraries(jtms_test PRIVATE jtms_core)
