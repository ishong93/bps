cmake_minimum_required(VERSION 3.14)
project(atms CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Core ATMS library
add_library(atms_core
    atms.cpp
    atre.cpp
    unify.cpp
    planner.cpp
)

target_include_directories(atms_core PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

# Main executable with all tests
add_executable(atms_test
    main.cpp
    atest.cpp
    atret.cpp
)

target_link_libraries(atms_test PRIVATE atms_core)
