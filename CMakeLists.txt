cmake_minimum_required(VERSION 3.18)
project(geo_fortran Fortran)

# ---------------------------------------------------------------------------
# Options
# ---------------------------------------------------------------------------
option(BUILD_EXAMPLES "Build example programs" ON)

set(CMAKE_Fortran_STANDARD 2008)

# Default to Release if not set
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
endif()

# ---------------------------------------------------------------------------
# Compiler flags
# ---------------------------------------------------------------------------
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(CMAKE_Fortran_FLAGS         "${CMAKE_Fortran_FLAGS} -Wall -Wextra")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -march=native")
  set(CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g -fcheck=all -fbacktrace")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "NVHPC|PGI")
  # nvfortran (CUDA Fortran host compiler)
  set(CMAKE_Fortran_FLAGS         "${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
  set(CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g -Mbounds")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(CMAKE_Fortran_FLAGS         "${CMAKE_Fortran_FLAGS} -warn all")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -xHost")
  set(CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g -check all -traceback")
endif()

# ---------------------------------------------------------------------------
# Library: geo_fortran
# ---------------------------------------------------------------------------
add_library(geo_fortran STATIC src/geo_fortran.f90)

target_include_directories(geo_fortran
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
)

# Make module files accessible to consumers
set_target_properties(geo_fortran PROPERTIES
  Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod
)

target_include_directories(geo_fortran
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>
)

# ---------------------------------------------------------------------------
# Examples
# ---------------------------------------------------------------------------
if(BUILD_EXAMPLES)
  add_executable(create_grids examples/create_grids.f90)
  target_link_libraries(create_grids PRIVATE geo_fortran)

  add_executable(read_grids examples/read_grids.f90)
  target_link_libraries(read_grids PRIVATE geo_fortran)
endif()

# ---------------------------------------------------------------------------
# Install rules
# ---------------------------------------------------------------------------
include(GNUInstallDirs)

install(TARGETS geo_fortran
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
)
install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod/
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/geo_fortran
  FILES_MATCHING PATTERN "*.mod"
)

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
message(STATUS "----------------------------------------------------")
message(STATUS "geo_fortran")
message(STATUS "  Build type   : ${CMAKE_BUILD_TYPE}")
message(STATUS "  Compiler     : ${CMAKE_Fortran_COMPILER_ID} ${CMAKE_Fortran_COMPILER_VERSION}")
message(STATUS "  Build examples: ${BUILD_EXAMPLES}")
message(STATUS "----------------------------------------------------")
