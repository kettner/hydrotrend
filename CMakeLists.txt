cmake_minimum_required(VERSION 2.6)
include(CheckIncludeFile)

project( hydrotrend )

option (WITH_PYTHON_BINDINGS "Build Python bindings" OFF)
option (WITH_CTEST "Use CTest to build tests" OFF)

set (BUILD_SHARED_LIBS ON)

set (HYDROTREND_VERSION 3.1)
set (HYDROTREND_EXE hydrotrend)

configure_file (${CMAKE_CURRENT_SOURCE_DIR}/hydrotrend.pc.cmake ${CMAKE_CURRENT_SOURCE_DIR}/hydrotrend.pc )

if (WITH_CTEST)

include (CTest)
ENABLE_TESTING()

add_test (HYDROTREND_VERSION ${HYDROTREND_EXE} --version)
add_test (HYDROTREND_HELP ${HYDROTREND_EXE} --help)
add_test (HYDROTREND_TEST_WITHOUT_ARGS
          ${CMAKE_CURRENT_BINARY_DIR}/data/hydrotrend_test_without_args.sh)
#add_test (HYDROTREND_TEST_WITH_ARGS
#          ${CMAKE_CURRENT_BINARY_DIR}/data/hydrotrend_test_with_args.sh)
endif (WITH_CTEST)

add_subdirectory( data )

configure_file (${CMAKE_CURRENT_SOURCE_DIR}/hydrotrend.pc.cmake
                ${CMAKE_CURRENT_SOURCE_DIR}/hydrotrend.pc )

########### libhydrotrend ###############

check_include_file(getopt.h WITH_GETOPT)
check_include_file(unistd.h WITH_UNISTD)
find_library(LIB_M m)

if (WITH_GETOPT)
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DWITH_GETOPT")
endif (WITH_GETOPT)

if (WITH_UNISTD)
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DWITH_UNISTD")
endif (WITH_UNISTD)
find_library(LIB_M m)

set(hydrotrend_lib_SRCS
   bmi_hydrotrend.c
   hydrotrend_irf.c
   hydroalloc_mem.c
   hydrocalqsnew.c
   hydrocheckinput.c
   hydroclimate.c
   hydrotrend_cli.c
   hydroexpdist.c
   hydrofree_mem.c
   hydroglacial.c
   hydrohypsom.c
   hydroinputalloc.c
   hydromaxevents.c
   hydroopenfiles.c
   hydrooutlet.c
   hydrooutput.c
   hydroprintannual.c
   hydroprintstat.c
   hydrorain.c
   hydroran2.c
   hydroran2sediment.c
   hydrorandom.c
   hydrorandomsediment.c
   hydroreadclimate.c
   hydroreadevaporation.c
   hydroreadearthquake.c
   hydroreadhypsom.c
   hydroreadinput.c
   hydrosecurityinputcheck.c
   hydrosedload.c
   hydrosetgeoparams.c
   hydrosetglobalpar.c
   hydrosetparams.c
   hydroshoulder.c
   hydroshuffle.c
   hydrosnow.c
   hydrosumflow.c
   hydroswap.c
   hydroweather.c)

########### BMI ###########

if (NOT WIN32)
  add_library(bmi_hydrotrend-static STATIC ${hydrotrend_lib_SRCS} )
  add_library(bmi_hydrotrend SHARED ${hydrotrend_lib_SRCS} )
  set(BMI_HYDROTREND_LIB bmi_hydrotrend-static)
  set(BMI_HYDROTREND_TARGETS bmi_hydrotrend bmi_hydrotrend-static)
else (WIN32)
  add_library(bmi_hydrotrend STATIC ${hydrotrend_lib_SRCS})
  set(BMI_HYDROTREND_LIB bmi_hydrotrend)
  set(BMI_HYDROTREND_TARGETS bmi_hydrotrend)
endif (NOT WIN32)

install(
  TARGETS ${BMI_HYDROTREND_TARGETS}
  EXPORT libbmi_hydrotrend
  RUNTIME DESTINATION bin
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  FRAMEWORK DESTINATION ${CMAKE_INSTALL_LIBDIR}
)
install(FILES bmi.h bmi_hydrotrend.h DESTINATION include/hydrotrend)

########### hydrotrend main program ###############

set(hydrotrend_SRCS hydrotrend_main.c)

add_executable(hydrotrend ${hydrotrend_SRCS})
if (LIB_M)
  target_link_libraries(hydrotrend ${BMI_HYDROTREND_LIB} m)
elseif (NOT LIB_M)
  target_link_libraries(hydrotrend ${BMI_HYDROTREND_LIB})
endif (LIB_M)

install(
  PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/Debug/hydrotrend${CMAKE_EXECUTABLE_SUFFIX}
  DESTINATION bin
  COMPONENT hydrotrend
)

########### Install files ###############

install(FILES  hydrotrend.pc DESTINATION lib/pkgconfig  COMPONENT hydrotrend)

########### Configuration Information ###############

if ( CMAKE_BUILD_TYPE MATCHES Release )
  set( cflags ${CMAKE_C_FLAGS_RELEASE} )
elseif ( CMAKE_BUILD_TYPE MATCHES Debug )
  set( cflags ${CMAKE_C_FLAGS_DEBUG} )
else ( )
  set( cflags ${CMAKE_C_FLAGS} )
endif ( CMAKE_BUILD_TYPE MATCHES Release )

message("------------------------------------------------------------------------")
message("Configuration:")
message("")
message("  Source code location:       ${CMAKE_SOURCE_DIR}")
message("  Build type:                 ${CMAKE_BUILD_TYPE}")
message("  Compiler:                   ${CMAKE_C_COMPILER}")
message("  Compiler flags:             ${cflags}")
message("  Host System Type:           ${CMAKE_HOST_SYSTEM}")
message("  Installation architecture:  ${CMAKE_SYSTEM}")
message("  Install path:               ${CMAKE_INSTALL_PREFIX}")
message("  With doxygen:               ${DOXYGEN}")
message("")
message("------------------------------------------------------------------------")
