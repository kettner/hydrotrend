cmake_minimum_required(VERSION 2.6)

project( hydrotrend )

include (CTest)

ENABLE_TESTING()

set (HYDROTREND_EXE run_hydrotrend)

add_test (HYDROTREND_VERSION ${HYDROTREND_EXE} --version)
add_test (HYDROTREND_HELP ${HYDROTREND_EXE} --help)
add_test (HYDROTREND_TEST_WITHOUT_ARGS
          ${CMAKE_CURRENT_BINARY_DIR}/data/hydrotrend_test_without_args.sh)
add_test (HYDROTREND_TEST_WITH_ARGS
          ${CMAKE_CURRENT_BINARY_DIR}/data/hydrotrend_test_with_args.sh)

add_subdirectory( data )

########### libhydrotrend ###############

set( hydrotrend_lib_SRCS
   hydrotrend_api.c
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
   hydroprinttable.c
   hydrorain.c
   hydroran2.c
   hydroran2sediment.c
   hydrorandom.c
   hydrorandomsediment.c
   hydroreadclimate.c
   hydroreadevaporation.c
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

add_library( hydrotrend ${hydrotrend_lib_SRCS} )

target_link_libraries( hydrotrend m )

#set_target_properties(hydrotrend PROPERTIES VERSION 4.2.0 SOVERSION 4)
install(TARGETS hydrotrend DESTINATION lib COMPONENT hydrotrend)
export(TARGETS hydrotrend FILE hydrotrend-exports.cmake)

########### hydrotrend main program ###############

SET(hydrotrend_SRCS
   hydrotrend_main.c)

add_executable(run_hydrotrend ${hydrotrend_SRCS})

target_link_libraries(run_hydrotrend hydrotrend)

install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/run_hydrotrend
        DESTINATION bin
        RENAME hydrotrend
        COMPONENT hydrotrend)

########### Python bindings ###############

# You may need to use -DCMAKE_INCLUDE_PATH and -DCMAKE_LIBRARY_PATH
# to find the correct libraries/include paths
find_package (SWIG REQUIRED)
include (${SWIG_USE_FILE})

find_package (PythonLibs 2.6 EXACT)
include_directories (${PYTHON_INCLUDE_PATH})
include_directories (${CMAKE_CURRENT_SOURCE_DIR})

set (CMAKE_SWIG_FLAGS "")

#set_source_files_properties (deltas_mod.i PROPERTIES CPLUSPLUS ON) 
#set_source_files_properties (deltas_mod.i PROPERTIES SWIG_FLAGS "-includeall")
#swig_add_module (hydrotrend_mod python hydrotrend_mod.i hydrotrend_api.c)
swig_add_module (hydrotrend_mod python hydrotrend_mod.i ${hydrotrend_lib_SRCS})
swig_link_libraries (hydrotrend_mod ${PYTHON_LIBRARIES})

#add_library( _deltas_mod deltas_mod_wrap.c ndelta4.c deltas_api.c )
#target_link_libraries( deltas_mod _deltas_mod)

message ("Python include path " ${PYTHON_INCLUDE_PATH})
message ("Python libs " ${PYTHON_LIBRARIES})
message ("Python lib path " ${PYTHON_LIBRARY_PATH})

install (FILES ${CMAKE_BINARY_DIR}/hydrotrend_mod.py
               ${CMAKE_BINARY_DIR}/_hydrotrend_mod.so
         DESTINATION lib/hydrotrend-python COMPONENT hydrotrend)

########### Install files ###############

install(FILES  hydrotrend_api.h hydrotrend_cli.h
        DESTINATION include
        COMPONENT hydrotrend)

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
