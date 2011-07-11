
set( HYDROTREND_EXE ${CMAKE_CURRENT_BINARY_DIR}/../run_hydrotrend )

configure_file (
  ${CMAKE_CURRENT_SOURCE_DIR}/hydrotrend_test_without_args.sh.in
  ${CMAKE_CURRENT_BINARY_DIR}/hydrotrend_test_without_args.sh )
configure_file( ${CMAKE_CURRENT_SOURCE_DIR}/hydrotrend_test_with_args.sh.in
                ${CMAKE_CURRENT_BINARY_DIR}/hydrotrend_test_with_args.sh )

########### Testing ###############

#ENABLE_TESTING()

#add_test(HYDROTREND_VERSION ${HYDROTREND_EXE} --version )
#add_test(HYDROTREND_HELP ${HYDROTREND_EXE} --help )
#add_test(HYDROTREND_TEST hydrotrend_test.sh )
##        ${CMAKE_CURRENT_SOURCE_DIR}/Hydrotest.sh
##          ${HYDROTREND_EXE} ${CMAKE_CURRENT_SOURCE_DIR})

install(DIRECTORY input DESTINATION share/hydrotrend COMPONENT hydrotrend)
install(DIRECTORY output DESTINATION share/hydrotrend COMPONENT hydrotrend)

