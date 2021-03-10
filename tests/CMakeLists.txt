cmake_minimum_required(VERSION 3.0)

include(CTest)

add_test(
  NAME "HYDROTREND_VERSION"
  COMMAND run_hydrotrend --version
)
add_test(
  NAME "HYDROTREND_HELP"
  COMMAND run_hydrotrend --help
)
add_test(
  NAME "HYDROTREND_RUN"
  COMMAND run_hydrotrend
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)
