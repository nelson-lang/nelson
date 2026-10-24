cmake_minimum_required(VERSION 3.24)
project(RuntimeLogTests LANGUAGES CXX)
find_package(Threads REQUIRED)
get_filename_component(NELSON_SOURCE
  "${CMAKE_CURRENT_SOURCE_DIR}/../../../../.."
  ABSOLUTE
)
enable_testing()
foreach(target runtime_log runtime_log_unicode)
  add_executable(${target} ${target}.cpp)
  target_compile_features(${target} PRIVATE cxx_std_17)
  target_include_directories(${target}
    PRIVATE
      "${NELSON_SOURCE}/modules/main/nelsonc"
      "${NELSON_SOURCE}/modules/file_archiver/src/include"
  )
  target_link_libraries(${target} PRIVATE Threads::Threads)
  if(MSVC)
    target_compile_definitions(${target} PRIVATE UNICODE _UNICODE)
    set_property(
      TARGET ${target}
      PROPERTY
        MSVC_RUNTIME_LIBRARY
        "MultiThreaded$<$<CONFIG:Debug>:Debug>"
    )
  endif()
  add_test(
    NAME ${target}_native
    COMMAND ${target} "${CMAKE_CURRENT_BINARY_DIR}"
  )
  set_tests_properties(${target}_native PROPERTIES TIMEOUT 20)
endforeach()
