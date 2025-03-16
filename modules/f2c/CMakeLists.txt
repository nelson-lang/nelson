# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name f2c)
set(module_library_name nlsF2C)
# ==============================================================================
if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
  add_definitions(-D_POSIX_SOURCE)
endif()
add_definitions(-DINTEGER_STAR_8)
add_definitions(-DNO_My_ctype)
add_definitions(-DSkip_f2c_Undefs)
# ==============================================================================
file(
  GLOB
  F2C_SRC
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/abort_.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/backspace.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/c_abs.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/cabs.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/c_cos.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/c_div.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/c_exp.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/c_log.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/close.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/c_sin.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/c_sqrt.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/d_abs.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/d_acos.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/d_asin.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/d_atan.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/d_atn2.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/d_cnjg.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/d_cos.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/d_cosh.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/d_dim.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/derf_.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/derfc_.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/d_exp.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/dfe.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/d_imag.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/d_int.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/d_lg10.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/d_log.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/d_mod.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/d_nint.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/dolio.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/d_prod.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/d_sign.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/d_sin.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/d_sinh.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/d_sqrt.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/d_tan.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/d_tanh.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/due.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/ef1asc_.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/ef1cmc_.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/endfile.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/erf_.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/erfc_.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/err.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/exit.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/F77_aloc.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/fmt.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/fmtlib.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/ftell_.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/getenv_.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/h_abs.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/h_dim.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/h_dnnt.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/h_indx.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/h_len.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/hl_ge.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/hl_gt.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/hl_le.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/hl_lt.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/h_mod.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/h_nint.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/h_sign.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/i_abs.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/i_dim.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/i_dnnt.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/i_indx.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/iio.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/i_len.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/ilnw.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/i_mod.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/i_nint.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/inquire.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/i_sign.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/lbitbits.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/lbitshft.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/l_ge.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/l_gt.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/l_le.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/l_lt.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/lread.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/lwrite.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/open.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/pow_ci.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/pow_dd.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/pow_di.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/pow_hh.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/pow_ii.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/pow_qq.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/pow_ri.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/pow_zi.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/pow_zz.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/qbitbits.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/qbitshft.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/r_abs.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/r_acos.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/r_asin.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/r_atan.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/r_atn2.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/r_cnjg.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/r_cos.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/r_cosh.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/rdfmt.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/r_dim.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/rewind.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/r_exp.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/r_imag.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/r_int.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/r_lg10.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/r_log.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/r_mod.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/r_nint.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/rsfe.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/r_sign.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/r_sin.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/r_sinh.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/rsli.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/rsne.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/r_sqrt.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/r_tan.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/r_tanh.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/s_cat.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/s_cmp.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/s_copy.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/sfe.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/sig_die.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/signal_.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/s_paus.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/s_rnge.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/s_stop.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/sue.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/system_.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/typesize.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/uio.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/util.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/Version.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/wref.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/wrtfmt.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/wsfe.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/wsle.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/wsne.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/xwsne.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/z_abs.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/z_cos.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/z_div.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/z_exp.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/z_log.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/z_sin.c"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/libf2c/z_sqrt.c")
# ==============================================================================
add_library(${module_library_name} ${F2C_SRC})
# ==============================================================================
target_include_directories(
  ${module_library_name}
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE ${EIGEN3_INCLUDE_DIR}
          ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../sparse/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../elementary_mathematics/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../operators/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
          ${Boost_INCLUDE_DIRS})
# ==============================================================================
target_link_libraries(${module_library_name} 
  PRIVATE
    nlsElementary_mathematics
    nlsError_manager
    nlsTypes
    nlsI18n)
# ==============================================================================
file(GLOB F2C_SRC "${CMAKE_CURRENT_SOURCE_DIR}/src/c/f2c/*.c")
# ==============================================================================
add_executable(nelson_f2c ${F2C_SRC})
# ==============================================================================
target_link_libraries(nelson_f2c)
# ==============================================================================
file(GLOB F2C_HEADER "${CMAKE_CURRENT_SOURCE_DIR}/src/include/*.hpp"
     "${CMAKE_CURRENT_SOURCE_DIR}/src/include/*.h")
set_target_properties(
  ${module_library_name}
  PROPERTIES VERSION ${Nelson_VERSION}
             SOVERSION ${Nelson_VERSION_MAJOR}
             PUBLIC_HEADER "${F2C_HEADER}")
# ==============================================================================
include(GNUInstallDirs)
# ==============================================================================
install(
  FILES ${BIN_DIRECTORY}/nelson_f2c
  DESTINATION ${CMAKE_INSTALL_BINDIR}
  PERMISSIONS WORLD_EXECUTE WORLD_READ OWNER_EXECUTE OWNER_WRITE OWNER_READ)
install(
  TARGETS ${module_library_name}
  EXPORT ${PROJECT_NAME}-targets
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/${PROJECT_NAME}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  PUBLIC_HEADER
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/${module_name})
# ==============================================================================
install(
  FILES ${CMAKE_CURRENT_SOURCE_DIR}/loader.m
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}")
install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/etc
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
  FILES_MATCHING
  PATTERN "*.m")
install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
  FILES_MATCHING
  PATTERN "*.m")
install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
  FILES_MATCHING
  PATTERN "*.c")
install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
  FILES_MATCHING
  PATTERN "*.f")
install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/help
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
  FILES_MATCHING
  PATTERN "*.qch"
  PATTERN "xml" EXCLUDE
  PATTERN "md" EXCLUDE)
install(
  DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/functions
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
  FILES_MATCHING
  PATTERN "*.m")
# ==============================================================================
