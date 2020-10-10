# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# This program is free software; you can redistribute it
# and/or modify it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 2.1 of the License,
# or (at your option) any later version.
#
# Alternatively, you can redistribute it and/or modify it under the terms of the
# GNU General Public License as published by the Free Software Foundation;
# either version 2 of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more
# details.
#
# You should have received a copy of the GNU Lesser General Public License along
# with this program. If not, see <http://www.gnu.org/licenses/>.
# LICENCE_BLOCK_END
# ==============================================================================
if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  set(BIN_DIRECTORY ${PROJECT_BINARY_DIR}/bin/macosx64)
  set(CMAKE_OSX_ARCHITECTURES "x86_64")
  set(Boost_NO_SYSTEM_PATHS "TRUE")
  set(MAC_FRAMEWORK_FOUNDATION_LIBRARY "-framework Foundation")
  set(MAC_FRAMEWORK_APPKIT_LIBRARY "-framework AppKit")
  if(EXISTS "/usr/local/lib/liblapacke.a")
    link_directories(/usr/local/lib)
    set(MAC_LAPACKE_LIBRARY -lblas -llapack -llapacke)
  else()
    get_filename_component(
      LAPACKE_LIB
      ${CMAKE_SOURCE_DIR}/../Nelson-thirdparty-macosx/LAPACKE/sierra/lib
      ABSOLUTE)
    if(EXISTS ${LAPACKE_LIB})
      link_directories(${LAPACKE_LIB})
      set(MAC_LAPACKE_LIBRARY -lblas -llapack -llapacke)
    else()
      message(SEND_ERROR "LAPACKE not detected.")
    endif()
  endif()
  # macports paths
  if(EXISTS "/opt/local/include")
    include_directories(/opt/local/include)
  endif()
  if(EXISTS "/opt/local/lib")
    link_directories(/opt/local/lib)
  endif()
endif()
# ==============================================================================