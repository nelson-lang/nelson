//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "File.hpp"
#include "Types.hpp"
#include "nlsStream_manager_exports.h"
#include <cstdio>
#include <string>
//=============================================================================
#if _MSC_VER
#if defined(_WIN64)
#define NLSFSEEK _fseeki64
#else
#define NLSFSEEK fseek
#endif
#else
#if defined(__APPLE__) || defined(__MACH__)
#define NLSFSEEK fseek
#else
#if defined(_LP64)
#if defined(HAVE_FSEEK64)
#define NLSFSEEK fseek64
#else
#define NLSFSEEK fseek
#endif
#else
#define NLSFSEEK fseek
#endif
#endif
#endif
//=============================================================================
namespace Nelson {
NLSSTREAM_MANAGER_IMPEXP bool
FileSeek(File* fp, int64 offset, int origin);
};
//=============================================================================
