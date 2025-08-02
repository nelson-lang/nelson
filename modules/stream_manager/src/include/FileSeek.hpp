//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
