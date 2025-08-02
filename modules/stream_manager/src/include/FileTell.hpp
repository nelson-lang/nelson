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
//=============================================================================
#if _MSC_VER
#if defined(_WIN64)
#define NLSFTELL _ftelli64
#else
#define NLSFTELL ftell
#endif
#else
#if defined(__APPLE__) || defined(__MACH__)
#define NLSFTELL ftell
#else
#if defined(_LP64)
#if defined(HAVE_FTELL64)
#define NLSFTELL ftell64
#else
#define NLSFTELL ftell
#endif
#else
#define NLSFTELL ftell
#endif
#endif
#endif
//=============================================================================
namespace Nelson {
NLSSTREAM_MANAGER_IMPEXP int64
FileTell(File* fp);
};
//=============================================================================
