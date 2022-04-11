//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "Types.hpp"
#include "nlsStream_manager_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP void
delimitedWrite(ArrayOf mat, const std::wstring& filenameDestination, bool bAppend,
    const std::wstring& delimiter, int64 rowsOffset, int64 colsOffset,
    const std::wstring& formatPrecision, bool isNewLinePc);
} // namespace Nelson
//=============================================================================
