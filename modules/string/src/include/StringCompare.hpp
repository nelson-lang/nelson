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
#include "ArrayOf.hpp"
#include "nlsString_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// strncmp with a count below one compares no character at all
constexpr indexType STRNCMP_ZERO_LENGTH = static_cast<indexType>(-1);
//=============================================================================
NLSSTRING_IMPEXP ArrayOf
StringCompare(const ArrayOf& A, const ArrayOf& B, bool bCaseSensitive = true, indexType len = 0);
//=============================================================================
} // namespace Nelson
//=============================================================================
