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
#include <string>
#include "ArrayOf.hpp"
#include "nlsSpreadsheet_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Writes a JSON array with one object per row. 'keys' are the object keys and
// 'columns' the matching n x k values: numeric or logical arrays, string
// arrays (missing -> null) or cell arrays of character vectors holding raw
// JSON values. A column with k > 1 is written as a JSON array per row.
NLSSPREADSHEET_IMPEXP void
WriteJsonTable(const std::wstring& filename, const stringVector& keys, const ArrayOfVector& columns,
    bool prettyPrint, bool preserveInfAndNaN, std::wstring& errorMessage);
//=============================================================================
} // namespace Nelson
//=============================================================================
