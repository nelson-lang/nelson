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
#include <vector>
#include "ArrayOf.hpp"
#include "nlsSpreadsheet_exports.h"
//=============================================================================
namespace Nelson {
NLSSPREADSHEET_IMPEXP ArrayOf
delimitedFileReader(const std::wstring& filename, const std::wstring& delimiter,
    std::vector<double> range, std::wstring& errorMessage);
};
//=============================================================================
