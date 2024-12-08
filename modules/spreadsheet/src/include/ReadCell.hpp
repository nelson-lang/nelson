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
#include "nlsSpreadsheet_exports.h"
#include "DetectImportOptions.hpp"
//=============================================================================
namespace Nelson {
NLSSPREADSHEET_IMPEXP ArrayOf
ReadCell(
    const std::wstring& filename, const detectImportOptions& options, std::string& errorMessage);
};
//=============================================================================
