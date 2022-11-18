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
#include <string>
#include "ArrayOf.hpp"
#include "nlsapi_nelson_exports.h"
//=============================================================================
namespace Nelson {
NLSAPI_NELSON_IMPEXP ArrayOf
ToCellStringAsRow(const wstringVector& vectorStr);
NLSAPI_NELSON_IMPEXP ArrayOf
ToCellStringAsColumn(const wstringVector& vectorStr);
NLSAPI_NELSON_IMPEXP ArrayOf
ToCellStringAsRow(const stringVector& vectorStr);
NLSAPI_NELSON_IMPEXP ArrayOf
ToCellStringAsColumn(const stringVector& vectorStr);
//=============================================================================
} // namespace Nelson
//=============================================================================
