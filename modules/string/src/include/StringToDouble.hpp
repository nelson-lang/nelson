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
#include "nlsString_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
#define NanString L"NaN"
#define InfString L"Inf"
#define NegInfString L"-Inf"
#define PosInfString L"+Inf"
#define NegNanString L"-NaN" /* no sense but it can be used */
#define PosNanString L"+NaN" /* no sense but it can be used */
//=============================================================================
NLSSTRING_IMPEXP double
stringToDouble(const std::wstring& str, bool& wasConverted);
//=============================================================================
} // namespace Nelson
//=============================================================================
