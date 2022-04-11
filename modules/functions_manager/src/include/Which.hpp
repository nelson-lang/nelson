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
#include "Evaluator.hpp"
#include "Types.hpp"
#include "nlsFunctions_manager_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
NLSFUNCTIONS_MANAGER_IMPEXP std::wstring
Which(const std::wstring& functionname);
NLSFUNCTIONS_MANAGER_IMPEXP wstringVector
WhichAll(const std::wstring& functionname);
NLSFUNCTIONS_MANAGER_IMPEXP wstringVector
WhichModule(const std::wstring& functionname);
} // namespace Nelson
//=============================================================================
