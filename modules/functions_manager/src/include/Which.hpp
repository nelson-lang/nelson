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
#include "Evaluator.hpp"
#include "Types.hpp"
#include "nlsFunctions_manager_exports.h"
//=============================================================================
namespace Nelson {
NLSFUNCTIONS_MANAGER_IMPEXP std::wstring
Which(const std::string& functionname);
NLSFUNCTIONS_MANAGER_IMPEXP wstringVector
WhichAll(const std::string& functionname);
NLSFUNCTIONS_MANAGER_IMPEXP wstringVector
WhichModule(const std::string& functionname);
//=============================================================================
} // namespace Nelson
//=============================================================================
