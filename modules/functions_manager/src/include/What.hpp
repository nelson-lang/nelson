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
#include "Evaluator.hpp"
#include "nlsFunctions_manager_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
NLSFUNCTIONS_MANAGER_IMPEXP wstringVector
WhatListOfBuiltin(Evaluator* eval, bool bWithPrivateFunction = false, bool bSorted = true);
NLSFUNCTIONS_MANAGER_IMPEXP wstringVector
WhatListOfBuiltin(bool bSorted = true);
NLSFUNCTIONS_MANAGER_IMPEXP wstringVector
WhatListOfMacro(Evaluator* eval);
NLSFUNCTIONS_MANAGER_IMPEXP wstringVector
WhatListOfMacro();
} // namespace Nelson
//=============================================================================
