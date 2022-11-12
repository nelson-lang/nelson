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
#include "Types.hpp"
#include "nlsAssert_functions_exports.h"
//=============================================================================
namespace Nelson {
NLSASSERT_FUNCTIONS_IMPEXP logical
Assert_IsTrue(logical value, const std::wstring& modifiedmsg, std::wstring& msg);
}
//=============================================================================
