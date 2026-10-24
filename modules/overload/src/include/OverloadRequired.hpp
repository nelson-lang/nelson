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
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
inline void
OverloadRequired(const std::string& functionName)
{
    Error("Nelson:UndefinedFunction:incorrectArgumentType",
        "Check for incorrect argument data type or missing argument in call to function '%s'.",
        functionName);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
