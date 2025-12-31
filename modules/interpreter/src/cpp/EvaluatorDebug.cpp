//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/core.h>
#include "i18n.hpp"
#include "Evaluator.hpp"
#include "Error.hpp"
#include "MacroFunctionDef.hpp"
#include "Warning.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
Evaluator::resetDebugDepth()
{
    depth = 0;
}
//=============================================================================
// Returns the current debug depth.
int
Evaluator::getDebugDepth()
{
    return depth;
}
//=============================================================================
// Increases the debug depth by one.
void
Evaluator::increaseDebugDepth()
{
    depth++;
}
//=============================================================================
// Decreases the debug depth by one.
void
Evaluator::decreaseDebugDepth()
{
    depth--;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
