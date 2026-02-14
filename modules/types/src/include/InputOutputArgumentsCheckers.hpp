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
#include "ArrayOf.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
/**
 * Validate number of input arguments
 */
inline void
nargincheck(const ArrayOfVector& argIn, int minArgs)
{
    if (argIn.size() < (size_t)minArgs) {
        raiseError(L"Nelson:error_manager:min_rhs", ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
}
//=============================================================================
/**
 * Validate number of input arguments
 */
inline void
nargincheck(const ArrayOfVector& argIn, int minArgs, int maxArgs)
{
    if (argIn.size() < (size_t)minArgs) {
        raiseError(L"Nelson:error_manager:min_rhs", ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (argIn.size() > (size_t)maxArgs) {
        raiseError(L"Nelson:error_manager:max_rhs", ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
}
//=============================================================================
/**
 * Validate number of output arguments
 */
inline void
nargoutcheck(int nLhs, int minArgs)
{
    if (nLhs < minArgs) {
        raiseError2(L"Nelson:error_manager:min_lhs");
    }
}
//=============================================================================
/**
 * Validate number of output arguments
 */
inline void
nargoutcheck(int nLhs, int minArgs, int maxArgs)
{
    if (nLhs < minArgs) {
        raiseError2(L"Nelson:error_manager:min_lhs");
    }
    if (nLhs > maxArgs) {
        raiseError2(L"Nelson:error_manager:max_lhs");
    }
}
//=============================================================================
}
//=============================================================================
