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
        raiseError2(_E("nelson:arguments:tooFewInputs"));
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
        raiseError2(_E("nelson:arguments:tooFewInputs"));
    }
    if (argIn.size() > (size_t)maxArgs) {
        raiseError2(_E("nelson:arguments:tooManyInputs"));
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
        raiseError2(_E("nelson:arguments:tooFewOutputs"));
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
        raiseError2(_E("nelson:arguments:tooFewOutputs"));
    }
    if (nLhs > maxArgs) {
        raiseError2(_E("nelson:arguments:tooManyOutputs"));
    }
}
//=============================================================================
}
//=============================================================================
