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
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
inline void
nargincheck(const ArrayOfVector& argIn, int minArgs)
{
    if (argIn.size() < (size_t)minArgs) {
        RaiseError(_W("Not enough input arguments."), L"Nelson:notEnoughInputs");
    }
}
//=============================================================================
inline void
nargincheck(const ArrayOfVector& argIn, int minArgs, int maxArgs)
{
    if (argIn.size() < (size_t)minArgs) {
        RaiseError(_W("Not enough input arguments."), L"Nelson:notEnoughInputs");
    }
    if (argIn.size() > (size_t)maxArgs) {
        RaiseError(_W("Too many input arguments."), L"Nelson:tooManyInputs");
    }
}
//=============================================================================
inline void
nargoutcheck(int nLhs, int minArgs)
{
    if (nLhs < minArgs) {
        RaiseError(_W("Not enough output arguments."), L"Nelson:notEnoughOutputs");
    }
}
//=============================================================================
inline void
nargoutcheck(int nLhs, int minArgs, int maxArgs)
{
    if (nLhs < minArgs) {
        RaiseError(_W("Not enough output arguments."), L"Nelson:notEnoughOutputs");
    }
    if (nLhs > maxArgs) {
        RaiseError(_W("Too many output arguments."), L"Nelson:tooManyOutputs");
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
