//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Error.hpp"
#include "ErrorID.hpp"
#include "GridFunctions.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
#include "meshgridBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::meshgridBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 3);
    nargincheck(argIn, 1, 3);
    if (argIn.size() > 1 && nLhs > static_cast<int>(argIn.size())) {
        Error("Nelson:tooManyOutputs", "Too many output arguments.");
    }
    return Meshgrid(nLhs, argIn);
}
//=============================================================================
