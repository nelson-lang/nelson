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
#include "i18n.hpp"
#include "ndgridBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::ndgridBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    if (argIn.empty()) {
        Error("Nelson:notEnoughInputs", "Not enough input arguments.");
    }
    if (argIn.size() > 1 && nLhs > static_cast<int>(argIn.size())) {
        Error("Nelson:tooManyOutputs", "Too many output arguments.");
    }
    return Ndgrid(nLhs, argIn);
}
//=============================================================================
