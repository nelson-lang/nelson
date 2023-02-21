//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isNullBuiltin.hpp"
#include "OverloadFunction.hpp"
#include "OverloadRequired.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DynamicLinkGateway::isNullBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    bool bSuccess = false;
    retval = OverloadFunction(eval, nLhs, argIn, "isNull", bSuccess);
    if (!bSuccess) {
        OverloadRequired(eval, argIn, Overload::OverloadClass::UNARY);
    }
    return retval;
}
//=============================================================================
