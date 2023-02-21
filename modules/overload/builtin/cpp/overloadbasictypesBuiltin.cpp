//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "overloadbasictypesBuiltin.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OverloadGateway::overloadbasictypesBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 1);
    bool previousValue = eval->mustOverloadBasicTypes();
    if (argIn.size() == 1) {
        bool newValue = argIn[0].getContentAsLogicalScalar();
        if (newValue) {
            eval->enableOverloadBasicTypes();
        } else {
            eval->disableOverloadBasicTypes();
        }
        if (nLhs > 0) {
            retval << ArrayOf::logicalConstructor(previousValue);
        }
    }
    retval << ArrayOf::logicalConstructor(previousValue);
    return retval;
}
//=============================================================================
