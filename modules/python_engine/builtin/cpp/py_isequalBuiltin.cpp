//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "py_isequalBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PythonObjectHandle.hpp"
#include "PyIsEqual.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::Python_engineGateway::py_isequalBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2);
    nargoutcheck(nLhs, 0, 1);

    ArrayOf param1 = argIn[0];
    for (size_t k = 1; k < argIn.size(); k++) {
        if (!PyIsEqual(param1, argIn[k])) {
            return ArrayOf::logicalConstructor(false);
        }
    }
    return ArrayOf::logicalConstructor(true);
}
//=============================================================================
