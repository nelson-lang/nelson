//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "lengthBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "OverloadFunction.hpp"
#include "ClassName.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::lengthBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1); // Call overload if it exists
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "length", bSuccess);
    }
    if (!bSuccess) {
        if (argIn[0].isSparse() || argIn[0].isCell() || argIn[0].isHandle() || argIn[0].isStruct()
            || argIn[0].isClassStruct()) {
            retval = OverloadFunction(eval, nLhs, argIn, "length", bSuccess);
            if (bSuccess) {
                return retval;
            }
        }
        ArrayOf param1 = argIn[0];
        if (param1.isClassStruct() && !param1.isFunctionHandle()) {
            Error(_("Undefined function 'length' for input arguments of type") + " '"
                + ClassName(param1) + "'.");
        }
        double len = 0;
        Dimensions sze(param1.getDimensions());
        for (indexType i = 0; i < sze.getLength(); i++) {
            if (static_cast<double>(sze[i]) == 0) {
                len = 0;
                break;
            }
            if (i == 0) {
                len = static_cast<double>(sze[i]);
            } else {
                if (static_cast<double>(sze[i]) > len) {
                    len = static_cast<double>(sze[i]);
                }
            }
        }
        retval << ArrayOf::doubleConstructor(len);
    }
    return retval;
}
//=============================================================================
