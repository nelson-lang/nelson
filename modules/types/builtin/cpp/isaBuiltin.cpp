//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isaBuiltin.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "HandleGenericObject.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TypeGateway::isaBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2, 2);
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    if (!param2.isRowVectorCharacterArray()) {
        Error(ERROR_WRONG_ARGUMENT_2_TYPE_STRING_EXPECTED);
    }
    std::string classnameExpected = param2.getContentAsCString();
    if (classnameExpected == "numeric") {
        retval << ArrayOf::logicalConstructor(param1.isNumeric());
    } else if (classnameExpected == "float") {
        bool bRes = (param1.getDataClass() == NLS_DOUBLE || param1.getDataClass() == NLS_DCOMPLEX
            || param1.getDataClass() == NLS_SINGLE || param1.getDataClass() == NLS_SCOMPLEX);
        retval << ArrayOf::logicalConstructor(bRes);
    } else if (classnameExpected == "integer") {
        bool bRes = IS_INTEGER_TYPE(param1.getDataClass());
        retval << ArrayOf::logicalConstructor(bRes);
    } else {
        bool res = false;
        std::string currentClassName;
        ClassName(param1, currentClassName);
        if (currentClassName == NLS_HANDLE_STR) {
            if (classnameExpected == NLS_HANDLE_STR) {
                res = true;
            } else {
                res = (param1.getHandleCategory() == classnameExpected);
            }
        } else {
            res = (currentClassName == classnameExpected);
        }
        retval << ArrayOf::logicalConstructor(res);
    }
    return retval;
}
//=============================================================================
