//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isaBuiltin.hpp"
#include "ClassName.hpp"
#include "ClassToString.hpp"
#include "ClassdefParser.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "HandleGenericObject.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TypeGateway::isaBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2, 2);
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    if (!(param2.isRowVectorCharacterArray() || param2.isScalarStringArray())) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2), L"string");
    }
    std::string classnameExpected = param2.asUtf8String();
    if (classnameExpected == "numeric") {
        retval << ArrayOf::logicalScalar(param1.isNumeric());
    } else if (classnameExpected == "float") {
        bool bRes = (param1.dataClass() == NLS_DOUBLE || param1.dataClass() == NLS_DCOMPLEX
            || param1.dataClass() == NLS_SINGLE || param1.dataClass() == NLS_SCOMPLEX);
        retval << ArrayOf::logicalScalar(bRes);
    } else if (classnameExpected == "integer") {
        bool bRes = IS_INTEGER_TYPE(param1.dataClass());
        retval << ArrayOf::logicalScalar(bRes);
    } else {
        bool res = false;
        if (param1.dataClass() == NLS_HANDLE) {
            res = (classnameExpected == NLS_HANDLE_STR)
                || classdefIsA(param1.handleClassName(), classnameExpected);
        } else if (param1.dataClass() == NLS_GO_HANDLE) {
            const std::string currentClassName = ClassName(param1);
            res = (classnameExpected == NLS_HANDLE_STR || classnameExpected == NLS_GO_HANDLE_STR
                || classnameExpected == NLS_GRAPHICS_OBJECT_CLASS_STR
                || currentClassName == classnameExpected);
        } else if (param1.dataClass() == NLS_CLASS_ARRAY
            || param1.dataClass() == NLS_CLASSDEF_ARRAY) {
            res = (classnameExpected == NLS_CLASS_ARRAY_STR)
                || classdefIsA(param1.classType(), classnameExpected);
        } else {
            std::string currentClassName
                = param1.isSparse() ? ClassToString(param1.dataClass()) : ClassName(param1);
            res = (currentClassName == classnameExpected);
        }
        retval << ArrayOf::logicalScalar(res);
    }
    return retval;
}
//=============================================================================
