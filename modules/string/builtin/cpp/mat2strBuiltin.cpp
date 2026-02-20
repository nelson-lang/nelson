//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "mat2strBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "MatrixToString.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::mat2strBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 3);
    indexType defautPrecision = 15;
    bool withClass = false;
    if (argIn.size() == 3) {
        ArrayOf param3 = argIn[2];
        ArrayOf param2 = argIn[1];
        if (param3.isRowVectorCharacterArray()) {
            std::wstring str = param3.getContentAsWideString();
            if (str == L"class") {
                withClass = true;
            } else {
                raiseError(
                    L"Nelson:string:ERROR_CLASS_ARGUMENT_EXPECTED", ERROR_CLASS_ARGUMENT_EXPECTED);
            }
        }
        if (param2.isNumeric()) {
            defautPrecision = param2.getContentAsScalarIndex();
        } else {
            raiseError(L"Nelson:string:ERROR_SECOND_INPUT_ARGUMENT_MUST_BE_REAL_POSITIVE_INTEGER",
                ERROR_SECOND_INPUT_ARGUMENT_MUST_BE_REAL_POSITIVE_INTEGER);
        }
    } else if (argIn.size() == 2) {
        ArrayOf param2 = argIn[1];
        if (param2.isRowVectorCharacterArray()) {
            std::wstring str = param2.getContentAsWideString();
            if (str == L"class") {
                defautPrecision = 15;
                withClass = true;
            } else {
                raiseError(
                    L"Nelson:string:ERROR_CLASS_ARGUMENT_EXPECTED", ERROR_CLASS_ARGUMENT_EXPECTED);
            }
        } else if (param2.isNumeric()) {
            defautPrecision = param2.getContentAsScalarIndex();
            withClass = false;
        } else {
            raiseError(L"Nelson:string:ERROR_SECOND_INPUT_ARGUMENT_MUST_BE_REAL_POSITIVE_INTEGER",
                ERROR_SECOND_INPUT_ARGUMENT_MUST_BE_REAL_POSITIVE_INTEGER);
        }
    } else {
        defautPrecision = 15;
        withClass = false;
    }
    ArrayOf A = argIn[0];
    bool canBeConvert = A.isNumeric() || A.isLogical() || A.isCharacterArray();
    if (!canBeConvert) {
        raiseError2(_E("nelson:validators:mustBeNumeric"));
    }
    if (A.isSparse()) {
        raiseError2(_E("nelson:runtime:typeNotSupported"));
    }
    std::wstring res = MatrixToString(A, defautPrecision, withClass);
    retval << ArrayOf::characterArrayConstructor(res);
    return retval;
}
//=============================================================================
