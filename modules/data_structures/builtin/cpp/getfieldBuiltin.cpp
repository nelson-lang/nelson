//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "getfieldBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "OverloadFunction.hpp"
#include "characters_encoding.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::getfieldBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2, 2);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "getfield", bSuccess);
    }
    if (!bSuccess) {
        ArrayOf param1 = argIn[0];
        ArrayOf param2 = argIn[1];
        if (param1.isClassStruct() || param1.isHandle()) {
            retval = OverloadFunction(eval, nLhs, argIn, "getfield", bSuccess);
            if (bSuccess) {
                return retval;
            }
            OverloadRequired(eval, argIn, Overload::OverloadClass::UNARY, "getfield");
        }
        if (param1.isStruct()) {
            std::wstring fieldname = param2.getContentAsWideString();
            if (param1.isScalar()) {
                retval << param1.getField(wstring_to_utf8(fieldname));
            } else {
                ArrayOfVector rv = param1.getFieldAsList(wstring_to_utf8(fieldname));
                retval << rv[0];
            }
        } else {
            Error(_W("Wrong type for argument #1. struct expected."));
        }
    }
    return retval;
}
//=============================================================================
