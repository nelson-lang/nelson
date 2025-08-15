//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "jsondecodeBuiltin.hpp"
#include "Error.hpp"
#include "JsonDecode.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::JsonGateway::jsondecodeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 2);
    bool usingFile = false;

    if (argIn[0].isSparse() || argIn[0].isCell() || argIn[0].isHandle() || argIn[0].isStruct()
        || argIn[0].isClassType()) {
        OverloadRequired("jsondecode");
    }
    if (argIn.size() > 1) {
        argIn[1].getContentAsWideString() == L"-file" ? usingFile = true : usingFile = false;
    }

    ArrayOf param1 = argIn[0];
    std::string errorMessage;
    ArrayOf res;
    if (usingFile) {
        std::wstring filename = param1.getContentAsWideString();
        res = jsonDecodeFile(filename, errorMessage);
    } else {
        std::string jsonString = param1.getContentAsCString();
        res = jsonDecode(jsonString, errorMessage);
    }

    if (!errorMessage.empty()) {
        Error(errorMessage);
    }
    retval << res;
    return retval;
}
//=============================================================================
