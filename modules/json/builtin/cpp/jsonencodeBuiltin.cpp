//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "jsonencodeBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "JsonEncode.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::JsonGateway::jsonencodeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    if (!((argIn.size() == 1 || argIn.size() == 3))) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (argIn[0].isHandle() || argIn[0].isClassType()) {
        OverloadRequired("jsonencode");
    }
    bool convertNanInf = false;
    ArrayOf param1 = argIn[0];
    if (argIn.size() == 3) {
        ArrayOf param2 = argIn[1];
        ArrayOf param3 = argIn[2];
        std::wstring fieldname = param2.getContentAsWideString();
        if (fieldname != L"ConvertInfAndNaN") {
            Error(_W("Wrong value for argument #2: 'ConvertInfAndNaN' expected."));
        }
        logical fieldvalue = param3.getContentAsLogicalScalar();
        convertNanInf = (fieldvalue != 0);
    }
    std::wstring errorMessage;
    ArrayOf res = jsonEncode(param1, convertNanInf, errorMessage);
    if (!errorMessage.empty()) {
        Error(errorMessage);
    }
    retval << res;

    return retval;
}
//=============================================================================
