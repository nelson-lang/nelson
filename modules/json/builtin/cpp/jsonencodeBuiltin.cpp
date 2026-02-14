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
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::JsonGateway::jsonencodeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    if (!((argIn.size() == 1 || argIn.size() == 3))) {
        raiseError2(L"Nelson:error_manager:wrong_rhs");
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
            raiseError(L"Nelson:json:ERROR_WRONG_VALUE_FOR_ARGUMENT_2_CONVERTINFANDNAN_EXPECTED",
                ERROR_WRONG_VALUE_FOR_ARGUMENT_2_CONVERTINFANDNAN_EXPECTED);
        }
        logical fieldvalue = param3.getContentAsLogicalScalar();
        convertNanInf = (fieldvalue != 0);
    }
    std::wstring errorMessage;
    ArrayOf res = jsonEncode(param1, convertNanInf, errorMessage);
    if (!errorMessage.empty()) {
        raiseError(L"Nelson:json:ERROR_CANNOT_ENCODE_JSON", ERROR_CANNOT_ENCODE_JSON, errorMessage);
    }
    retval << res;

    return retval;
}
//=============================================================================
