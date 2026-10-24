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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "JsonEncode.hpp"
#include "JsonPrettyPrint.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::JsonGateway::jsonencodeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    if (argIn.size() < 1) {
        Error(L"Nelson:notEnoughInputs", L"Not enough input arguments.");
    }
    if ((argIn.size() % 2) != 1) {
        Error(L"Nelson:wrongNumberOfInputs", L"Wrong number of input arguments.");
    }
    if (argIn[0].isHandle() || argIn[0].isClassType()) {
        OverloadRequired("jsonencode");
    }
    // The flag keeps Inf/NaN as literals when true. The ConvertInfAndNaN option is the
    // opposite and defaults to converting, so the flag starts false.
    bool keepInfAndNaN = false;
    bool prettyPrint = false;
    ArrayOf param1 = argIn[0];
    for (size_t k = 1; k + 1 < argIn.size(); k += 2) {
        std::wstring fieldname = argIn[k].asWideString();
        logical fieldvalue = argIn[k + 1].asLogicalScalar();
        if (fieldname == L"ConvertInfAndNaN") {
            keepInfAndNaN = (fieldvalue == 0);
        } else if (fieldname == L"PrettyPrint") {
            prettyPrint = (fieldvalue != 0);
        } else {
            Error(L"Nelson:jsonencode:invalidOption", L"%s expected.",
                L"Wrong value for argument: 'ConvertInfAndNaN' or 'PrettyPrint'");
        }
    }
    std::wstring errorMessage;
    ArrayOf res = jsonEncode(param1, keepInfAndNaN, errorMessage);
    if (!errorMessage.empty()) {
        Error(L"Nelson:jsonencode:encodeFailed", errorMessage);
    }
    if (prettyPrint) {
        res = ArrayOf::characterArray(jsonPrettyPrintForEncode(res.asWideString()));
    }
    retval << res;

    return retval;
}
//=============================================================================
