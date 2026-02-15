//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "calendarBuiltin.hpp"
#include "Calendar.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "NelsonPrint.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::calendarBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    Calendar* cal = nullptr;
    nargoutcheck(nLhs, 0, 2);
    nargincheck(argIn, 0, 2);
    switch (argIn.size()) {
    case 0: {
        cal = new Calendar();
    } break;
    case 1: {
        ArrayOf param1 = argIn[0];
        double d = param1.getContentAsDoubleScalar();
        cal = new Calendar(d);
    } break;
    case 2: {
        ArrayOf param1 = argIn[0];
        uint64 cyear = param1.getContentAsUnsignedInteger64Scalar();
        if (cyear < 1400 || cyear > 9999) {
            raiseError2(L"nelson:validators:invalidValue", 1);
        }
        ArrayOf param2 = argIn[1];
        int32 cmonth = param2.getContentAsInteger32Scalar();
        if (cmonth > 0 && cmonth < 13) {
            cal = new Calendar(cyear, (uint8)cmonth);
        } else {
            raiseError2(L"nelson:validators:invalidValue", 2);
        }
    } break;
    default: {
        raiseError2(L"nelson:arguments:tooFewInputs");
    } break;
    }
    if (cal == nullptr) {
        raiseError(L"Nelson:time:ERROR_CALENDAR_NOT_INITIALIZED", ERROR_CALENDAR_NOT_INITIALIZED);
    }
    if (cal->getYear() < 1400 || cal->getYear() > 9999) {
        delete cal;
        raiseError(L"Nelson:time:ERROR_YEAR_VALUE_OUT_OF_RANGE", ERROR_YEAR_VALUE_OUT_OF_RANGE);
    }
    if (!(cal->getMonth() > 0 && cal->getMonth() < 13)) {
        delete cal;
        raiseError(L"Nelson:time:ERROR_MONTH_VALUE_OUT_OF_RANGE", ERROR_MONTH_VALUE_OUT_OF_RANGE);
    }
    switch (nLhs) {
    case 0: {
        std::wstring msg = cal->getAsFormatedText();
        delete cal;
        NelsonPrint(msg);
    } break;
    case 1: {
        retval << cal->get();
        delete cal;
    } break;
    case 2: {
        retval << cal->get();
        retval << ArrayOf::toCellArrayOfCharacterRowVectors(cal->getNameOfDays());
        delete cal;
    } break;
    default: {
        delete cal;
        raiseError2(L"nelson:arguments:wrongNumberOfOutputs");
    } break;
    }
    return retval;
}
//=============================================================================
