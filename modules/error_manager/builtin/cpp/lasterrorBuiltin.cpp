//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "lasterrorBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "ErrorToStruct.hpp"
#include "IsErrorStruct.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ErrorManagerGateway::lasterrorBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval(nLhs);
    nargincheck(argIn, 0, 1);
    if (argIn.size() == 1) {
        ArrayOf arg1 = argIn[0];
        if (arg1.isRowVectorCharacterArray()) {
            std::wstring str = arg1.asWideString();
            if (str == L"reset") {
                eval->setLastErrorException(NelsonException());
            } else {
                RaiseError(formatI18nMessage(Nelson::resolveMessageTemplate(
                                                 L"Nelson:error_manager:wrongValueForArgument",
                                                 L"Wrong value for #%d argument."),
                               2)
                        + formatI18nMessage(
                            Nelson::resolveMessageTemplate(
                                L"Nelson:error_manager:expected", L"'%s' expected."),
                            L"reset"),
                    L"Nelson:lasterror:invalidValue");
            }
        } else if (arg1.isStruct()) {
            NelsonException e;
            if (IsErrorStruct(arg1, e)) {
                eval->setLastErrorException(e);
            } else {
                Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.", (2));
            }
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"structure or the string 'reset'");
        }
    }
    NelsonException lasterror = eval->getLastErrorException();
    retval << ErrorToStruct(lasterror);
    return retval;
}
//=============================================================================
