//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "betaincBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "BetaIncomplete.hpp"
#include "OverloadFunction.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ClassToString.hpp"
#include "ClassName.hpp"
#include "FunctionsInMemory.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SpecialFunctionsGateway::betaincBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 4);
    nargoutcheck(nLhs, 0, 1);
    NelsonType destinationType = argIn[0].getDataClass();
    std::string destinationTypeName;
    if (destinationType >= NLS_STRUCT_ARRAY) {
        destinationTypeName = ClassName(argIn[0]);
    } else {
        destinationTypeName = ClassToString(destinationType);
    }

    FunctionDef* funcDef = nullptr;
    std::string overloadTypeName = overloadFunctionName(destinationTypeName, "betainc");
    if (!FunctionsInMemory::getInstance()->find(overloadTypeName, funcDef,
            eval->isOverloadAllowed() ? FunctionsInMemory::ALL : FunctionsInMemory::BUILTIN)) {
        Context* context = eval->getContext();
        context->lookupFunction(overloadTypeName, funcDef, !eval->isOverloadAllowed());
    }
    if (!funcDef) {
        std::wstring msgfmt = _W(
            "Check for incorrect argument data type or missing argument in call to function '%s'.");
        std::wstring msg = fmt::sprintf(msgfmt, L"betainc");
        Error(msg, L"Nelson:UndefinedFunction");
    }
    return funcDef->evaluateFunction(eval, argIn, nLhs);
}
//=============================================================================
ArrayOfVector
Nelson::SpecialFunctionsGateway::generic_betaincBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn[0].isSparse() || argIn[0].getDataClass() == NLS_DCOMPLEX
        || argIn[0].getDataClass() == NLS_SCOMPLEX) {
        Error(_W("Input argument must be dense and real."), L"Nelson:betainc:notFullReal");
    }
    bool isLower = true;
    if (argIn.size() == 4) {
        std::wstring tail = argIn[3].getContentAsWideString();
        if (tail.compare(L"upper") == 0 || tail.compare(L"lower") == 0) {
            if (tail.compare(L"upper") == 0) {
                isLower = false;
            } else {
                isLower = true;
            }
        } else {
            Error(_("Wrong value of the fourth argument 'upper' or 'lower' expected."));
        }
    }
    retval << BetaIncomplete(argIn[0], argIn[1], argIn[2], isLower);
    return retval;
}
//=============================================================================
