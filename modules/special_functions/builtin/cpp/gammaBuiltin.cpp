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
#include "gammaBuiltin.hpp"
#include "FunctionsInMemory.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Gamma.hpp"
#include "OverloadFunction.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ClassToString.hpp"
#include "ClassName.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SpecialFunctionsGateway::gammaBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    NelsonType destinationType = argIn[0].getDataClass();
    std::string destinationTypeName;
    if (destinationType >= NLS_STRUCT_ARRAY) {
        destinationTypeName = ClassName(argIn[0]);
    } else {
        destinationTypeName = ClassToString(destinationType);
    }

    FunctionDef* funcDef = nullptr;
    std::string overloadTypeName = overloadFunctionName(destinationTypeName, "gamma");
    if (!FunctionsInMemory::getInstance()->find(overloadTypeName, funcDef,
            eval->isOverloadAllowed() ? FunctionsInMemory::ALL : FunctionsInMemory::BUILTIN)) {
        Context* context = eval->getContext();
        context->lookupFunction(overloadTypeName, funcDef, !eval->isOverloadAllowed());
    }
    if (!funcDef) {
        std::wstring msgfmt = _W(
            "Check for incorrect argument data type or missing argument in call to function '%s'.");
        std::wstring msg = fmt::sprintf(msgfmt, L"gamma");
        Error(msg, L"Nelson:UndefinedFunction");
    }
    return funcDef->evaluateFunction(eval, argIn, nLhs);
}
//=============================================================================
ArrayOfVector
Nelson::SpecialFunctionsGateway::generic_gammaBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn[0].isSparse() || argIn[0].getDataClass() == NLS_DCOMPLEX
        || argIn[0].getDataClass() == NLS_SCOMPLEX) {
        Error(_W("Input argument must be dense and real."), L"Nelson:gamma:notFullReal");
    }
    retval << Gamma(argIn[0]);
    return retval;
}
//=============================================================================
