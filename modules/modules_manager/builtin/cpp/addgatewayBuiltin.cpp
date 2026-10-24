//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "addgatewayBuiltin.hpp"
#include "AddGateway.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool
isGatewayLoadMode(const std::wstring& value)
{
    return value == L"auto" || value == L"loaded";
}
//=============================================================================
ArrayOfVector
Nelson::ModulesManagerGateway::addgatewayBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 3);
    nargoutcheck(nLhs, 0, 0);
    std::wstring moduleName;
    bool forceLoaded = false;
    if (!argIn[0].isRowVectorCharacterArray()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    if (argIn.size() > 1) {
        if (!argIn[1].isRowVectorCharacterArray()) {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2),
                L"string");
        }
        std::wstring secondArgument = argIn[1].asWideString();
        if (isGatewayLoadMode(secondArgument)) {
            forceLoaded = secondArgument == L"loaded";
        } else {
            moduleName = secondArgument;
        }
    }
    if (argIn.size() > 2) {
        if (argIn[2].isLogical() && argIn[2].isScalar()) {
            forceLoaded = argIn[2].asLogicalScalar();
        } else if (argIn[2].isRowVectorCharacterArray()) {
            std::wstring mode = argIn[2].asWideString();
            if (!isGatewayLoadMode(mode)) {
                Error(L"Nelson:addgateway:invalidValue", L"%s expected.",
                    L"Wrong value for argument #3: 'auto' or 'loaded'");
            }
            forceLoaded = mode == L"loaded";
        } else {
            Error(L"Nelson:types:wrongArgumentTypeExpected",
                L"Wrong type for argument #%d: %s expected.", 3, L"string");
        }
    }
    std::wstring dynlibName = argIn[0].asWideString();
    AddGateway(eval, dynlibName, moduleName, forceLoaded);
    return retval;
}
//=============================================================================
