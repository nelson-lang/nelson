//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "macroargsBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "MacroArguments.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::macroargsBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 2);
    nargincheck(argIn, 1, 1);
    std::wstring wfunctionname;
    if (argIn[0].isRowVectorCharacterArray()) {
        wfunctionname = argIn[0].asWideString();
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    wstringVector Inputs;
    wstringVector Outputs;
    bool bOK = MacroArguments(eval, wfunctionname, Inputs, Outputs);
    if (bOK) {
        retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(Inputs);
        if (nLhs > 1) {
            retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(Outputs);
        }
    } else {
        Error(L"Nelson:macroargs:functionNotFound", L"function macro name not found.");
    }
    return retval;
}
//=============================================================================
