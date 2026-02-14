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
#include "i18n.hpp"
#include "MacroArguments.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
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
        wfunctionname = argIn[0].getContentAsWideString();
    } else {
        raiseError2(L"Nelson:error_manager:wrong_type_with_expected", 1, NLS_STRING_ARRAY_STR);
    }
    wstringVector Inputs;
    wstringVector Outputs;
    bool bOK = MacroArguments(eval, wfunctionname, Inputs, Outputs);
    if (bOK) {
        retval << ArrayOf::toCellArrayOfCharacterColumnVectors(Inputs);
        if (nLhs > 1) {
            retval << ArrayOf::toCellArrayOfCharacterColumnVectors(Outputs);
        }
    } else {
        raiseError(L"Nelson:functions_manager:ERROR_FUNCTION_MACRO_NAME_NOT_FOUND",
            ERROR_FUNCTION_MACRO_NAME_NOT_FOUND);
    }
    return retval;
}
//=============================================================================
