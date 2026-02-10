//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "jlrunBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "JuliaRun.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::Julia_engineGateway::jlrunBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    if (!eval) {
        raiseError(
            L"Nelson:julia_engine:ERROR_EVALUATOR_NOT_AVAILABLE", ERROR_EVALUATOR_NOT_AVAILABLE);
    }

    wstringVector commands = argIn[0].getContentAsWideStringVector();
    wstringVector outputs;
    wstringVector names;
    ArrayOfVector values;

    // jlrun(code)
    size_t positionNameValue = argIn.size();
    if (argIn.size() == 2) {
        // outvars = jlrun(code, outputs)
        outputs = argIn[1].getContentAsWideStringVector(false);
        positionNameValue = argIn.size();
    } else if (argIn.size() == 3) {
        // outvars = jlrun(code, jlName, jlValue)
        positionNameValue = 1;
    } else if (argIn.size() > 3) {
        if (argIn.size() % 2 == 0) {
            // outvars = jlrun(code, outputs, jlName, jlValue)
            // outvars = jlrun(code, outputs, jlName, jlValue, jlName, jlValue)
            outputs = argIn[1].getContentAsWideStringVector(false);
            positionNameValue = 2;
        } else {
            // outvars = jlrun(code, jlName, jlValue, jlName, jlValue)
            positionNameValue = 1;
        }
    }

    for (size_t k = positionNameValue; k < argIn.size(); k = k + 2) {
        if (argIn[k].isRowVectorCharacterArray() || argIn[k].isScalarStringArray()) {
            names.push_back(argIn[k].getContentAsWideString());
            values.push_back(argIn[k + 1]);
        } else {
            raiseError(L"Nelson:julia_engine:ERROR_FIELD_NAMES_MUST_BE_STRING_SCALARS_OR_CHARACTER_"
                       L"VECTORS",
                ERROR_FIELD_NAMES_MUST_BE_STRING_SCALARS_OR_CHARACTER_VECTORS);
        }
    }

    if ((size_t)nLhs > outputs.size()) {
        raiseError(L"Nelson:julia_engine:ERROR_WRONG_NUMBERS_OUTPUT_ARGS",
            ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOfVector res = JuliaRun(
        eval->getInterface(), eval->haveEventsLoop(), nullptr, commands, outputs, names, values);
    return res;
}
//=============================================================================
