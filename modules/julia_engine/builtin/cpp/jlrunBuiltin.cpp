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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "JuliaRun.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::Julia_engineGateway::jlrunBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    if (!eval) {
        Error(L"Nelson:julia:evaluatorNotAvailable", L"Evaluator not available.");
    }

    wstringVector commands = argIn[0].asWideStringVector();
    wstringVector outputs;
    wstringVector names;
    ArrayOfVector values;

    // jlrun(code)
    size_t positionNameValue = argIn.size();
    if (argIn.size() == 2) {
        // outvars = jlrun(code, outputs)
        outputs = argIn[1].asWideStringVector(false);
        positionNameValue = argIn.size();
    } else if (argIn.size() == 3) {
        // outvars = jlrun(code, jlName, jlValue)
        positionNameValue = 1;
    } else if (argIn.size() > 3) {
        if (argIn.size() % 2 == 0) {
            // outvars = jlrun(code, outputs, jlName, jlValue)
            // outvars = jlrun(code, outputs, jlName, jlValue, jlName, jlValue)
            outputs = argIn[1].asWideStringVector(false);
            positionNameValue = 2;
        } else {
            // outvars = jlrun(code, jlName, jlValue, jlName, jlValue)
            positionNameValue = 1;
        }
    }

    for (size_t k = positionNameValue; k < argIn.size(); k = k + 2) {
        if (argIn[k].isRowVectorCharacterArray() || argIn[k].isScalarStringArray()) {
            names.push_back(argIn[k].asWideString());
            values.push_back(argIn[k + 1]);
        } else {
            Error(L"Nelson:julia_engine:NonStringFieldNames",
                L"Field names must be string scalars or character vectors.");
        }
    }

    if ((size_t)nLhs > outputs.size()) {
        Error(L"Nelson:tooManyOutputs", L"Too many output arguments.");
    }
    ArrayOfVector res = JuliaRun(
        eval->getInterface(), eval->haveEventsLoop(), nullptr, commands, outputs, names, values);
    return res;
}
//=============================================================================
