//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "jlrunfileBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "JuliaRunFile.hpp"
#include <sstream>
#include <iostream>
#include <iomanip>
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::Julia_engineGateway::jlrunfileBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    if (!eval) {
        Error(_W("Evaluator not available."));
    }
    ArrayOfVector retval = {};
    nargincheck(argIn, 1);
    std::wstring filename = argIn[0].getContentAsWideString();
    wstringVector arguments;

    wstringVector outputs;
    wstringVector names;
    ArrayOfVector values;
    size_t positionNameValue = argIn.size();
    if (argIn.size() == 2) {
        // jlrunfile(file, outputs)
        outputs = argIn[1].getContentAsWideStringVector(false);
        positionNameValue = argIn.size();
    } else if (argIn.size() == 3) {
        // jlrunfile(file, outputs, jlName = jlValue)
        positionNameValue = 1;
    } else if (argIn.size() > 3) {
        if (argIn.size() % 2 == 0) {
            // outvars = jlrunfile(file, outputs, jlName, jlValue)
            // outvars = jlrunfile(file, outputs, jlName, jlValue, jlName, jlValue)
            outputs = argIn[1].getContentAsWideStringVector(false);
            positionNameValue = 2;
        } else {
            // outvars = jlrunfile(file, jlName, jlValue, jlName, jlValue)
            positionNameValue = 1;
        }
    }

    for (size_t k = positionNameValue; k < argIn.size(); k = k + 2) {
        if (argIn[k].isRowVectorCharacterArray() || argIn[k].isScalarStringArray()) {
            names.push_back(argIn[k].getContentAsWideString());
            values.push_back(argIn[k + 1]);
        } else {
            Error(_W("Field names must be string scalars or character vectors."),
                L"Nelson:Juliarunfile:NonStringFieldNames");
        }
    }

    if (nLhs > outputs.size()) {
        Error(_W("Wrong number of output arguments."));
    }

    return JuliaRunFile(
        eval->getInterface(), eval->haveEventsLoop(), filename, arguments, outputs, names, values);
}
//=============================================================================
