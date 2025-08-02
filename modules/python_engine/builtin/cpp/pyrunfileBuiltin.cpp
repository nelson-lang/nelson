//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "pyrunfileBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PyRunFile.hpp"
#include <sstream>
#include <iostream>
#include <iomanip>
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::Python_engineGateway::pyrunfileBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    if (!eval) {
        Error(_W("Evaluator not available."));
    }
    ArrayOfVector retval = {};
    nargincheck(argIn, 1);
    std::wstring command = argIn[0].getContentAsWideString();
    std::wstring filename;
    wstringVector arguments;
    std::wistringstream iss(command);
    std::wstring token;

    const wchar_t delim { '\'' };
    while (iss >> std::quoted(token, delim)) {
        if (filename.empty()) {
            filename = token;
        } else {
            if (token.size() > 0 && (token[0] == L'\'' || token[0] == L'"')) {
                token.erase(0, 1);
            }
            if (token.size() > 0
                && (token[token.size() - 1] == L'\'' || token[token.size() - 1] == L'"')) {
                token.pop_back();
            }
            arguments.push_back(token);
        }
    }

    wstringVector outputs;
    wstringVector names;
    ArrayOfVector values;
    size_t positionNameValue = argIn.size();
    if (argIn.size() == 2) {
        // pyrunfile(file, outputs)
        outputs = argIn[1].getContentAsWideStringVector(false);
        positionNameValue = argIn.size();
    } else if (argIn.size() == 3) {
        // pyrunfile(file, outputs, pyName = pyValue)
        positionNameValue = 1;
    } else if (argIn.size() > 3) {
        if (argIn.size() % 2 == 0) {
            // outvars = pyrunfile(file, outputs, pyName, pyValue)
            // outvars = pyrunfile(file, outputs, pyName, pyValue, pyName, pyValue)
            outputs = argIn[1].getContentAsWideStringVector(false);
            positionNameValue = 2;
        } else {
            // outvars = pyrunfie(file, pyName, pyValue, pyName, pyValue)
            positionNameValue = 1;
        }
    }

    for (size_t k = positionNameValue; k < argIn.size(); k = k + 2) {
        if (argIn[k].isRowVectorCharacterArray() || argIn[k].isScalarStringArray()) {
            names.push_back(argIn[k].getContentAsWideString());
            values.push_back(argIn[k + 1]);
        } else {
            Error(_W("Field names must be string scalars or character vectors."),
                L"Nelson:Pyrunfile:NonStringFieldNames");
        }
    }

    if (nLhs > outputs.size()) {
        Error(_W("Wrong number of output arguments."));
    }

    return PyRunFile(
        eval->getInterface(), eval->haveEventsLoop(), filename, arguments, outputs, names, values);
}
//=============================================================================
