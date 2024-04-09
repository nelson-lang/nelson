//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "pyrunBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PyRun.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::Python_engineGateway::pyrunBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    if (!eval) {
        Error(_W("Evaluator not available."));
    }
    ArrayOfVector retval = {};
    nargincheck(argIn, 1, 10000);
    wstringVector commands;
    void* po = nullptr;
    if (argIn[0].isHandle() && argIn[0].getHandleCategory() == NLS_HANDLE_PYOBJECT_CATEGORY_STR) {
        po = argIn[0].getContentAsHandleScalar();
    } else {
        commands = argIn[0].getContentAsWideStringVector(false);
    }
    wstringVector outputs;
    wstringVector names;
    ArrayOfVector values;
    // pyrun(code)
    size_t positionNameValue = argIn.size();
    if (argIn.size() == 2) {
        // outvars = pyrun(code, outputs)
        outputs = argIn[1].getContentAsWideStringVector(false);
        positionNameValue = argIn.size();
    } else if (argIn.size() == 3) {
        // outvars = pyrun(code, pyName, pyValue)
        positionNameValue = 1;
    } else if (argIn.size() > 3) {
        if (argIn.size() % 2 == 0) {
            // outvars = pyrun(code, outputs, pyName, pyValue)
            // outvars = pyrun(code, outputs, pyName, pyValue, pyName, pyValue)
            outputs = argIn[1].getContentAsWideStringVector(false);
            positionNameValue = 2;
        } else {
            // outvars = pyrun(code, pyName, pyValue, pyName, pyValue)
            positionNameValue = 1;
        }
    }

    for (size_t k = positionNameValue; k < argIn.size(); k = k + 2) {
        if (argIn[k].isRowVectorCharacterArray() || argIn[k].isScalarStringArray()) {
            names.push_back(argIn[k].getContentAsWideString());
            values.push_back(argIn[k + 1]);
        } else {
            Error(_W("Field names must be string scalars or character vectors."),
                L"Nelson:Pyrun:NonStringFieldNames");
        }
    }

    if (nLhs > outputs.size()) {
        Error(_W("Wrong number of output arguments."));
    }

    return PyRun(
        eval->getInterface(), eval->haveEventsLoop(), po, commands, outputs, names, values);
}
//=============================================================================
