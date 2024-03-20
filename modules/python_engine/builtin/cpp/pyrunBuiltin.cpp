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
    wstringVector commands = argIn[0].getContentAsWideStringVector(false);
    wstringVector outputs;
    wstringVector names;
    ArrayOfVector values;
    // pyrun(code)
    // outvars = pyrun(code, outputs)
    // outvars = pyrun(code, outputs, pyName, pyValue)
    // outvars = pyrun(code, pyName, pyValue)
    // outvars = pyrun(code, outputs, pyName, pyValue, pyName, pyValue)
    // outvars = pyrun(code, pyName, pyValue, pyName, pyValue)
    size_t positionNameValue = argIn.size();
    if (argIn.size() == 2) {
        outputs = argIn[1].getContentAsWideStringVector(false);
        positionNameValue = argIn.size();
    } else if (argIn.size() == 3) {
        positionNameValue = 1;
    } else if (argIn.size() > 3) {
        if (argIn.size() % 2) {
            outputs = argIn[1].getContentAsWideStringVector(false);
            positionNameValue = 2;
        } else {
            positionNameValue = 1;
        }
    }
    for (size_t k = positionNameValue; k < argIn.size(); k = k + 2) {
        names.push_back(argIn[k].getContentAsWideString());
        values.push_back(argIn[k + 1]);
    }

    if (nLhs > outputs.size()) {
        Error(_W("Wrong number of output arguments."));
    }

    return PyRun(eval->getInterface(), eval->haveEventsLoop(), commands, outputs, names, values);
}
//=============================================================================
