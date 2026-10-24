//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "PythonEnvironment_structBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PythonEnvironment.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::Python_engineGateway::PythonEnvironment_structBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];

    if (param1.handleCategory() != NLS_HANDLE_PYTHON_ENVIRONMENT_CATEGORY_STR) {
        Error(L"Nelson:python:invalidEnvironment", L"PythonEnvironment object expected.");
    }

    if (!param1.isScalar()) {
        Error(L"Nelson:python:scalarExpected", L"Wrong size for argument #1. scalar expected");
    }

    PythonEnvironment* pythonEnvironment = PythonEnvironment::getInstance();
    ArrayOfVector retval;

    Dimensions dimsRes(1, 1);
    stringVector fieldnames;
    fieldnames.reserve(8);
    fieldnames.push_back("Version");
    fieldnames.push_back("Executable");
    fieldnames.push_back("Library");
    fieldnames.push_back("Home");
    fieldnames.push_back("Status");
    fieldnames.push_back("ExecutionMode");

    ArrayOf* elementRes = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dimsRes.elementCount(), fieldnames, false));
    ArrayOf res = ArrayOf(NLS_STRUCT_ARRAY, dimsRes, elementRes, false, fieldnames);
    ArrayOfVector versionField(ArrayOf::stringArray(pythonEnvironment->getVersion()));
    res.setFieldAsList(fieldnames[0], versionField);
    ArrayOfVector executableField(ArrayOf::stringArray(pythonEnvironment->getExecutable()));
    res.setFieldAsList(fieldnames[1], executableField);
    ArrayOfVector libraryField(ArrayOf::stringArray(pythonEnvironment->getLibrary()));
    res.setFieldAsList(fieldnames[2], libraryField);
    ArrayOfVector homeField(ArrayOf::stringArray(pythonEnvironment->getHome()));
    res.setFieldAsList(fieldnames[3], homeField);
    ArrayOfVector statusField(ArrayOf::doubleScalar(pythonEnvironment->getStatus()));
    res.setFieldAsList(fieldnames[4], statusField);
    ArrayOfVector executionModeField(ArrayOf::doubleScalar(pythonEnvironment->getExecutionMode()));
    res.setFieldAsList(fieldnames[5], executionModeField);
    retval << res;

    return retval;
}
//=============================================================================
