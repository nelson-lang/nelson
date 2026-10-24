//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "pyfunctionBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PythonEngine.hpp"
#include "PyNelsonCallback.hpp"
#include "PythonObjectHandle.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// c = pyfunction(fun_handle)
//
// Wraps a Nelson function handle into a Python callable object so Python code
// can call it back synchronously into the same (single-threaded) Nelson session.
//=============================================================================
ArrayOfVector
Nelson::Python_engineGateway::pyfunctionBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);

    const ArrayOf& arg = argIn[0];
    if (!arg.isFunctionHandle()) {
        Error(L"Nelson:pyfunction:functionHandleExpected", L"%s must be a function handle.",
            L"Value");
    }

    initializePythonEngine();

    void* callable = createNelsonCallable(arg);
    if (callable == nullptr) {
        Error(L"Nelson:pyfunction:creationFailed",
            L"Unable to create the Python callable from the function handle.");
    }

    PythonObjectHandle* pythonObjectHandle = new PythonObjectHandle(callable);
    retval << ArrayOf::handle(pythonObjectHandle);
    return retval;
}
//=============================================================================
