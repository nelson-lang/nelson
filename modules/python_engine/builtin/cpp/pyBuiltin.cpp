//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "pyBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PythonObjectHandle.hpp"
#include "PythonEngine.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::Python_engineGateway::pyBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);

    initializePythonEngine();

    PythonObjectHandle* pythonObjectHandle = new PythonObjectHandle(getPythonInterpreter());
    retval << ArrayOf::handleConstructor(pythonObjectHandle);

    return retval;
}
//=============================================================================
