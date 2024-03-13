//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__pyenv__Builtin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include "InputOutputArgumentsCheckers.hpp"
#include "PythonEnvironment.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::Python_engineGateway::__pyenv__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 4);
    nargoutcheck(nLhs, 0, 1);

    PythonEnvironment* pythonEnvironment = PythonEnvironment::getInstance();

    if (argIn.size() == 0) {

    } else if (argIn.size() == 4) {
        pythonEnvironment->setVersion(argIn[0].getContentAsWideString());
        pythonEnvironment->setExecutable(argIn[1].getContentAsWideString());
        pythonEnvironment->setLibrary(argIn[2].getContentAsWideString());
        pythonEnvironment->setHome(argIn[3].getContentAsWideString());
        pythonEnvironment->saveCurrentState();
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }

    ArrayOfVector retval;
    retval << ArrayOf::handleConstructor((HandleGenericObject*)pythonEnvironment);
    return retval;
}
//=============================================================================
