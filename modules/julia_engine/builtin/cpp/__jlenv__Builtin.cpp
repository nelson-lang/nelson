//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__jlenv__Builtin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include "InputOutputArgumentsCheckers.hpp"
#include "JuliaEnvironment.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::Julia_engineGateway::__jlenv__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 4);
    nargoutcheck(nLhs, 0, 1);

    JuliaEnvironment* juliaEnvironment = JuliaEnvironment::getInstance();

    if (argIn.size() == 0) {
        // Do nothing when no arguments are provided
    } else if (argIn.size() == 4) {
        juliaEnvironment->setVersion(argIn[0].getContentAsWideString());
        juliaEnvironment->setExecutable(argIn[1].getContentAsWideString());
        juliaEnvironment->setLibrary(argIn[2].getContentAsWideString());
        juliaEnvironment->setHome(argIn[3].getContentAsWideString());
        juliaEnvironment->saveCurrentState();
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }

    ArrayOfVector retval;
    retval << ArrayOf::handleConstructor((HandleGenericObject*)juliaEnvironment);
    return retval;
}
//=============================================================================
