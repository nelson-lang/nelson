//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "sioemitBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "SioClientEmit.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SioClientGateway::sioemitBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 0);
    ArrayOf param1 = argIn[0];
    std::string name = param1.getContentAsCString();
    std::string message;
    if (argIn.size() == 2) {
        ArrayOf param2 = argIn[1];
        message = param2.getContentAsCString();
    }
    sioemit(name, message);
    return retval;
}
//=============================================================================
