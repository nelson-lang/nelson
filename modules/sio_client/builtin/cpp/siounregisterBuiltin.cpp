//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "siounregisterBuiltin.hpp"
#include "Error.hpp"
#include "SioClientRegister.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SioClientGateway::siounregisterBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    ArrayOf param1 = argIn[0];
    std::string name = param1.getContentAsCString();
    if (issioreserved(name)) {
        Error("Impossible to unregister an reserved event name.");
    }
    if (issioregistered(name)) {
        siounregister(name);
    }
    return retval;
}
//=============================================================================
