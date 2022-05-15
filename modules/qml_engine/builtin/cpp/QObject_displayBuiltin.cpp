//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QObject_displayBuiltin.hpp"
#include "DispQObjectHandleObject.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::QmlEngineGateway::QObject_displayBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 0);
    ArrayOf param1 = argIn[0];
    if (eval == nullptr) {
        return retval;
    }
    Interface* io = eval->getInterface();
    if (io == nullptr) {
        return retval;
    }
    std::string name;
    if (argIn.size() == 2) {
        name = argIn[1].getContentAsCString();
    }
    DispQObjectHandleObject(io, param1, name);
    return retval;
}
//=============================================================================
