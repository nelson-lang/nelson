//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "COM_displayBuiltin.hpp"
#include "DispComHandleObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ComEngineGateway::COM_dispBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
#ifdef _MSC_VER
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 2);
    ArrayOf param1 = argIn[0];
    std::string name;
    Interface* io = nullptr;
    if (eval) {
        io = eval->getInterface();
    }
    DispComHandleObject(io, param1, name);
#else
    Error(_W("Not implemented on this platform."));
#endif
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::ComEngineGateway::COM_displayBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
#ifdef _MSC_VER
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 2);
    ArrayOf param1 = argIn[0];
    std::string name = param1.name();
    if (argIn.size() == 2) {
        name = argIn[1].getContentAsCString();
    }
    Interface* io = nullptr;
    if (eval) {
        io = eval->getInterface();
    }
    DispComHandleObject(io, param1, name);
#else
    Error(_W("Not implemented on this platform."));
#endif
    return retval;
}
//=============================================================================
