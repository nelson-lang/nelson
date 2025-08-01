//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "JuliaEnvironment_displayBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "DisplayVariableHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "JuliaEnvironment.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::Julia_engineGateway::JuliaEnvironment_displayBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 2);
    ArrayOf param1 = argIn[0];
    if (param1.isHandle()) {
        std::wstring name;
        if (argIn.size() == 2) {
            name = argIn[1].getContentAsWideString();
        }
        Interface* io = eval->getInterface();
        DisplayVariableHeader(io, param1, name, false);
        if (param1.isScalar()) {

            auto* juliaEnvironment = (JuliaEnvironment*)param1.getContentAsHandleScalar();
            juliaEnvironment->display(io);
        }
        DisplayVariableFooter(io, name.empty());
    } else {
        Error(_W("JuliaEnvironment object expected."));
    }
    return retval;
}
//=============================================================================
