//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "JuliaEnvironment_setBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "JuliaEnvironment.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::Julia_engineGateway::JuliaEnvironment_setBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, 3);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];

    if (param1.getHandleCategory() != NLS_HANDLE_JULIA_ENVIRONMENT_CATEGORY_STR) {
        Error(_W("JuliaEnvironment object expected."));
    }

    std::wstring msg = _W("Unable to set property of class 'JuliaEnvironment' it is read-only.");
    Error(msg, L"Nelson:class:SetProhibited");

    return {};
}
//=============================================================================
