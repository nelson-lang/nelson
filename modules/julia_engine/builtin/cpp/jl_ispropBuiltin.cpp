//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "jl_ispropBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::Julia_engineGateway::jl_ispropBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    ArrayOf param1 = argIn[0];
    if (param1.getHandleCategory() != NLS_HANDLE_JULIA_CATEGORY_STR) {
        Error(_W("Julia object expected."));
    }
    ArrayOf param2 = argIn[1];
    std::wstring propertyName = param2.getContentAsWideString();
    retval << ArrayOf::logicalConstructor(param1.isHandleProperty(propertyName));
    return retval;
}
//=============================================================================
