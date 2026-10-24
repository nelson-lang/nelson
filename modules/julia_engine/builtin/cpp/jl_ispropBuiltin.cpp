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
#include "ErrorID.hpp"
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
    if (param1.handleCategory() != NLS_HANDLE_JULIA_CATEGORY_STR) {
        Error(L"Nelson:Julia:invalidObject", L"Julia object expected.");
    }
    ArrayOf param2 = argIn[1];
    std::wstring propertyName = param2.asWideString();
    retval << ArrayOf::logicalScalar(param1.isHandleProperty(propertyName));
    return retval;
}
//=============================================================================
