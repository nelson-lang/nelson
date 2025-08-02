//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "libpointer_ismethodBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "LibPointerObject.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DynamicLinkGateway::libpointer_ismethodBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    ArrayOfVector retval;
    if (param1.getHandleCategory() != NLS_HANDLE_LIBPOINTER_CATEGORY_STR) {
        Error(_W("libpointer handle expected."));
    }
    ArrayOf param2 = argIn[1];
    std::wstring methodName = param2.getContentAsWideString();
    retval << ArrayOf::logicalConstructor(param1.isHandleMethod(methodName));
    return retval;
}
//=============================================================================
