//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "dllib_ismethodBuiltin.hpp"
#include "DynamicLinkLibraryObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DynamicLinkGateway::dllib_ismethodBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    ArrayOf param1 = argIn[0];
    if (param1.getHandleCategory() != NLS_HANDLE_DLLIB_CATEGORY_STR) {
        Error(_W("dllib handle expected."));
    }
    ArrayOf param2 = argIn[1];
    std::wstring methodName = param2.getContentAsWideString();
    auto* objDllib = (DynamicLinkLibraryObject*)param1.getContentAsHandleScalar();
    retval << ArrayOf::logicalConstructor(objDllib->isMethod(methodName));
    return retval;
}
//=============================================================================
