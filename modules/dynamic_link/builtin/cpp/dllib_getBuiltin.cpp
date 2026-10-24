//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "dllib_getBuiltin.hpp"
#include "DynamicLinkLibraryObject.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DynamicLinkGateway::dllib_getBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    std::wstring propertyName = param2.asWideString();
    ArrayOfVector retval;
    if (param1.handleCategory() != NLS_HANDLE_DLLIB_CATEGORY_STR) {
        Error(L"Nelson:dynamic_link:dllibHandle", L"dllib handle expected.");
    }
    auto* objDllib = (DynamicLinkLibraryObject*)param1.asHandleScalar();
    ArrayOf res;
    if (!objDllib->get(propertyName, res)) {
        Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.", (2));
    }
    retval << res;
    return retval;
}
//=============================================================================
