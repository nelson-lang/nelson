//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "dlsym_getBuiltin.hpp"
#include "DynamicLinkSymbolObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DynamicLinkGateway::dlsym_getBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    std::wstring propertyName = param2.getContentAsWideString();
    ArrayOfVector retval;
    if (param1.getHandleCategory() != NLS_HANDLE_DLSYM_CATEGORY_STR) {
        raiseError(L"Nelson:dynamic_link:ERROR_DLSYM_HANDLE_EXPECTED", ERROR_DLSYM_HANDLE_EXPECTED);
    }
    auto* objDlsym = static_cast<DynamicLinkSymbolObject*>(param1.getContentAsHandleScalar());
    ArrayOf res;
    if (!objDlsym->get(propertyName, res)) {
        raiseError2(L"nelson:validators:invalidValue", 2);
    }
    retval << res;
    return retval;
}
//=============================================================================
