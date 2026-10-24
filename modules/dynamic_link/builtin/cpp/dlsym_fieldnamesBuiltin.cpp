//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "dlsym_fieldnamesBuiltin.hpp"
#include "DynamicLinkSymbolObject.hpp"
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
Nelson::DynamicLinkGateway::dlsym_fieldnamesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    ArrayOf param1 = argIn[0];
    if (param1.handleCategory() != NLS_HANDLE_DLSYM_CATEGORY_STR) {
        Error(L"Nelson:dynamic_link:dlsymHandle", L"dlsym handle expected.");
    }
    auto* objDlsym = static_cast<DynamicLinkSymbolObject*>(param1.asHandleScalar());
    retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(objDlsym->fieldnames());
    return retval;
}
//=============================================================================
