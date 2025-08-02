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
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
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
    std::wstring propertyName = param2.getContentAsWideString();
    ArrayOfVector retval;
    if (param1.getHandleCategory() != NLS_HANDLE_DLLIB_CATEGORY_STR) {
        Error(_W("dllib handle expected."));
    }
    auto* objDllib = (DynamicLinkLibraryObject*)param1.getContentAsHandleScalar();
    ArrayOf res;
    if (!objDllib->get(propertyName, res)) {
        Error(ERROR_WRONG_ARGUMENT_2_VALUE);
    }
    retval << res;
    return retval;
}
//=============================================================================
