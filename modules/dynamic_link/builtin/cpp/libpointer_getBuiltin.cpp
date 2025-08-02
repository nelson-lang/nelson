//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "libpointer_getBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "LibPointerObject.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DynamicLinkGateway::libpointer_getBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    ArrayOfVector retval;
    if (param1.getHandleCategory() != NLS_HANDLE_LIBPOINTER_CATEGORY_STR) {
        Error(_W("libpointer handle expected."));
    }
    LibPointerObject* objLibPointer = (LibPointerObject*)param1.getContentAsHandleScalar();
    ArrayOf res;
    if (argIn.size() == 1) {
        objLibPointer->get(res);
    } else {
        ArrayOf param2 = argIn[1];
        std::wstring propertyName = param2.getContentAsWideString();
        if (!objLibPointer->get(propertyName, res)) {
            Error(ERROR_WRONG_ARGUMENT_2_VALUE);
        }
    }
    retval << res;
    return retval;
}
//=============================================================================
