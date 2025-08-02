//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "libpointer_setdatatypeBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "LibPointerObject.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DynamicLinkGateway::libpointer_setdatatypeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    if (param1.getHandleCategory() != NLS_HANDLE_LIBPOINTER_CATEGORY_STR) {
        Error(_W("libpointer handle expected."));
    }
    auto* objLibPointer = (LibPointerObject*)param1.getContentAsHandleScalar();
    ArrayOf param2 = argIn[1];
    std::wstring newDatatype = param2.getContentAsWideString();
    objLibPointer->setDataType(newDatatype);
    return retval;
}
//=============================================================================
