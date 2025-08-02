//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "libpointer_isNullBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "LibPointerObject.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DynamicLinkGateway::libpointer_isNullBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    if (param1.getHandleCategory() != NLS_HANDLE_LIBPOINTER_CATEGORY_STR) {
        Error(_W("libpointer handle expected."));
    }
    LibPointerObject* libPointerObj = (LibPointerObject*)param1.getContentAsHandleScalar();
    retval << ArrayOf::logicalConstructor(libPointerObj->isNull());
    return retval;
}
//=============================================================================
