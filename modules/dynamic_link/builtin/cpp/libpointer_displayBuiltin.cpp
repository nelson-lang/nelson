//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "libpointer_displayBuiltin.hpp"
#include "Error.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "LibPointerObject.hpp"
#include "DisplayVariableHelpers.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DynamicLinkGateway::libpointer_displayBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 0);
    ArrayOf param1 = argIn[0];
    std::wstring name;
    if (argIn.size() == 2) {
        name = argIn[1].getContentAsWideString();
    } else {
        name = argIn[0].wname();
    }
    if (param1.isHandle()) {
        Interface* io = eval->getInterface();
        DisplayVariableHeader(io, param1, name, false);
        if (param1.isScalar()) {
            if (param1.getHandleCategory() != NLS_HANDLE_LIBPOINTER_CATEGORY_STR) {
                Error(_W("libpointer handle expected."));
            }
            LibPointerObject* lipPointerObj
                = static_cast<LibPointerObject*>(param1.getContentAsHandleScalar());
            lipPointerObj->disp(io);
        }
        DisplayVariableFooter(io, name.empty());
    } else {
        Error(_W("libpointer handle expected."));
    }
    return retval;
}
//=============================================================================
