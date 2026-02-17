//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "libpointer_displayBuiltin.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
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
Nelson::DynamicLinkGateway::libpointer_dispBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    ArrayOf param1 = argIn[0];
    if (param1.isHandle()) {
        std::wstring name;
        Interface* io = eval->getInterface();
        DisplayVariableHeader(io, param1, name, false);
        if (param1.isScalar()) {
            if (param1.getHandleCategory() != NLS_HANDLE_LIBPOINTER_CATEGORY_STR) {
                raiseError2(L"nelson:arguments:libpointerHandleExpected");
            }
            LibPointerObject* lipPointerObj = (LibPointerObject*)param1.getContentAsHandleScalar();
            lipPointerObj->disp(io);
        }
        DisplayVariableFooter(io, name.empty());
    } else {
        raiseError2(L"nelson:arguments:libpointerHandleExpected");
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::DynamicLinkGateway::libpointer_displayBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 0);
    ArrayOf param1 = argIn[0];
    if (param1.isHandle()) {
        std::wstring name = argIn[0].wname();
        if (argIn.size() == 2) {
            name = argIn[1].getContentAsWideString();
        }
        Interface* io = eval->getInterface();
        DisplayVariableHeader(io, param1, name, false);
        if (param1.isScalar()) {
            if (param1.getHandleCategory() != NLS_HANDLE_LIBPOINTER_CATEGORY_STR) {
                raiseError2(L"nelson:arguments:libpointerHandleExpected");
            }
            LibPointerObject* lipPointerObj = (LibPointerObject*)param1.getContentAsHandleScalar();
            lipPointerObj->disp(io);
        }
        DisplayVariableFooter(io, name.empty());
    } else {
        raiseError2(L"nelson:arguments:libpointerHandleExpected");
    }
    return retval;
}
//=============================================================================
