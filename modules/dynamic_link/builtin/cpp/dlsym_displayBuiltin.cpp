//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "dlsym_displayBuiltin.hpp"
#include "DynamicLinkSymbolObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "DisplayVariableHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DynamicLinkGateway::dlsym_dispBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);

    ArrayOf param1 = argIn[0];
    if (param1.isHandle()) {
        Interface* io = eval->getInterface();
        std::wstring name;
        DisplayVariableHeader(io, param1, name, false);
        if (param1.isScalar()) {
            if (param1.getHandleCategory() != NLS_HANDLE_DLSYM_CATEGORY_STR) {
                Error(_W("dlsym handle expected."));
            }
            auto* dlsymObj = (DynamicLinkSymbolObject*)param1.getContentAsHandleScalar();
            dlsymObj->disp(io);
        }
        DisplayVariableFooter(io, name.empty());
    } else {
        Error(_W("dlsym handle expected."));
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::DynamicLinkGateway::dlsym_displayBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 0);
    ArrayOf param1 = argIn[0];
    if (param1.isHandle()) {
        std::wstring name;
        if (argIn.size() == 2) {
            name = argIn[1].getContentAsWideString();
        } else {
            name = argIn[0].wname();
        }
        Interface* io = eval->getInterface();
        DisplayVariableHeader(io, param1, name, false);
        if (param1.isScalar()) {
            if (param1.getHandleCategory() != NLS_HANDLE_DLSYM_CATEGORY_STR) {
                Error(_W("dlsym handle expected."));
            }
            auto* dlsymObj = (DynamicLinkSymbolObject*)param1.getContentAsHandleScalar();
            dlsymObj->disp(io);
        }
        DisplayVariableFooter(io, name.empty());
    } else {
        Error(_W("dlsym handle expected."));
    }
    return retval;
}
//=============================================================================
