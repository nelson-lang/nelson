//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "dllib_displayBuiltin.hpp"
#include "DynamicLinkLibraryObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "DisplayVariableHelpers.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DynamicLinkGateway::dllib_displayBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 0);
    ArrayOf param1 = argIn[0];
    std::wstring name;
    if (argIn.size() == 2) {
        name = argIn[1].getContentAsWideString();
    }
    if (param1.isHandle()) {
        Interface* io = eval->getInterface();
        DisplayVariableHeader(io, param1, name, false);
        if (param1.isScalar()) {
            if (param1.getHandleCategory() != DLLIB_CATEGORY_STR) {
                Error(_W("dllib handle expected."));
            }
            auto* dllibObj = (DynamicLinkLibraryObject*)param1.getContentAsHandleScalar();
            dllibObj->disp(io);
        }
        DisplayVariableFooter(io, name.empty());
    } else {
        Error(_W("dllib handle expected."));
    }
    return retval;
}
//=============================================================================
