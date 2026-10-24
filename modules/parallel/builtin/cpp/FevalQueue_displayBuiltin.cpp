//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FevalQueue_displayBuiltin.hpp"
#include "FevalQueueObject.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "DisplayVariableHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ParallelGateway::FevalQueue_displayBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 2);
    ArrayOf param1 = argIn[0];
    if (param1.isHandle()) {
        std::wstring name;
        if (argIn.size() == 2) {
            name = argIn[1].asWideString();
        }
        Interface* io = eval->getInterface();
        DisplayVariableHeader(io, param1, name, false);
        if (param1.isScalar()) {
            if (param1.handleCategory() != NLS_HANDLE_FEVALQUEUE_CATEGORY_STR) {
                Error(L"Nelson:fevalQueue:handleExpected", L"FevalQueue handle expected.");
            }
            auto* fevalQueueObject = (FevalQueueObject*)param1.asHandleScalar();
            fevalQueueObject->display(io);
        }
        DisplayVariableFooter(io, name.empty());
    } else {
        Error(L"Nelson:fevalQueue:handleExpected", L"FevalQueue handle expected.");
    }
    return retval;
}
//=============================================================================
