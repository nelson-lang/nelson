//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Future_displayBuiltin.hpp"
#include "FevalFutureObject.hpp"
#include "AfterAllFutureObject.hpp"
#include "AfterEachFutureObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "DisplayVariableHelpers.hpp"
#include "FutureObjectHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ParallelGateway::Future_displayBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 2);
    ArrayOf param1 = argIn[0];
    if (param1.isHandle()) {
        std::wstring name;
        if (argIn.size() == 2) {
            name = argIn[1].getContentAsWideString();
        }
        Interface* io = eval->getInterface();
        DisplayVariableHeader(io, param1, name, false);
        std::vector<FutureObject*> futures = ArrayOfToFutures(param1);
        if (futures.size() == 1) {
            futures[0]->display(io);
        } else {
            for (size_t k = 0; k < futures.size(); ++k) {
                if (futures[k]) {
                    futures[k]->displayOnOneLine(io, k + 1);
                } else {
                    FutureObject::displayOnOneLineEmpty(io, k + 1);
                }
            }
        }
        DisplayVariableFooter(io, name.empty());
    } else {
        Error(_W("Future handle expected."));
    }
    return retval;
}
//=============================================================================
