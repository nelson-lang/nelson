//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Future_getBuiltin.hpp"
#include "FevalFutureObject.hpp"
#include "AfterAllFutureObject.hpp"
#include "AfterEachFutureObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleManager.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ParallelGateway::Future_getBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    std::wstring propertyName = param2.getContentAsWideString();
    ArrayOfVector retval(1);
    bool isSupportedFuture = (param1.getHandleCategory() == NLS_HANDLE_FEVALFUTURE_CATEGORY_STR)
        || (param1.getHandleCategory() == NLS_HANDLE_AFTERALLFUTURE_CATEGORY_STR)
        || (param1.getHandleCategory() == NLS_HANDLE_AFTEREACHFUTURE_CATEGORY_STR);
    if (!isSupportedFuture) {
        Error(_W("Future handle expected."));
    }

    auto* ptr = (nelson_handle*)param1.getDataPointer();
    indexType nbElements = param1.getElementCount();
    if (nbElements > 0) {
        for (indexType k = 0; k < nbElements; k++) {
            HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(ptr[k]);
            auto* objFevalFuture = (FevalFutureObject*)hlObj;
            ArrayOf res;
            if (!objFevalFuture->get(propertyName, res)) {
                Error(ERROR_WRONG_ARGUMENT_2_VALUE + L" " + propertyName);
            }
            if (propertyName == L"Function") {
                FunctionDef* funcDef = nullptr;
                std::string str2func = "str2func";
                if (eval->getContext()->lookupFunction(str2func, funcDef)) {
                    ArrayOfVector argsIn;
                    argsIn << res;
                    ArrayOfVector resVect = funcDef->evaluateFunction(eval, argsIn, 1);
                    res = resVect[0];
                }
            }
            retval << res;
        }
    }
    return retval;
}
//=============================================================================
