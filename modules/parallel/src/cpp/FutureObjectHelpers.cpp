//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FutureObjectHelpers.hpp"
#include "HandleManager.hpp"
#include "FutureObject.hpp"
#include "AfterAllFutureObject.hpp"
#include "AfterEachFutureObject.hpp"
#include "FevalFutureObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "VertCatOperator.hpp"
#include "ParallelEvaluator.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
FuturesToArrayOf(const std::vector<FutureObject*>& futures)
{
    Dimensions dims(1, futures.size());
    nelson_handle* ptrObject = static_cast<nelson_handle*>(
        ArrayOf::allocateArrayOf(NLS_HANDLE, 1, stringVector(), false));
    for (size_t k = 0; k < futures.size(); ++k) {
        ptrObject[k] = futures[k]->asNelsonHandle;
    }
    return ArrayOf(NLS_HANDLE, dims, (void*)ptrObject);
}
//=============================================================================
std::vector<FutureObject*>
ArrayOfToFutures(const ArrayOf& _param)
{
    std::vector<FutureObject*> futures;
    nelson_handle* qp = (nelson_handle*)(_param.getDataPointer());

    for (size_t k = 0; k < _param.getElementCount(); ++k) {
        nelson_handle hl = qp[k];
        HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(hl);
        if (hlObj) {
            if (_param.getHandleCategory() == FEVALFUTURE_CATEGORY_STR) {
                futures.push_back((FevalFutureObject*)hlObj);
            } else if (_param.getHandleCategory() == AFTERALLFUTURE_CATEGORY_STR) {
                futures.push_back((AfterAllFutureObject*)hlObj);
            } else if (_param.getHandleCategory() == AFTEREACHFUTURE_CATEGORY_STR) {
                futures.push_back((AfterEachFutureObject*)hlObj);
            } else {
                futures.push_back(nullptr);
            }
        } else {
            futures.push_back(nullptr);
        }
    }
    return futures;
}
//=============================================================================
ArrayOfVector
vertCatArrayOfVector(const ArrayOfVector& args1, const ArrayOfVector& args2, Exception& e)
{
    if (args1.size() != args2.size()) {
        Error(_("Same size"));
    }
    size_t nbElements = args1.size();
    ArrayOfVector result;
    result.resize(nbElements);
    Evaluator* localEvaluator = createParallelEvaluator(nullptr, SIZE_MAX);
    for (size_t k = 0; k < nbElements; ++k) {
        ArrayOfVector args;
        args << args1[k];
        args << args2[k];
        try {
            result[k] = VertCatOperator(localEvaluator, args);
        } catch (Exception& ex) {
            e = ex;
            if (localEvaluator) {
                deleteParallelEvaluator(localEvaluator, false);
                localEvaluator = nullptr;
            }
            return result;
        }
    }
    if (localEvaluator) {
        deleteParallelEvaluator(localEvaluator, false);
        localEvaluator = nullptr;
    }
    return result;
}
//=============================================================================
}
//=============================================================================
