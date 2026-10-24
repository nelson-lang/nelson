//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FutureObjectHelpers.hpp"
#include "HandleManager.hpp"
#include "HandleState.hpp"
#include "FutureObject.hpp"
#include "AfterAllFutureObject.hpp"
#include "AfterEachFutureObject.hpp"
#include "FevalFutureObject.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "ParallelEvaluator.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
FuturesToArrayOf(const std::vector<FutureObject*>& futures)
{
    Dimensions dims(1, futures.size());
    HandleRef* ptrObject = static_cast<HandleRef*>(
        ArrayOf::allocateArrayOf(NLS_HANDLE, dims.elementCount(), stringVector(), false));
    for (size_t k = 0; k < futures.size(); ++k) {
        ptrObject[k] = HandleManager::getInstance()->getHandleRef(futures[k]->asNelsonHandle);
    }
    return ArrayOf(NLS_HANDLE, dims, (void*)ptrObject);
}
//=============================================================================
std::vector<FutureObject*>
ArrayOfToFutures(const ArrayOf& _param)
{
    std::vector<FutureObject*> futures;
    HandleRef* qp = (HandleRef*)(_param.dataPointer());

    for (size_t k = 0; k < _param.elementCount(); ++k) {
        HandleGenericObject* hlObj = qp[k] ? qp[k]->object() : nullptr;
        if (hlObj) {
            if (_param.handleCategory() == NLS_HANDLE_FEVALFUTURE_CATEGORY_STR) {
                futures.push_back((FevalFutureObject*)hlObj);
            } else if (_param.handleCategory() == NLS_HANDLE_AFTERALLFUTURE_CATEGORY_STR) {
                futures.push_back((AfterAllFutureObject*)hlObj);
            } else if (_param.handleCategory() == NLS_HANDLE_AFTEREACHFUTURE_CATEGORY_STR) {
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
vertCatArrayOfVector(const ArrayOfVector& args1, const ArrayOfVector& args2, NelsonException& e)
{
    if (args1.size() != args2.size()) {
        Error("Nelson:parallel:sameSizeExpected", "Same size");
    }
    size_t nbElements = args1.size();
    ArrayOfVector result;
    result.resize(nbElements);
    auto localEvaluator = ParallelEvaluator::create(nullptr, SIZE_MAX);
    for (size_t k = 0; k < nbElements; ++k) {
        ArrayOfVector args;
        args << args1[k];
        args << args2[k];
        try {
            result[k] = localEvaluator->vertcatOperator(args);
        } catch (NelsonException& ex) {
            e = ex;
            if (localEvaluator) {
                ParallelEvaluator::destroy(localEvaluator, false);
                localEvaluator = nullptr;
            }
            return result;
        }
    }
    if (localEvaluator) {
        ParallelEvaluator::destroy(localEvaluator, false);
        localEvaluator = nullptr;
    }
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
