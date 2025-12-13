//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <vector>
#include <algorithm>
#include "AfterEachFutureObject.hpp"
#include "FevalFutureObject.hpp"
#include "HandleManager.hpp"
#include "FutureObjectHelpers.hpp"
#include "WaitFutures.hpp"
//=============================================================================
AfterEachFutureObject::AfterEachFutureObject(
    const std::wstring& functionName, const std::vector<FutureObject*>& predecessors)
    : FutureObject(functionName)
    , HandleGenericObject(NLS_HANDLE_AFTEREACHFUTURE_CATEGORY_STR, this, false)
{
    setType(NLS_HANDLE_AFTEREACHFUTURE_CATEGORY_STR);
    setPredecessors(predecessors);
}
//=============================================================================
static inline bool
allDoOnce(const std::vector<bool>& doOnce)
{
    for (bool k : doOnce) {
        if (!k) {
            return false;
        }
    }
    return true;
}
//=============================================================================
void
AfterEachFutureObject::afterEach(FunctionDef* funcDef, int nLhs, bool uniformOutput)
{
    std::vector<FutureObject*> futures = getPredecessors();
    std::vector<bool> doAfterEach(futures.size(), false);

    ArrayOfMatrix allFutureResults;
    allFutureResults.resize(futures.size());

    while (!allDoOnce(doAfterEach)) {
        for (size_t k = 0; k < futures.size(); ++k) {
            if (futures[k]) {
                bool isFinished = false;
                {
                    FutureStateGuardRead guard(futures[k]->stateMutex);
                    isFinished = (futures[k]->state == THREAD_STATE::FINISHED);
                }
                if (isFinished && (doAfterEach[k] == false)) {
                    if (!futures[k]->getException(false).isEmpty()) {
                        this->setException(futures[k]->getException(false));
                        {
                            FutureStateGuardWrite guard(this->stateMutex);
                            this->state = THREAD_STATE::FINISHED;
                        }
                        return;
                    }
                    ArrayOfVector args = futures[k]->getResult(false);
                    this->setResult(ArrayOfVector());
                    this->evaluateFunction(funcDef, nLhs, args, false);
                    allFutureResults[k] = this->getResult(false);
                    this->setResult(ArrayOfVector());
                    doAfterEach[k] = true;
                }
            }
        }
    }
    ArrayOfVector concat = allFutureResults[0];
    for (size_t k = 1; k < futures.size(); ++k) {
        Exception e;
        concat = vertCatArrayOfVector(concat, allFutureResults[k], e);
        if (!e.isEmpty()) {
            this->setException(e);
            {
                FutureStateGuardWrite guard(this->stateMutex);
                this->state = THREAD_STATE::FINISHED;
            }
            return;
        }
    }
    this->setResult(concat);
    {
        FutureStateGuardWrite guard(this->stateMutex);
        this->state = THREAD_STATE::FINISHED;
    }
}
//=============================================================================
