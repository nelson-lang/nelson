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
#include "AfterAllFutureObject.hpp"
#include "FevalFutureObject.hpp"
#include "HandleManager.hpp"
#include "ParallelEvaluator.hpp"
#include "WaitFutures.hpp"
//=============================================================================
AfterAllFutureObject::AfterAllFutureObject(
    const std::wstring& functionName, const std::vector<FutureObject*>& predecessors)
    : FutureObject(functionName)
    , HandleGenericObject(NLS_HANDLE_AFTERALLFUTURE_CATEGORY_STR, this, false)

{
    setType(NLS_HANDLE_AFTERALLFUTURE_CATEGORY_STR);
    setPredecessors(predecessors);
}
//=============================================================================
static bool
allFinish(const std::vector<FutureObject*>& objs)
{
    for (auto obj : objs) {
        FutureStateGuard guard(obj->stateMutex);
        if (obj->state != THREAD_STATE::FINISHED) {
            return false;
        }
    }
    return true;
}
//=============================================================================
void
AfterAllFutureObject::afterAll(FunctionDef* funcDef, int nLhs, bool uniformOutput)
{
    bool allFinished = false;
    std::vector<FutureObject*> fevalFutures = this->getPredecessors();
    do {
        allFinished = allFinish(fevalFutures);
    } while (!allFinished);

    ArrayOfVector argsToConcate;
    for (auto& fevalFuture : fevalFutures) {
        argsToConcate << fevalFuture->getResult(false)[0];
    }
    ArrayOf concated = {};
    auto localEvaluator = ParallelEvaluator::create(nullptr, SIZE_MAX);
    try {
        if (localEvaluator) {
            concated = localEvaluator->vertcatOperator(argsToConcate);
            ParallelEvaluator::destroy(localEvaluator, false);
        }
    } catch (Exception& e) {
        this->setException(e);
        this->state = THREAD_STATE::FINISHED;
        if (localEvaluator) {
            ParallelEvaluator::destroy(localEvaluator, false);
        }
        return;
    }
    ArrayOfVector args;
    args << concated;
    this->evaluateFunction(funcDef, nLhs, args);
}
//=============================================================================
