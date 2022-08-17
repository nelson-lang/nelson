//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
#include "VertCatOperator.hpp"
#include "ParallelEvaluator.hpp"
//=============================================================================
AfterAllFutureObject::AfterAllFutureObject(
    const std::wstring& functionName, const std::vector<FutureObject*>& predecessors)
    : FutureObject(functionName)
    , HandleGenericObject(std::wstring(AFTERALLFUTURE_CATEGORY_STR), this, false)

{
    setType(AFTERALLFUTURE_CATEGORY_STR);
    setPredecessors(predecessors);
}
//=============================================================================
static bool
allFinish(const std::vector<FutureObject*>& objs)
{
    for (size_t k = 0; k < objs.size(); ++k) {
        if (objs[k]->state != THREAD_STATE::FINISHED) {
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
    for (size_t k = 0; k < fevalFutures.size(); ++k) {
        argsToConcate << fevalFutures[k]->getResult(false)[0];
    }
    ArrayOf concated;
    Evaluator* localEvaluator = createParallelEvaluator(nullptr, SIZE_MAX);
    try {
        concated = VertCatOperator(localEvaluator, argsToConcate);
        if (localEvaluator) {
            localEvaluator = deleteParallelEvaluator(localEvaluator, false);
        }
    } catch (Exception& e) {
        this->setException(e);
        this->state = THREAD_STATE::FINISHED;
        if (localEvaluator) {
            localEvaluator = deleteParallelEvaluator(localEvaluator, false);
        }
        return;
    }
    ArrayOfVector args;
    args << concated;
    this->evaluateFunction(funcDef, nLhs, args);
}
//=============================================================================
