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
#include <limits>
#include "AfterAllFutureObject.hpp"
#include "FevalFutureObject.hpp"
#include "HandleManager.hpp"
#include "ParallelEvaluator.hpp"
#include "WaitFutures.hpp"
#include "NelsonConfiguration.hpp"
#include "i18n.hpp"
#include "ErrorID.hpp"
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
void
AfterAllFutureObject::afterAll(FunctionDef* funcDef, int nLhs, bool uniformOutput)
{
    // keep this object alive for the whole run so it is not freed while this thread uses it
    WorkerActivityGuard workerActivityGuard(this);
    std::vector<FutureObject*> fevalFutures = this->getPredecessors();
    for (auto& fevalFuture : fevalFutures) {
        if (fevalFuture == nullptr) {
            this->setException(NelsonException(
                Nelson::resolveMessageTemplate(L"Nelson:parallel:cannotRetrieveOutputsFrom",
                    L"Cannot retrieve outputs from an unavailable future."),
                L"Nelson:parallel:future:UnavailableFuture"));
            {
                FutureStateGuardWrite guard(this->stateMutex);
                this->setState(THREAD_STATE::FINISHED);
            }
            return;
        }
    }
    // wait for the predecessors, but let a cancel on this future break the wait (no destructor
    // hang)
    while (!WaitFutures(nullptr, fevalFutures, THREAD_STATE::FINISHED, 0.05)) {
        if (NelsonConfiguration::getInstance()->getInterruptPending(this->getID())) {
            FutureStateGuardWrite guard(this->stateMutex);
            this->setState(THREAD_STATE::FINISHED);
            return;
        }
    }

    ArrayOfVector argsToConcate;
    for (auto& fevalFuture : fevalFutures) {
        NelsonException e = fevalFuture->getException(false);
        if (!e.isEmpty()) {
            this->setException(e);
            {
                FutureStateGuardWrite guard(this->stateMutex);
                this->setState(THREAD_STATE::FINISHED);
            }
            return;
        }
        ArrayOfVector result = fevalFuture->getResult(false);
        if (result.empty()) {
            this->setException(NelsonException(
                Nelson::resolveMessageTemplate(
                    L"Nelson:parallel:futureProducedOutputs", L"Future produced no outputs."),
                L"Nelson:parallel:future:NoOutputs"));
            {
                FutureStateGuardWrite guard(this->stateMutex);
                this->setState(THREAD_STATE::FINISHED);
            }
            return;
        }
        argsToConcate << result[0];
    }
    ArrayOf concated = {};
    auto localEvaluator = ParallelEvaluator::create(nullptr, SIZE_MAX);
    try {
        if (localEvaluator) {
            concated = localEvaluator->vertcatOperator(argsToConcate);
            ParallelEvaluator::destroy(localEvaluator, false);
        }
    } catch (NelsonException& e) {
        this->setException(e);
        {
            FutureStateGuardWrite guard(this->stateMutex);
            this->setState(THREAD_STATE::FINISHED);
        }
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
