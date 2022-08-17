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
#include "AfterEachFutureObject.hpp"
#include "FevalFutureObject.hpp"
#include "HandleManager.hpp"
#include "VertCatOperator.hpp"
#include "FutureObjectHelpers.hpp"
//=============================================================================
AfterEachFutureObject::AfterEachFutureObject(
    const std::wstring& functionName, const std::vector<FutureObject*>& predecessors)
    : FutureObject(functionName)
    , HandleGenericObject(std::wstring(AFTEREACHFUTURE_CATEGORY_STR), this, false)
{
    setType(AFTEREACHFUTURE_CATEGORY_STR);
    setPredecessors(predecessors);
}
//=============================================================================
static bool
allDoOnce(const std::vector<bool>& doOnce)
{
    for (size_t k = 0; k < doOnce.size(); ++k) {
        if (!doOnce[k]) {
            return false;
        }
    }
    return true;
}
//=============================================================================
void
AfterEachFutureObject::afterEach(FunctionDef* funcDef, int nLhs, bool uniformOutput)
{

    bool haveOinished = false;
    std::vector<FutureObject*> futures = getPredecessors();

    std::vector<bool> doAfterEach(futures.size(), false);

    bool isFirst = true;
    while (!allDoOnce(doAfterEach)) {
        for (size_t k = 0; k < futures.size(); ++k) {
            if (futures[k] && (futures[k]->state == THREAD_STATE::FINISHED)
                && (doAfterEach[k] == false)) {
                if (!futures[k]->getException(false).isEmpty()) {
                    this->setException(futures[k]->getException(false));
                    this->state = THREAD_STATE::FINISHED;
                    return;
                }
                ArrayOfVector args = futures[k]->getResult(false);
                ArrayOfVector previous = this->getResult(false);
                this->evaluateFunction(funcDef, nLhs, args, false);
                if (isFirst) {
                    isFirst = false;
                } else {
                    Exception e;
                    this->setResult(vertCatArrayOfVector(previous, this->getResult(false), e));
                    if (!e.isEmpty()) {
                        this->setException(e);
                        this->state = THREAD_STATE::FINISHED;
                        return;
                    }
                }
                doAfterEach[k] = true;
            }
        }
    }
    this->state = THREAD_STATE::FINISHED;
}
//=============================================================================
