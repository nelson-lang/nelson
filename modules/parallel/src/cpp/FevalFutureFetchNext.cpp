//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <thread>
#include <chrono>
#include "FevalFutureFetchNext.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleManager.hpp"
#include "ProcessEventsDynamicFunction.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
inline ArrayOfVector
emptyResultFetchNext()
{
    ArrayOfVector _result;
    _result << ArrayOf::emptyConstructor();
    _result << ArrayOf::emptyConstructor();
    return _result;
}
//=============================================================================
ArrayOfVector
FevalFutureFetchNext(
    Evaluator* eval, const std::vector<FutureObject*>& futures, int nLhs, double timeout)
{
    ArrayOfVector _result;

    if (futures.empty()) {
        return emptyResultFetchNext();
    }

    std::chrono::nanoseconds begin_time
        = std::chrono::high_resolution_clock::now().time_since_epoch();

    while (true) {
        bool haveRunnnings = false;

        for (size_t k = 0; k < futures.size(); k++) {
            if (futures[k]) {
                switch (futures[k]->state) {
                case THREAD_STATE::FINISHED: {
                    if (!futures[k]->wasRead()) {
                        Exception e = futures[k]->getException();
                        if (!e.isEmpty()) {
                            throw e;
                        }
                        _result << ArrayOf::doubleConstructor((double)k + 1);
                        _result += futures[k]->getResult();
                        return _result;
                    }
                } break;
                case THREAD_STATE::RUNNING:
                case THREAD_STATE::QUEUED: {
                    haveRunnnings = true;
                } break;
                default: {
                    haveRunnnings = false;
                } break;
                }
            }
        }
        if (!haveRunnnings) {
            Error(_W("There are no unread Futures to fetch."));
        }

        std::this_thread::sleep_for(std::chrono::milliseconds(uint64(1)));
        std::chrono::nanoseconds current_time
            = std::chrono::high_resolution_clock::now().time_since_epoch();
        std::chrono::nanoseconds difftime = (current_time - begin_time);
        if ((timeout >= 0) && (difftime.count() > int64(timeout * 1e9))) {
            return emptyResultFetchNext();
        }
        if (eval != nullptr) {
            bool isInterrupted
                = NelsonConfiguration::getInstance()->getInterruptPending(eval->getID());
            if (isInterrupted) {
                return emptyResultFetchNext();
            }
        }
        if (eval != nullptr && eval->haveEventsLoop()) {
            ProcessEventsDynamicFunctionWithoutWait();
        }
    }
    return _result;
}
//=============================================================================
}
//=============================================================================
