//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <chrono>
#include "FevalFutureFetchNext.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "HandleManager.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
inline ArrayOfVector
emptyResultFetchNext()
{
    ArrayOfVector _result;
    _result << ArrayOf::empty();
    _result << ArrayOf::empty();
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
                        NelsonException e = futures[k]->getException();
                        if (!e.isEmpty()) {
                            throw e;
                        }
                        _result << ArrayOf::doubleScalar((double)k + 1);
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
            Error(L"Nelson:fetchNext:noUnreadFutures", L"There are no unread Futures to fetch.");
        }

        FutureObject::waitForAnyStateChange(std::chrono::milliseconds(10));
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
    }
    return _result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
