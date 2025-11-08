//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <mutex>
#include <vector>
#include "TimerCallback.hpp"
#include "nlsInterpreter_exports.h"
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSINTERPRETER_IMPEXP TimerQueue
{
private:
    static TimerQueue* m_pInstance;
    std::mutex m_mutex;
    std::vector<TimerCallback> waitingCallbacks;
    std::vector<TimerCallback> inProgressCallbacks;

    TimerQueue();
    TimerQueue(const TimerQueue&) = delete;
    TimerQueue&
    operator=(const TimerQueue&)
        = delete;

public:
    void
    clear();
    bool
    isEmpty();
    void
    add(const TimerCallback& timerCallback);

    ~TimerQueue();
    static TimerQueue*
    getInstance();
    static void
    destroy();
    bool
    processCallback(Evaluator* eval);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
