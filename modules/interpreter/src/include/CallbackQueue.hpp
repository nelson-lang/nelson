//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <mutex>
#include <vector>
#include "GraphicCallback.hpp"
#include "nlsInterpreter_exports.h"
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSINTERPRETER_IMPEXP CallbackQueue
{
private:
    static CallbackQueue* m_pInstance;
    std::mutex m_mutex;
    std::vector<GraphicCallback> waitingCallbacks;
    std::vector<GraphicCallback> inProgressCallbacks;

    CallbackQueue();
    CallbackQueue(const CallbackQueue&) = delete;
    CallbackQueue&
    operator=(const CallbackQueue&)
        = delete;

public:
    void
    clear();
    bool
    isEmpty();
    void
    add(const GraphicCallback& graphicCallback);

    ~CallbackQueue();
    static CallbackQueue*
    getInstance();
    static void
    destroy();
    bool
    processCallback(Evaluator* eval);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
