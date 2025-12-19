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
#include "EventCallback.hpp"
#include "nlsInterpreter_exports.h"
#include "Evaluator.hpp"
#include "QueueCommon.hpp"
#include "SingletonHelper.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSINTERPRETER_IMPEXP EventQueue : public SingletonHelper<EventQueue>,
                                         private detail::QueueCommon<EventCallback>
{
    friend class SingletonHelper<EventQueue>;

private:
    using Base = detail::QueueCommon<EventCallback>;
    // storage & synchronization handled by Base
    EventQueue() = default;

public:
    void
    clear()
    {
        clearPending();
    }
    bool
    isEmpty()
    {
        return empty();
    }
    void
    add(const EventCallback& eventCallback)
    {
        enqueue(eventCallback);
    }
    bool
    processCallback(Evaluator* eval)
    {
        if (!eval || !eval->haveEventsLoop()) {
            return false;
        }
        return process(eval);
    }
    ~EventQueue() { clearAllQueues(); }
};
//=============================================================================
} // namespace Nelson
//=============================================================================
