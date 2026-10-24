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
#include "GraphicCallback.hpp"
#include "nlsInterpreter_exports.h"
#include "Evaluator.hpp"
#include "QueueCommon.hpp"
#include "SingletonHelper.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace detail {
    struct GraphicCallbackQueueTraits : QueueDefaultTraits<GraphicCallback>
    {
        static bool
        canInterrupt(const GraphicCallback& running, const GraphicCallback&)
        {
            return running.isInterruptible();
        }

        static bool
        shouldDropWaitingWhenBusy(const GraphicCallback& waiting)
        {
            return waiting.getBusyActionState() == BUSY_ACTION::CANCEL;
        }

        static bool
        execute(GraphicCallback& callback, Evaluator* eval)
        {
            return callback.execute(eval);
        }
    };
    //=============================================================================
} // namespace detail
//=============================================================================
class NLSINTERPRETER_IMPEXP CallbackQueue
    : public SingletonHelper<CallbackQueue>,
      private detail::QueueCommon<GraphicCallback, detail::GraphicCallbackQueueTraits>
{
    friend class SingletonHelper<CallbackQueue>;

private:
    using Base = detail::QueueCommon<GraphicCallback, detail::GraphicCallbackQueueTraits>;
    CallbackQueue() = default;

public:
    static CallbackQueue*
    getInstance();
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
    add(const GraphicCallback& graphicCallback)
    {
        enqueue(graphicCallback);
    }
    bool
    processCallback(Evaluator* eval)
    {
        if (!eval || !eval->haveEventsLoop()) {
            return false;
        }
        return process(eval);
    }
    bool
    processCallbackNow(Evaluator* eval)
    {
        return eval != nullptr && process(eval);
    }
    ~CallbackQueue() { clearAllQueues(); }
};
//=============================================================================
} // namespace Nelson
//=============================================================================
