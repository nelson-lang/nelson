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
#include "CallbackQueue.hpp"
#include "EventQueue.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
inline void
processPendingCallbacksAndTimers(Evaluator* eval = nullptr)
{
    if (eval == nullptr) {
        eval = static_cast<Evaluator*>(NelsonConfiguration::getInstance()->getMainEvaluator());
    }
    if (eval == nullptr) {
        return;
    }
    CallbackQueue::getInstance()->processCallback(eval);
    EventQueue::getInstance()->processCallback(eval);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
