//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CommandQueue.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
CommandQueue::CommandQueue() = default;
//=============================================================================
CommandQueue::~CommandQueue() { this->clearPending(); }
//=============================================================================
bool
CommandQueue::isEmpty()
{
    return pendingCount.load(std::memory_order_acquire) == 0;
}
//=============================================================================
void
CommandQueue::add(const std::wstring& cmdline, bool bIsPriority)
{
    if (bIsPriority) {
        this->enqueueFront(cmdline);
    } else {
        this->enqueue(cmdline);
    }
    pendingCount.fetch_add(1, std::memory_order_release);
}
//=============================================================================
void
CommandQueue::clear()
{
    this->clearPending();
    pendingCount.store(0, std::memory_order_release);
}
//=============================================================================
bool
CommandQueue::get(std::wstring& cmd)
{
    if (this->tryPopBack(cmd)) {
        decrementPendingCount();
        return true;
    }
    cmd.clear();
    return false;
}
//=============================================================================
void
CommandQueue::decrementPendingCount()
{
    size_t count = pendingCount.load(std::memory_order_acquire);
    while (count > 0
        && !pendingCount.compare_exchange_weak(
            count, count - 1, std::memory_order_acq_rel, std::memory_order_acquire)) { }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
