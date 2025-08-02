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
CommandQueue::CommandQueue()
{
    std::lock_guard<std::mutex> lock(m_mutex);
    //        commands.reserve(4096);
}
//=============================================================================
CommandQueue::~CommandQueue()
{
    std::lock_guard<std::mutex> lock(m_mutex);
    commands.clear();
}
//=============================================================================
bool
CommandQueue::isEmpty()
{
    std::lock_guard<std::mutex> lock(m_mutex);
    return commands.empty();
}
//=============================================================================
void
CommandQueue::add(const std::wstring& cmdline, bool bIsPriority)
{
    std::lock_guard<std::mutex> lock(m_mutex);
    commands.push_back(cmdline);
}
//=============================================================================
void
CommandQueue::clear()
{
    std::lock_guard<std::mutex> lock(m_mutex);
    commands.clear();
}
//=============================================================================
bool
CommandQueue::get(std::wstring& cmd)
{
    std::lock_guard<std::mutex> lock(m_mutex);
    if (!commands.empty()) {
        cmd = *(commands.end() - 1);
        commands.pop_back();
        return true;
    }
    cmd.clear();
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
