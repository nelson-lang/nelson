//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
    if (commands.size() > 0) {
        cmd = (commands.end() - 1)->c_str();
        commands.pop_back();
        return true;
    }
    cmd = L"";
    return false;
}
//=============================================================================
}
//=============================================================================
