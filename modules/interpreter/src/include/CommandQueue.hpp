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
#include <string>
#include <vector>
#include <mutex>
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
class NLSINTERPRETER_IMPEXP CommandQueue
{
private:
    std::mutex m_mutex;
    std::vector<std::wstring> commands;

public:
    CommandQueue();
    ~CommandQueue();
    bool
    isEmpty();
    void
    add(const std::wstring& cmdline, bool bIsPriority = false);
    void
    clear();
    bool
    get(std::wstring& cmd);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
