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
#include "nlsInterpreter_exports.h"
#include "QueueCommon.hpp"
//=============================================================================
namespace Nelson {
namespace detail {
    struct CommandQueueTraits
    {
        static bool
        execute(std::wstring&, Evaluator*)
        {
            return true;
        }

        static bool
        canInterrupt(const std::wstring&, const std::wstring&)
        {
            return false;
        }

        static bool
        shouldDropWaitingWhenBusy(const std::wstring&)
        {
            return false;
        }
    };
} // namespace detail

class NLSINTERPRETER_IMPEXP CommandQueue
    : private detail::QueueCommon<std::wstring, detail::CommandQueueTraits>
{
private:
    using Base = detail::QueueCommon<std::wstring, detail::CommandQueueTraits>;

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
