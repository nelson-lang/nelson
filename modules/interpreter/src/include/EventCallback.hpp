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
#include <functional>
#include <utility>
#include "nlsInterpreter_exports.h"
#include "CallbackBase.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSINTERPRETER_IMPEXP EventCallback : public CallbackBase
{
private:
    std::function<void()> failureHandler;
    std::function<void(bool)> completionHandler;

public:
    EventCallback() { };
    EventCallback(const ArrayOf& _callbackAsArrayOf) : CallbackBase(_callbackAsArrayOf) { };
    EventCallback(const ArrayOf& _callbackAsArrayOf, std::function<void()> _failureHandler)
        : CallbackBase(_callbackAsArrayOf), failureHandler(std::move(_failureHandler)) { };
    EventCallback(const ArrayOf& _callbackAsArrayOf, std::function<void()> _failureHandler,
        std::function<void(bool)> _completionHandler)
        : CallbackBase(_callbackAsArrayOf)
        , failureHandler(std::move(_failureHandler))
        , completionHandler(std::move(_completionHandler)) { };
    ~EventCallback() { };

    bool
    execute(Evaluator* eval)
    {
        bool result = executeCallbackImpl(eval);
        if (!result && failureHandler) {
            failureHandler();
        }
        if (completionHandler) {
            completionHandler(result);
        }
        return result;
    }
};
//=============================================================================
} // namespace Nelson
//=============================================================================
