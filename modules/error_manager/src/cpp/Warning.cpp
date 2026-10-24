//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstdlib>
#include "Warning.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
#include "NelsonConfiguration.hpp"
#include "Evaluator.hpp"
#include "DebugStack.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
warningEmitter(const std::wstring& message, const std::wstring& id, bool display = true)
{
    size_t mainEvaluatorID = 0;
    if (!message.empty()) {
        Evaluator* eval = (Evaluator*)NelsonConfiguration::getInstance()->getMainEvaluator();
        if (!eval) {
            return;
        }
        Interface* io = (Interface*)NelsonConfiguration::getInstance()->getMainIOInterface();
        if (!io) {
            return;
        }
        stackTrace trace;
        DebugStack(eval->callstack, 1, trace);
        try {
            NelsonException* exception = new NelsonException(message, trace, id);
            NelsonException* previousException
                = (NelsonException*)NelsonConfiguration::getInstance()->getLastWarningException(
                    mainEvaluatorID);
            if (previousException) {
                delete previousException;
            }
            NelsonConfiguration::getInstance()->setLastWarningException(mainEvaluatorID, exception);
            if (display) {
                io->warningMessage(exception->formattedWarningMessage(io->supportsHyperlinks()));
            }
        } catch (const std::bad_alloc&) {
        }
    }
}
//=============================================================================
void
RaiseWarning(const std::wstring& id, const std::wstring& message)
{
    if (message != L"") {
        WARNING_STATE state = warningCheckState(id);
        switch (state) {
        case WARNING_STATE::AS_ERROR: {
            RaiseError(message, id);
        } break;
        case WARNING_STATE::DISABLED:
            // A disabled warning is not displayed but lastwarn still reports it.
            warningEmitter(message, id, false);
            break;
        case WARNING_STATE::ENABLED:
        case WARNING_STATE::NOT_FOUND:
        default: {
            warningEmitter(message, id);
        } break;
        }
    }
}
//=============================================================================
void
Warning(const std::wstring& message)
{
    RaiseWarning(L"", message);
}
//=============================================================================
void
Warning(const std::string& message)
{
    RaiseWarning(L"", utf8_to_wstring(message));
}
//=============================================================================
void
RaiseWarning(const std::string& id, const std::string& message)
{
    RaiseWarning(utf8_to_wstring(id), utf8_to_wstring(message));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
