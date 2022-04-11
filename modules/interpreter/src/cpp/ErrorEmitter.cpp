//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ErrorEmitter.h"
#include "DebugStack.hpp"
#include "Interface.hpp"
//=============================================================================
static Nelson::Evaluator* evaluatorError = nullptr;
//=============================================================================
namespace Nelson {
//=============================================================================
void
setErrorEvaluator(Evaluator* eval)
{
    evaluatorError = eval;
}
//=============================================================================
void
throwException(const Exception& e)
{
    throw e;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
void
NelsonErrorEmitter(const wchar_t* msg, const wchar_t* id, bool asCaller)
{
    std::wstring message(msg);
    if (!message.empty()) {
        if (evaluatorError != nullptr) {
            std::wstring identifier(id);
            Nelson::stackTrace trace;
            DebugStack(evaluatorError->callstack, asCaller ? 1 : 0, trace);
            Nelson::Exception exception(message, trace, identifier);
            Nelson::throwException(exception);
        }
    }
}
//=============================================================================
