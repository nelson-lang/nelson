//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "WarningEmitter.h"
#include "Exception.hpp"
#include "Interface.hpp"
#include "Error.hpp"
#include "DebugStack.hpp"
//=============================================================================
static Nelson::Evaluator* evaluatorWarning = nullptr;
//=============================================================================
namespace Nelson {
//=============================================================================
void
setWarningEvaluator(Evaluator* eval)
{
    evaluatorWarning = eval;
}
//=============================================================================
}
//=============================================================================
void
NelsonWarningEmitter(const wchar_t* msg, const wchar_t* id, bool asError)
{
    std::wstring message = std::wstring(msg);
    std::wstring identifier = std::wstring(id);
    if (!message.empty()) {
        if (asError) {
            Nelson::Error(message, identifier);
        } else {
            if (evaluatorWarning != nullptr) {
                Nelson::stackTrace trace;
                DebugStack(evaluatorWarning->callstack, 1, trace);
                Nelson::Exception exception(message, trace, identifier);
                evaluatorWarning->setLastWarningException(exception);
                Nelson::Interface* io = evaluatorWarning->getInterface();
                if (io != nullptr) {
                    io->warningMessage(exception.getMessage());
                }
            }
        }
    }
}
//=============================================================================
