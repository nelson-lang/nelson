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
#include "Error.hpp"
#include "Evaluator.hpp"
#include "DebugStack.hpp"
#include "characters_encoding.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//============================================================================
void
Error(const std::wstring& msg, const std::wstring& id, bool asCaller)
{
    if (!msg.empty()) {
        stackTrace trace;
        Evaluator* eval = (Evaluator*)NelsonConfiguration::getInstance()->getMainEvaluator();
        if (!eval) {
            return;
        }
        DebugStack(eval->callstack, asCaller ? 1 : 0, trace);
        Exception exception(msg, trace, id);
        throw exception;
    }
}
//=============================================================================
void
Error(const std::string& msg, const std::string& id, bool asCaller)
{
    Error(utf8_to_wstring(msg), utf8_to_wstring(id), asCaller);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
