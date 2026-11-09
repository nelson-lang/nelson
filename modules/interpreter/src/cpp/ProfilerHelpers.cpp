//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "ProfilerHelpers.hpp"
#include "characters_encoding.hpp"
#include "CallStack.hpp"
#include "PositionScript.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
internalProfileFunction
computeProfileStack(Evaluator* eval, const std::string& currentFunctionName,
    const std::wstring& currentFilename, bool isBuiltin)
{
    profileParentStack profilerStack;

    if (eval != nullptr) {
        std::tuple<std::string, uint64> previousProfilerStackElement;
        std::tuple<std::string, uint64> profilerStackElement;

        for (size_t k = 0; k < eval->callstack.size(); ++k) {
            std::string filename = eval->callstack.getContext(k);
            if (filename != "evaluator" && filename != "EvaluateScript") {
                int line = eval->callstack.getID(k) & 0xffff;
                if (line > 0) {
                    profilerStackElement = std::make_tuple(filename, line);
                    if (profilerStackElement != previousProfilerStackElement) {
                        profilerStack.push_back(profilerStackElement);
                        previousProfilerStackElement = profilerStackElement;
                    }
                }
            }
        }
    }
    // a local function is listed as file>local, as the call stack displays it
    if (!isBuiltin && !currentFilename.empty()) {
        const std::string displayName = wstring_to_utf8(
            PositionScript(utf8_to_wstring(currentFunctionName), currentFilename).getDisplayName());
        return std::make_tuple(profilerStack, displayName, currentFilename, isBuiltin);
    }
    return std::make_tuple(profilerStack, currentFunctionName, currentFilename, isBuiltin);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
