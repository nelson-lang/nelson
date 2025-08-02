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
#include "StringHelpers.hpp"
#include "DebugStack.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isM(PositionScript& pos)
{
    return StringHelpers::ends_with(pos.getFilename(), L".m");
}
//=============================================================================
static bool
isEvaluateString(PositionScript& pos)
{
    return StringHelpers::starts_with(pos.getFunctionName(), L"evaluator");
}
//=============================================================================
static bool
isBuiltin(PositionScript& pos)
{
    return StringHelpers::starts_with(pos.getFunctionName(), L"built-in");
}
//=============================================================================
static stackTrace
cleanupDebugStack(stackTrace stackPositions)
{
    stackTrace cleanedPositions;
    size_t k = 0;
    while (k < stackPositions.size()) {
        if (isEvaluateString(stackPositions[k])) {
            k++;
        } else if (isM(stackPositions[k])) {
            cleanedPositions.push_back(stackPositions[k]);
            k++;
        } else if (isBuiltin(stackPositions[k])) {
            PositionScript pos(stackPositions[k].getFilename(), L"", stackPositions[k].getLine());
            cleanedPositions.push_back(pos);
            k++;
        } else {
            k++;
        }
    }
    return cleanedPositions;
}
//=============================================================================
void
DebugStack(const CallStack& callstack, int nbOmitLines, stackTrace& stackPositions)
{
    stackPositions.clear();
    size_t i = 0;
    while (i < callstack.size()) {
        if (callstack.getID(i) == 0) {
            size_t j = i + 1;
            while ((j < callstack.size()) && (callstack.getContext(j) == callstack.getContext(i))
                && (callstack.getDetail(j) == callstack.getDetail(i))
                && (callstack.getID(j) != 0)) {
                j++;
            }
            std::wstring filename = utf8_to_wstring(callstack.getContext(j - 1));
            std::wstring functionname = utf8_to_wstring(callstack.getDetail(j - 1));
            int lineposition = callstack.getID(j - 1) & 0x0000FFFF;
            stackPositions.emplace_back(PositionScript(functionname, filename, lineposition));
            i = j;
        } else {
            i++;
        }
    }
    std::reverse(std::begin(stackPositions), std::end(stackPositions));
    stackPositions = cleanupDebugStack(stackPositions);
    for (int k = 0; k < nbOmitLines; k++) {
        if (stackPositions.size() > 0) {
            stackPositions.erase(stackPositions.begin());
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
