//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/filesystem.hpp>
#include <boost/algorithm/string.hpp>
#include "DebugStack.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isNlf(PositionScript& pos)
{
    return boost::algorithm::ends_with(pos.getFilename(), L".nlf");
}
//=============================================================================
static bool
isNls(PositionScript& pos)
{
    return boost::algorithm::ends_with(pos.getFilename(), L".nls");
}
//=============================================================================
static bool
isEvaluateString(PositionScript& pos)
{
    return boost::algorithm::starts_with(pos.getFunctionName(), L"evaluator");
}
//=============================================================================
static bool
isBuiltin(PositionScript& pos)
{
    return boost::algorithm::starts_with(pos.getFunctionName(), L"built-in");
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
        } else if (isNlf(stackPositions[k])) {
            cleanedPositions.push_back(stackPositions[k]);
            k++;
        } else if (isNls(stackPositions[k])) {
            boost::filesystem::path p(stackPositions[k].getFilename());
            PositionScript pos(p.stem().generic_wstring(), stackPositions[k].getFilename(),
                stackPositions[k].getLine());
            cleanedPositions.push_back(pos);
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
DebugStack(const CallStack &callstack, int nbOmitLines, stackTrace& stackPositions)
{
    stackPositions.clear();
    size_t i = 0;
    while (i < callstack.size()) {
        if (callstack.getID(i) == 0) {
            size_t j = i + 1;
            while ((j < callstack.size()) && (callstack.getContext(j) == callstack.getContext(i))
                && (callstack.getDetail(j) == callstack.getDetail(i)) && (callstack.getID(j) != 0)) {
                j++;
            }
            std::wstring filename = utf8_to_wstring(callstack.getContext(j - 1));
            std::wstring functionname = utf8_to_wstring(callstack.getDetail(j - 1));
            int lineposition = callstack.getID(j - 1) & 0x0000FFFF;
            stackPositions.push_back(PositionScript(functionname, filename, lineposition));
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
