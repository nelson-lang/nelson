//=============================================================================
// Copyright (c) 2016-2019 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "ProfilerHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
internalProfileFunction
computeProfileStack(Evaluator* eval, const std::string& currentFunctionName,
    const std::wstring& currentFilename, bool isBuiltin)
{
    profileParentStack profilerStack;

    std::vector<StackEntry> cstack;
    if (eval != nullptr) {
        cstack = eval->cstack;
        size_t stackDepth = cstack.size();
        std::tuple<std::string, uint64> previousProfilerStackElement;
        std::tuple<std::string, uint64> profilerStackElement;
        while (stackDepth > 1) {
            std::string filename = cstack[stackDepth - 1].cname.c_str();
            int line = cstack[stackDepth - 1].tokid & 0x0000FFFF;
            profilerStackElement = std::make_tuple(filename, line);
            if (profilerStackElement != previousProfilerStackElement) {
                profilerStack.push_back(profilerStackElement);
                previousProfilerStackElement = profilerStackElement;
            }
            stackDepth--;
        }
    }
    std::reverse(profilerStack.begin(), profilerStack.end());
    return std::make_tuple(profilerStack, currentFunctionName, currentFilename, isBuiltin);
}
//=============================================================================
}
//=============================================================================
