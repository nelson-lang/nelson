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
#include <algorithm>
#include "ProfilerHelpers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
internalProfileFunction
computeProfileStack(Evaluator* eval, const std::string& currentFunctionName,
    const std::wstring& currentFilename, bool isBuiltin)
{
    profileParentStack profilerStack;

    if (eval != nullptr) {
        std::vector<StackEntry> cstack = eval->cstack;
        std::tuple<std::string, uint64> previousProfilerStackElement;
        std::tuple<std::string, uint64> profilerStackElement;

        for (StackEntry entry : cstack) {
            std::string filename = entry.cname;
            if (filename != "evaluator" && filename != "EvaluateScript") {
                int line = entry.tokid & 0xffff;
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
    return std::make_tuple(profilerStack, currentFunctionName, currentFilename, isBuiltin);
}
//=============================================================================
}
//=============================================================================
