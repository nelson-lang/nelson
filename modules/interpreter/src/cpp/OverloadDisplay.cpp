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
#include "OverloadDisplay.hpp"
#include "ClassName.hpp"
#include "DisplayVariable.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "OverloadRequired.hpp"
#include "characters_encoding.hpp"
#include "Profiler.hpp"
#include "ProfilerHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
OverloadDisplay(Evaluator* eval, const ArrayOf& a, const std::wstring& name)
{
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        Context* context = eval->getContext();
        if (context != nullptr) {
            FunctionDef* funcDef = nullptr;
            std::string OverloadName = ClassName(a) + "_display";
            if (context->lookupFunction(OverloadName, funcDef)) {
                bSuccess = true;
                ArrayOfVector argsIn;
                argsIn.push_back(a);
                argsIn.push_back(ArrayOf::characterArrayConstructor(name));
                int nargout = 0;
                funcDef->evaluateFunction(eval, argsIn, nargout);
            }
        }
    }
    if (!bSuccess) {
        bool needToOverload;
        uint64 ticProfile = Profiler::getInstance()->tic();
        DisplayVariable(eval->getInterface(), a, name, needToOverload);
        if (ticProfile != 0U) {
            internalProfileFunction stack = computeProfileStack(eval, "display", L"evaluator");
            Profiler::getInstance()->toc(ticProfile, stack);
        }
        if (needToOverload) {
            Context* context = eval->getContext();
            if (context != nullptr) {
                FunctionDef* funcDef = nullptr;
                std::string OverloadName = ClassName(a) + "_display";
                if (context->lookupFunction(OverloadName, funcDef)) {
                    bSuccess = true;
                    ArrayOfVector argsIn;
                    argsIn.push_back(a);
                    argsIn.push_back(ArrayOf::characterArrayConstructor(name));
                    int nargout = 0;
                    funcDef->evaluateFunction(eval, argsIn, nargout);
                } else {
                    Error(utf8_to_wstring(
                        _("function") + " " + OverloadName + " " + _("undefined.")));
                }
            }
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
