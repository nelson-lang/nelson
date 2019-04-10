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
OverloadDisplay(Evaluator* eval, ArrayOf a, bool fromBuiltin)
{
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        Context* context = eval->getContext();
        if (context != nullptr) {
            FunctionDef* funcDef = nullptr;
            std::string OverloadName = ClassName(a) + "_disp";
            if (context->lookupFunction(OverloadName, funcDef)) {
                bSuccess = true;
                ArrayOfVector argsIn;
                argsIn.push_back(a);
                int nargout = 0;
                funcDef->evaluateFunction(eval, argsIn, nargout);
            }
        }
    }
    if (!bSuccess) {
        bool needToOverload;
        uint64 ticProfile = Profiler::getInstance()->tic();
        DisplayVariable(eval->getInterface(), a, fromBuiltin, needToOverload);
        if (ticProfile) {
            internalProfileFunction stack = computeProfileStack(eval, "disp", L"evaluator");
            Profiler::getInstance()->toc(ticProfile, stack);
        }
        if (needToOverload) {
            Context* context = eval->getContext();
            if (context != nullptr) {
                FunctionDef* funcDef = nullptr;
                std::string OverloadName = ClassName(a) + "_disp";
                if (context->lookupFunction(OverloadName, funcDef)) {
                    bSuccess = true;
                    ArrayOfVector argsIn;
                    argsIn.push_back(a);
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
