//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
OverloadDisplay(Evaluator* eval, const ArrayOf& a, const std::wstring& name, bool asDispBuiltin)
{
    std::string OverloadName;
    if (asDispBuiltin) {
        OverloadName = ClassName(a) + "_disp";
    } else {
        OverloadName = ClassName(a) + "_display";
    }
    Context* context = eval->getContext();
    FunctionDef* funcDef = nullptr;
    bool bSuccess = false;
    if (context->lookupFunction(OverloadName, funcDef)) {
        bSuccess = true;
        ArrayOfVector argsIn;
        argsIn.push_back(a);
        if (!asDispBuiltin) {
            argsIn.push_back(ArrayOf::characterArrayConstructor(name));
        }
        int nargout = 0;
        funcDef->evaluateFunction(eval, argsIn, nargout);
    }

    if (!bSuccess) {
        bool needToOverload;
        uint64 ticProfile = Profiler::getInstance()->tic();
        DisplayVariable(
            eval->getID(), eval->getInterface(), a, name, asDispBuiltin, needToOverload);
        if (ticProfile != 0U) {
            internalProfileFunction stack
                = computeProfileStack(eval, asDispBuiltin ? "disp" : "display", L"evaluator");
            Profiler::getInstance()->toc(ticProfile, stack);
        }

        if (needToOverload) {
            Error(utf8_to_wstring(_("function") + " " + OverloadName + " " + _("undefined.")));
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
