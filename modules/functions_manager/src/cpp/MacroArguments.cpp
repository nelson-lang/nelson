//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "MacroArguments.hpp"
#include "MacroFunctionDef.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
MacroArguments(Evaluator* eval, const std::wstring& functionname, wstringVector& Inputs,
    wstringVector& Outputs)
{
    std::string fun = wstring_to_utf8(functionname);
    Context* ctx = eval->getContext();
    FunctionDef* funcDef = nullptr;
    bool isFun = ctx->lookupFunction(fun, funcDef);
    if (isFun) {
        if (funcDef->type() == NLS_MACRO_FUNCTION) {
            auto* valc = (MacroFunctionDef*)funcDef;
            for (const auto& returnVal : valc->returnVals) {
                Inputs.push_back(utf8_to_wstring(returnVal));
            }
            for (const auto& argument : valc->arguments) {
                Outputs.push_back(utf8_to_wstring(argument));
            }
            return true;
        }
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
