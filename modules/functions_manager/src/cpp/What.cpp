//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "What.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "Error.hpp"
#include "NelsonConfiguration.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
wstringVector
WhatListOfBuiltin(Evaluator* eval, bool bWithPrivateFunction, bool bSorted)
{
    wstringVector functionsList;
    stringVector funcs = BuiltInFunctionDefManager::getInstance()->getNameList();
    for (auto& func : funcs) {
        if (bWithPrivateFunction) {
            functionsList.push_back(utf8_to_wstring(func));
        } else {
            if (func[0] != '_') {
                functionsList.push_back(utf8_to_wstring(func));
            }
        }
    }
    if (bSorted) {
        if (!functionsList.empty()) {
            std::sort(functionsList.begin(), functionsList.end());
        }
    }
    return functionsList;
}
//=============================================================================
wstringVector
WhatListOfBuiltin(bool bSorted)
{
    wstringVector functionsList;
    auto* eval = static_cast<Evaluator*>(NelsonConfiguration::getInstance()->getMainEvaluator());
    if (eval != nullptr) {
        functionsList = WhatListOfBuiltin(eval, bSorted);
    }
    return functionsList;
}
//=============================================================================
wstringVector
WhatListOfMacro(Evaluator* eval)
{
    return PathFunctionIndexerManager::getInstance()->getMacrosList();
}
//=============================================================================
wstringVector
WhatListOfMacro()
{
    wstringVector macroList;
    auto* eval = static_cast<Evaluator*>(NelsonConfiguration::getInstance()->getMainEvaluator());
    if (eval != nullptr) {
        macroList = WhatListOfMacro(eval);
    }
    return macroList;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
