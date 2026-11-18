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
#include "BuiltInFunctionDef.hpp"
#include "ModulesManager.hpp"
#include "Error.hpp"
#include "NelsonConfiguration.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "characters_encoding.hpp"
#include <algorithm>
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
    return PathFunctionIndexerManager::getInstance()->getMacrosList(false);
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
static std::wstring
normalizeSeparators(const std::wstring& path)
{
    std::wstring out = path;
    std::replace(out.begin(), out.end(), L'\\', L'/');
    return out;
}
//=============================================================================
bool
WhatIsModule(const std::wstring& moduleName)
{
    return ModulesManager::Instance().isModule(moduleName);
}
//=============================================================================
wstringVector
WhatListOfBuiltin(Evaluator* eval, const std::wstring& moduleName, bool bSorted)
{
    wstringVector functionsList;
    std::vector<FunctionDefPtr> table = BuiltInFunctionDefManager::getInstance()->getTable();
    for (auto& def : table) {
        if (def == nullptr || def->type() != NLS_BUILT_IN_FUNCTION) {
            continue;
        }
        auto* builtinDef = static_cast<BuiltInFunctionDef*>(def);
        if (builtinDef->moduleName != moduleName) {
            continue;
        }
        std::string name = def->getName();
        if (name.empty() || name[0] == '_') {
            continue;
        }
        functionsList.push_back(utf8_to_wstring(name));
    }
    if (bSorted && !functionsList.empty()) {
        std::sort(functionsList.begin(), functionsList.end());
        functionsList.erase(
            std::unique(functionsList.begin(), functionsList.end()), functionsList.end());
    }
    return functionsList;
}
//=============================================================================
wstringVector
WhatListOfMacro(Evaluator* eval, const std::wstring& moduleName)
{
    wstringVector macroList;
    std::wstring moduleRoot;
    if (!ModulesManager::Instance().findModule(moduleName, moduleRoot)) {
        return macroList;
    }
    std::wstring prefix = normalizeSeparators(moduleRoot);
    if (!prefix.empty() && prefix.back() != L'/') {
        prefix.push_back(L'/');
    }
    PathFunctionIndexerManager* manager = PathFunctionIndexerManager::getInstance();
    wstringVector allMacros = manager->getMacrosList(false);
    for (auto& macroName : allMacros) {
        std::wstring filename;
        if (!manager->find(wstring_to_utf8(macroName), filename)) {
            continue;
        }
        std::wstring normalized = normalizeSeparators(filename);
        if (normalized.size() >= prefix.size()
            && normalized.compare(0, prefix.size(), prefix) == 0) {
            macroList.push_back(macroName);
        }
    }
    std::sort(macroList.begin(), macroList.end());
    macroList.erase(std::unique(macroList.begin(), macroList.end()), macroList.end());
    return macroList;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
