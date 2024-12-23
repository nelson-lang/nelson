//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Which.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "Error.hpp"
#include "GatewayInfo.hpp"
#include "ModulesManager.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
wstringVector
WhichAll(const std::string& functionname)
{
    wstringVector result;
    PathFunctionIndexerManager::getInstance()->find(functionname, result);
    wstringVector builtinFunctions;
    BuiltInFunctionDefManager::getInstance()->find(functionname, builtinFunctions);
    result.insert(result.end(), builtinFunctions.begin(), builtinFunctions.end());
    return result;
}
//=============================================================================
std::wstring
Which(const std::string& functionname)
{
    std::wstring origin;
    if (!PathFunctionIndexerManager::getInstance()->find(functionname, origin)) {
        BuiltInFunctionDefManager::getInstance()->find(functionname, origin);
    }
    return origin;
}
//=============================================================================
wstringVector
WhichModule(const std::string& functionname)
{
    wstringVector res;
    wstringVector paths = WhichAll(functionname);
    if (!paths.empty()) {
        for (const auto& path : paths) {
            std::wstring moduleName = ModulesManager::Instance().findModuleNameByPath(path);
            if (!moduleName.empty()) {
                res.push_back(moduleName);
            } else {
                stringVector functionsList;
                std::wstring errorMessage;
                bool bRes = GatewayInfo(path, moduleName, functionsList, errorMessage);
                if (bRes) {
                    res.push_back(moduleName);
                } else {
                    res.push_back(L"");
                }
            }
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
