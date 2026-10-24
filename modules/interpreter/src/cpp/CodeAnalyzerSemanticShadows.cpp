//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CodeAnalyzerSemanticShadows.hpp"
#include "BuiltInFunctionDefManager.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
const std::set<std::string>&
codeAnalyzerBuiltinFunctionNames()
{
    static const std::set<std::string> names = []() {
        std::set<std::string> result = { "abs", "disp", "error", "length", "load", "max", "mean",
            "min", "ones", "plot", "size", "sum", "warning", "zeros" };
        BuiltInFunctionDefManager* manager = BuiltInFunctionDefManager::getInstance();
        if (manager != nullptr) {
            stringVector runtimeNames = manager->getNameList();
            result.insert(runtimeNames.begin(), runtimeNames.end());
        }
        return result;
    }();
    return names;
}
//=============================================================================
std::set<std::string>
codeAnalyzerClassPropertyNamesFor(
    const CodeAnalyzerFunction& function, const std::vector<CodeAnalyzerClassdef>& classdefs)
{
    std::set<std::string> names;
    if (!function.classdefMethod || function.classdefOwnerName.empty()) {
        return names;
    }
    for (const CodeAnalyzerClassdef& classdef : classdefs) {
        if (classdef.name != function.classdefOwnerName) {
            continue;
        }
        for (const CodeAnalyzerClassProperty& property : classdef.properties) {
            names.insert(property.name);
        }
        break;
    }
    return names;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
