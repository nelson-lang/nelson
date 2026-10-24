//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ClassdefParserDefinitionQuery.hpp"
#include <functional>
#include <unordered_set>
#include "ClassdefParserHelpers.hpp"
#include "ClassdefParserConstants.hpp"
#include "ClassdefParserRegistry.hpp"
#include "ClassName.hpp"
//=============================================================================
namespace Nelson::classdef_detail {
bool
definitionIsA(const ClassdefDefinition& definition, const std::string& expectedClassName)
{
    std::unordered_set<std::string> visited;
    std::function<bool(const ClassdefDefinition&)> visitDefinition
        = [&](const ClassdefDefinition& currentDefinition) -> bool {
        if (equalsIgnoreCase(currentDefinition.name, expectedClassName)) {
            return true;
        }
        if (equalsIgnoreCase(currentDefinition.name, CLASSDEF_DYNAMICPROPS_STR)
            && equalsIgnoreCase(expectedClassName, NLS_HANDLE_STR)) {
            return true;
        }
        if (visited.find(currentDefinition.name) != visited.end()) {
            return false;
        }
        visited.insert(currentDefinition.name);
        for (const auto& superclass : currentDefinition.superclasses) {
            const std::string trimmedClass = trim(superclass);
            if (trimmedClass.empty()) {
                continue;
            }
            if (equalsIgnoreCase(trimmedClass, expectedClassName)) {
                return true;
            }
            if (!classdefLoadClass(trimmedClass)) {
                continue;
            }
            const ClassdefDefinition* superclassDefinition = findClassdefDefinition(trimmedClass);
            if (superclassDefinition != nullptr && visitDefinition(*superclassDefinition)) {
                return true;
            }
        }
        return false;
    };
    return visitDefinition(definition);
}
//=============================================================================
bool
definitionIsHandleClass(const ClassdefDefinition& definition)
{
    return definitionIsA(definition, NLS_HANDLE_STR);
}
//=============================================================================
bool
definitionUsesCopyable(const ClassdefDefinition& definition)
{
    return definitionIsA(definition, copyableMixinName());
}
//=============================================================================
bool
definitionUsesCustomDisplay(const ClassdefDefinition& definition)
{
    return definitionIsA(definition, customDisplayMixinName());
}
//=============================================================================
bool
definitionUsesSetGet(const ClassdefDefinition& definition)
{
    return definitionIsA(definition, setGetMixinName());
}
//=============================================================================
bool
definitionUsesSetGetExactNames(const ClassdefDefinition& definition)
{
    return definitionIsA(definition, setGetExactNamesMixinName());
}
//=============================================================================
bool
definitionUsesRedefinesParen(const ClassdefDefinition& definition)
{
    return definitionIsA(definition, redefinesParenMixinName());
}
//=============================================================================
bool
definitionUsesRedefinesBrace(const ClassdefDefinition& definition)
{
    return definitionIsA(definition, redefinesBraceMixinName());
}
//=============================================================================
bool
definitionUsesRedefinesDot(const ClassdefDefinition& definition)
{
    return definitionIsA(definition, redefinesDotMixinName());
}
//=============================================================================
bool
definitionUsesScalar(const ClassdefDefinition& definition)
{
    return definitionIsA(definition, scalarMixinName());
}
//=============================================================================
bool
definitionUsesCompactDisplayProvider(const ClassdefDefinition& definition)
{
    return definitionIsA(definition, NLS_MIXIN_CUSTOM_COMPACT_DISPLAY_PROVIDER);
}
//=============================================================================
} // namespace Nelson::classdef_detail
//=============================================================================
