//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ClassdefParserRegistry.hpp"
//=============================================================================
namespace Nelson::classdef_detail {
std::unordered_map<std::string, ClassdefDefinition>&
classdefDefinitions()
{
    static std::unordered_map<std::string, ClassdefDefinition> definitions;
    return definitions;
}
//=============================================================================
size_t&
classdefGenerationCounter()
{
    static size_t generation = 1;
    return generation;
}
//=============================================================================
std::unordered_map<std::string, ClassdefDefinition>&
builtinClassdefDefinitions()
{
    static std::unordered_map<std::string, ClassdefDefinition> definitions;
    return definitions;
}
//=============================================================================
const ClassdefDefinition*
findClassdefDefinition(const std::string& className)
{
    const auto classdefIt = classdefDefinitions().find(className);
    if (classdefIt != classdefDefinitions().end()) {
        return &classdefIt->second;
    }
    const auto builtinIt = builtinClassdefDefinitions().find(className);
    if (builtinIt != builtinClassdefDefinitions().end()) {
        return &builtinIt->second;
    }
    return nullptr;
}
//=============================================================================
bool
isBuiltinClassdef(const std::string& className)
{
    return builtinClassdefDefinitions().find(className) != builtinClassdefDefinitions().end();
}
//=============================================================================
std::unordered_map<std::string, std::wstring>&
classdefFilenames()
{
    static std::unordered_map<std::string, std::wstring> filenames;
    return filenames;
}
//=============================================================================
std::unordered_map<std::string, ClassdefLayoutPtr>&
classdefLayouts()
{
    static std::unordered_map<std::string, ClassdefLayoutPtr> layouts;
    return layouts;
}
//=============================================================================
// Values of property default expressions, evaluated once per defining class
// and shared by every instance. Entries are dropped when their class is
// reloaded or the registry is cleared, not on unrelated registrations, so
// loading another class never re-triggers a default expression.
std::unordered_map<std::string, std::unordered_map<std::string, ArrayOf>>&
classdefPropertyDefaultValues()
{
    static std::unordered_map<std::string, std::unordered_map<std::string, ArrayOf>> values;
    return values;
}
//=============================================================================
} // namespace Nelson::classdef_detail
//=============================================================================
