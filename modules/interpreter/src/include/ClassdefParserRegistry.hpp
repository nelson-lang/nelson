//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include <unordered_map>
#include "ClassdefParser.hpp"
#include "ClassdefLayout.hpp"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson::classdef_detail {
//=============================================================================
std::unordered_map<std::string, ClassdefDefinition>&
classdefDefinitions();
size_t&
classdefGenerationCounter();
std::unordered_map<std::string, ClassdefDefinition>&
builtinClassdefDefinitions();
const ClassdefDefinition*
findClassdefDefinition(const std::string& className);
bool
isBuiltinClassdef(const std::string& className);
// merged (inherited) property definition with the attributes the access paths
// query, resolved once instead of re-parsing the attribute list per query
struct ClassdefResolvedProperty
{
    ClassdefPropertyDefinition definition;
    bool isConstant = false;
    bool isDependent = false;
    bool isGetObservable = false;
    bool isSetObservable = false;
    std::string getAccess;
    std::string setAccess;
};
//=============================================================================
// memoized per class; the pointer is valid until the classdef registry
// changes (do not keep it across code that may load or clear classes)
const ClassdefResolvedProperty*
findClassdefResolvedProperty(const std::string& className, const std::string& propertyName);
std::unordered_map<std::string, std::wstring>&
classdefFilenames();
std::unordered_map<std::string, ClassdefLayoutPtr>&
classdefLayouts();
std::unordered_map<std::string, std::unordered_map<std::string, ArrayOf>>&
classdefPropertyDefaultValues();
//=============================================================================
} // namespace Nelson::classdef_detail
//=============================================================================
