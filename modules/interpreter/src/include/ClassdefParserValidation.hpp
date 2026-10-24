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
#include <algorithm>
#include <string>
#include <vector>
#include "ClassdefParser.hpp"
#include "ClassdefParserHelpers.hpp"
//=============================================================================
namespace Nelson::classdef_detail {
//=============================================================================
bool
isCallableFromClassdefContext(const std::string& definingClassName, const stringVector& attributes,
    const std::string& callerClassName);
bool
isVisibleMember(const stringVector& attributes);
bool
isVisibleProperty(const stringVector& attributes);
bool
isVisibleEvent(const stringVector& attributes);
void
appendUnique(stringVector& values, const std::string& value);
//=============================================================================
// Template helpers must be defined in the header so other translation units can
// instantiate them.
// Member names (properties, methods, events, enumerations) are case-sensitive,
// so members differing only in letter case (for example a private 'foo' and a
// dependent 'Foo') are distinct and must both be kept; only an exact-case name
// shadows or overrides an inherited member.
template <typename T>
void
appendUniqueByName(std::vector<T>& values, const T& value)
{
    auto found = std::find_if(values.begin(), values.end(),
        [&value](const T& current) { return current.name == value.name; });
    if (found == values.end()) {
        values.push_back(value);
    }
}
//=============================================================================
template <typename T>
void
replaceOrAppendByName(std::vector<T>& values, const T& value)
{
    auto found = std::find_if(values.begin(), values.end(),
        [&value](const T& current) { return current.name == value.name; });
    if (found == values.end()) {
        values.push_back(value);
    } else {
        *found = value;
    }
}
//=============================================================================
bool
functionExistsInMemoryOnly(const std::string& functionName);
bool
functionExistsInMemoryOrPath(const std::string& functionName);
std::string
inheritedMemberOwner(const std::string& fallbackClassName, const std::string& definingClassName);
bool
isInheritedMethodConflictCandidate(
    const ClassdefMethodDefinition& method, const std::string& ownerClassName);
void
validateDuplicateProperties(const ClassdefDefinition& definition);
void
validateDuplicateMethods(const ClassdefDefinition& definition);
void
validateDuplicateEnumerations(const ClassdefDefinition& definition);
void
enumerationMemberConflictName(const ClassdefDefinition& definition, std::string& conflictName);
void
validateEnumerationMemberConflicts(const ClassdefDefinition& definition);
void
validateInheritedPropertyConflicts(const ClassdefDefinition& definition);
void
validateInheritedMethodConflicts(const ClassdefDefinition& definition);
void
validateClassAttributes(const ClassdefDefinition& definition);
//=============================================================================
} // namespace Nelson::classdef_detail
//=============================================================================
