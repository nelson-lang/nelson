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
#include "ArrayOf.hpp"
#include "ClassName.hpp"
#include "ClassdefConcatenationHelpers.hpp"
#include "ClassdefHandleObject.hpp"
#include "ClassdefParserConstants.hpp"
#include "HandleState.hpp"
#include "Operators.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// A handle created from a user classdef (not a graphics, COM, python,
// containers.Map, listener, ... native handle).
inline bool
isClassdefHandleObject(const ArrayOf& value)
{
    if (!value.isHandle() || value.isEmpty()) {
        return false;
    }
    const auto* refs = static_cast<const HandleRef*>(value.dataPointer());
    return refs != nullptr && refs[0]
        && dynamic_cast<const ClassdefHandleObject*>(refs[0]->object()) != nullptr;
}
//=============================================================================
// The indexing functions act on their first argument only: subsasgn(x, S, obj)
// assigns obj into x.
inline bool
dispatchesOnFirstArgumentOnly(const std::string& functionName)
{
    return functionName == SUBSASGN_OPERATOR_STR || functionName == SUBSREF_OPERATOR_STR
        || functionName == SUBSINDEX_OPERATOR_STR;
}
//=============================================================================
// Accessors generated for every handle class (registerHandleSupport): a handle
// passed as data, as in struct('a', h) or set(gcf, 'UserData', h), must keep
// reaching the builtin.
inline bool
isGeneratedHandleAccessor(const std::string& functionName)
{
    return functionName == CLASSDEF_FUNCTION_GET || functionName == CLASSDEF_FUNCTION_SET
        || functionName == CLASSDEF_FUNCTION_ISVALID || functionName == CLASSDEF_FUNCTION_FIELDNAMES
        || functionName == CLASSDEF_FUNCTION_STRUCT || functionName == CLASSDEF_FUNCTION_DELETE;
}
//=============================================================================
// True when an argument may select a function overload: a call bypassing the
// regular dispatch (e.g. a direct self-recursive bytecode call) must not be
// taken.
inline bool
hasOverloadDispatchCandidate(const ArrayOf* args, size_t nargs)
{
    for (size_t k = 0; k < nargs; ++k) {
        if (args[k].isClassType() || args[k].isHandle()) {
            return true;
        }
    }
    return false;
}
//=============================================================================
// Index of the argument whose class selects a function overload. A class
// object dominates the builtin types, so the dispatch class is the dominant
// class among all the object arguments (e.g. intersect(5, T) calls the table
// method): a class listed in the InferiorClasses of another loses to it,
// otherwise the leftmost object wins. A native handle in first position keeps
// selecting its own methods, as does a graphics object (set(h, 'P', obj));
// in later positions only user classdef objects are considered.
inline size_t
overloadDispatchArgumentIndex(const ArrayOfVector& argsIn, const std::string& functionName)
{
    if (argsIn.empty()) {
        return 0;
    }
    const ArrayOf& first = argsIn[0];
    if (first.isGraphicsObject() || (first.isHandle() && !isClassdefHandleObject(first))) {
        return 0;
    }
    constexpr size_t noObject = static_cast<size_t>(-1);
    size_t selected = (first.isClassType() || first.isHandle()) ? 0 : noObject;
    std::string selectedClass;
    for (size_t k = 1; k < argsIn.size(); ++k) {
        const ArrayOf& arg = argsIn[k];
        const bool isHandleObject = arg.isHandle() && isClassdefHandleObject(arg);
        if (!arg.isClassType() && !isHandleObject) {
            continue;
        }
        if (dispatchesOnFirstArgumentOnly(functionName)) {
            return 0;
        }
        if (isHandleObject && isGeneratedHandleAccessor(functionName)) {
            continue;
        }
        if (selected == noObject) {
            selected = k;
            continue;
        }
        // InferiorClasses is a classdef attribute: other classes keep the
        // leftmost rule without probing the class definitions.
        const ArrayOf& current = argsIn[selected];
        if (!(arg.isClassdefType() || isHandleObject)
            || !(current.isClassdefType() || current.isHandle())) {
            continue;
        }
        if (selectedClass.empty()) {
            selectedClass = ClassName(current);
        }
        const std::string candidateClass = ClassName(arg);
        if (candidateClass != selectedClass
            && ClassdefConcatenationHelpers::classdefDominates(candidateClass, selectedClass)) {
            selected = k;
            selectedClass = candidateClass;
        }
    }
    return selected == noObject ? 0 : selected;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
