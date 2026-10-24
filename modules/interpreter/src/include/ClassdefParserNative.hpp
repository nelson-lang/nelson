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
#include "Types.hpp"
#include "ClassdefParser.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
// Recursive-descent classdef parser over the native token stream.
//=============================================================================
namespace Nelson::classdef_detail {
//=============================================================================
// Parse the class name and superclass list from classdef source using the
// native tokenizer. Returns false with errorMessage set when the header is not
// a well-formed "classdef [(attrs)] Name [< A & B.C ...]".
NLSINTERPRETER_IMPEXP bool
parseClassdefHeaderNative(const std::string& source, std::string& name, stringVector& superclasses,
    std::string& errorMessage);
//=============================================================================
// Structural skeleton of a classdef, as recovered from the native token stream:
// the class name, superclasses, and the ordered member names of each block.
struct ClassdefSkeletonNative
{
    std::string name;
    stringVector superclasses;
    stringVector classAttributes;
    stringVector properties;
    // Parallel to properties: the default expression and the validation
    // (size/class/validators) source of each property, captured from the token
    // offsets and folded with the same helpers as the text scanner.
    stringVector propertyDefaults;
    stringVector propertyValidations;
    // Parallel to properties: the ";"-joined block attributes each inherits.
    stringVector propertyAttributes;
    stringVector methods;
    // Parallel to methods: block attributes, the static flag ("0"/"1"), and the
    // normalized method source (empty for abstract declarations).
    stringVector methodAttributes;
    stringVector methodStatic;
    stringVector methodSources;
    // Parallel to methods: 1-based source line where each method begins.
    std::vector<size_t> methodSourceStartLines;
    // Source of the local functions that follow the closing classdef 'end'.
    std::string localFunctionsSource;
    stringVector events;
    stringVector eventAttributes;
    stringVector enumerations;
    // Parallel to enumerations: the argument list source of each member.
    stringVector enumerationArguments;
};
//=============================================================================
// Parse the classdef structure (header plus the properties/methods/events/
// enumeration blocks) recovering the member names. Returns false with
// errorMessage set on a structural error.
NLSINTERPRETER_IMPEXP bool
parseClassdefSkeletonNative(
    const std::string& source, ClassdefSkeletonNative& skeleton, std::string& errorMessage);
//=============================================================================
// Parse a complete ClassdefDefinition (all fields) from classdef source using
// the native token-driven parser.
NLSINTERPRETER_IMPEXP bool
parseClassdefSourceNativeFull(
    const std::string& source, ClassdefDefinition& definition, std::string& errorMessage);
//=============================================================================
} // namespace Nelson::classdef_detail
//=============================================================================
