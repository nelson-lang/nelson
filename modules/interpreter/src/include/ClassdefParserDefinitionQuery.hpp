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
#include "ClassdefParser.hpp"
//=============================================================================
namespace Nelson::classdef_detail {
//=============================================================================
bool
definitionIsA(const ClassdefDefinition& definition, const std::string& expectedClassName);
bool
definitionIsHandleClass(const ClassdefDefinition& definition);
bool
definitionUsesCopyable(const ClassdefDefinition& definition);
bool
definitionUsesCustomDisplay(const ClassdefDefinition& definition);
bool
definitionUsesSetGet(const ClassdefDefinition& definition);
bool
definitionUsesSetGetExactNames(const ClassdefDefinition& definition);
bool
definitionUsesRedefinesParen(const ClassdefDefinition& definition);
bool
definitionUsesRedefinesBrace(const ClassdefDefinition& definition);
bool
definitionUsesRedefinesDot(const ClassdefDefinition& definition);
bool
definitionUsesScalar(const ClassdefDefinition& definition);
bool
definitionUsesCompactDisplayProvider(const ClassdefDefinition& definition);
//=============================================================================
} // namespace Nelson::classdef_detail
//=============================================================================
