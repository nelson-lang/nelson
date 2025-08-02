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
#include "nlsTypes_exports.h"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSTYPES_IMPEXP
bool
FindCommonType(const ArrayOfVector& argIn, NelsonType& commonType, bool& isSparse, bool& isComplex,
    std::string& typeName);
//=============================================================================
NLSTYPES_IMPEXP
bool
FindCommonTypeRelationalOperators(const ArrayOfVector& args, NelsonType& commonType, bool& isSparse,
    bool& isComplex, std::string& typeName);
//=============================================================================
}
//=============================================================================
