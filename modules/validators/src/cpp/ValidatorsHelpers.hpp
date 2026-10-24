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
//=============================================================================
// Helpers shared by the validators implementation files (not exported).
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
invalidPositionMessage(int argPosition);
//=============================================================================
ArrayOfVector
evaluateFunction(const ArrayOfVector& args, int nLhs, const std::string& functionName);
//=============================================================================
// true when every element of op(arg, c) is true (implicit expansion applies);
// an empty arg is always accepted.
bool
allCompare(const ArrayOf& arg, const ArrayOf& c, const std::string& operatorName);
//=============================================================================
} // namespace Nelson
//=============================================================================
