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
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Argument helpers shared by the interp1, interp2 and interp3 builtins.
//=============================================================================
// char array or scalar string, as accepted for a method name
bool
isInterpolationText(const ArrayOf& value);
//=============================================================================
// reads a row char vector or a non-missing scalar string
bool
readInterpolationText(const ArrayOf& value, std::wstring& text);
//=============================================================================
// Trailing '..., method' or '..., method, extrapval' of interp2/interp3.
// count is reduced by the consumed arguments. Returns false when the method
// cannot be read or is not linear, nearest, cubic (v5cubic), makima or spline.
bool
parseGriddedInterpolationTail(const ArrayOfVector& argIn, size_t& count, std::wstring& method,
    bool& useConstant, ArrayOf& extrapolationValue);
//=============================================================================
ArrayOfVector
callInterpolationFunction(
    Evaluator* eval, const std::string& name, const ArrayOfVector& args, int nLhs);
//=============================================================================
} // namespace Nelson
//=============================================================================
