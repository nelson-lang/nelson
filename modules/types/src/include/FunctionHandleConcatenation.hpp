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
#include "ArrayOf.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
//=============================================================================
// Class rules shared by every concatenation of function handles (operators
// and cat).
//=============================================================================
namespace Nelson {
//=============================================================================
// Function handles cannot form nonscalar arrays.
inline void
raiseNonScalarFunctionHandles()
{
    Error("Nelson:err_non_scalar_function_handles",
        "Nonscalar arrays of function handles are not allowed; use cell arrays instead.");
}
//=============================================================================
// A nonempty operand of another class cannot become a function handle: the
// first one in argument order names the error, before any size rule.
inline void
checkFunctionHandleConcatenationOperands(const ArrayOfVector& args)
{
    for (const ArrayOf& arg : args) {
        if (!arg.isEmpty() && !arg.isFunctionHandle()) {
            Error("Nelson:UnableToConvertToFunctionHandle",
                "Conversion to function_handle from %s is not possible.", ClassName(arg));
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
