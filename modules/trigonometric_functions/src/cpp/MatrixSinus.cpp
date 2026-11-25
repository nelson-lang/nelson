//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <complex>
#include "MatrixSinus.hpp"
#include "MatrixTrigonometricFunction.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
MatrixSin(const ArrayOf& A, bool& needToOverload)
{
    return matrixTrigonometricDispatch(
        A, needToOverload, [](const auto& z) { return std::sin(z); });
}
//=============================================================================
} // namespace Nelson
//=============================================================================
