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
#include "nlsLinear_algebra_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP
bool
GeneralizedEigenDecompositionCompactSymmetric(const ArrayOf& A, const ArrayOf& B, ArrayOf& D,
    bool& needToOverload, std::wstring& errorMessage);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP
bool
GeneralizedEigenDecompositionFullSymmetric(const ArrayOf& A, const ArrayOf& B, ArrayOf& V,
    ArrayOf& D, bool& needToOverload, std::wstring& errorMessage);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP
bool
GeneralizedEigenDecompositionFullGeneral(const ArrayOf& A, const ArrayOf& B, ArrayOf& V, ArrayOf& D,
    bool& needToOverload, std::wstring& errorMessage);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP
bool
GeneralizedEigenDecompositionCompactGeneral(const ArrayOf& A, const ArrayOf& B, ArrayOf& D,
    bool& needToOverload, std::wstring& errorMessage);
//=============================================================================
}
//=============================================================================
