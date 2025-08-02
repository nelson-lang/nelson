//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SVDDecomposition.hpp"
#include "SVDDecompositionComplexTemplate.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
solveSVDDecompositionDoubleComplex(const ArrayOf& matA, const ArrayOf& matB)
{
    return solveSVDDecompositionComplex<double>(NLS_DCOMPLEX, matA, matB);
}
//=============================================================================
}
//=============================================================================
