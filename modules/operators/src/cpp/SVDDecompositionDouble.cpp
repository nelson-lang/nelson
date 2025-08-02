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
#include "SVDDecompositionRealTemplate.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
solveSVDDecompositionDouble(const ArrayOf& matA, const ArrayOf& matB)
{
    return solveSVDDecompositionReal<double>(NLS_DOUBLE, matA, matB);
}
//=============================================================================
}
//=============================================================================
