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
#include "nlsSparse_exports.h"
//=============================================================================
namespace Nelson {
NLSSPARSE_IMPEXP ArrayOf
SparseConstructor(indexType m, indexType n);
NLSSPARSE_IMPEXP ArrayOf
SparseConstructor(const ArrayOf& a);
NLSSPARSE_IMPEXP ArrayOf
SparseConstructor(ArrayOf I, ArrayOf J, ArrayOf V);
NLSSPARSE_IMPEXP ArrayOf
SparseConstructor(const ArrayOf& I, const ArrayOf& J, const ArrayOf& V, indexType m, indexType n);
NLSSPARSE_IMPEXP ArrayOf
SparseConstructor(
    const ArrayOf& I, const ArrayOf& J, const ArrayOf& V, indexType m, indexType n, indexType nnz);
} // namespace Nelson
//=============================================================================
