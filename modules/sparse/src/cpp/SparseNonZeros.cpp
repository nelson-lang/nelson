//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _SCL_SECURE_NO_WARNINGS
//=============================================================================
#include "SparseNonZeros.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include <Eigen/Sparse>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
static indexType
sparseNonZerosOf(const ArrayOf& a)
{
    auto* spmat = (Eigen::SparseMatrix<T, 0, signedIndexType>*)a.sparseDataPointer();
    return spmat ? spmat->nonZeros() : 0;
}
//=============================================================================
indexType
SparseNonZeros(const ArrayOf& a)
{
    if (!a.isSparse()) {
        Error(L"Nelson:types:typeNotManaged", L"Type not supported.");
    }
    switch (a.dataClass()) {
    case NLS_LOGICAL:
        return sparseNonZerosOf<logical>(a);
    case NLS_DOUBLE:
        return sparseNonZerosOf<double>(a);
    case NLS_SINGLE:
        return sparseNonZerosOf<single>(a);
    case NLS_DCOMPLEX:
        return sparseNonZerosOf<doublecomplex>(a);
    case NLS_SCOMPLEX:
        return sparseNonZerosOf<singlecomplex>(a);
    default:
        Error(L"Nelson:types:typeNotManaged", L"Type not supported.");
    }
    return 0;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
