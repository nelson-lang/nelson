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
#include "SparseImagPart.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "SparseType.hpp"
#include "SparseComplexComponent.hpp"
#include <Eigen/Sparse>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
SparseImagPart(const ArrayOf& a)
{
    ArrayOf res;
    if (!a.isSparse()) {
        Error(L"Nelson:types:sparseExpected", L"Sparse expected.");
    }
    switch (a.dataClass()) {
    case NLS_DOUBLE:
    case NLS_LOGICAL: {
        indexType rows = a.dimensionLength(0);
        indexType cols = a.dimensionLength(1);
        try {
            Eigen::SparseMatrix<double, 0, signedIndexType>* spmat
                = new Eigen::SparseMatrix<double, 0, signedIndexType>(rows, cols);
            spmat->finalize();
            spmat->makeCompressed();
            void* pRes = (void*)spmat;
            res = ArrayOf(NLS_DOUBLE, a.dimensions(), pRes, true);
        } catch (const std::bad_alloc&) {
            Error(L"Nelson:outOfMemory",
                L"Memory allocation error... You may have run out of memory!");
        }
    } break;
    case NLS_SINGLE: {
        indexType rows = a.dimensionLength(0);
        indexType cols = a.dimensionLength(1);
        try {
            Eigen::SparseMatrix<single, 0, signedIndexType>* spmat
                = new Eigen::SparseMatrix<single, 0, signedIndexType>(rows, cols);
            spmat->finalize();
            spmat->makeCompressed();
            void* pRes = (void*)spmat;
            res = ArrayOf(NLS_SINGLE, a.dimensions(), pRes, true);
        } catch (const std::bad_alloc&) {
            Error(L"Nelson:outOfMemory",
                L"Memory allocation error... You may have run out of memory!");
        }
    } break;
    case NLS_DCOMPLEX: {
        return sparseComplexComponent<doublecomplex, double>(
            a, NLS_DOUBLE, [](const doublecomplex& z) { return z.imag(); });
    } break;
    case NLS_SCOMPLEX: {
        return sparseComplexComponent<singlecomplex, single>(
            a, NLS_SINGLE, [](const singlecomplex& z) { return z.imag(); });
    } break;
    default: {
        Error("Nelson:types:imagUnsupportedType", "Cannot do imag with current type '%s'.",
            ClassName(a));
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
