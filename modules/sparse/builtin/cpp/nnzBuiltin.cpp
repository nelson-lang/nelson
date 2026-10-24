//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nnzBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadHelpers.hpp"
#include "OverloadRequired.hpp"
#include <Eigen/Sparse>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
template <class T>
double
sparseActualNnz(const ArrayOf& value)
{
    const auto* matrix
        = static_cast<const Eigen::SparseMatrix<T, 0, signedIndexType>*>(value.sparseDataPointer());
    if (matrix == nullptr) {
        return 0;
    }
    double count = 0;
    for (signedIndexType col = 0; col < matrix->outerSize(); ++col) {
        for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(*matrix, col);
            it; ++it) {
            if (it.value() != T(0)) {
                ++count;
            }
        }
    }
    return count;
}
//=============================================================================
double
sparseActualNnz(const ArrayOf& value)
{
    switch (value.dataClass()) {
    case NLS_LOGICAL:
        return sparseActualNnz<logical>(value);
    case NLS_DOUBLE:
        return sparseActualNnz<double>(value);
    case NLS_SINGLE:
        return sparseActualNnz<single>(value);
    case NLS_DCOMPLEX:
        return sparseActualNnz<doublecomplex>(value);
    case NLS_SCOMPLEX:
        return sparseActualNnz<singlecomplex>(value);
    default:
        OverloadRequired("nnz");
    }
    return 0;
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::SparseGateway::nnzBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);

    switch (argIn[0].dataClass()) {
    case NLS_LOGICAL:
    case NLS_INT8:
    case NLS_UINT8:
    case NLS_CHAR:
    case NLS_INT16:
    case NLS_UINT16:
    case NLS_INT32:
    case NLS_UINT32:
    case NLS_INT64:
    case NLS_UINT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
        if (argIn[0].isSparse()) {
            return ArrayOf::doubleScalar(sparseActualNnz(argIn[0]));
        }
        return ArrayOf::doubleScalar(static_cast<double>(argIn[0].nnz()));
        break;
    default: {
        OverloadRequired("nnz");
    } break;
    }
    return {};
}
//=============================================================================
