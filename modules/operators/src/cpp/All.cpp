//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include "All.hpp"
#include "Error.hpp"
#include "Exception.hpp"
#include "EmptyHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
All(ArrayOf& A, indexType dim, bool doOverAllElements, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    bool mustBeOverloaded = !(A.isNumeric() || A.isLogical() || A.isCharacterArray());
    if (mustBeOverloaded) {
        needToOverload = true;
        return {};
    }
    try {
        A.promoteType(NLS_LOGICAL);
    } catch (Exception&) {
        needToOverload = true;
        return {};
    }
    if (A.isEmpty()) {
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsRes = emptyDimensionsHelper(dimsA, dim);
        logical* logicalarray = (logical*)ArrayOf::allocateArrayOf(
            NLS_LOGICAL, dimsRes.getElementCount(), stringVector(), false);
        memset(logicalarray, 1, dimsRes.getElementCount());
        res = ArrayOf(NLS_LOGICAL, dimsRes, logicalarray);
    } else if (doOverAllElements) {
        bool bres = true;
        indexType elementCount = A.getElementCount();
        auto* pLogical = (logical*)A.getDataPointer();
        for (indexType k = 0; k < elementCount; k++) {
            if (!(pLogical[k] != 0)) {
                bres = false;
                break;
            }
        }
        res = ArrayOf::logicalConstructor(bres);
    } else if (A.isVector()) {
        auto* pLogical = (logical*)A.getDataPointer();
        bool bRes = true;
        indexType elementCount = A.getElementCount();
        for (indexType k = 0; k < elementCount; k++) {
            if (!(pLogical[k] != 0)) {
                bRes = false;
                break;
            }
        }
        res = ArrayOf::logicalConstructor(bRes);
    } else if (!A.isEmpty() && !A.isVector()) {
        Dimensions dims = A.getDimensions();
        indexType mA = dims.getRows();
        indexType nA = dims.getColumns();
        Eigen::Map<Eigen::Matrix<logical, Eigen::Dynamic, Eigen::Dynamic>> matA(
            (logical*)A.getDataPointer(), mA, nA);
        if ((dim == 0) || (dim == 1)) {
            logical* logicalarray
                = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, nA, stringVector(), false);
            Eigen::Map<Eigen::Matrix<logical, Eigen::Dynamic, Eigen::Dynamic>> matC(
                logicalarray, 1, nA);
            matC = matA.colwise().all().cast<logical>();
            res = ArrayOf(NLS_LOGICAL, Dimensions(1, nA), logicalarray);
        } else if (dim == 2) {
            logical* logicalarray = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, mA);
            Eigen::Map<Eigen::Matrix<logical, Eigen::Dynamic, Eigen::Dynamic>> matC(
                logicalarray, mA, 1);
            matC = matA.rowwise().all().cast<logical>();
            res = ArrayOf(NLS_LOGICAL, Dimensions(mA, 1), logicalarray);
        } else {
            res = A;
            res.ensureSingleOwner();
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
