//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <Eigen/Dense>
#include "Any.hpp"
#include "Error.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Any(ArrayOf& A, indexType dim, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    try {
        A.promoteType(NLS_LOGICAL);
    } catch (Exception&) {
        needToOverload = true;
        return ArrayOf();
    }
    if (A.isEmpty()) {
        if (dim == 0) {
            Dimensions dims2Dzeros(0, 0);
            Dimensions dimsA = A.getDimensions();
            if (dimsA.equals(dims2Dzeros)) {
                res = ArrayOf::logicalConstructor(false);
            } else {
                if (dimsA.getRows() > dimsA.getColumns()) {
                    Dimensions dims(1, 0);
                    res = ArrayOf::emptyConstructor(dims);
                    res.promoteType(NLS_LOGICAL);
                } else {
                    logical* logicalarray = (logical*)ArrayOf::allocateArrayOf(
                        NLS_LOGICAL, dimsA.getColumns(), stringVector(), true);
                    Dimensions dims(1, dimsA.getColumns());
                    res = ArrayOf(NLS_LOGICAL, dims, logicalarray);
                }
            }
        } else {
            Dimensions dims2Dzeros(0, 0);
            if (A.getDimensions().equals(dims2Dzeros)) {
                if (dim - 1 == 0) {
                    Dimensions dims(1, 0);
                    res = ArrayOf::emptyConstructor(dims);
                    res.promoteType(NLS_LOGICAL);
                } else if (dim - 1 == 1) {
                    Dimensions dims(0, 1);
                    res = ArrayOf::emptyConstructor(dims);
                    res.promoteType(NLS_LOGICAL);
                } else {
                    res = ArrayOf::emptyConstructor();
                    res.promoteType(NLS_LOGICAL);
                }
            } else {
                if (A.getRows() > A.getColumns()) {
                    if (dim - 1 == 0) {
                        Dimensions dims(1, 0);
                        res = ArrayOf::emptyConstructor(dims);
                        res.promoteType(NLS_LOGICAL);
                    } else if (dim - 1 == 1) {
                        logical* logicalarray = (logical*)ArrayOf::allocateArrayOf(
                            NLS_LOGICAL, A.getRows(), stringVector(), true);
                        Dimensions dims(A.getRows(), 1);
                        res = ArrayOf(NLS_LOGICAL, dims, logicalarray);
                    } else {
                        Dimensions dims(A.getRows(), 0);
                        res = ArrayOf::emptyConstructor(dims);
                        res.promoteType(NLS_LOGICAL);
                    }
                } else {
                    if (dim - 1 == 0) {
                        logical* logicalarray = (logical*)ArrayOf::allocateArrayOf(
                            NLS_LOGICAL, A.getColumns(), stringVector(), true);
                        Dimensions dims(1, A.getColumns());
                        res = ArrayOf(NLS_LOGICAL, dims, logicalarray);
                    } else if (dim - 1 == 1) {
                        Dimensions dims(0, 1);
                        res = ArrayOf::emptyConstructor(dims);
                        res.promoteType(NLS_LOGICAL);
                    } else {
                        Dimensions dims(0, A.getColumns());
                        res = ArrayOf::emptyConstructor(dims);
                        res.promoteType(NLS_LOGICAL);
                    }
                }
            }
        }
    } else if (A.isVector()) {
        auto* pLogical = (logical*)A.getDataPointer();
        bool bRes = false;
        indexType elementCount = A.getElementCount();
        for (indexType k = 0; k < elementCount; k++) {
            if (pLogical[k] != 0) {
                bRes = true;
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
            matC = matA.colwise().any();
            res = ArrayOf(NLS_LOGICAL, Dimensions(1, nA), logicalarray);
        } else if (dim == 2) {
            logical* logicalarray
                = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, mA, stringVector(), false);
            Eigen::Map<Eigen::Matrix<logical, Eigen::Dynamic, Eigen::Dynamic>> matC(
                logicalarray, mA, 1);
            matC = matA.rowwise().any();
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
