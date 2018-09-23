//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <Eigen/Dense>
#include "All.hpp"
#include "Error.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
All(ArrayOf& A, indexType dim, bool& needToOverload)
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
            if (A.getDimensions().equals(dims2Dzeros)) {
                res = ArrayOf::logicalConstructor(true);
            } else {
                if (A.getDimensions().getRows() > A.getDimensions().getColumns()) {
                    Dimensions dims(1, 0);
                    res = ArrayOf::emptyConstructor(dims);
                    res.promoteType(NLS_LOGICAL);
                } else {
                    logical* logicalarray = (logical*)ArrayOf::allocateArrayOf(
                        NLS_LOGICAL, A.getDimensions().getColumns());
                    memset(logicalarray, 1, A.getDimensions().getColumns());
                    res = ArrayOf(
                        NLS_LOGICAL, Dimensions(1, A.getDimensions().getColumns()), logicalarray);
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
                if (A.getDimensions().getRows() > A.getDimensions().getColumns()) {
                    if (dim - 1 == 0) {
                        Dimensions dims(1, 0);
                        res = ArrayOf::emptyConstructor(dims);
                        res.promoteType(NLS_LOGICAL);
                    } else if (dim - 1 == 1) {
                        logical* logicalarray = (logical*)ArrayOf::allocateArrayOf(
                            NLS_LOGICAL, A.getDimensions().getRows());
                        memset(logicalarray, 1, A.getDimensions().getRows());
                        res = ArrayOf(
                            NLS_LOGICAL, Dimensions(A.getDimensions().getRows(), 1), logicalarray);
                    } else {
                        Dimensions dims(A.getDimensions().getRows(), 0);
                        res = ArrayOf::emptyConstructor(dims);
                        res.promoteType(NLS_LOGICAL);
                    }
                } else {
                    if (dim - 1 == 0) {
                        logical* logicalarray = (logical*)ArrayOf::allocateArrayOf(
                            NLS_LOGICAL, A.getDimensions().getColumns());
                        memset(logicalarray, 1, A.getDimensions().getColumns());
                        res = ArrayOf(NLS_LOGICAL, Dimensions(1, A.getDimensions().getColumns()),
                            logicalarray);
                    } else if (dim - 1 == 1) {
                        Dimensions dims(0, 1);
                        res = ArrayOf::emptyConstructor(dims);
                        res.promoteType(NLS_LOGICAL);
                    } else {
                        Dimensions dims(0, A.getDimensions().getColumns());
                        res = ArrayOf::emptyConstructor(dims);
                        res.promoteType(NLS_LOGICAL);
                    }
                }
            }
        }
    } else if (A.isVector()) {
        logical* pLogical = (logical*)A.getDataPointer();
        bool bRes = true;
        for (size_t k = 0; k < A.getDimensions().getElementCount(); k++) {
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
            logical* logicalarray = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, nA);
            Eigen::Map<Eigen::Matrix<logical, Eigen::Dynamic, Eigen::Dynamic>> matC(
                logicalarray, 1, nA);
            matC = matA.colwise().all();
            res = ArrayOf(NLS_LOGICAL, Dimensions(1, nA), logicalarray);
        } else if (dim == 2) {
            logical* logicalarray = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, mA);
            Eigen::Map<Eigen::Matrix<logical, Eigen::Dynamic, Eigen::Dynamic>> matC(
                logicalarray, mA, 1);
            matC = matA.rowwise().all();
            res = ArrayOf(NLS_LOGICAL, Dimensions(mA, 1), logicalarray);
        } else {
            res = A;
            res.ensureSingleOwner();
        }
    }
    return res;
}
//=============================================================================
}
//=============================================================================
