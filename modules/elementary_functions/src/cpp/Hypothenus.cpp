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
#include <algorithm>
#include "nlsConfig.h"
#include "Hypothenus.hpp"
#include "MatrixCheck.hpp"
#include "Exception.hpp"
#include "HypothenusReal.hpp"
#include "HypothenusComplex.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static NelsonType
computeCommonClass(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = true;
    if (A.isSparse() || B.isSparse()) {
        return NLS_NOT_TYPED;
    }
    if ((A.isDoubleClass() || A.isSingleClass()) && (B.isDoubleClass() || B.isSingleClass())) {
        needToOverload = false;
        bool asComplex = A.isComplex() || B.isComplex();
        NelsonType destinationClass;
        if (A.getDataClass() == B.getDataClass()) {
            destinationClass = A.getDataClass();
        } else {
            if (A.isDoubleClass() && B.isDoubleClass()) {
                if (asComplex) {
                    destinationClass = NLS_DCOMPLEX;
                } else {
                    destinationClass = NLS_DOUBLE;
                }
            } else if (A.isSingleClass() && B.isSingleClass()) {
                if (asComplex) {
                    destinationClass = NLS_SCOMPLEX;
                } else {
                    destinationClass = NLS_SINGLE;
                }
            } else {
                if (asComplex) {
                    destinationClass = NLS_SCOMPLEX;
                } else {
                    destinationClass = NLS_SINGLE;
                }
            }
        }
        return destinationClass;
    }
    return NLS_NOT_TYPED;
}
//=============================================================================
static ArrayOf
Hypothenuse(const ArrayOf& A, const ArrayOf& B, NelsonType commonClass)
{
    ArrayOf res;
    if (A.isEmpty() || B.isEmpty()) {
        if (A.isScalar() || B.isScalar()) {
            if (A.isScalar()) {
                res = B;
            } else {
                res = A;
            }
        } else {
            Error(_W("Arrays have incompatible sizes for this operation."),
                L"Nelson:sizeDimensionsMustMatch");
        }
        if (res.getDataClass() == NLS_DCOMPLEX) {
            if (res.allReal()) {
                res.promoteType(NLS_DOUBLE);
            }
        }
        if (res.getDataClass() == NLS_SCOMPLEX) {
            if (res.allReal()) {
                res.promoteType(NLS_SINGLE);
            }
        }
        return res;
    }
    switch (commonClass) {
    case NLS_SINGLE: {
        if (A.isScalar() && B.isScalar()) {
            res = scalar_scalar_real_hypothenuse<single>(NLS_SINGLE, A, B);
        } else {
            Dimensions dimsA = A.getDimensions();
            Dimensions dimsB = B.getDimensions();
            if (SameSizeCheck(dimsA, dimsB)) {
                res = matrix_matrix_real_hypothenuse<single>(NLS_SINGLE, A, B);
            } else {
                res = real_hypothenuse<single>(NLS_SINGLE, A, B);
            }
        }
    } break;
    case NLS_DOUBLE: {
        if (A.isScalar() && B.isScalar()) {
            res = scalar_scalar_real_hypothenuse<double>(NLS_DOUBLE, A, B);
        } else {
            Dimensions dimsA = A.getDimensions();
            Dimensions dimsB = B.getDimensions();
            if (SameSizeCheck(dimsA, dimsB)) {
                res = matrix_matrix_real_hypothenuse<double>(NLS_DOUBLE, A, B);
            } else {
                res = real_hypothenuse<double>(NLS_DOUBLE, A, B);
            }
        }
    } break;
    case NLS_SCOMPLEX: {
        if (A.isScalar() && B.isScalar()) {
            res = scalar_scalar_complex_hypothenuse<single>(NLS_SCOMPLEX, A, B);
        } else {
            Dimensions dimsA = A.getDimensions();
            Dimensions dimsB = B.getDimensions();
            if (SameSizeCheck(dimsA, dimsB)) {
                res = matrix_matrix_complex_hypothenuse<single>(NLS_SCOMPLEX, A, B);
            } else {
                res = complex_hypothenuse<single>(NLS_SCOMPLEX, A, B);
            }
        }
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_DCOMPLEX: {
        if (A.isScalar() && B.isScalar()) {
            res = scalar_scalar_complex_hypothenuse<double>(NLS_DCOMPLEX, A, B);
        } else {
            Dimensions dimsA = A.getDimensions();
            Dimensions dimsB = B.getDimensions();
            if (SameSizeCheck(dimsA, dimsB)) {
                res = matrix_matrix_complex_hypothenuse<double>(NLS_DCOMPLEX, A, B);
            } else {
                res = complex_hypothenuse<double>(NLS_DCOMPLEX, A, B);
            }
        }
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    default: {
        Error(_W("Type not managed."));
    } break;
    }
    return res;
}
//=============================================================================
ArrayOf
Hypothenuse(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf res;
    NelsonType commonClass = computeCommonClass(A, B, needToOverload);
    if (!needToOverload) {
        if (A.getDataClass() == commonClass && B.getDataClass() == commonClass) {
            res = Hypothenuse(A, B, commonClass);
        } else {
            ArrayOf a = A;
            a.promoteType(commonClass);
            ArrayOf b = B;
            b.promoteType(commonClass);
            res = Hypothenuse(a, b, commonClass);
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
