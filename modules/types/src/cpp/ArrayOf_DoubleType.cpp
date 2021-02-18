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
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ArrayOf::isDoubleClass() const
{
    if (dp == nullptr) {
        return false;
    }
    return (dp->dataClass == NLS_DOUBLE || dp->dataClass == NLS_DCOMPLEX);
}
//=============================================================================
/**
 * Returns TRUE if it is a double type (not ndarray, not sparse)
 */
//=============================================================================
bool
ArrayOf::isDoubleType(bool realOnly) const
{
    bool res = false;
    if (dp) {
        if (realOnly) {
            res = (dp->dataClass == NLS_DOUBLE) && (!dp->sparse) && is2D();
        } else {
            res = (isDoubleClass() && (!dp->sparse) && is2D());
        }
    }
    return res;
}
//=============================================================================
bool
ArrayOf::isNdArrayDoubleType(bool realOnly) const
{
    bool res = false;
    if (dp) {
        if (realOnly) {
            res = (dp->dataClass == NLS_DOUBLE) && (!dp->sparse) && !is2D();
        } else {
            res = (isDoubleClass() && (!dp->sparse) && !is2D());
        }
    }
    return res;
}
//=============================================================================
ArrayOf
ArrayOf::doubleConstructor(double aval)
{
    double* data = static_cast<double*>(allocateArrayOf(NLS_DOUBLE, 1, stringVector(), false));
    *data = aval;
    return ArrayOf(NLS_DOUBLE, Dimensions(1, 1), data);
}
//=============================================================================
ArrayOf
ArrayOf::doubleVectorConstructor(indexType len)
{
    double* data = static_cast<double*>(allocateArrayOf(NLS_DOUBLE, len, stringVector(), true));
    return ArrayOf(NLS_DOUBLE, Dimensions(1, len), data);
}
//=============================================================================
ArrayOf
ArrayOf::doubleMatrix2dConstructor(indexType m, indexType n)
{
    double* data = static_cast<double*>(allocateArrayOf(NLS_DOUBLE, m * n, stringVector(), true));
    return ArrayOf(NLS_DOUBLE, Dimensions(m, n), data);
}
//=============================================================================
ArrayOf
ArrayOf::dcomplexConstructor(double aval, double bval)
{
    double* data = static_cast<double*>(allocateArrayOf(NLS_DCOMPLEX, 1, stringVector(), false));
    data[0] = aval;
    data[1] = bval;
    return ArrayOf(NLS_DCOMPLEX, Dimensions(1, 1), data);
}
//=============================================================================
double
ArrayOf::getContentAsDoubleScalar(bool arrayAsScalar)
{
    if (isEmpty() || isComplex() || isReferenceType() || isCharacterArray() || isSparse()
        || (!arrayAsScalar && !isScalar())) {
        Error(_W("Expected a real value scalar."));
    }
    promoteType(NLS_DOUBLE);
    auto* qp = (double*)dp->getData();
    return (*qp);
}
//=============================================================================
doublecomplex
ArrayOf::getContentAsDoubleComplexScalar(bool arrayAsScalar)
{
    if (isEmpty() || isReferenceType() || isCharacterArray() || isSparse()
        || (!arrayAsScalar && !isScalar())) {
        Error(_W("Expected a real valued scalar"));
    }
    promoteType(NLS_DCOMPLEX);
    auto* qp = (double*)dp->getData();
    doublecomplex cx(qp[0], qp[1]);
    return cx;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
