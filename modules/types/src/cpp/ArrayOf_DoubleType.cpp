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
    if (realOnly) {
        res = (dp->dataClass == NLS_DOUBLE) && (!dp->sparse) && is2D();
    } else {
        res = (isDoubleClass() && (!dp->sparse) && is2D());
    }
    return res;
}
//=============================================================================
bool
ArrayOf::isNdArrayDoubleType(bool realOnly) const
{
    bool res = false;
    if (realOnly) {
        res = (dp->dataClass == NLS_DOUBLE) && (!dp->sparse) && !is2D();
    } else {
        res = (isDoubleClass() && (!dp->sparse) && !is2D());
    }
    return res;
}
//=============================================================================
ArrayOf
ArrayOf::doubleConstructor(double aval)
{
    Dimensions dim;
    dim.makeScalar();
    double* data = (double*)allocateArrayOf(NLS_DOUBLE, 1);
    *data = aval;
    return ArrayOf(NLS_DOUBLE, dim, data);
}
//=============================================================================
ArrayOf
ArrayOf::doubleVectorConstructor(indexType len)
{
    Dimensions dim;
    dim.makeScalar();
    dim[1] = len;
    double* data = (double*)allocateArrayOf(NLS_DOUBLE, len);
    return ArrayOf(NLS_DOUBLE, dim, data);
}
//=============================================================================
ArrayOf
ArrayOf::doubleMatrix2dConstructor(indexType m, indexType n)
{
    Dimensions dim(m, n);
    double* data = (double*)allocateArrayOf(NLS_DOUBLE, dim.getElementCount());
    return ArrayOf(NLS_DOUBLE, dim, data);
}
//=============================================================================
ArrayOf
ArrayOf::dcomplexConstructor(double aval, double bval)
{
    Dimensions dim;
    dim.makeScalar();
    double* data = (double*)allocateArrayOf(NLS_DCOMPLEX, 1);
    data[0] = aval;
    data[1] = bval;
    return ArrayOf(NLS_DCOMPLEX, dim, data);
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
    double* qp = (double*)dp->getData();
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
    double* qp = (double*)dp->getData();
    doublecomplex cx(qp[0], qp[1]);
    return cx;
}
//=============================================================================
}
//=============================================================================
