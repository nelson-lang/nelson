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
ArrayOf::isSingleClass() const
{
    if (dp) {
        return (dp->dataClass == NLS_SINGLE || dp->dataClass == NLS_SCOMPLEX);
    }
    return false;
}
//=============================================================================
/**
 * Returns TRUE if it is a single type (not ndarray, not sparse)
 */
bool
ArrayOf::isSingleType(bool realOnly) const
{
    bool res = false;
    if (dp) {
        if (realOnly) {
            res = (dp->dataClass == NLS_SINGLE) && (!dp->sparse) && is2D();
        } else {
            res = (isSingleClass() && (!dp->sparse) && is2D());
        }
    }
    return res;
}
//=============================================================================
bool
ArrayOf::isNdArraySingleType(bool realOnly) const
{
    bool res = false;
    if (dp) {
        if (realOnly) {
            res = (dp->dataClass == NLS_SINGLE) && (!dp->sparse) && !is2D();
        } else {
            res = (isSingleClass() && (!dp->sparse) && !is2D());
        }
    }
    return res;
}
//=============================================================================
ArrayOf
ArrayOf::singleConstructor(float aval)
{
    Dimensions dim;
    dim.makeScalar();
    float* data = static_cast<float*>(allocateArrayOf(NLS_SINGLE, 1, stringVector(), false));
    *data = aval;
    return ArrayOf(NLS_SINGLE, dim, data);
}
//=============================================================================
ArrayOf
ArrayOf::singleVectorConstructor(indexType len)
{
    Dimensions dim;
    dim.makeScalar();
    dim[1] = len;
    single* data = static_cast<single*>(allocateArrayOf(NLS_SINGLE, len, stringVector(), true));
    return ArrayOf(NLS_SINGLE, dim, data);
}
//=============================================================================
ArrayOf
ArrayOf::complexConstructor(float aval, float bval)
{
    Dimensions dim;
    dim.makeScalar();
    float* data = static_cast<float*>(allocateArrayOf(NLS_SCOMPLEX, 1, stringVector(), false));
    data[0] = aval;
    data[1] = bval;
    return ArrayOf(NLS_SCOMPLEX, dim, data);
}
//=============================================================================
single
ArrayOf::getContentAsSingleScalar(bool arrayAsScalar)
{
    single* qp;
    if (isComplex() || isReferenceType() || isCharacterArray() || isSparse() || isEmpty()
        || (!arrayAsScalar && !isScalar())) {
        Error(_W("Expected a real value scalar."));
    }
    promoteType(NLS_SINGLE);
    qp = (single*)dp->getData();
    return (*qp);
}
//=============================================================================
std::complex<single>
ArrayOf::getContentAsSingleComplexScalar(bool arrayAsScalar)
{
    if (isReferenceType() || isCharacterArray() || isEmpty() || (!arrayAsScalar && !isScalar())) {
        Error(_W("Expected a real valued scalar"));
    }
    promoteType(NLS_SCOMPLEX);
    auto* qp = (single*)dp->getData();
    std::complex<single> cx(qp[0], qp[1]);
    return cx;
}
//=============================================================================

} // namespace Nelson
//=============================================================================
