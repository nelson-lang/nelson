//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
ArrayOf::getContentAsSingleScalar(bool arrayAsScalar, bool checkIsIntegerValue) const
{
    single* qp;
    if (isComplex() || isReferenceType() || isCharacterArray() || isSparse() || isEmpty()
        || (!arrayAsScalar && !isScalar())) {
        Error(_W("Expected a real value scalar."));
    }
    single value = 0;
    if (getDataClass() != NLS_SINGLE) {
        ArrayOf P(*this);
        P.promoteType(NLS_SINGLE);
        qp = (single*)P.getDataPointer();
        value = *qp;
    } else {
        qp = (single*)dp->getData();
        value = *qp;
    }
    if (checkIsIntegerValue) {
        single f = std::floor(value);
        if (f != value) {
            Error(_W("A real integer value scalar expected."));
        }
    }

    return (*qp);
}
//=============================================================================
std::complex<single>
ArrayOf::getContentAsSingleComplexScalar(bool arrayAsScalar) const
{
    if (isReferenceType() || isCharacterArray() || isEmpty() || (!arrayAsScalar && !isScalar())) {
        Error(_W("Expected a real valued scalar"));
    }
    if (getDataClass() != NLS_SCOMPLEX) {
        ArrayOf P(*this);
        P.promoteType(NLS_SCOMPLEX);
        auto* qp = (single*)P.getDataPointer();
        std::complex<single> cx(qp[0], qp[1]);
        return cx;
    }
    auto* qp = (single*)dp->getData();
    std::complex<single> cx(qp[0], qp[1]);
    return cx;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
