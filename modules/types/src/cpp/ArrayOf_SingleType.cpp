//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include <cstring>
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ArrayOf::isSingleClass() const
{
    if (hasScalarStorage()) {
        return scalarType_ == NLS_SINGLE;
    }
    if (dp) {
        return (dp->dataClass == NLS_SINGLE || dp->dataClass == NLS_SCOMPLEX);
    }
    return false;
}
//=============================================================================
bool
ArrayOf::isSingleType(bool realOnly) const
{
    if (hasScalarStorage()) {
        return scalarType_ == NLS_SINGLE && is2D();
    }
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
    if (hasScalarStorage()) {
        return scalarType_ == NLS_SINGLE && !is2D();
    }
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
ArrayOf::singleScalar(float aval)
{
    return ArrayOf(Representation::InlineScalar, aval);
}
//=============================================================================
ArrayOf
ArrayOf::singleRowVector(indexType len)
{
    if (len == 1) {
        return singleScalar(0.f);
    }
    Dimensions dim;
    dim.makeScalar();
    dim[1] = len;
    single* data = static_cast<single*>(allocateArrayOf(NLS_SINGLE, len, stringVector(), true));
    return ArrayOf(NLS_SINGLE, dim, data);
}
//=============================================================================
ArrayOf
ArrayOf::singleComplexRowVector(const std::vector<std::complex<single>>& values)
{
    const size_t count = values.size();
    single* data
        = static_cast<single*>(allocateArrayOf(NLS_SCOMPLEX, count, stringVector(), false));
    std::memcpy(data, values.data(), count * sizeof(std::complex<single>));
    return ArrayOf(NLS_SCOMPLEX, Dimensions(1, count), data);
}
//=============================================================================
ArrayOf
ArrayOf::singleRowVector(const std::vector<single>& values)
{
    if (values.size() == 1) {
        return singleScalar(values.front());
    }
    const size_t count = values.size();
    single* data = static_cast<single*>(allocateArrayOf(NLS_SINGLE, count, stringVector(), false));
    std::memcpy(data, values.data(), count * sizeof(single));
    return ArrayOf(NLS_SINGLE, Dimensions(1, count), data);
}
//=============================================================================
ArrayOf
ArrayOf::singleMatrix2d(indexType m, indexType n)
{
    if (m == 1 && n == 1) {
        return singleScalar(0.f);
    }
    single* data = static_cast<single*>(allocateArrayOf(NLS_SINGLE, m * n, stringVector(), true));
    return ArrayOf(NLS_SINGLE, Dimensions(m, n), data);
}
//=============================================================================
ArrayOf
ArrayOf::singleComplexScalar(float aval, float bval)
{
    Dimensions dim;
    dim.makeScalar();
    float data[2] = { aval, bval };
    return ArrayOf(NLS_SCOMPLEX, dim, static_cast<const void*>(data), sizeof(data));
}
//=============================================================================
single
ArrayOf::asSingleScalar(bool arrayAsScalar, bool checkIsIntegerValue) const
{
    single value = 0;

    if (isComplex() || isReferenceType() || isCharacterArray() || isSparse() || isEmpty()
        || (!arrayAsScalar && !isScalar())) {
        Error(L"Nelson:types:scalarExpected", L"A scalar expected.");
    }
    if (dataClass() != NLS_SINGLE) {
        single* qp = nullptr;
        ArrayOf P = *this;
        P.promoteType(NLS_SINGLE);
        qp = (single*)P.dataPointer();
        value = qp[0];

    } else {
        single* qp = nullptr;
        qp = (single*)dataPointer();
        value = qp[0];
    }

    if (checkIsIntegerValue) {
        single f = std::floor(value);
        if (std::abs(f - value) >= std::numeric_limits<double>::epsilon()) {
            Error(L"Nelson:integerValueExpected", L"A real integer value scalar expected.");
        }
    }
    return value;
}
//=============================================================================
std::complex<single>
ArrayOf::asSingleComplexScalar(bool arrayAsScalar) const
{
    if (isReferenceType() || isCharacterArray() || isEmpty() || (!arrayAsScalar && !isScalar())) {
        Error(L"Nelson:types:scalarExpected", L"A scalar expected.");
    }
    if (dataClass() != NLS_SCOMPLEX) {
        ArrayOf P(*this);
        P.promoteType(NLS_SCOMPLEX);
        auto* qp = (single*)P.dataPointer();
        std::complex<single> cx(qp[0], qp[1]);
        return cx;
    }
    auto* qp = (single*)dp->data();
    std::complex<single> cx(qp[0], qp[1]);
    return cx;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
