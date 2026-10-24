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
ArrayOf::isDoubleClass() const
{
    if (hasScalarStorage()) {
        return scalarType_ == NLS_DOUBLE;
    }
    if (dp == nullptr) {
        return false;
    }
    return (dp->dataClass == NLS_DOUBLE || dp->dataClass == NLS_DCOMPLEX);
}
//=============================================================================
bool
ArrayOf::isDoubleType(bool realOnly) const
{
    if (hasScalarStorage()) {
        return scalarType_ == NLS_DOUBLE && is2D();
    }
    if (!dp) {
        return false;
    }
    if (dp->sparse || !is2D()) {
        return false;
    }
    return realOnly ? (dp->dataClass == NLS_DOUBLE) : isDoubleClass();
}
//=============================================================================
bool
ArrayOf::isNdArrayDoubleType(bool realOnly) const
{
    if (hasScalarStorage()) {
        return scalarType_ == NLS_DOUBLE && !is2D();
    }
    if (!dp || dp->sparse || is2D()) {
        return false;
    }
    return realOnly ? (dp->dataClass == NLS_DOUBLE) : isDoubleClass();
}
//=============================================================================
ArrayOf
ArrayOf::doubleScalar(double aval)
{
    return ArrayOf(Representation::InlineScalar, aval);
}
//=============================================================================
ArrayOf
ArrayOf::doubleComplexRowVector(const std::vector<std::complex<double>>& values)
{
    const size_t count = values.size();
    double* data
        = static_cast<double*>(allocateArrayOf(NLS_DCOMPLEX, count, stringVector(), false));
    std::memcpy(data, values.data(), count * sizeof(std::complex<double>));
    return ArrayOf(NLS_DCOMPLEX, Dimensions(1, count), data);
}
//=============================================================================
ArrayOf
ArrayOf::doubleRowVector(const std::vector<double>& values)
{
    if (values.size() == 1) {
        return doubleScalar(values.front());
    }
    double* data
        = static_cast<double*>(allocateArrayOf(NLS_DOUBLE, values.size(), stringVector(), false));
    std::copy(values.begin(), values.end(), data);

    return ArrayOf(NLS_DOUBLE, Dimensions(1, values.size()), data);
}
//=============================================================================
ArrayOf
ArrayOf::doubleRowVector(indexType len)
{
    if (len == 1) {
        return doubleScalar(0.);
    }
    double* data = static_cast<double*>(allocateArrayOf(NLS_DOUBLE, len, stringVector(), true));
    return ArrayOf(NLS_DOUBLE, Dimensions(1, len), data);
}
//=============================================================================
ArrayOf
ArrayOf::doubleMatrix2d(indexType m, indexType n)
{
    if (m == 1 && n == 1) {
        return doubleScalar(0.);
    }
    double* data = static_cast<double*>(allocateArrayOf(NLS_DOUBLE, m * n, stringVector(), true));
    return ArrayOf(NLS_DOUBLE, Dimensions(m, n), data);
}
//=============================================================================
ArrayOf
ArrayOf::dsingleComplexScalar(double aval, double bval)
{
    double data[2] = { aval, bval };
    return ArrayOf(NLS_DCOMPLEX, Dimensions(1, 1), static_cast<const void*>(data), sizeof(data));
}
//=============================================================================
std::vector<double>
ArrayOf::asDoubleVector() const
{
    if (isComplex() || isReferenceType() || isCharacterArray() || isSparse()) {
        Error(L"Nelson:validators:mustBeReal", L"%s must be real.", L"Value");
    }
    size_t nbElements = elementCount();
    std::vector<double> values(nbElements);

    if (dataClass() != NLS_DOUBLE) {
        ArrayOf P(*this);
        P.promoteType(NLS_DOUBLE);
        const auto* data = static_cast<const double*>(P.dataPointer());
        std::copy(data, data + nbElements, values.begin());
    } else {
        const auto* data = static_cast<const double*>(dataPointer());
        std::copy(data, data + nbElements, values.begin());
    }
    return values;
}
//=============================================================================
double
ArrayOf::asDoubleScalar(bool arrayAsScalar, bool checkIsIntegerValue) const
{
    if (isEmpty() || isComplex() || isReferenceType() || isCharacterArray() || isSparse()
        || (!arrayAsScalar && !isScalar())) {
        Error(L"Nelson:types:scalarExpected", L"A scalar expected.");
    }
    double value = 0;
    if (dataClass() == NLS_DOUBLE) {
        const double* qp = static_cast<const double*>(dataPointer());
        value = qp[0];
    } else {
        ArrayOf P(*this);
        P.promoteType(NLS_DOUBLE);
        double* qp = (double*)P.dataPointer();
        value = qp[0];
    }
    if (checkIsIntegerValue) {
        double f = std::floor(value);
        if (std::abs(f - value) >= std::numeric_limits<double>::epsilon()) {
            Error(L"Nelson:integerValueExpected", L"A real integer value scalar expected.");
        }
    }
    return value;
}
//=============================================================================
doublecomplex
ArrayOf::asDoubleComplexScalar(bool arrayAsScalar) const
{
    if (isEmpty() || isReferenceType() || isCharacterArray() || isSparse()
        || (!arrayAsScalar && !isScalar())) {
        Error(L"Nelson:types:scalarExpected", L"A scalar expected.");
    }
    if (dataClass() != NLS_DCOMPLEX) {
        ArrayOf P(*this);
        P.promoteType(NLS_DCOMPLEX);
        const auto* qp = static_cast<const double*>(P.dataPointer());
        return doublecomplex { qp[0], qp[1] };
    }
    const auto* qp = static_cast<const double*>(dp->data());
    return doublecomplex { qp[0], qp[1] };
}
//=============================================================================
} // namespace Nelson
//=============================================================================
