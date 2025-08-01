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
#include "i18n.hpp"
#include <cstring>
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
ArrayOf::doubleComplexRowVectorConstructor(std::vector<std::complex<double>>& values)
{
    const size_t count = values.size();
    double* data
        = static_cast<double*>(allocateArrayOf(NLS_DCOMPLEX, count, stringVector(), false));
    std::memcpy(data, values.data(), count * sizeof(std::complex<double>));
    return ArrayOf(NLS_DCOMPLEX, Dimensions(1, count), data);
}
//=============================================================================
ArrayOf
ArrayOf::doubleRowVectorConstructor(std::vector<double>& values)
{
    double* data
        = static_cast<double*>(allocateArrayOf(NLS_DOUBLE, values.size(), stringVector(), false));
    std::copy(values.begin(), values.end(), data);
    return ArrayOf(NLS_DOUBLE, Dimensions(1, values.size()), data);
}
//=============================================================================
ArrayOf
ArrayOf::doubleRowVectorConstructor(indexType len)
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
std::vector<double>
ArrayOf::getContentAsDoubleVector() const
{
    if (isComplex() || isReferenceType() || isCharacterArray() || isSparse()) {
        Error(_W("Expected a real value."));
    }
    size_t elementCount = getElementCount();
    std::vector<double> values(elementCount);
    if (getDataClass() != NLS_DOUBLE) {
        ArrayOf P(*this);
        P.promoteType(NLS_DOUBLE);
        const double* data = static_cast<const double*>(P.getDataPointer());
        std::copy(data, data + elementCount, values.begin());
    } else {
        const double* data = static_cast<const double*>(dp->getData());
        std::copy(data, data + elementCount, values.begin());
    }
    return values;
}
//=============================================================================
double
ArrayOf::getContentAsDoubleScalar(bool arrayAsScalar, bool checkIsIntegerValue) const
{
    if (isEmpty() || isComplex() || isReferenceType() || isCharacterArray() || isSparse()
        || (!arrayAsScalar && !isScalar())) {
        Error(_W("Expected a real value scalar."));
    }
    double value = 0;
    if (getDataClass() != NLS_DOUBLE) {
        ArrayOf P(*this);
        P.promoteType(NLS_DOUBLE);
        auto* qp = (double*)P.getDataPointer();
        value = *qp;
    } else {
        auto* qp = (double*)dp->getData();
        value = *qp;
    }
    if (checkIsIntegerValue) {
        double f = std::floor(value);
        if (std::abs(f - value) >= std::numeric_limits<double>::epsilon()) {
            Error(_W("A real integer value scalar expected."));
        }
    }
    return value;
}
//=============================================================================
doublecomplex
ArrayOf::getContentAsDoubleComplexScalar(bool arrayAsScalar) const
{
    if (isEmpty() || isReferenceType() || isCharacterArray() || isSparse()
        || (!arrayAsScalar && !isScalar())) {
        Error(_W("Expected a real valued scalar"));
    }
    if (getDataClass() != NLS_DCOMPLEX) {
        ArrayOf P(*this);
        P.promoteType(NLS_DCOMPLEX);
        double* qp = (double*)P.getDataPointer();
        doublecomplex cx(qp[0], qp[1]);
        return cx;
    }
    double* qp = (double*)dp->getData();
    doublecomplex cx(qp[0], qp[1]);
    return cx;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
