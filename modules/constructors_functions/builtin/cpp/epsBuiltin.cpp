//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <complex>
#include "epsBuiltin.hpp"
#include "Epsilon.hpp"
#include "ConstantClassArguments.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
template <class T>
static ArrayOf
epsOfArray(const ArrayOf& X, NelsonType realType)
{
    indexType n = X.elementCount();
    T* out = static_cast<T*>(ArrayOf::allocateArrayOf(realType, n));
    if (X.isComplex()) {
        const auto* in = static_cast<const std::complex<T>*>(X.dataPointer());
        for (indexType k = 0; k < n; ++k) {
            out[k] = Epsilon(std::abs(in[k]));
        }
    } else {
        const T* in = static_cast<const T*>(X.dataPointer());
        for (indexType k = 0; k < n; ++k) {
            out[k] = Epsilon(in[k]);
        }
    }
    return ArrayOf(realType, X.dimensions(), out);
}
//=============================================================================
ArrayOfVector
Nelson::ConstructorsGateway::epsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 0, 2);
    ArrayOfVector retval;
    NelsonType cls = argIn.size() == 1 ? argIn[0].dataClass() : NLS_UNKNOWN;
    if (cls == NLS_DOUBLE || cls == NLS_DCOMPLEX || cls == NLS_SINGLE || cls == NLS_SCOMPLEX) {
        // spacing of each element, always real and full
        ArrayOf X = argIn[0];
        if (X.isSparse()) {
            X.makeDense();
        }
        retval << (X.isSingleType() ? epsOfArray<single>(X, NLS_SINGLE)
                                    : epsOfArray<double>(X, NLS_DOUBLE));
        return retval;
    }
    ConstantClassRequest request = parseConstantClassArguments(argIn, false);
    ArrayOf value = request.className == L"single"
        ? ArrayOf::singleScalar(Epsilon(static_cast<single>(1.0)))
        : ArrayOf::doubleScalar(Epsilon(1.0));
    retval << applyConstantClassRequest(value, request);
    return retval;
}
//=============================================================================
