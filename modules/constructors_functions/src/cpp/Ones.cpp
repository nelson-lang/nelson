//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include "omp_for_loop.hpp"
#include "Ones.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
ArrayOf
Ones(NelsonType cl)
{
    Dimensions dims(1, 1);
    return Ones(dims, cl);
}
//=============================================================================
template <class T>
ArrayOf
realOnes(NelsonType classOut, void* dp, const Dimensions& outDims)
{
    T value = static_cast<T>(1.);
    T* ptrA = static_cast<T*>(dp);
    ompIndexType nbElements = (ompIndexType)outDims.getElementCount();
    if (nbElements == 1) {
        ptrA[0] = value;
    } else {
        OMP_PARALLEL_FOR_LOOP(nbElements)
        for (ompIndexType k = 0; k < (ompIndexType)nbElements; k++) {
            ptrA[k] = value;
        }
    }
    return ArrayOf(classOut, outDims, dp, false);
}
//=============================================================================
template <class T>
ArrayOf
complexOnes(NelsonType classOut, void* dp, const Dimensions& outDims)
{
    auto* ptrDpz = reinterpret_cast<std::complex<T>*>((T*)dp);
    std::complex<T> value = { 1, 0 };
    ompIndexType nbElements = (ompIndexType)outDims.getElementCount();
    if (nbElements == 1) {
        ptrDpz[0] = value;
    } else {
        OMP_PARALLEL_FOR_LOOP(nbElements)
        for (ompIndexType k = 0; k < (ompIndexType)nbElements; k++) {
            ptrDpz[k] = value;
        }
    }
    return ArrayOf(classOut, outDims, dp, false);
}
//=============================================================================
ArrayOf
Ones(Dimensions& dims, NelsonType cl)
{
    dims.simplify();
    if (dims.isEmpty(false)) {
        ArrayOf res = ArrayOf::emptyConstructor(dims);
        res.promoteType(cl);
        return res;
    }

    indexType nbElements = dims.getElementCount();
    void* dp = ArrayOf::allocateArrayOf(cl, nbElements, Nelson::stringVector(), false);

    switch (cl) {
    case NLS_LOGICAL: {
        return realOnes<logical>(cl, dp, dims);
    } break;
    case NLS_INT8: {
        return realOnes<int8>(cl, dp, dims);
    } break;
    case NLS_UINT8: {
        return realOnes<uint8>(cl, dp, dims);
    } break;
    case NLS_INT16: {
        return realOnes<int16>(cl, dp, dims);
    } break;
    case NLS_UINT16: {
        return realOnes<uint16>(cl, dp, dims);
    } break;
    case NLS_INT32: {
        return realOnes<int32>(cl, dp, dims);
    } break;
    case NLS_UINT32: {
        return realOnes<uint32>(cl, dp, dims);
    } break;
    case NLS_INT64: {
        return realOnes<int64>(cl, dp, dims);
    } break;
    case NLS_UINT64: {
        return realOnes<uint64>(cl, dp, dims);
    } break;
    case NLS_SINGLE: {
        return realOnes<single>(cl, dp, dims);
    } break;
    case NLS_DOUBLE: {
        return realOnes<double>(cl, dp, dims);
    } break;
    case NLS_SCOMPLEX: {
        return complexOnes<single>(cl, dp, dims);
    } break;
    case NLS_DCOMPLEX: {
        return complexOnes<double>(cl, dp, dims);
    } break;
    case NLS_CHAR: {
        return realOnes<charType>(cl, dp, dims);
    } break;
    default:
        Error(ERROR_TYPE_NOT_SUPPORTED);
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
