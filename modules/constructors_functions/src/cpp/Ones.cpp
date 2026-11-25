//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
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
#include "NelsonTypeVisitor.hpp"
#include "NelsonComplexStorage.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
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
    ompIndexType nbElements = (ompIndexType)outDims.elementCount();
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
    auto* ptrDpz = TypeTraits::asComplex<T>(dp);
    std::complex<T> value = { 1, 0 };
    ompIndexType nbElements = (ompIndexType)outDims.elementCount();
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
        ArrayOf res = ArrayOf::empty(dims);
        res.promoteType(cl);
        return res;
    }

    indexType nbElements = dims.elementCount();
    void* dp = ArrayOf::allocateArrayOf(cl, nbElements, Nelson::stringVector(), false);

    return TypeTraits::visitStorage(cl, [&](auto tag) {
        using T = decltype(tag);
        if constexpr (TypeTraits::isComplexElement<T>) {
            return complexOnes<typename T::value_type>(cl, dp, dims);
        } else {
            return realOnes<T>(cl, dp, dims);
        }
    });
}
//=============================================================================
} // namespace Nelson
//=============================================================================
