//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include <cmath>
#include <complex>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
inline bool
isAllFinite(const ArrayOf& A)
{
    indexType elementCount = A.elementCount();
    if (A.isComplex()) {
        auto* data = reinterpret_cast<const std::complex<T>*>((const T*)A.dataPointer());
        for (indexType k = 0; k < elementCount; ++k) {
            if (!std::isfinite(data[k].real()) || !std::isfinite(data[k].imag())) {
                return false;
            }
        }
        return true;
    }
    auto* data = reinterpret_cast<const T*>(A.dataPointer());
    for (indexType k = 0; k < elementCount; ++k) {
        if (!std::isfinite(data[k])) {
            return false;
        }
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
