//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
/**
 * @brief Check if all elements in the given array are finite.
 *
 * This function checks if all elements in the input array are finite. The function
 * uses Eigen's `allFinite()` method to perform the check.
 *
 * @tparam T The data type of the elements in the array.
 * @param A The input array to check for finiteness.
 * @return True if all elements are finite, false otherwise.
 */
template <class T>
inline bool
isAllFinite(const ArrayOf& A)
{
    if (A.isComplex()) {
        auto* pzA = reinterpret_cast<std::complex<T>*>((T*)A.getDataPointer());
        Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matArrayIn(
            pzA, (Eigen::Index)A.getRows(), (Eigen::Index)A.getColumns());
        return matArrayIn.allFinite();

    } else {
        Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matArrayIn(
            (T*)A.getDataPointer(), (Eigen::Index)A.getRows(), (Eigen::Index)A.getColumns());
        return matArrayIn.allFinite();
    }
    return false;
}
//=============================================================================
}
//=============================================================================
