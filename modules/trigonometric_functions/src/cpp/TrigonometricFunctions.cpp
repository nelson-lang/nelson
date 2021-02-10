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
#include <Eigen/Dense>
#include <cmath>
#include <complex>
#include <functional>
#include "TrigonometricFunctions.hpp"
#include "nlsConfig.h"
#include "ClassName.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Cos(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf R;
    if (A.isEmpty()) {
        R = A;
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        needToOverload = true;
        return R;
    }
    switch (A.getDataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR
            = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        auto* Rz = reinterpret_cast<singlecomplex*>(ptrR);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            Rz[k] = std::cos(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        single* ptrR
            = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_SINGLE, A.getDimensions(), ptrR);
        single* ptrA = (single*)A.getDataPointer();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            ptrR[k] = cos(ptrA[k]);
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_DCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        auto* Rz = reinterpret_cast<doublecomplex*>(ptrR);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            Rz[k] = std::cos(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_DOUBLE, A.getDimensions(), ptrR);
        double* ptrA = (double*)A.getDataPointer();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            ptrR[k] = cos(ptrA[k]);
        }
    } break;
    }
    return R;
}
//=============================================================================
ArrayOf
Sin(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf R;
    if (A.isEmpty()) {
        R = A;
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        needToOverload = true;
        return R;
    }
    switch (A.getDataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR
            = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        auto* Rz = reinterpret_cast<singlecomplex*>(ptrR);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            Rz[k] = std::sin(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        single* ptrR
            = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_SINGLE, A.getDimensions(), ptrR);
        single* ptrA = (single*)A.getDataPointer();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            ptrR[k] = std::sin(ptrA[k]);
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_DCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        auto* Rz = reinterpret_cast<doublecomplex*>(ptrR);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            Rz[k] = std::sin(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_DOUBLE, A.getDimensions(), ptrR);
        double* ptrA = (double*)A.getDataPointer();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            ptrR[k] = sin(ptrA[k]);
        }
    } break;
    }
    return R;
}
//=============================================================================
ArrayOf
Tan(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf R;
    if (A.isEmpty()) {
        R = A;
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        needToOverload = true;
        return R;
    }
    switch (A.getDataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR
            = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        auto* Rz = reinterpret_cast<singlecomplex*>(ptrR);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            Rz[k] = std::tan(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        single* ptrR
            = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_SINGLE, A.getDimensions(), ptrR);
        single* ptrA = (single*)A.getDataPointer();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            ptrR[k] = tan(ptrA[k]);
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_DCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        auto* Rz = reinterpret_cast<doublecomplex*>(ptrR);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            Rz[k] = tan(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_DOUBLE, A.getDimensions(), ptrR);
        double* ptrA = (double*)A.getDataPointer();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            ptrR[k] = tan(ptrA[k]);
        }
    } break;
    }
    return R;
}
//=============================================================================
ArrayOf
Cosh(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf R;
    if (A.isEmpty()) {
        R = A;
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        needToOverload = true;
        return R;
    }
    switch (A.getDataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR
            = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        auto* Rz = reinterpret_cast<singlecomplex*>(ptrR);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            Rz[k] = std::cosh(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        single* ptrR
            = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_SINGLE, A.getDimensions(), ptrR);
        single* ptrA = (single*)A.getDataPointer();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            ptrR[k] = cosh(ptrA[k]);
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_DCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        auto* Rz = reinterpret_cast<doublecomplex*>(ptrR);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            Rz[k] = std::cosh(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_DOUBLE, A.getDimensions(), ptrR);
        double* ptrA = (double*)A.getDataPointer();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            ptrR[k] = cosh(ptrA[k]);
        }
    } break;
    }
    return R;
}
//=============================================================================
ArrayOf
Sinh(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf R;
    if (A.isEmpty()) {
        R = A;
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        needToOverload = true;
        return R;
    }
    switch (A.getDataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR
            = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        auto* Rz = reinterpret_cast<singlecomplex*>(ptrR);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            Rz[k] = std::sinh(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        single* ptrR
            = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_SINGLE, A.getDimensions(), ptrR);
        single* ptrA = (single*)A.getDataPointer();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            ptrR[k] = sinh(ptrA[k]);
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_DCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        auto* Rz = reinterpret_cast<doublecomplex*>(ptrR);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            Rz[k] = std::sinh(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_DOUBLE, A.getDimensions(), ptrR);
        double* ptrA = (double*)A.getDataPointer();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            ptrR[k] = sinh(ptrA[k]);
        }
    } break;
    }
    return R;
}
//=============================================================================
ArrayOf
Tanh(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf R;
    if (A.isEmpty()) {
        R = A;
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        needToOverload = true;
        return R;
    }
    switch (A.getDataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR
            = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        auto* Rz = reinterpret_cast<singlecomplex*>(ptrR);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            Rz[k] = std::tanh(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        single* ptrR
            = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_SINGLE, A.getDimensions(), ptrR);
        single* ptrA = (single*)A.getDataPointer();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            ptrR[k] = tanh(ptrA[k]);
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_DCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        auto* Rz = reinterpret_cast<doublecomplex*>(ptrR);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            Rz[k] = std::tanh(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_DOUBLE, A.getDimensions(), ptrR);
        double* ptrA = (double*)A.getDataPointer();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            ptrR[k] = tanh(ptrA[k]);
        }
    } break;
    }
    return R;
}
//=============================================================================
ArrayOf
Acos(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf R;
    if (A.isEmpty()) {
        R = A;
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        needToOverload = true;
        return R;
    }
    switch (A.getDataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR
            = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        auto* Rz = reinterpret_cast<singlecomplex*>(ptrR);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            Rz[k] = std::acos(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        auto* ptrA = (single*)A.getDataPointer();
        bool needToConvertAsComplex = false;
        indexType elementCount = A.getElementCount();
        for (indexType i = 0; i < elementCount; i++) {
            if (std::fabs(ptrA[i]) > 1) {
                needToConvertAsComplex = true;
                break;
            }
        }
        if (needToConvertAsComplex) {
            R = A;
            R.promoteType(NLS_SCOMPLEX);
            R = Acos(R, needToOverload);
        } else {
            single* ptrR = (single*)ArrayOf::allocateArrayOf(
                NLS_SINGLE, A.getLength(), stringVector(), false);
            R = ArrayOf(NLS_SINGLE, A.getDimensions(), ptrR);
            single* ptrA = (single*)A.getDataPointer();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
                ptrR[k] = acos(ptrA[k]);
            }
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_DCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        auto* Rz = reinterpret_cast<doublecomplex*>(ptrR);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            Rz[k] = std::acos(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        auto* ptrA = (double*)A.getDataPointer();
        bool needToConvertAsComplex = false;
        indexType elementCount = A.getElementCount();
        for (indexType i = 0; i < elementCount; i++) {
            if (std::abs(ptrA[i]) > 1) {
                needToConvertAsComplex = true;
                break;
            }
        }
        if (needToConvertAsComplex) {
            R = A;
            R.promoteType(NLS_DCOMPLEX);
            R = Acos(R, needToOverload);
        } else {
            double* ptrR = (double*)ArrayOf::allocateArrayOf(
                NLS_DOUBLE, A.getLength(), stringVector(), false);
            R = ArrayOf(NLS_DOUBLE, A.getDimensions(), ptrR);
            double* ptrA = (double*)A.getDataPointer();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
                ptrR[k] = acos(ptrA[k]);
            }
        }
    } break;
    }
    return R;
}
//=============================================================================
ArrayOf
Asin(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf R;
    if (A.isEmpty()) {
        R = A;
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        needToOverload = true;
        return R;
    }
    switch (A.getDataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR
            = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        auto* Rz = reinterpret_cast<singlecomplex*>(ptrR);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            Rz[k] = std::asin(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        auto* ptrA = (single*)A.getDataPointer();
        bool needToConvertAsComplex = false;
        indexType elementCount = A.getElementCount();
        for (indexType i = 0; i < elementCount; i++) {
            if (std::fabs(ptrA[i]) > 1) {
                needToConvertAsComplex = true;
                break;
            }
        }
        if (needToConvertAsComplex) {
            R = A;
            R.ensureSingleOwner();
            R.promoteType(NLS_SCOMPLEX);
            R = Asin(R, needToOverload);
        } else {
            single* ptrR = (single*)ArrayOf::allocateArrayOf(
                NLS_SINGLE, A.getLength(), stringVector(), false);
            R = ArrayOf(NLS_SINGLE, A.getDimensions(), ptrR);
            single* ptrA = (single*)A.getDataPointer();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
                ptrR[k] = asin(ptrA[k]);
            }
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_DCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        auto* Rz = reinterpret_cast<doublecomplex*>(ptrR);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            Rz[k] = std::asin(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        auto* ptrA = (double*)A.getDataPointer();
        bool needToConvertAsComplex = false;
        indexType elementCount = A.getElementCount();
        for (indexType i = 0; i < elementCount; i++) {
            if (std::abs(ptrA[i]) > 1) {
                needToConvertAsComplex = true;
                break;
            }
        }
        if (needToConvertAsComplex) {
            R = A;
            R.ensureSingleOwner();
            R.promoteType(NLS_DCOMPLEX);
            R = Asin(R, needToOverload);
        } else {
            double* ptrR = (double*)ArrayOf::allocateArrayOf(
                NLS_DOUBLE, A.getLength(), stringVector(), false);
            R = ArrayOf(NLS_DOUBLE, A.getDimensions(), ptrR);
            double* ptrA = (double*)A.getDataPointer();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
                ptrR[k] = asin(ptrA[k]);
            }
        }
    } break;
    }
    return R;
}
//=============================================================================
ArrayOf
Atan(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf R;
    if (A.isEmpty()) {
        R = A;
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        needToOverload = true;
        return R;
    }
    switch (A.getDataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR
            = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        auto* Rz = reinterpret_cast<singlecomplex*>(ptrR);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            Rz[k] = std::atan(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        single* ptrR
            = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_SINGLE, A.getDimensions(), ptrR);
        single* ptrA = (single*)A.getDataPointer();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            ptrR[k] = atan(ptrA[k]);
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_DCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        auto* Rz = reinterpret_cast<doublecomplex*>(ptrR);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            Rz[k] = std::atan(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, A.getLength(), stringVector(), false);
        R = ArrayOf(NLS_DOUBLE, A.getDimensions(), ptrR);
        double* ptrA = (double*)A.getDataPointer();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getLength(); ++k) {
            ptrR[k] = atan(ptrA[k]);
        }
    } break;
    }
    return R;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
