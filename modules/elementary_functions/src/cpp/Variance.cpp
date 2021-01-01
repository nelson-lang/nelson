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
#include "nlsConfig.h"
#include "Variance.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
VarianceReal(const T* sp, T* dp, int planes, int planesize, int linesize)
{
    ompIndexType i;
    ompIndexType j;
    ompIndexType k;
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for private(j, k)
#endif
    for (i = 0; i < (ompIndexType)planes; i++) {
        for (j = 0; j < (ompIndexType)planesize; j++) {
            T accum_first = 0;
            for (k = 0; k < (ompIndexType)linesize; k++) {
                accum_first += sp[i * planesize * linesize + j + k * planesize] / linesize;
            }
            T accum_second = 0;
            for (k = 0; k < (ompIndexType)linesize; k++) {
                T tmp = sp[i * planesize * linesize + j + k * planesize] - accum_first;
                accum_second += (T)(tmp * tmp / (linesize - 1.0));
            }
            dp[i * planesize + j] = (T)accum_second;
        }
    }
}
//=============================================================================
template <class T>
void
VarianceComplex(const std::complex<T>* sp, T* dp, int planes, int planesize, int linesize)
{
    ompIndexType i;
    ompIndexType j;
    ompIndexType k;
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for private(j, k)
#endif
    for (i = 0; i < (ompIndexType)planes; i++) {
        for (j = 0; j < (ompIndexType)planesize; j++) {
            T accum_first = 0;
            for (k = 0; k < (ompIndexType)linesize; k++) {
                accum_first += sp[i * planesize * linesize + j + k * planesize].real() / linesize;
            }
            T accum_second = 0;
            for (k = 0; k < (ompIndexType)linesize; k++) {
                T tmp = sp[i * planesize * linesize + j + k * planesize].real() - accum_first;
                accum_second += (T)(tmp * tmp / (linesize - 1.0));
            }
            dp[i * planesize + j] = (T)accum_second;
        }
    }
}
//=============================================================================
ArrayOf
Variance(const ArrayOf& A, int w, int dim, bool& needToOverload)
{
    needToOverload = false;
    if (w != 0) {
        Error(_W("Second argument must be 0."));
    }
    Dimensions dimsA = A.getDimensions();
    if (dim < 0) {
        int d = 0;
        while (dimsA[d] == 1) {
            d++;
        }
        dim = d;
    } else {
        dim = dim - 1;
    }
    Dimensions outDim(dimsA);
    outDim[dim] = 1;
    int linesize = (int)dimsA[dim];
    int planesize = 1;
    for (int k = 0; k < dim; k++) {
        planesize *= (int)dimsA[k];
    }
    int planecount = 1;
    for (int k = dim + 1; k < (int)dimsA.getLength(); k++) {
        planecount *= (int)dimsA[k];
    }
    ArrayOf res;
    switch (A.getDataClass()) {
    case NLS_DOUBLE: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
        res = ArrayOf(NLS_DOUBLE, outDim, ptr);
        VarianceReal<double>(
            (const double*)A.getDataPointer(), ptr, planecount, planesize, linesize);
    } break;
    case NLS_SINGLE: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, outDim.getElementCount());
        res = ArrayOf(NLS_SINGLE, outDim, ptr);
        VarianceReal<single>(
            (const single*)A.getDataPointer(), ptr, planecount, planesize, linesize);
    } break;
    case NLS_DCOMPLEX: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
        res = ArrayOf(NLS_DOUBLE, outDim, ptr);
        auto* ptrZ = reinterpret_cast<std::complex<double>*>((double*)A.getDataPointer());
        VarianceComplex<double>(ptrZ, ptr, planecount, planesize, linesize);
    } break;
    case NLS_SCOMPLEX: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, outDim.getElementCount());
        res = ArrayOf(NLS_SINGLE, outDim, ptr);
        auto* ptrZ = reinterpret_cast<std::complex<single>*>((single*)A.getDataPointer());
        VarianceComplex<single>(ptrZ, ptr, planecount, planesize, linesize);
    } break;
    default: {
        needToOverload = true;
    } break;
    }
    return res;
}
//=============================================================================
}
//=============================================================================
