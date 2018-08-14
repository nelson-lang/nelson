//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _SCL_SECURE_NO_WARNINGS
#endif
#include "IsSymmetric.hpp"
#include "ClassName.hpp"
#include <Eigen/Dense>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
bool
isSymmetricComplex(T* data, indexType N, bool skew)
{
    indexType i, j;
    for (i = 0; i < N; i++) {
        for (j = 1; j < N; j++) {
            T realA = data[2 * (i + j * N)];
            T realB = data[2 * (j + i * N)];
            T imagA = data[2 * (i + j * N) + 1];
            T imagB = data[2 * (j + i * N) + 1];
            if (skew) {
                if (realA != -realB) {
                    return false;
                }
                if (imagA != -imagB) {
                    return false;
                }
            } else {
                if (realA != realB) {
                    return false;
                }
                if (imagA != imagB) {
                    return false;
                }
            }
        }
    }
    return true;
}
//=============================================================================
template <class T>
bool
isSymmetricNoSkew(T* data, indexType N)
{
    indexType i, j;
    for (i = 0; i < N; i++) {
        for (j = 1; j < N; j++) {
            T realA = data[i + j * N];
            T realB = data[j + i * N];
            if (realA != realB) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
template <class T>
bool
isSymmetric(T* data, indexType N, bool skew)
{
    indexType i, j;
    for (i = 0; i < N; i++) {
        for (j = 1; j < N; j++) {
            T realA = data[i + j * N];
            T realB = data[j + i * N];
            if (skew) {
                if (realA != -realB) {
                    return false;
                }
            } else {
                if (realA != realB) {
                    return false;
                }
            }
        }
    }
    return true;
}
//=============================================================================
template <class T>
bool
isSymmetric(T* data, indexType N, double tol)
{
    indexType i, j;
    for (i = 0; i < N; i++) {
        for (j = 1; j < N; j++) {
            T realA = data[i + j * N];
            T realB = data[j + i * N];
            if (double(realA - realB) > tol) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
template <class T>
bool
isSymmetricComplex(T* data, indexType N, double tol)
{
    indexType i, j;
    for (i = 0; i < N; i++) {
        for (j = 1; j < N; j++) {
            T realA = data[2 * (i + j * N)];
            T realB = data[2 * (j + i * N)];
            T imagA = data[2 * (i + j * N) + 1];
            T imagB = data[2 * (j + i * N) + 1];
            if (realA - realB > (T)tol) {
                return false;
            }
            if (imagA - imagB > (T)tol) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
bool
IsSymmetric(ArrayOf A, bool skew)
{
    bool res = false;
    if (!A.is2D()) {
        return false;
    }
    if (!A.isSquare()) {
        return false;
    }
    switch (A.getDataClass()) {
    case NLS_SINGLE:
        return isSymmetric<single>((single*)A.getDataPointer(), A.getDimensions().getRows(), skew);
    case NLS_DOUBLE:
        return isSymmetric<double>((double*)A.getDataPointer(), A.getDimensions().getRows(), skew);
    case NLS_SCOMPLEX:
        return isSymmetricComplex<single>(
            (single*)A.getDataPointer(), A.getDimensions().getRows(), skew);
    case NLS_DCOMPLEX:
        return isSymmetricComplex<double>(
            (double*)A.getDataPointer(), A.getDimensions().getRows(), skew);
    case NLS_INT8:
        return isSymmetric<int8>((int8*)A.getDataPointer(), A.getDimensions().getRows(), skew);
    case NLS_INT16:
        return isSymmetric<int16>((int16*)A.getDataPointer(), A.getDimensions().getRows(), skew);
    case NLS_INT32:
        return isSymmetric<int32>((int32*)A.getDataPointer(), A.getDimensions().getRows(), skew);
    case NLS_INT64:
        return isSymmetric<int64>((int64*)A.getDataPointer(), A.getDimensions().getRows(), skew);
    case NLS_UINT8: {
        if (skew) {
            return false;
        }
        return isSymmetricNoSkew<uint8>((uint8*)A.getDataPointer(), A.getDimensions().getRows());
    }
    case NLS_UINT16: {
        if (skew) {
            return false;
        }
        return isSymmetricNoSkew<uint16>((uint16*)A.getDataPointer(), A.getDimensions().getRows());
    }
    case NLS_UINT32: {
        if (skew) {
            return false;
        }
        return isSymmetricNoSkew<uint32>((uint32*)A.getDataPointer(), A.getDimensions().getRows());
    }
    case NLS_UINT64: {
        if (skew) {
            return false;
        }
        return isSymmetricNoSkew<uint64>((uint64*)A.getDataPointer(), A.getDimensions().getRows());
    }
    default: {
        Error(
            _("Undefined function 'inv' for input arguments of type") + " '" + ClassName(A) + "'.");
    } break;
    }
    return res;
}
//=============================================================================
bool
IsSymmetric(ArrayOf A, double tol)
{
    bool res = false;
    if (!A.is2D()) {
        return false;
    }
    if (!A.isSquare()) {
        return false;
    }
    switch (A.getDataClass()) {
    case NLS_SINGLE:
        return isSymmetric<single>((single*)A.getDataPointer(), A.getDimensions().getRows(), tol);
    case NLS_DOUBLE:
        return isSymmetric<double>((double*)A.getDataPointer(), A.getDimensions().getRows(), tol);
    case NLS_SCOMPLEX:
        return isSymmetricComplex<single>(
            (single*)A.getDataPointer(), A.getDimensions().getRows(), tol);
    case NLS_DCOMPLEX:
        return isSymmetricComplex<double>(
            (double*)A.getDataPointer(), A.getDimensions().getRows(), tol);
    case NLS_INT8:
        return isSymmetric<int8>((int8*)A.getDataPointer(), A.getDimensions().getRows(), tol);
    case NLS_INT16:
        return isSymmetric<int16>((int16*)A.getDataPointer(), A.getDimensions().getRows(), tol);
    case NLS_INT32:
        return isSymmetric<int32>((int32*)A.getDataPointer(), A.getDimensions().getRows(), tol);
    case NLS_INT64:
        return isSymmetric<int64>((int64*)A.getDataPointer(), A.getDimensions().getRows(), tol);
    case NLS_UINT8:
        return isSymmetric<uint8>((uint8*)A.getDataPointer(), A.getDimensions().getRows(), tol);
    case NLS_UINT16:
        return isSymmetric<uint16>((uint16*)A.getDataPointer(), A.getDimensions().getRows(), tol);
    case NLS_UINT32:
        return isSymmetric<uint32>((uint32*)A.getDataPointer(), A.getDimensions().getRows(), tol);
    case NLS_UINT64:
        return isSymmetric<uint64>((uint64*)A.getDataPointer(), A.getDimensions().getRows(), tol);
    default: {
        Error(
            _("Undefined function 'inv' for input arguments of type") + " '" + ClassName(A) + "'.");
    } break;
    }
    return res;
}
//=============================================================================
}
//=============================================================================
