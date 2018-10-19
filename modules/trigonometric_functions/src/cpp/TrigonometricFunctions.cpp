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
#include <Eigen/Dense>
#include <cmath>
#include <complex>
#include <functional>
#include "TrigonometricFunctions.hpp"
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
        singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        singlecomplex* Rz = reinterpret_cast<singlecomplex*>(ptrR);
        Eigen::Map<Eigen::MatrixXcf> matA(Az, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXcf> matR(Rz, 1, A.getLength());
        matR = matA.array().cos();
        R = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), ptrR);
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        single* ptrR
            = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, A.getLength(), stringVector(), false);
        Eigen::Map<Eigen::MatrixXf> matA((single*)A.getDataPointer(), 1, A.getLength());
        Eigen::Map<Eigen::MatrixXf> matR(ptrR, 1, A.getLength());
        matR = matA.array().cos();
        R = ArrayOf(NLS_SINGLE, A.getDimensions(), ptrR);
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, A.getLength(), stringVector(), false);
        doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        doublecomplex* Rz = reinterpret_cast<doublecomplex*>((double*)ptrR);
        Eigen::Map<Eigen::MatrixXcd> matA(Az, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXcd> matR(Rz, 1, A.getLength());
        matR = matA.array().cos();
        R = ArrayOf(NLS_DCOMPLEX, A.getDimensions(), ptrR);
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, A.getLength(), stringVector(), false);
        Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), 1, A.getLength());
        Eigen::Map<Eigen::MatrixXd> matR((double*)ptrR, 1, A.getLength());
        R = ArrayOf(NLS_DOUBLE, A.getDimensions(), ptrR);
        matR = matA.array().cos();
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
        singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        singlecomplex* Rz = reinterpret_cast<singlecomplex*>((single*)ptrR);
        Eigen::Map<Eigen::MatrixXcf> matA(Az, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXcf> matR(Rz, 1, A.getLength());
        matR = matA.array().sin();
        R = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), ptrR);
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        single* ptrR
            = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, A.getLength(), stringVector(), false);
        Eigen::Map<Eigen::MatrixXf> matA((single*)A.getDataPointer(), 1, A.getLength());
        Eigen::Map<Eigen::MatrixXf> matR((single*)ptrR, 1, A.getLength());
        matR = matA.array().sin();
        R = ArrayOf(NLS_SINGLE, A.getDimensions(), ptrR);
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, A.getLength(), stringVector(), false);
        doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        doublecomplex* Rz = reinterpret_cast<doublecomplex*>((double*)ptrR);
        Eigen::Map<Eigen::MatrixXcd> matA(Az, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXcd> matR(Rz, 1, A.getLength());
        matR = matA.array().sin();
        R = ArrayOf(NLS_DCOMPLEX, A.getDimensions(), ptrR);
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, A.getLength(), stringVector(), false);
        Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), 1, A.getLength());
        Eigen::Map<Eigen::MatrixXd> matR((double*)ptrR, 1, A.getLength());
        matR = matA.array().sin();
        R = ArrayOf(NLS_DOUBLE, A.getDimensions(), ptrR);
    } break;
    }
    return R;
}
//=============================================================================
ArrayOf
Tan(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    if (A.isEmpty()) {
        ArrayOf R(A);
        R.ensureSingleOwner();
        return R;
    }
    ArrayOf R;
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
        singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        singlecomplex* Rz = reinterpret_cast<singlecomplex*>((single*)ptrR);
        Eigen::Map<Eigen::MatrixXcf> matA(Az, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXcf> matR(Rz, 1, A.getLength());
        matR = matA.array().tan();
        R = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), ptrR);
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        single* ptrR
            = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, A.getLength(), stringVector(), false);
        Eigen::Map<Eigen::MatrixXf> matA((single*)A.getDataPointer(), 1, A.getLength());
        Eigen::Map<Eigen::MatrixXf> matR((single*)ptrR, 1, A.getLength());
        matR = matA.array().tan();
        R = ArrayOf(NLS_SINGLE, A.getDimensions(), ptrR);
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, A.getLength(), stringVector(), false);
        doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        doublecomplex* Rz = reinterpret_cast<doublecomplex*>((double*)ptrR);
        Eigen::Map<Eigen::MatrixXcd> matA(Az, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXcd> matR(Rz, 1, A.getLength());
        matR = matA.array().tan();
        R = ArrayOf(NLS_DCOMPLEX, A.getDimensions(), ptrR);
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, A.getLength(), stringVector(), false);
        Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), 1, A.getLength());
        Eigen::Map<Eigen::MatrixXd> matR((double*)ptrR, 1, A.getLength());
        matR = matA.array().tan();
        R = ArrayOf(NLS_DOUBLE, A.getDimensions(), ptrR);
    } break;
    }
    return R;
}
//=============================================================================
ArrayOf
Cosh(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    if (A.isEmpty()) {
        ArrayOf R(A);
        R.ensureSingleOwner();
        return R;
    }
    ArrayOf R;
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
        singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        singlecomplex* Rz = reinterpret_cast<singlecomplex*>((single*)ptrR);
        Eigen::Map<Eigen::MatrixXcf> matA(Az, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXcf> matR(Rz, 1, A.getLength());
        matR = matA.array().cosh();
        R = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), ptrR);
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        single* ptrR
            = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, A.getLength(), stringVector(), false);
        single* pA = (single*)A.getDataPointer();
        Eigen::Map<Eigen::MatrixXf> matA(pA, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXf> matR(ptrR, 1, A.getLength());
        matR = matA.array().cosh();
        R = ArrayOf(NLS_SINGLE, A.getDimensions(), ptrR);
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, A.getLength(), stringVector(), false);
        doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        doublecomplex* Rz = reinterpret_cast<doublecomplex*>((double*)ptrR);
        Eigen::Map<Eigen::MatrixXcd> matA(Az, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXcd> matR(Rz, 1, A.getLength());
        matR = matA.array().cosh();
        R = ArrayOf(NLS_DCOMPLEX, A.getDimensions(), ptrR);
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, A.getLength(), stringVector(), false);
        double* pA = (double*)A.getDataPointer();
        Eigen::Map<Eigen::MatrixXd> matA(pA, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXd> matR(ptrR, 1, A.getLength());
        matR = matA.array().cosh();
        R = ArrayOf(NLS_DOUBLE, A.getDimensions(), ptrR);
    } break;
    }
    return R;
}
//=============================================================================
ArrayOf
Sinh(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    if (A.isEmpty()) {
        ArrayOf R(A);
        R.ensureSingleOwner();
        return R;
    }
    ArrayOf R;
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
        singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        singlecomplex* Rz = reinterpret_cast<singlecomplex*>((single*)ptrR);
        Eigen::Map<Eigen::MatrixXcf> matA(Az, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXcf> matR(Rz, 1, A.getLength());
        matR = matA.array().sinh();
        R = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), ptrR);
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        single* ptrR
            = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, A.getLength(), stringVector(), false);
        single* pA = (single*)A.getDataPointer();
        Eigen::Map<Eigen::MatrixXf> matA(pA, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXf> matR(ptrR, 1, A.getLength());
        matR = matA.array().sinh();
        R = ArrayOf(NLS_SINGLE, A.getDimensions(), ptrR);
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, A.getLength(), stringVector(), false);
        doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        doublecomplex* Rz = reinterpret_cast<doublecomplex*>((double*)ptrR);
        Eigen::Map<Eigen::MatrixXcd> matA(Az, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXcd> matR(Rz, 1, A.getLength());
        matR = matA.array().sinh();
        R = ArrayOf(NLS_DCOMPLEX, A.getDimensions(), ptrR);
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, A.getLength(), stringVector(), false);
        double* pA = (double*)A.getDataPointer();
        Eigen::Map<Eigen::MatrixXd> matA(pA, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXd> matR(ptrR, 1, A.getLength());
        matR = matA.array().sinh();
        R = ArrayOf(NLS_DOUBLE, A.getDimensions(), ptrR);
    } break;
    }
    return R;
}
//=============================================================================
ArrayOf
Tanh(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    if (A.isEmpty()) {
        ArrayOf R(A);
        R.ensureSingleOwner();
        return R;
    }
    ArrayOf R;
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
        singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        singlecomplex* Rz = reinterpret_cast<singlecomplex*>(ptrR);
        Eigen::Map<Eigen::MatrixXcf> matA(Az, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXcf> matR(Rz, 1, A.getLength());
        matR = matA.array().tanh();
        R = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), ptrR);
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        single* ptrR
            = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, A.getLength(), stringVector(), false);
        single* pA = (single*)A.getDataPointer();
        Eigen::Map<Eigen::MatrixXf> matA(pA, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXf> matR(ptrR, 1, A.getLength());
        matR = matA.array().tanh();
        R = ArrayOf(NLS_SINGLE, A.getDimensions(), ptrR);
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, A.getLength(), stringVector(), false);
        doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        doublecomplex* Rz = reinterpret_cast<doublecomplex*>((double*)ptrR);
        Eigen::Map<Eigen::MatrixXcd> matA(Az, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXcd> matR(Rz, 1, A.getLength());
        matR = matA.array().tanh();
        R = ArrayOf(NLS_DCOMPLEX, A.getDimensions(), ptrR);
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, A.getLength(), stringVector(), false);
        double* pA = (double*)A.getDataPointer();
        Eigen::Map<Eigen::MatrixXd> matA(pA, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXd> matR(ptrR, 1, A.getLength());
        matR = matA.array().tanh();
        R = ArrayOf(NLS_DOUBLE, A.getDimensions(), ptrR);
    } break;
    }
    return R;
}
//=============================================================================
ArrayOf
Acos(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    if (A.isEmpty()) {
        ArrayOf R(A);
        R.ensureSingleOwner();
        return R;
    }
    ArrayOf R;
    if (A.isSparse()) {
        needToOverload = true;
        return R;
    }
    switch (A.getDataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SINGLE: {
        R = A;
        single* ptrA = (single*)A.getDataPointer();
        bool needToConvertAsComplex = false;
        for (indexType i = 0; i < A.getDimensions().getElementCount(); i++) {
            if (std::abs(ptrA[i]) > 1) {
                needToConvertAsComplex = true;
                break;
            }
        }
        if (needToConvertAsComplex) {
            R.ensureSingleOwner();
            R.promoteType(NLS_SCOMPLEX);
            singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)R.getDataPointer());
            singlecomplex* Rz = reinterpret_cast<singlecomplex*>((single*)R.getDataPointer());
            Eigen::Map<Eigen::MatrixXcf> matA(Az, 1, A.getLength());
            Eigen::Map<Eigen::MatrixXcf> matR(Rz, 1, R.getLength());
            matR = matA.array().acos();
            if (R.allReal()) {
                R.promoteType(NLS_SINGLE);
            }
        } else {
            R.ensureSingleOwner();
            Eigen::Map<Eigen::MatrixXf> matA((single*)A.getDataPointer(), 1, A.getLength());
            Eigen::Map<Eigen::MatrixXf> matR((single*)R.getDataPointer(), 1, R.getLength());
            matR = matA.array().acos();
        }
    } break;
    case NLS_SCOMPLEX: {
        R = A;
        R.ensureSingleOwner();
        singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)R.getDataPointer());
        singlecomplex* Rz = reinterpret_cast<singlecomplex*>((single*)R.getDataPointer());
        Eigen::Map<Eigen::MatrixXcf> matA(Az, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXcf> matR(Rz, 1, R.getLength());
        matR = matA.array().acos();
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrA = (double*)A.getDataPointer();
        bool needToConvertAsComplex = false;
        for (indexType i = 0; i < A.getDimensions().getElementCount(); i++) {
            if (std::abs(ptrA[i]) > 1) {
                needToConvertAsComplex = true;
                break;
            }
        }
        if (needToConvertAsComplex) {
            R = A;
            R.ensureSingleOwner();
            R.promoteType(NLS_DCOMPLEX);
            doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)R.getDataPointer());
            doublecomplex* Rz = reinterpret_cast<doublecomplex*>((double*)R.getDataPointer());
            Eigen::Map<Eigen::MatrixXcd> matA(Az, 1, A.getLength());
            Eigen::Map<Eigen::MatrixXcd> matR(Rz, 1, R.getLength());
            matR = matA.array().acos();
            if (R.allReal()) {
                R.promoteType(NLS_DOUBLE);
            }
        } else {
            double* ptrR = (double*)ArrayOf::allocateArrayOf(
                NLS_DOUBLE, A.getLength(), stringVector(), false);
            Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), 1, A.getLength());
            Eigen::Map<Eigen::MatrixXd> matR(ptrR, 1, A.getLength());
            matR = matA.array().acos();
            R = ArrayOf(NLS_DOUBLE, A.getDimensions(), ptrR);
        }
    } break;
    case NLS_DCOMPLEX: {
        R = A;
        R.ensureSingleOwner();
        doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)R.getDataPointer());
        doublecomplex* Rz = reinterpret_cast<doublecomplex*>((double*)R.getDataPointer());
        Eigen::Map<Eigen::MatrixXcd> matA(Az, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXcd> matR(Rz, 1, R.getLength());
        matR = matA.array().acos();
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
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
    if (A.isEmpty()) {
        ArrayOf R(A);
        R.ensureSingleOwner();
        return R;
    }
    ArrayOf R;
    if (A.isSparse()) {
        needToOverload = true;
        return R;
    }
    switch (A.getDataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SINGLE: {
        R = A;
        single* ptrA = (single*)A.getDataPointer();
        bool needToConvertAsComplex = false;
        for (indexType i = 0; i < A.getDimensions().getElementCount(); i++) {
            if (std::abs(ptrA[i]) > 1) {
                needToConvertAsComplex = true;
                break;
            }
        }
        if (needToConvertAsComplex) {
            R.ensureSingleOwner();
            R.promoteType(NLS_SCOMPLEX);
            singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)R.getDataPointer());
            singlecomplex* Rz = reinterpret_cast<singlecomplex*>((single*)R.getDataPointer());
            Eigen::Map<Eigen::MatrixXcf> matA(Az, 1, A.getLength());
            Eigen::Map<Eigen::MatrixXcf> matR(Rz, 1, R.getLength());
            matR = matA.array().asin();
            if (R.allReal()) {
                R.promoteType(NLS_SINGLE);
            }
        } else {
            R.ensureSingleOwner();
            Eigen::Map<Eigen::MatrixXf> matA((single*)A.getDataPointer(), 1, A.getLength());
            Eigen::Map<Eigen::MatrixXf> matR((single*)R.getDataPointer(), 1, R.getLength());
            matR = matA.array().asin();
        }
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR
            = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, A.getLength(), stringVector(), false);
        singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        singlecomplex* Rz = reinterpret_cast<singlecomplex*>((single*)ptrR);
        Eigen::Map<Eigen::MatrixXcf> matA(Az, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXcf> matR(Rz, 1, A.getLength());
        matR = matA.array().asin();
        R = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), ptrR);
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrA = (double*)A.getDataPointer();
        bool needToConvertAsComplex = false;
        for (indexType i = 0; i < A.getDimensions().getElementCount(); i++) {
            if (std::fabs(ptrA[i]) > 1) {
                needToConvertAsComplex = true;
                break;
            }
        }
        if (needToConvertAsComplex) {
            R = A;
            R.ensureSingleOwner();
            R.promoteType(NLS_DCOMPLEX);
            doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)R.getDataPointer());
            doublecomplex* Rz = reinterpret_cast<doublecomplex*>((double*)R.getDataPointer());
            Eigen::Map<Eigen::MatrixXcd> matA(Az, 1, A.getLength());
            Eigen::Map<Eigen::MatrixXcd> matR(Rz, 1, R.getLength());
            matR = matA.array().asin();
            if (R.allReal()) {
                R.promoteType(NLS_DOUBLE);
            }
        } else {
            double* ptrR = (double*)ArrayOf::allocateArrayOf(
                NLS_DOUBLE, A.getLength(), stringVector(), false);
            Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), 1, A.getLength());
            Eigen::Map<Eigen::MatrixXd> matR(ptrR, 1, A.getLength());
            matR = matA.array().asin();
            R = ArrayOf(NLS_DOUBLE, A.getDimensions(), ptrR);
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, A.getLength(), stringVector(), false);
        doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        doublecomplex* Rz = reinterpret_cast<doublecomplex*>((double*)ptrR);
        Eigen::Map<Eigen::MatrixXcd> matA(Az, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXcd> matR(Rz, 1, R.getLength());
        matR = matA.array().asin();
        R = ArrayOf(NLS_DCOMPLEX, A.getDimensions(), ptrR);
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
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
    if (A.isEmpty()) {
        ArrayOf R(A);
        R.ensureSingleOwner();
        return R;
    }
    ArrayOf R;
    if (A.isSparse()) {
        needToOverload = true;
        return R;
    }
    switch (A.getDataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SINGLE: {
        single* ptrR
            = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, A.getLength(), stringVector(), false);
        single* pA = (single*)A.getDataPointer();
        Eigen::Map<Eigen::MatrixXf> matA(pA, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXf> matR(ptrR, 1, A.getLength());
        matR = matA.array().atan();
        R = ArrayOf(NLS_SINGLE, A.getDimensions(), ptrR);
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR
            = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, A.getLength(), stringVector(), false);
        singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        singlecomplex* Rz = reinterpret_cast<singlecomplex*>((single*)ptrR);
        Eigen::Map<Eigen::MatrixXcf> matA(Az, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXcf> matR(Rz, 1, A.getLength());
        matR = matA.array().atan();
        R = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), ptrR);
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, A.getLength(), stringVector(), false);
        double* pA = (double*)A.getDataPointer();
        Eigen::Map<Eigen::MatrixXd> matA(pA, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXd> matR(ptrR, 1, A.getLength());
        matR = matA.array().atan();
        R = ArrayOf(NLS_DOUBLE, A.getDimensions(), ptrR);
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR
            = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, A.getLength(), stringVector(), false);
        doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)R.getDataPointer());
        doublecomplex* Rz = reinterpret_cast<doublecomplex*>((double*)ptrR);
        Eigen::Map<Eigen::MatrixXcd> matA(Az, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXcd> matR(Rz, 1, A.getLength());
        matR = matA.array().atan();
        R = ArrayOf(NLS_DCOMPLEX, A.getDimensions(), ptrR);
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    }
    return R;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
