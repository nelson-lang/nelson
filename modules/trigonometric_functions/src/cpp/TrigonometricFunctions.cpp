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
#include "TrigonometricFunctions.hpp"
#include "ClassName.hpp"
#include <Eigen/Dense>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Cos(ArrayOf A)
{
    if (A.isEmpty()) {
        ArrayOf R(A);
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        throw Exception(
            _("Undefined function 'cos' for input arguments of type") + " '" + ClassName(A) + "'.");
    }
    switch (A.getDataClass()) {
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_CHAR: {
        throw Exception(
            _("Undefined function 'cos' for input arguments of type") + " '" + ClassName(A) + "'.");
    } break;
    case NLS_SCOMPLEX: {
        ArrayOf R(A);
        R.ensureSingleOwner();
        singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        singlecomplex* Rz = reinterpret_cast<singlecomplex*>((single*)R.getDataPointer());
        Eigen::Map<Eigen::MatrixXcf> matA(Az, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXcf> matR(Rz, 1, R.getLength());
        matR = matA.array().cos();
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
        return R;
    }
    case NLS_SINGLE: {
        ArrayOf R(A);
        R.ensureSingleOwner();
        // to speed up computations, we use a vector with eigen library and MKL
        Eigen::Map<Eigen::MatrixXf> matA((single*)A.getDataPointer(), 1, A.getLength());
        Eigen::Map<Eigen::MatrixXf> matR((single*)R.getDataPointer(), 1, R.getLength());
        matR = matA.array().cos();
        return R;
    } break;
    case NLS_DCOMPLEX: {
        ArrayOf R(A);
        R.ensureSingleOwner();
        doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        doublecomplex* Rz = reinterpret_cast<doublecomplex*>((double*)R.getDataPointer());
        Eigen::Map<Eigen::MatrixXcd> matA(Az, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXcd> matR(Rz, 1, R.getLength());
        matR = matA.array().cos();
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
        return R;
    } break;
    case NLS_DOUBLE: {
        ArrayOf R(A);
        R.ensureSingleOwner();
        Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), 1, A.getLength());
        Eigen::Map<Eigen::MatrixXd> matR((double*)R.getDataPointer(), 1, R.getLength());
        matR = matA.array().cos();
        return R;
    } break;
    default: {
        throw Exception(_W("Invalid conversion."));
    } break;
    }
    return ArrayOf();
}
//=============================================================================
ArrayOf
Sin(ArrayOf A)
{
    if (A.isEmpty()) {
        ArrayOf R(A);
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        throw Exception(
            _("Undefined function 'sin' for input arguments of type") + " '" + ClassName(A) + "'.");
    }
    switch (A.getDataClass()) {
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_CHAR: {
        throw Exception(
            _("Undefined function 'sin' for input arguments of type") + " '" + ClassName(A) + "'.");
    } break;
    case NLS_SCOMPLEX: {
        ArrayOf R(A);
        R.ensureSingleOwner();
        singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        singlecomplex* Rz = reinterpret_cast<singlecomplex*>((single*)R.getDataPointer());
        Eigen::Map<Eigen::MatrixXcf> matA(Az, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXcf> matR(Rz, 1, R.getLength());
        matR = matA.array().sin();
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
        return R;
    }
    case NLS_SINGLE: {
        ArrayOf R(A);
        R.ensureSingleOwner();
        // to speed up computations, we use a vector with eigen library and MKL
        Eigen::Map<Eigen::MatrixXf> matA((single*)A.getDataPointer(), 1, A.getLength());
        Eigen::Map<Eigen::MatrixXf> matR((single*)R.getDataPointer(), 1, R.getLength());
        matR = matA.array().sin();
        return R;
    } break;
    case NLS_DCOMPLEX: {
        ArrayOf R(A);
        R.ensureSingleOwner();
        doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        doublecomplex* Rz = reinterpret_cast<doublecomplex*>((double*)R.getDataPointer());
        Eigen::Map<Eigen::MatrixXcd> matA(Az, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXcd> matR(Rz, 1, R.getLength());
        matR = matA.array().sin();
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
        return R;
    } break;
    case NLS_DOUBLE: {
        ArrayOf R(A);
        R.ensureSingleOwner();
        Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), 1, A.getLength());
        Eigen::Map<Eigen::MatrixXd> matR((double*)R.getDataPointer(), 1, R.getLength());
        matR = matA.array().sin();
        return R;
    } break;
    default: {
        throw Exception(_W("Invalid conversion."));
    } break;
    }
    return ArrayOf();
}
//=============================================================================
ArrayOf
Tan(ArrayOf A)
{
    if (A.isEmpty()) {
        ArrayOf R(A);
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        throw Exception(
            _("Undefined function 'tan' for input arguments of type") + " '" + ClassName(A) + "'.");
    }
    switch (A.getDataClass()) {
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_CHAR: {
        throw Exception(
            _("Undefined function 'tan' for input arguments of type") + " '" + ClassName(A) + "'.");
    } break;
    case NLS_SCOMPLEX: {
        ArrayOf R(A);
        R.ensureSingleOwner();
        singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        singlecomplex* Rz = reinterpret_cast<singlecomplex*>((single*)R.getDataPointer());
        Eigen::Map<Eigen::MatrixXcf> matA(Az, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXcf> matR(Rz, 1, R.getLength());
        matR = matA.array().tan();
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
        return R;
    }
    case NLS_SINGLE: {
        ArrayOf R(A);
        R.ensureSingleOwner();
        // to speed up computations, we use a vector with eigen library and MKL
        Eigen::Map<Eigen::MatrixXf> matA((single*)A.getDataPointer(), 1, A.getLength());
        Eigen::Map<Eigen::MatrixXf> matR((single*)R.getDataPointer(), 1, R.getLength());
        matR = matA.array().tan();
        return R;
    } break;
    case NLS_DCOMPLEX: {
        ArrayOf R(A);
        R.ensureSingleOwner();
        doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        doublecomplex* Rz = reinterpret_cast<doublecomplex*>((double*)R.getDataPointer());
        Eigen::Map<Eigen::MatrixXcd> matA(Az, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXcd> matR(Rz, 1, R.getLength());
        matR = matA.array().tan();
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
        return R;
    } break;
    case NLS_DOUBLE: {
        ArrayOf R(A);
        R.ensureSingleOwner();
        Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), 1, A.getLength());
        Eigen::Map<Eigen::MatrixXd> matR((double*)R.getDataPointer(), 1, R.getLength());
        matR = matA.array().tan();
        return R;
    } break;
    default: {
        throw Exception(_W("Invalid conversion."));
    } break;
    }
    return ArrayOf();
}
//=============================================================================
ArrayOf
Cosh(ArrayOf A)
{
    if (A.isEmpty()) {
        ArrayOf R(A);
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        throw Exception(_("Undefined function 'cosh' for input arguments of type") + " '"
            + ClassName(A) + "'.");
    }
    switch (A.getDataClass()) {
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_CHAR: {
        throw Exception(_("Undefined function 'cosh' for input arguments of type") + " '"
            + ClassName(A) + "'.");
    } break;
    // cosh not implemented in Eigen, we use std cosh
    case NLS_SCOMPLEX: {
        ArrayOf R(A);
        R.ensureSingleOwner();
        singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        singlecomplex* Rz = reinterpret_cast<singlecomplex*>((single*)R.getDataPointer());
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (indexType z = 0; z < A.getLength(); z++) {
            Rz[z] = cosh(Az[z]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
        return R;
    }
    case NLS_SINGLE: {
        ArrayOf R(A);
        R.ensureSingleOwner();
        single* pA = (single*)A.getDataPointer();
        single* pR = (single*)R.getDataPointer();
        for (indexType z = 0; z < A.getLength(); z++) {
            pR[z] = cosh(pA[z]);
        }
        return R;
    } break;
    case NLS_DCOMPLEX: {
        ArrayOf R(A);
        R.ensureSingleOwner();
        doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        doublecomplex* Rz = reinterpret_cast<doublecomplex*>((double*)R.getDataPointer());
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (indexType z = 0; z < A.getLength(); z++) {
            Rz[z] = cosh(Az[z]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
        return R;
    } break;
    case NLS_DOUBLE: {
        ArrayOf R(A);
        R.ensureSingleOwner();
        double* pA = (double*)A.getDataPointer();
        double* pR = (double*)R.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (indexType z = 0; z < A.getLength(); z++) {
            pR[z] = cosh(pA[z]);
        }
        return R;
    } break;
    default: {
        throw Exception(_W("Invalid conversion."));
    } break;
    }
    return ArrayOf();
}
//=============================================================================
ArrayOf
Sinh(ArrayOf A)
{
    if (A.isEmpty()) {
        ArrayOf R(A);
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        throw Exception(_("Undefined function 'sinh' for input arguments of type") + " '"
            + ClassName(A) + "'.");
    }
    switch (A.getDataClass()) {
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_CHAR: {
        throw Exception(_("Undefined function 'sinh' for input arguments of type") + " '"
            + ClassName(A) + "'.");
    } break;
    // sinh not implemented in Eigen, we use std sinh
    case NLS_SCOMPLEX: {
        ArrayOf R(A);
        R.ensureSingleOwner();
        singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        singlecomplex* Rz = reinterpret_cast<singlecomplex*>((single*)R.getDataPointer());
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (indexType z = 0; z < A.getLength(); z++) {
            Rz[z] = sinh(Az[z]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
        return R;
    }
    case NLS_SINGLE: {
        ArrayOf R(A);
        R.ensureSingleOwner();
        single* pA = (single*)A.getDataPointer();
        single* pR = (single*)R.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (indexType z = 0; z < A.getLength(); z++) {
            pR[z] = sinh(pA[z]);
        }
        return R;
    } break;
    case NLS_DCOMPLEX: {
        ArrayOf R(A);
        R.ensureSingleOwner();
        doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        doublecomplex* Rz = reinterpret_cast<doublecomplex*>((double*)R.getDataPointer());
        for (indexType z = 0; z < A.getLength(); z++) {
            Rz[z] = sinh(Az[z]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
        return R;
    } break;
    case NLS_DOUBLE: {
        ArrayOf R(A);
        R.ensureSingleOwner();
        double* pA = (double*)A.getDataPointer();
        double* pR = (double*)R.getDataPointer();
        for (indexType z = 0; z < A.getLength(); z++) {
            pR[z] = sinh(pA[z]);
        }
        return R;
    } break;
    default: {
        throw Exception(_W("Invalid conversion."));
    } break;
    }
    return ArrayOf();
}
//=============================================================================
ArrayOf
Tanh(ArrayOf A)
{
    if (A.isEmpty()) {
        ArrayOf R(A);
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        throw Exception(_("Undefined function 'tanh' for input arguments of type") + " '"
            + ClassName(A) + "'.");
    }
    switch (A.getDataClass()) {
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_CHAR: {
        throw Exception(_("Undefined function 'tanh' for input arguments of type") + " '"
            + ClassName(A) + "'.");
    } break;
    // tanh not implemented in Eigen, we use std tanh
    case NLS_SCOMPLEX: {
        ArrayOf R(A);
        R.ensureSingleOwner();
        singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        singlecomplex* Rz = reinterpret_cast<singlecomplex*>((single*)R.getDataPointer());
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (indexType z = 0; z < A.getLength(); z++) {
            Rz[z] = tanh(Az[z]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
        return R;
    }
    case NLS_SINGLE: {
        ArrayOf R(A);
        R.ensureSingleOwner();
        single* pA = (single*)A.getDataPointer();
        single* pR = (single*)R.getDataPointer();
        for (indexType z = 0; z < A.getLength(); z++) {
            pR[z] = tanh(pA[z]);
        }
        return R;
    } break;
    case NLS_DCOMPLEX: {
        ArrayOf R(A);
        R.ensureSingleOwner();
        doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        doublecomplex* Rz = reinterpret_cast<doublecomplex*>((double*)R.getDataPointer());
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (indexType z = 0; z < A.getLength(); z++) {
            Rz[z] = tanh(Az[z]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
        return R;
    } break;
    case NLS_DOUBLE: {
        ArrayOf R(A);
        R.ensureSingleOwner();
        double* pA = (double*)A.getDataPointer();
        double* pR = (double*)R.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (indexType z = 0; z < A.getLength(); z++) {
            pR[z] = tanh(pA[z]);
        }
        return R;
    } break;
    default: {
        throw Exception(_W("Invalid conversion."));
    } break;
    }
    return ArrayOf();
}
//=============================================================================
ArrayOf
Acos(ArrayOf A)
{
    if (A.isEmpty()) {
        ArrayOf R(A);
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        throw Exception(_("Undefined function 'acos' for input arguments of type") + " '"
            + ClassName(A) + "'.");
    }
    switch (A.getDataClass()) {
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_CHAR: {
        throw Exception(_("Undefined function 'acos' for input arguments of type") + " '"
            + ClassName(A) + "'.");
    } break;
    case NLS_SINGLE:
    case NLS_SCOMPLEX: {
        A.ensureSingleOwner();
        A.promoteType(NLS_SCOMPLEX);
        ArrayOf R(A);
        R.ensureSingleOwner();
        singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        singlecomplex* Rz = reinterpret_cast<singlecomplex*>((single*)R.getDataPointer());
        Eigen::Map<Eigen::MatrixXcf> matA(Az, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXcf> matR(Rz, 1, R.getLength());
        matR = matA.array().acos();
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
        return R;
    }
    case NLS_DOUBLE:
    case NLS_DCOMPLEX: {
        A.ensureSingleOwner();
        A.promoteType(NLS_DCOMPLEX);
        ArrayOf R(A);
        R.ensureSingleOwner();
        doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        doublecomplex* Rz = reinterpret_cast<doublecomplex*>((double*)R.getDataPointer());
        Eigen::Map<Eigen::MatrixXcd> matA(Az, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXcd> matR(Rz, 1, R.getLength());
        matR = matA.array().acos();
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
        return R;
    } break;
    default: {
        throw Exception(_W("Invalid conversion."));
    } break;
    }
    return ArrayOf();
}
//=============================================================================
ArrayOf
Asin(ArrayOf A)
{
    if (A.isEmpty()) {
        ArrayOf R(A);
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        throw Exception(_("Undefined function 'asin' for input arguments of type") + " '"
            + ClassName(A) + "'.");
    }
    switch (A.getDataClass()) {
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_CHAR: {
        throw Exception(_("Undefined function 'asin' for input arguments of type") + " '"
            + ClassName(A) + "'.");
    } break;
    case NLS_SINGLE:
    case NLS_SCOMPLEX: {
        A.ensureSingleOwner();
        A.promoteType(NLS_SCOMPLEX);
        ArrayOf R(A);
        R.ensureSingleOwner();
        singlecomplex* Az = reinterpret_cast<singlecomplex*>((float*)A.getDataPointer());
        singlecomplex* Rz = reinterpret_cast<singlecomplex*>((float*)R.getDataPointer());
        Eigen::Map<Eigen::MatrixXcf> matA(Az, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXcf> matR(Rz, 1, R.getLength());
        matR = matA.array().asin();
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
        return R;
    }
    case NLS_DOUBLE:
    case NLS_DCOMPLEX: {
        A.ensureSingleOwner();
        A.promoteType(NLS_DCOMPLEX);
        ArrayOf R(A);
        R.ensureSingleOwner();
        doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        doublecomplex* Rz = reinterpret_cast<doublecomplex*>((double*)R.getDataPointer());
        Eigen::Map<Eigen::MatrixXcd> matA(Az, 1, A.getLength());
        Eigen::Map<Eigen::MatrixXcd> matR(Rz, 1, R.getLength());
        matR = matA.array().asin();
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
        return R;
    } break;
    default: {
        throw Exception(_W("Invalid conversion."));
    } break;
    }
    return ArrayOf();
}
//=============================================================================
ArrayOf
Atan(ArrayOf A)
{
    if (A.isEmpty()) {
        ArrayOf R(A);
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        throw Exception(_("Undefined function 'atan' for input arguments of type") + " '"
            + ClassName(A) + "'.");
    }
    switch (A.getDataClass()) {
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_CHAR: {
        throw Exception(_("Undefined function 'atan' for input arguments of type") + " '"
            + ClassName(A) + "'.");
    } break;
    case NLS_SINGLE:
    case NLS_SCOMPLEX: {
        if (A.getDataClass() == NLS_SINGLE) {
            A.ensureSingleOwner();
            A.promoteType(NLS_SCOMPLEX);
        }
        ArrayOf R(A);
        R.ensureSingleOwner();
        singlecomplex* Az = reinterpret_cast<singlecomplex*>((float*)A.getDataPointer());
        singlecomplex* Rz = reinterpret_cast<singlecomplex*>((float*)R.getDataPointer());
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (indexType z = 0; z < A.getLength(); z++) {
            Rz[z] = atan(Az[z]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
        return R;
    }
    case NLS_DOUBLE:
    case NLS_DCOMPLEX: {
        if (A.getDataClass() == NLS_DOUBLE) {
            A.ensureSingleOwner();
            A.promoteType(NLS_DCOMPLEX);
        }
        ArrayOf R(A);
        R.ensureSingleOwner();
        doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        doublecomplex* Rz = reinterpret_cast<doublecomplex*>((double*)R.getDataPointer());
        indexType z = 0;
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (z = 0; z < A.getLength(); z++) {
            Rz[z] = atan(Az[z]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
        return R;
    } break;
    default: {
        throw Exception(_W("Invalid conversion."));
    } break;
    }
    return ArrayOf();
}
//=============================================================================
}
//=============================================================================
