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
#include "ComplexConjugate.hpp"
#include "ClassName.hpp"
#include <Eigen/Dense>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
ComplexConjugate(ArrayOf A)
{
    Class classA = A.getDataClass();
    if (classA < NLS_LOGICAL || A.isSparse()) {
        Error(_("Undefined function 'conj' for input arguments of type") + " '" + ClassName(A)
            + "'.");
    }
    Dimensions dimsA = A.getDimensions();
    ArrayOf C(A);
    C.ensureSingleOwner();
    switch (classA) {
    case NLS_SCOMPLEX: {
        if (!A.isEmpty()) {
            single* psingleA = (single*)A.getDataPointer();
            singlecomplex* Az = reinterpret_cast<singlecomplex*>(psingleA);
            Eigen::Map<Eigen::MatrixXcf> matA(Az, 1, dimsA.getElementCount());
            single* psingleC = (single*)C.getDataPointer();
            singlecomplex* Cz = reinterpret_cast<singlecomplex*>(psingleC);
            Eigen::Map<Eigen::MatrixXcf> matC(Cz, 1, dimsA.getElementCount());
            matC = matA.conjugate().eval();
        }
    } break;
    case NLS_DCOMPLEX: {
        double* pdoubleA = (double*)A.getDataPointer();
        doublecomplex* Az = reinterpret_cast<doublecomplex*>(pdoubleA);
        Eigen::Map<Eigen::MatrixXcd> matA(Az, 1, dimsA.getElementCount());
        double* pdoubleC = (double*)C.getDataPointer();
        doublecomplex* Cz = reinterpret_cast<doublecomplex*>(pdoubleC);
        Eigen::Map<Eigen::MatrixXcd> matC(Cz, 1, dimsA.getElementCount());
        matC = matA.conjugate().eval();
    } break;
    case NLS_DOUBLE:
    case NLS_SINGLE:
    case NLS_INT8:
    case NLS_UINT8:
    case NLS_INT16:
    case NLS_UINT16:
    case NLS_INT32:
    case NLS_UINT32:
    case NLS_INT64:
    case NLS_UINT64: {
        // returns same value
    } break;
    case NLS_CHAR:
    case NLS_LOGICAL:
    default: {
        Error(_("Undefined function 'conj' for input arguments of type") + " '" + ClassName(A)
            + "'.");
    } break;
    }
    return C;
}
//=============================================================================
}
//=============================================================================
