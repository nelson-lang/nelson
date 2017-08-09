//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include "ComplexTranspose.hpp"
#include "ClassName.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    ArrayOf ComplexTranspose(ArrayOf A)
    {
        Class classA = A.getDataClass();
        if (classA < NLS_LOGICAL || A.isSparse())
        {
            std::wstring classname;
            ClassName(A, classname);
            std::wstring msg = _W("function") + L" " + classname + L"_ctranspose" + L" " + _W("undefined.");
            throw Exception(msg);
        }
        Dimensions dimsA = A.getDimensions();
        bool isSupported = (A.isEmpty() || A.isScalar() || A.is2D());
        if (!isSupported)
        {
            std::wstring msg = _W("ctranspose on N-D array is undefined.");
            throw Exception(msg);
        }
        ArrayOf Res(A);
        Res.ensureSingleOwner();
        Dimensions dimsRes(dimsA.getColumns(), dimsA.getRows());
        if (A.isEmpty())
        {
            Res.reshape(dimsRes);
            return Res;
        }
        switch (classA)
        {
            case NLS_LOGICAL:
            {
                Eigen::Map<Eigen::Matrix<logical, Eigen::Dynamic, Eigen::Dynamic>> matOrigin((logical*)A.getDataPointer(), dimsA.getRows(), dimsA.getColumns());
                Eigen::Map<Eigen::Matrix<logical, Eigen::Dynamic, Eigen::Dynamic>> matTransposed((logical*)Res.getDataPointer(), dimsRes.getRows(), dimsRes.getColumns());
                matTransposed = matOrigin.conjugate().transpose().eval();
            }
            break;
            case NLS_UINT8:
            {
                Eigen::Map<Eigen::Matrix<uint8, Eigen::Dynamic, Eigen::Dynamic>> matOrigin((uint8*)A.getDataPointer(), dimsA.getRows(), dimsA.getColumns());
                Eigen::Map<Eigen::Matrix<uint8, Eigen::Dynamic, Eigen::Dynamic>> matTransposed((uint8*)Res.getDataPointer(), dimsRes.getRows(), dimsRes.getColumns());
                matTransposed = matOrigin.conjugate().transpose().eval();
            }
            break;
            case NLS_INT8:
            {
                Eigen::Map<Eigen::Matrix<int8, Eigen::Dynamic, Eigen::Dynamic>> matOrigin((int8*)A.getDataPointer(), dimsA.getRows(), dimsA.getColumns());
                Eigen::Map<Eigen::Matrix<int8, Eigen::Dynamic, Eigen::Dynamic>> matTransposed((int8*)Res.getDataPointer(), dimsRes.getRows(), dimsRes.getColumns());
                matTransposed = matOrigin.conjugate().transpose().eval();
            }
            break;
            case NLS_UINT16:
            {
                Eigen::Map<Eigen::Matrix<uint16, Eigen::Dynamic, Eigen::Dynamic>> matOrigin((uint16*)A.getDataPointer(), dimsA.getRows(), dimsA.getColumns());
                Eigen::Map<Eigen::Matrix<uint16, Eigen::Dynamic, Eigen::Dynamic>> matTransposed((uint16*)Res.getDataPointer(), dimsRes.getRows(), dimsRes.getColumns());
                matTransposed = matOrigin.conjugate().transpose().eval();
            }
            break;
            case NLS_INT16:
            {
                Eigen::Map<Eigen::Matrix<int16, Eigen::Dynamic, Eigen::Dynamic>> matOrigin((int16*)A.getDataPointer(), dimsA.getRows(), dimsA.getColumns());
                Eigen::Map<Eigen::Matrix<int16, Eigen::Dynamic, Eigen::Dynamic>> matTransposed((int16*)Res.getDataPointer(), dimsRes.getRows(), dimsRes.getColumns());
                matTransposed = matOrigin.conjugate().transpose().eval();
            }
            break;
            case NLS_UINT32:
            {
                Eigen::Map<Eigen::Matrix<uint32, Eigen::Dynamic, Eigen::Dynamic>> matOrigin((uint32*)A.getDataPointer(), dimsA.getRows(), dimsA.getColumns());
                Eigen::Map<Eigen::Matrix<uint32, Eigen::Dynamic, Eigen::Dynamic>> matTransposed((uint32*)Res.getDataPointer(), dimsRes.getRows(), dimsRes.getColumns());
                matTransposed = matOrigin.conjugate().transpose().eval();
            }
            break;
            case NLS_INT32:
            {
                Eigen::Map<Eigen::Matrix<int32, Eigen::Dynamic, Eigen::Dynamic>> matOrigin((int32*)A.getDataPointer(), dimsA.getRows(), dimsA.getColumns());
                Eigen::Map<Eigen::Matrix<int32, Eigen::Dynamic, Eigen::Dynamic>> matTransposed((int32*)Res.getDataPointer(), dimsRes.getRows(), dimsRes.getColumns());
                matTransposed = matOrigin.conjugate().transpose().eval();
            }
            break;
            case NLS_UINT64:
            {
                Eigen::Map<Eigen::Matrix<uint64, Eigen::Dynamic, Eigen::Dynamic>> matOrigin((uint64*)A.getDataPointer(), dimsA.getRows(), dimsA.getColumns());
                Eigen::Map<Eigen::Matrix<uint64, Eigen::Dynamic, Eigen::Dynamic>> matTransposed((uint64*)Res.getDataPointer(), dimsRes.getRows(), dimsRes.getColumns());
                matTransposed = matOrigin.conjugate().transpose().eval();
            }
            break;
            case NLS_INT64:
            {
                Eigen::Map<Eigen::Matrix<int64, Eigen::Dynamic, Eigen::Dynamic>> matOrigin((int64*)A.getDataPointer(), dimsA.getRows(), dimsA.getColumns());
                Eigen::Map<Eigen::Matrix<int64, Eigen::Dynamic, Eigen::Dynamic>> matTransposed((int64*)Res.getDataPointer(), dimsRes.getRows(), dimsRes.getColumns());
                matTransposed = matOrigin.conjugate().transpose().eval();
            }
            break;
            case NLS_SINGLE:
            {
                Eigen::Map<Eigen::MatrixXf> matOrigin((single*)A.getDataPointer(), dimsA.getRows(), dimsA.getColumns());
                Eigen::Map<Eigen::MatrixXf> matTransposed((single*)Res.getDataPointer(), dimsRes.getRows(), dimsRes.getColumns());
                matTransposed = matOrigin.conjugate().transpose().eval();
            }
            break;
            case NLS_DOUBLE:
            {
                Eigen::Map<Eigen::MatrixXd> matOrigin((double*)A.getDataPointer(), dimsA.getRows(), dimsA.getColumns());
                Eigen::Map<Eigen::MatrixXd> matTransposed((double*)Res.getDataPointer(), dimsRes.getRows(), dimsRes.getColumns());
                matTransposed = matOrigin.conjugate().transpose().eval();
            }
            break;
            case NLS_SCOMPLEX:
            {
                singlecomplex* matCplxA = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
                singlecomplex* matCplxRes = reinterpret_cast<singlecomplex*>((single*)Res.getDataPointer());
                Eigen::Map<Eigen::Matrix<singlecomplex, Eigen::Dynamic, Eigen::Dynamic>> matOrigin(matCplxA, dimsA.getRows(), dimsA.getColumns());
                Eigen::Map<Eigen::Matrix<singlecomplex, Eigen::Dynamic, Eigen::Dynamic>> matTransposed(matCplxRes, dimsRes.getRows(), dimsRes.getColumns());
                matTransposed = matOrigin.conjugate().transpose().eval();
            }
            break;
            case NLS_DCOMPLEX:
            {
                doublecomplex* matCplxA = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
                doublecomplex* matCplxRes = reinterpret_cast<doublecomplex*>((double*)Res.getDataPointer());
                Eigen::Map<Eigen::Matrix<doublecomplex, Eigen::Dynamic, Eigen::Dynamic>> matOrigin(matCplxA, dimsA.getRows(), dimsA.getColumns());
                Eigen::Map<Eigen::Matrix<doublecomplex, Eigen::Dynamic, Eigen::Dynamic>> matTransposed(matCplxRes, dimsRes.getRows(), dimsRes.getColumns());
                matTransposed = matOrigin.conjugate().transpose().eval();
            }
            break;
            case NLS_CHAR:
            {
                Eigen::Map<Eigen::Matrix<charType, Eigen::Dynamic, Eigen::Dynamic>> matOrigin((charType*)A.getDataPointer(), dimsA.getRows(), dimsA.getColumns());
                Eigen::Map<Eigen::Matrix<charType, Eigen::Dynamic, Eigen::Dynamic>> matTransposed((charType*)Res.getDataPointer(), dimsRes.getRows(), dimsRes.getColumns());
                matTransposed = matOrigin.conjugate().transpose().eval();
            }
            break;
        }
        Res.reshape(dimsRes);
        return Res;
    }
    //=============================================================================
}
//=============================================================================
