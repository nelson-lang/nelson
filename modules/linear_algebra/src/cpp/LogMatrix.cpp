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
#ifdef _MSC_VER
#define _SCL_SECURE_NO_WARNINGS
#endif
#include <iostream>
#include <unsupported/Eigen/MatrixFunctions>
#include "LogMatrix.hpp"
#include "ClassName.hpp"
#include "ComplexConstructor.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    ArrayOf LogMatrix(ArrayOf A)
    {
        if (!A.isSquare())
        {
            throw Exception(_("Square matrix expected."));
        }
        if (A.isEmpty())
        {
            ArrayOf R(A);
            R.ensureSingleOwner();
            return R;
        }
        if (A.isSparse())
        {
            throw Exception(_("Undefined function 'cosm' for input arguments of type") + " '" + ClassName(A) + "'.");
        }
        switch (A.getDataClass())
        {
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
            case NLS_CHAR:
            {
                throw Exception(_("Undefined function 'cosm' for input arguments of type") + " '" + ClassName(A) + "'.");
            }
            break;
            case NLS_DCOMPLEX:
            {
                /*
                ArrayOf R(A);
                ArrayOf RealPart(A);
                ArrayOf ImagPart(A);

                RealPart.promoteType(NLS_DOUBLE);
                ImagPart.promoteType(NLS_DOUBLE);

                Eigen::Map<Eigen::MatrixXd> matRealPart((double*)RealPart.getDataPointer(), (Eigen::Index)R.getDimensions().getRows(), (Eigen::Index)R.getDimensions().getColumns());
                Eigen::Map<Eigen::MatrixXd> matImagPart((double*)ImagPart.getDataPointer(), (Eigen::Index)R.getDimensions().getRows(), (Eigen::Index)R.getDimensions().getColumns());


                doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
                Eigen::Map<Eigen::Matrix<doublecomplex, -1, -1>> matA(Az, (Eigen::Index)A.getDimensions().getRows(), (Eigen::Index)A.getDimensions().getColumns());

                doublecomplex* Rz = reinterpret_cast<doublecomplex*>((double*)R.getDataPointer());
                Eigen::Map<Eigen::Matrix<doublecomplex, -1, -1>> matR(Rz, (Eigen::Index)A.getDimensions().getRows(), (Eigen::Index)A.getDimensions().getColumns());

                auto aa = matA.log();
                std::cout << aa;
                return R;
                */
            }
            break;
            case NLS_DOUBLE:
            {
                ArrayOf R(A);
                R.ensureSingleOwner();
                Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), (Eigen::Index)A.getDimensions().getRows(), (Eigen::Index)A.getDimensions().getColumns());
                Eigen::Map<Eigen::MatrixXd> matR((double*)R.getDataPointer(), (Eigen::Index)R.getDimensions().getRows(), (Eigen::Index)R.getDimensions().getColumns());
                auto res = matA.log();
                matR = res.array();
                return R;
            }
            break;
            default:
            {
                throw Exception(_W("Invalid conversion."));
            }
            break;
        }
        return ArrayOf();
    }
    //=============================================================================
}
//=============================================================================
