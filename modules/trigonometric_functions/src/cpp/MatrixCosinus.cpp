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
#include "MatrixCosinus.hpp"
#include <unsupported/Eigen/MatrixFunctions>
#include "ClassName.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    ArrayOf MatrixCos(ArrayOf A)
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
            case NLS_STRING:
            {
                throw Exception(_("Undefined function 'cosm' for input arguments of type") + " '" + ClassName(A) + "'.");
            }
            break;
            case NLS_SCOMPLEX:
            {
                // 0.5*(expm(i*A) + expm(-i*A))
                // 0.5*(expm(B) + expm(C)) with B = i*A and C = -i*A
                ArrayOf R(A);
                R.ensureSingleOwner();
                singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
                singlecomplex* Rz = reinterpret_cast<singlecomplex*>((single*)R.getDataPointer());
                Eigen::Map<Eigen::MatrixXcf> matA(Az, (Eigen::Index)A.getDimensions().getRows(), (Eigen::Index)A.getDimensions().getColumns());
                Eigen::Map<Eigen::MatrixXcf> matR(Rz, (Eigen::Index)R.getDimensions().getRows(), (Eigen::Index)R.getDimensions().getColumns());
                singlecomplex I(0, 1);
                singlecomplex minusI(0, -1);
                matR = 0.5 * (((I*matA).exp()) + ((minusI* matA).exp()));
                if (R.allReal())
                {
                    R.promoteType(NLS_SINGLE);
                }
                return R;
            }
            case NLS_SINGLE:
            {
                ArrayOf R(A);
                R.ensureSingleOwner();
                Eigen::Map<Eigen::MatrixXf> matA((single*)A.getDataPointer(), (Eigen::Index)A.getDimensions().getRows(), (Eigen::Index)A.getDimensions().getColumns());
                Eigen::Map<Eigen::MatrixXf> matR((single*)R.getDataPointer(), (Eigen::Index)R.getDimensions().getRows(), (Eigen::Index)R.getDimensions().getColumns());
                matR = matA.cos();
                return R;
            }
            break;
            case NLS_DCOMPLEX:
            {
                // 0.5*(expm(i*A) + expm(-i*A))
                // 0.5*(expm(B) + expm(C)) with B = i*A and C = -i*A
                ArrayOf R(A);
                R.ensureSingleOwner();
                doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
                doublecomplex* Rz = reinterpret_cast<doublecomplex*>((double*)R.getDataPointer());
                Eigen::Map<Eigen::MatrixXcd> matA(Az, (Eigen::Index)A.getDimensions().getRows(), (Eigen::Index)A.getDimensions().getColumns());
                Eigen::Map<Eigen::MatrixXcd> matR(Rz, (Eigen::Index)R.getDimensions().getRows(), (Eigen::Index)R.getDimensions().getColumns());
                doublecomplex I(0, 1);
                doublecomplex minusI(0, -1);
                matR = 0.5 * (((I*matA).exp()) + ((minusI* matA).exp()));
                if (R.allReal())
                {
                    R.promoteType(NLS_DOUBLE);
                }
                return R;
            }
            break;
            case NLS_DOUBLE:
            {
                ArrayOf R(A);
                R.ensureSingleOwner();
                Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), (Eigen::Index)A.getDimensions().getRows(), (Eigen::Index)A.getDimensions().getColumns());
                Eigen::Map<Eigen::MatrixXd> matR((double*)R.getDataPointer(), (Eigen::Index)R.getDimensions().getRows(), (Eigen::Index)R.getDimensions().getColumns());
                matR = matA.cos();
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
