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
#include "lapack_eigen.hpp"
#include <Eigen/Dense>
#include "InverseMatrix.hpp"
#include "ClassName.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    ArrayOf InverseMatrix(ArrayOf A)
    {
        bool isSupportedTypes = (A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_SINGLE ||
                                 A.getDataClass() == NLS_DCOMPLEX || A.getDataClass() == NLS_SCOMPLEX) && !A.isSparse();
        if (!isSupportedTypes)
        {
            throw Exception(_("Undefined function 'inv' for input arguments of type") + " '" + ClassName(A) + "'.");
        }
        if (!A.isSquare())
        {
            throw Exception(_("Square matrix expected."));
        }
        if (A.isEmpty())
        {
            ArrayOf RES(A);
            RES.ensureSingleOwner();
            return RES;
        }
        if (A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_DCOMPLEX)
        {
            if (A.getDataClass() == NLS_DOUBLE)
            {
                ArrayOf R(A);
                R.ensureSingleOwner();
                Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), (Eigen::Index)A.getDimensions().getRows(), (Eigen::Index)A.getDimensions().getColumns());
                Eigen::Map<Eigen::MatrixXd> matR((double*)R.getDataPointer(), (Eigen::Index)R.getDimensions().getRows(), (Eigen::Index)R.getDimensions().getColumns());
                Eigen::FullPivLU<Eigen::MatrixXd> luFull(matA);
                if (luFull.isInvertible())
                {
                    matR = luFull.inverse();
                }
                else
                {
                    Eigen::PartialPivLU<Eigen::MatrixXd> luPartial(matA);
                    double rcond = luPartial.rcond();
                    double det = luPartial.determinant();
                    if (rcond == 0 && det == 0)
                    {
                        double p = 100;
                        double z = 0;
                        double inf = p / z;
                        matR.setConstant(inf);
                    }
                    else
                    {
                        matR = matA.inverse();
                    }
                }
                return R;
            }
            else // NLS_DCOMPLEX
            {
                ArrayOf R(A);
                R.ensureSingleOwner();
                doublecomplex* Az = reinterpret_cast<doublecomplex*>((single*)A.getDataPointer());
                doublecomplex* Rz = reinterpret_cast<doublecomplex*>((single*)R.getDataPointer());
                Eigen::Map<Eigen::MatrixXcd> matA(Az, (Eigen::Index)A.getDimensions().getRows(), (Eigen::Index)A.getDimensions().getColumns());
                Eigen::Map<Eigen::MatrixXcd> matR(Rz, (Eigen::Index)R.getDimensions().getRows(), (Eigen::Index)R.getDimensions().getColumns());
                Eigen::FullPivLU<Eigen::MatrixXcd> luFull(matA);
                if (luFull.isInvertible())
                {
                    matR = luFull.inverse();
                }
                else
                {
                    Eigen::PartialPivLU<Eigen::MatrixXcd> luPartial(matA);
                    double rcond = luPartial.rcond();
                    doublecomplex det = luPartial.determinant();
                    if (std::isnan(rcond))
                    {
                        if ( std::isnan(det.real()) && std::isnan(det.imag()) )
                        {
                            doublecomplex cst(std::nan("NaN"), std::nan("NaN"));
                            matR.setConstant(cst);
                        }
                        else
                        {
                            double p = 100;
                            double z = 0;
                            double infinity = p / z;
                            doublecomplex cst(infinity, 0);
                            matR.setConstant(cst);
                        }
                    }
                    else if (rcond == 0)
                    {
                        doublecomplex cst(std::nan("NaN"), std::nan("NaN"));
                        matR.setConstant(cst);
                    }
                    else
                    {
                        matR = matA.inverse();
                    }
                }
                if (R.allReal())
                {
                    R.promoteType(NLS_DOUBLE);
                }
                return R;
            }
        }
        else
        {
            if (A.getDataClass() == NLS_SINGLE)
            {
                ArrayOf R(A);
                R.ensureSingleOwner();
                Eigen::Map<Eigen::MatrixXf> matA((single*)A.getDataPointer(), (Eigen::Index)A.getDimensions().getRows(), (Eigen::Index)A.getDimensions().getColumns());
                Eigen::Map<Eigen::MatrixXf> matR((single*)R.getDataPointer(), (Eigen::Index)R.getDimensions().getRows(), (Eigen::Index)R.getDimensions().getColumns());
                Eigen::FullPivLU<Eigen::MatrixXf> luFull(matA);
                if (luFull.isInvertible())
                {
                    matR = luFull.inverse();
                }
                else
                {
                    Eigen::PartialPivLU<Eigen::MatrixXf> luPartial(matA);
                    single rcond = luPartial.rcond();
                    single det = luPartial.determinant();
                    if (rcond == 0 && det == 0)
                    {
                        single p = 100;
                        single z = 0;
                        single inf = p / z;
                        matR.setConstant(inf);
                    }
                    else
                    {
                        matR = matA.inverse();
                    }
                }
                return R;
            }
            else  // NLS_SCOMPLEX
            {
                ArrayOf R(A);
                R.ensureSingleOwner();
                singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
                singlecomplex* Rz = reinterpret_cast<singlecomplex*>((single*)R.getDataPointer());
                Eigen::Map<Eigen::MatrixXcf> matA(Az, (Eigen::Index)A.getDimensions().getRows(), (Eigen::Index)A.getDimensions().getColumns());
                Eigen::Map<Eigen::MatrixXcf> matR(Rz, (Eigen::Index)R.getDimensions().getRows(), (Eigen::Index)R.getDimensions().getColumns());
                Eigen::FullPivLU<Eigen::MatrixXcf> luFull(matA);
                if (luFull.isInvertible())
                {
                    matR = luFull.inverse();
                }
                else
                {
                    Eigen::PartialPivLU<Eigen::MatrixXcf> luPartial(matA);
                    single rcond = luPartial.rcond();
                    singlecomplex det = luPartial.determinant();
                    if (std::isnan(rcond))
                    {
                        if (std::isnan(det.real()) && std::isnan(det.imag()))
                        {
                            singlecomplex cst(std::nanf("NaN"), std::nanf("NaN"));
                            matR.setConstant(cst);
                        }
                        else
                        {
                            single p = 100;
                            single z = 0;
                            single infinity = p / z;
                            singlecomplex cst(infinity, 0);
                            matR.setConstant(cst);
                        }
                    }
                    else if (rcond == 0)
                    {
                        singlecomplex cst(std::nanf("NaN"), std::nanf("NaN"));
                        matR.setConstant(cst);
                    }
                    else
                    {
                        matR = matA.inverse();
                    }
                }
                if (R.allReal())
                {
                    R.promoteType(NLS_SINGLE);
                }
                return R;
            }
        }
    }
    //=============================================================================
}
//=============================================================================
