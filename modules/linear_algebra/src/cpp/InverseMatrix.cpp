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
#include "InverseMatrix.hpp"
#include "ClassName.hpp"
#include "ReciprocalConditionNumber.hpp"
#include "lapack_eigen.hpp"
#include <Eigen/Dense>
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
InverseDouble(ArrayOf A, double rcond)
{
    ArrayOf res(A);
    res.ensureSingleOwner();
    Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(),
        (Eigen::Index)A.getDimensions().getRows(), (Eigen::Index)A.getDimensions().getColumns());
    Eigen::Map<Eigen::MatrixXd> matR((double*)res.getDataPointer(),
        (Eigen::Index)res.getDimensions().getRows(),
        (Eigen::Index)res.getDimensions().getColumns());
    if (matA.hasNaN()) {
        matR.setConstant(std::nan("NaN"));
    } else {
        Eigen::FullPivLU<Eigen::MatrixXd> luFull(matA);
        if (luFull.isInvertible()) {
            matR = luFull.inverse();
        } else {
            double det = luFull.determinant();
            if (rcond == 0 && det == 0) {
                matR.setConstant(std::numeric_limits<double>::infinity());
            } else {
                matR = matA.inverse();
            }
        }
    }
    return res;
}
//=============================================================================
static ArrayOf
InverseDoubleComplex(ArrayOf A, double rcond)
{
    ArrayOf res(A);
    res.ensureSingleOwner();
    doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
    doublecomplex* Rz = reinterpret_cast<doublecomplex*>((double*)res.getDataPointer());
    Eigen::Map<Eigen::MatrixXcd> matA(Az, (Eigen::Index)A.getDimensions().getRows(),
        (Eigen::Index)A.getDimensions().getColumns());
    Eigen::Map<Eigen::MatrixXcd> matR(Rz, (Eigen::Index)res.getDimensions().getRows(),
        (Eigen::Index)res.getDimensions().getColumns());
    if (matA.hasNaN()) {
        doublecomplex cst(std::nan("NaN"), std::nan("NaN"));
        matR.setConstant(cst);
    } else {
        Eigen::FullPivLU<Eigen::MatrixXcd> luFull(matA);
        if (luFull.isInvertible()) {
            matR = luFull.inverse();
        } else {
            Eigen::PartialPivLU<Eigen::MatrixXcd> luPartial(matA);
            doublecomplex det = luPartial.determinant();
            if (std::isnan(rcond)) {
                if (std::isnan(det.real()) && std::isnan(det.imag())) {
                    doublecomplex cst(std::nan("NaN"), std::nan("NaN"));
                    matR.setConstant(cst);
                }
            } else {
                if ((rcond == 0.) && (det == 0.)) {
                    doublecomplex cst(std::numeric_limits<double>::infinity(), 0);
                    matR.setConstant(cst);
                } else {
                    if (rcond == 0) {
                        doublecomplex cst(std::nan("NaN"), std::nan("NaN"));
                        matR.setConstant(cst);
                    } else {
                        matR = matA.inverse();
                    }
                }
            }
        }
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    }
    return res;
}
//=============================================================================
static ArrayOf
InverseSingle(ArrayOf A, single rcond)
{
    ArrayOf res(A);
    res.ensureSingleOwner();
    Eigen::Map<Eigen::MatrixXf> matA((single*)A.getDataPointer(),
        (Eigen::Index)A.getDimensions().getRows(), (Eigen::Index)A.getDimensions().getColumns());
    Eigen::Map<Eigen::MatrixXf> matR((single*)res.getDataPointer(),
        (Eigen::Index)res.getDimensions().getRows(),
        (Eigen::Index)res.getDimensions().getColumns());
    if (matA.hasNaN()) {
        matR.setConstant(std::nanf("NaN"));
    } else {
        Eigen::FullPivLU<Eigen::MatrixXf> luFull(matA);
        if (luFull.isInvertible()) {
            matR = luFull.inverse();
        } else {
            single det = luFull.determinant();
            if (rcond == 0 && det == 0) {
                matR.setConstant(std::numeric_limits<single>::infinity());
            } else {
                matR = matA.inverse();
            }
        }
    }
    return res;
}
//=============================================================================
static ArrayOf
InverseSingleComplex(ArrayOf A, single rcond)
{
    ArrayOf res(A);
    res.ensureSingleOwner();
    singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
    singlecomplex* Rz = reinterpret_cast<singlecomplex*>((single*)res.getDataPointer());
    Eigen::Map<Eigen::MatrixXcf> matA(Az, (Eigen::Index)A.getDimensions().getRows(),
        (Eigen::Index)A.getDimensions().getColumns());
    Eigen::Map<Eigen::MatrixXcf> matR(Rz, (Eigen::Index)res.getDimensions().getRows(),
        (Eigen::Index)res.getDimensions().getColumns());
    if (matA.hasNaN()) {
        singlecomplex cst(std::nanf("NaN"), std::nanf("NaN"));
        matR.setConstant(cst);
    } else {
        Eigen::FullPivLU<Eigen::MatrixXcf> luFull(matA);
        if (luFull.isInvertible()) {
            matR = luFull.inverse();
        } else {
            Eigen::PartialPivLU<Eigen::MatrixXcf> luPartial(matA);
            singlecomplex det = luPartial.determinant();
            if (std::isnan(rcond)) {
                if (std::isnan(det.real()) && std::isnan(det.imag())) {
                    singlecomplex cst(std::nanf("NaN"), std::nanf("NaN"));
                    matR.setConstant(cst);
                }
            } else {
                if ((rcond == 0.) && (det.real() == 0.) && (det.imag() == 0.)) {
                    singlecomplex cst(std::numeric_limits<single>::infinity(), 0);
                    matR.setConstant(cst);
                } else {
                    if (rcond == 0) {
                        singlecomplex cst(std::nanf("NaN"), std::nanf("NaN"));
                        matR.setConstant(cst);
                    } else {
                        matR = matA.inverse();
                    }
                }
            }
        }
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    }
    return res;
}
//=============================================================================
ArrayOf
InverseMatrix(ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    bool isSupportedTypes
        = (A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_SINGLE
              || A.getDataClass() == NLS_DCOMPLEX || A.getDataClass() == NLS_SCOMPLEX)
        && !A.isSparse();
    if (!isSupportedTypes) {
        needToOverload = true;
        return ArrayOf();
    }
    if (!A.isSquare()) {
        Error(_("Square matrix expected."));
    }
    if (A.isEmpty()) {
        ArrayOf RES(A);
        RES.ensureSingleOwner();
        return RES;
    }
    ArrayOf rCondArray = ReciprocalConditionNumber(A);
    double rcond = rCondArray.getContentAsDoubleScalar();
    ArrayOf R;
    if (A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_DCOMPLEX) {
        if (A.getDataClass() == NLS_DOUBLE) {
            R = InverseDouble(A, rcond);
        } else {
            R = InverseDoubleComplex(A, rcond);
        }
    } else {
        if (A.getDataClass() == NLS_SINGLE) {
            R = InverseSingle(A, (single)rcond);
        } else {
            R = InverseSingleComplex(A, (single)rcond);
        }
    }
    return R;
}
//=============================================================================
}
//=============================================================================
