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
#include "MtimesDouble.hpp"
#include "MatrixCheck.hpp"
#include <Eigen/Dense>
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
double_mtimes(ArrayOf a, ArrayOf b)
{
    Dimensions Cdim;
    if (a.isVector() && b.isScalar()) {
        Cdim = a.getDimensions();
    } else if (b.isVector() && a.isScalar()) {
        Cdim = b.getDimensions();
    } else if ((a.isRowVector() && b.isColumnVector()) || (b.isRowVector() && a.isColumnVector())) {
        Cdim[0] = a.getDimensions().getRows();
        Cdim[1] = b.getDimensions().getColumns();
    } else {
        if (a.isScalar()) {
            Cdim = b.getDimensions();
        } else if (b.isScalar()) {
            Cdim = a.getDimensions();
        } else {
            Cdim[0] = a.getDimensions().getRows();
            Cdim[1] = b.getDimensions().getColumns();
        }
    }
    indexType Clen = Cdim.getElementCount();
    void* Cp = new_with_exception<double>(Clen);
    size_t mC = Cdim.getRows();
    size_t nC = Cdim.getColumns();
    Eigen::Map<Eigen::MatrixXd> matC((double*)Cp, mC, nC);
    Dimensions dimA = a.getDimensions();
    size_t mA = dimA.getRows();
    size_t nA = dimA.getColumns();
    Dimensions dimB = b.getDimensions();
    size_t mB = dimB.getRows();
    size_t nB = dimB.getColumns();
    if (a.isScalar()) {
        Eigen::Map<Eigen::MatrixXd> matB((double*)b.getDataPointer(), mB, nB);
        matC = a.getContentAsDoubleScalar() * matB.array();
    } else if (b.isScalar()) {
        Eigen::Map<Eigen::MatrixXd> matA((double*)a.getDataPointer(), mA, nA);
        matC = matA.array() * b.getContentAsDoubleScalar();
    } else {
        Eigen::Map<Eigen::MatrixXd> matA((double*)a.getDataPointer(), mA, nA);
        Eigen::Map<Eigen::MatrixXd> matB((double*)b.getDataPointer(), mB, nB);
        matC = matA * matB;
    }
    return ArrayOf(NLS_DOUBLE, Cdim, Cp, false);
}
//=============================================================================
static ArrayOf
dcomplex_mtimes(ArrayOf a, ArrayOf b)
{
    Dimensions Cdim;
    a.promoteType(NLS_DCOMPLEX);
    b.promoteType(NLS_DCOMPLEX);
    if (a.isVector() && b.isScalar()) {
        Cdim = a.getDimensions();
    } else if (b.isVector() && a.isScalar()) {
        Cdim = b.getDimensions();
    } else if ((a.isRowVector() && b.isColumnVector()) || (b.isRowVector() && a.isColumnVector())) {
        Cdim[0] = a.getDimensions().getRows();
        Cdim[1] = b.getDimensions().getColumns();
    } else {
        if (a.isScalar()) {
            Cdim = b.getDimensions();
        } else if (b.isScalar()) {
            Cdim = a.getDimensions();
        } else {
            Cdim[0] = a.getDimensions().getRows();
            Cdim[1] = b.getDimensions().getColumns();
        }
    }
    indexType Clen = Cdim.getElementCount();
    void* Cp = new_with_exception<double>(Clen * 2);
    doublecomplex* Cz = reinterpret_cast<doublecomplex*>(Cp);
    size_t mC = Cdim.getRows();
    size_t nC = Cdim.getColumns();
    Eigen::Map<Eigen::MatrixXcd> matC(Cz, mC, nC);
    Dimensions dimA = a.getDimensions();
    size_t mA = dimA.getRows();
    size_t nA = dimA.getColumns();
    Dimensions dimB = b.getDimensions();
    size_t mB = dimB.getRows();
    size_t nB = dimB.getColumns();
    if (a.isScalar() && b.isScalar()) {
        doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)a.getDataPointer());
        doublecomplex cxa = Az[0];
        doublecomplex* Bz = reinterpret_cast<doublecomplex*>((double*)b.getDataPointer());
        doublecomplex cxb = Bz[0];
        if ((cxa.real() == 0.) && (cxa.imag() == 0.) || (cxb.real() == 0.) && (cxb.imag() == 0.)) {
            double* pd = (double*)Cp;
            delete[] pd;
            pd = nullptr;
            return ArrayOf::doubleConstructor(0);
        } else {
            Cz[0] = cxa * cxb;
        }
    } else if (a.isScalar()) {
        doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)a.getDataPointer());
        doublecomplex* Bz = reinterpret_cast<doublecomplex*>((double*)b.getDataPointer());
        Eigen::Map<Eigen::MatrixXcd> matB(Bz, mB, nB);
        if ((Az[0].real() == 0.) && (Az[0].imag() == 0.)) {
            double* pd = (double*)Cp;
            delete[] pd;
            pd = nullptr;
            Cp = ArrayOf::allocateArrayOf(NLS_DOUBLE, Cdim.getElementCount());
            return ArrayOf(NLS_DOUBLE, Cdim, Cp);
        } else {
            matC = Az[0] * matB.array();
        }
    } else if (b.isScalar()) {
        doublecomplex* Bz = reinterpret_cast<doublecomplex*>((double*)b.getDataPointer());
        doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)a.getDataPointer());
        Eigen::Map<Eigen::MatrixXcd> matA(Az, mA, nA);
        matC = matA.array() * Bz[0];
        if ((Bz[0].real() == 0.) && (Bz[0].imag() == 0.)) {
            double* pd = (double*)Cp;
            delete[] pd;
            pd = nullptr;
            Cp = ArrayOf::allocateArrayOf(NLS_DOUBLE, Cdim.getElementCount());
            return ArrayOf(NLS_DOUBLE, Cdim, Cp);
        } else {
            matC = matA.array() * Bz[0];
        }
    } else {
        doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)a.getDataPointer());
        Eigen::Map<Eigen::MatrixXcd> matA(Az, mA, nA);
        doublecomplex* Bz = reinterpret_cast<doublecomplex*>((double*)b.getDataPointer());
        Eigen::Map<Eigen::MatrixXcd> matB(Bz, mB, nB);
        matC = matA * matB;
    }
    ArrayOf res = ArrayOf(NLS_DCOMPLEX, Cdim, Cp, false);
    if (res.allReal()) {
        res.promoteType(NLS_DOUBLE);
    }
    return res;
}
//=============================================================================
ArrayOf
double_mtimes_double(ArrayOf a, ArrayOf b)
{
    if (!a.isDoubleType() || !b.isDoubleType()) {
        Error(ERROR_WRONG_ARGUMENTS_TYPE_DOUBLE_EXPECTED);
    }
    if (a.isSparse() || b.isSparse()) {
        Error(ERROR_WRONG_ARGUMENTS_SIZE_FULL_MATRIX_EXPECTED);
    }
    if (a.isEmpty() || b.isEmpty()) {
        Dimensions dimsA = a.getDimensions();
        Dimensions dimsB = b.getDimensions();
        dimsA.simplify();
        dimsB.simplify();
        // [] * 2
        // 2 * []
        if (a.isScalar() || b.isScalar()) {
            if (a.isScalar()) {
                return ArrayOf(b);
            } else {
                return ArrayOf(a);
            }
        }
        // [] * [] = []
        if (a.isEmpty(true) && b.isEmpty(true)) {
            return ArrayOf::emptyConstructor(dimsA);
        }
        // [](mx0) * [](0xn) = 0(mxn)
        if ((dimsA[1] == 0) && (dimsB[0] == 0) && (dimsB.getLength() < 3)) {
            Dimensions dimsC(dimsA[0], dimsB[1]);
            double* pDouble
                = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsC.getElementCount());
            return ArrayOf(NLS_DOUBLE, dimsC, pDouble, false);
        }
        // [](0xm) * M(mxn) = [](0xn)
        if ((dimsA[0] == 0) && (dimsA[1] == dimsB[0])) {
            Dimensions dimsC(0, dimsB[1]);
            return ArrayOf::emptyConstructor(dimsC);
        }
        // M(mxn) * [](nx0) = [](mx0)
        if ((dimsB[0] == dimsA[1]) && (dimsB.getLength() < 3)) {
            Dimensions dimsC(dimsA[0], 0);
            return ArrayOf::emptyConstructor(dimsC);
        }
        if (dimsA.getLength() > 2 || dimsB.getLength() > 2) {
            Error(ERROR_WRONG_ARGUMENTS_SIZE_2D_MATRIX_EXPECTED);
        } else {
            Error(_W("Size mismatch on arguments to arithmetic operator ") + L"*");
        }
    }
    if (!a.is2D() || !b.is2D()) {
        Error(ERROR_WRONG_ARGUMENTS_SIZE_2D_MATRIX_EXPECTED);
    }
    bool isVector = ((a.isVector() && b.isScalar()) || (b.isVector() && a.isScalar())
        || (a.isRowVector() && b.isColumnVector()) || (b.isRowVector() && a.isColumnVector()));
    if (!(SameSizeCheck(a.getDimensions(), b.getDimensions()) || a.isScalar() || b.isScalar())
        && !isVector && a.getDimensions().getColumns() != b.getDimensions().getRows()) {
        Error(_W("Size mismatch on arguments to arithmetic operator ") + L"*");
    }
    if (a.isEmpty()) {
        Dimensions dimA = a.getDimensions();
        size_t mA = dimA.getRows();
        size_t nA = dimA.getColumns();
        if (mA == nA) {
            if (b.isScalar()) {
                // [] + X returns []
                return ArrayOf(b.getDataClass());
            } else {
                Error(_W("using operator '*' \n Matrix dimensions must agree."));
            }
        }
    }
    if (a.isComplex() || b.isComplex()) {
        return dcomplex_mtimes(a, b);
    }
    return double_mtimes(a, b);
}
//=============================================================================
}
//=============================================================================
