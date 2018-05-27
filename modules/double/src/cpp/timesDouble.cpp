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
#include "timesDouble.hpp"
#include "MatrixCheck.hpp"
#include <Eigen/Dense>
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
double_times(ArrayOf a, ArrayOf b)
{
    if (a.isScalar() && b.isScalar()) {
        // s .* s
        return ArrayOf::doubleConstructor(
            a.getContentAsDoubleScalar() * b.getContentAsDoubleScalar());
    } else {
        if (a.isScalar() || b.isScalar()) {
            // mxn .* s
            // s .* mxn
            Dimensions dimsC;
            void* Cp = nullptr;
            indexType Clen;
            if (a.isScalar()) {
                double da = a.getContentAsDoubleScalar();
                dimsC = b.getDimensions();
                Clen = dimsC.getElementCount();
                Cp = new_with_exception<double>(Clen);
                Eigen::Map<Eigen::MatrixXd> matC((double*)Cp, 1, Clen);
                Eigen::Map<Eigen::MatrixXd> matB((double*)b.getDataPointer(), 1, Clen);
                matC = da * matB.array();
            } else {
                double db = b.getContentAsDoubleScalar();
                dimsC = a.getDimensions();
                Clen = dimsC.getElementCount();
                Cp = new_with_exception<double>(Clen);
                Eigen::Map<Eigen::MatrixXd> matC((double*)Cp, 1, Clen);
                Eigen::Map<Eigen::MatrixXd> matA((double*)a.getDataPointer(), 1, Clen);
                matC = matA.array() * db;
            }
            return ArrayOf(NLS_DOUBLE, dimsC, Cp, false);
        } else {
            // mxn .* mxn
            Dimensions dimsC = a.getDimensions();
            if (a.isEmpty(true)) {
                return ArrayOf::emptyConstructor(dimsC);
            } else {
                indexType Clen = dimsC.getElementCount();
                void* Cp = new_with_exception<double>(Clen);
                Eigen::Map<Eigen::MatrixXd> matC((double*)Cp, 1, Clen);
                Eigen::Map<Eigen::MatrixXd> matA((double*)a.getDataPointer(), 1, Clen);
                Eigen::Map<Eigen::MatrixXd> matB((double*)b.getDataPointer(), 1, Clen);
                matC = matA.cwiseProduct(matB);
                return ArrayOf(NLS_DOUBLE, dimsC, Cp, false);
            }
        }
    }
    return ArrayOf();
}
//=============================================================================
static ArrayOf
dcomplex_times(ArrayOf a, ArrayOf b)
{
    ArrayOf res;
    a.promoteType(NLS_DCOMPLEX);
    b.promoteType(NLS_DCOMPLEX);
    if (a.isScalar() && b.isScalar()) {
        // s .* s
        void* Cp = new_with_exception<double>(2);
        doublecomplex* Cz = reinterpret_cast<doublecomplex*>(Cp);
        doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)a.getDataPointer());
        doublecomplex cxa = Az[0];
        doublecomplex* Bz = reinterpret_cast<doublecomplex*>((double*)b.getDataPointer());
        doublecomplex cxb = Bz[0];
        Cz[0] = cxa * cxb;
        res = ArrayOf(NLS_DCOMPLEX, a.getDimensions(), Cp, false);
    } else {
        if (a.isScalar() || b.isScalar()) {
            // mxn .* s
            // s .* mxn
            // s .* s
            Dimensions dimsC;
            void* Cp = nullptr;
            if (a.isScalar()) {
                dimsC = b.getDimensions();
                Cp = new_with_exception<double>(2 * dimsC.getElementCount());
                doublecomplex* Cz = reinterpret_cast<doublecomplex*>(Cp);
                Eigen::Map<Eigen::MatrixXcd> matC(Cz, 1, dimsC.getElementCount());
                doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)a.getDataPointer());
                doublecomplex* Bz = reinterpret_cast<doublecomplex*>((double*)b.getDataPointer());
                Eigen::Map<Eigen::MatrixXcd> matB(Bz, 1, dimsC.getElementCount());
                matC = Az[0] * matB.array();
            } else {
                dimsC = a.getDimensions();
                Cp = new_with_exception<double>(2 * dimsC.getElementCount());
                doublecomplex* Cz = reinterpret_cast<doublecomplex*>(Cp);
                Eigen::Map<Eigen::MatrixXcd> matC(Cz, 1, dimsC.getElementCount());
                doublecomplex* Bz = reinterpret_cast<doublecomplex*>((double*)b.getDataPointer());
                doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)a.getDataPointer());
                Eigen::Map<Eigen::MatrixXcd> matA(Az, 1, dimsC.getElementCount());
                matC = matA.array() * Bz[0];
            }
            res = ArrayOf(NLS_DCOMPLEX, dimsC, Cp, false);
        } else {
            // mxn .* mxn
            Dimensions dimsC = a.getDimensions();
            if (a.isEmpty(true)) {
                return ArrayOf::emptyConstructor(dimsC);
            } else {
                void* Cp = new_with_exception<double>(2 * dimsC.getElementCount());
                doublecomplex* Cz = reinterpret_cast<doublecomplex*>(Cp);
                doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)a.getDataPointer());
                Eigen::Map<Eigen::MatrixXcd> matA(Az, 1, dimsC.getElementCount());
                doublecomplex* Bz = reinterpret_cast<doublecomplex*>((double*)b.getDataPointer());
                Eigen::Map<Eigen::MatrixXcd> matB(Bz, 1, dimsC.getElementCount());
                Eigen::Map<Eigen::MatrixXcd> matC(Cz, 1, dimsC.getElementCount());
                matC = matA.cwiseProduct(matB);
                res = ArrayOf(NLS_DCOMPLEX, dimsC, Cp, false);
            }
        }
    }
    if (res.allReal()) {
        res.promoteType(NLS_DOUBLE);
    }
    return res;
}
//=============================================================================
ArrayOf
double_times_double(ArrayOf a, ArrayOf b)
{
    if (!a.isDoubleType() || !b.isDoubleType()) {
        throw Exception(ERROR_WRONG_ARGUMENTS_TYPE_DOUBLE_EXPECTED);
    }
    if (a.isSparse() || b.isSparse()) {
        throw Exception(ERROR_WRONG_ARGUMENTS_SIZE_FULL_MATRIX_EXPECTED);
    }
    if (!(SameSizeCheck(a.getDimensions(), b.getDimensions()) || a.isScalar() || b.isScalar())) {
        throw Exception(_W("Size mismatch on arguments to arithmetic operator ") + L"*");
    }
    if (a.isComplex() || b.isComplex()) {
        return dcomplex_times(a, b);
    }
    return double_times(a, b);
}
//=============================================================================

}
//=============================================================================
