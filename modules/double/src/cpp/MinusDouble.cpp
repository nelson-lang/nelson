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
#include "MinusDouble.hpp"
#include "MatrixCheck.hpp"
#include <Eigen/Dense>
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
double_matrix_matrix_subtraction(const ArrayOf& a, const ArrayOf& b)
{
    Dimensions dimsC = a.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = new_with_exception<double>(Clen, false);
    Eigen::Map<Eigen::MatrixXd> matC((double*)Cp, 1, Clen);
    Eigen::Map<Eigen::MatrixXd> matA((double*)a.getDataPointer(), 1, Clen);
    Eigen::Map<Eigen::MatrixXd> matB((double*)b.getDataPointer(), 1, Clen);
    matC = matA - matB;
    return ArrayOf(NLS_DOUBLE, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf
doublecomplex_matrix_matrix_subtraction(const ArrayOf& a, const ArrayOf& b)
{
    Dimensions dimsC = a.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = new_with_exception<double>(Clen * 2, false);
    doublecomplex* Cz = reinterpret_cast<doublecomplex*>(Cp);
    Eigen::Map<Eigen::MatrixXcd> matC(Cz, 1, Clen);
    doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)a.getDataPointer());
    Eigen::Map<Eigen::MatrixXcd> matA(Az, 1, Clen);
    doublecomplex* Bz = reinterpret_cast<doublecomplex*>((double*)b.getDataPointer());
    Eigen::Map<Eigen::MatrixXcd> matB(Bz, 1, Clen);
    matC = matA - matB;
    return ArrayOf(NLS_DCOMPLEX, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf
double_scalar_matrix_subtraction(ArrayOf& a, ArrayOf& b, bool reverse = false)
{
    Dimensions dimsC = b.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = new_with_exception<double>(Clen, false);
    Eigen::Map<Eigen::MatrixXd> matC((double*)Cp, 1, Clen);
    Eigen::Map<Eigen::MatrixXd> matB((double*)b.getDataPointer(), 1, Clen);
    if (reverse) {
        matC = matB.array() - a.getContentAsDoubleScalar();
	} else {
        matC = a.getContentAsDoubleScalar() - matB.array();
    }
    return ArrayOf(NLS_DOUBLE, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf
doublecomplex_scalar_matrix_subtraction(ArrayOf& a, ArrayOf& b, bool reverse = false)
{
    Dimensions dimsC = b.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = new_with_exception<double>(Clen * 2, false);
    double* da = (double*)a.getDataPointer();
    doublecomplex* Az = reinterpret_cast<doublecomplex*>(da);
    doublecomplex* Cz = reinterpret_cast<doublecomplex*>(Cp);
    Eigen::Map<Eigen::MatrixXcd> matC(Cz, 1, Clen);
    doublecomplex* Bz = reinterpret_cast<doublecomplex*>((double*)b.getDataPointer());
    Eigen::Map<Eigen::MatrixXcd> matB(Bz, 1, Clen);
    if (reverse) {
        matC = matB.array() - Az[0];
	} else {
        matC = Az[0] - matB.array();
    }
    return ArrayOf(NLS_DCOMPLEX, dimsC, Cp, false);
}
//=============================================================================
static void
double_vector_subtraction(double* C, const double* A, indexType NA, const double* B, indexType NB, bool reverse = false)
{
    indexType m = 0;
    if (reverse) {
        for (indexType i = 0; i < NA; i++) {
            for (indexType j = 0; j < NB; j++) {
                C[m] = B[j] - A[i];
                m++;
            }
        }
	} else {
        for (indexType i = 0; i < NA; i++) {
            for (indexType j = 0; j < NB; j++) {
                C[m] = A[i] - B[j];
                m++;
            }
        }
    }
    
}
//=============================================================================
static void
doublecomplex_vector_subtraction(double* C, double* A, indexType NA, double* B, indexType NB, bool reverse = false)
{
    indexType m = 0;
    doublecomplex* Az = reinterpret_cast<doublecomplex*>(A);
    doublecomplex* Bz = reinterpret_cast<doublecomplex*>(B);
    doublecomplex* Cz = reinterpret_cast<doublecomplex*>(C);
    if (reverse) {
        for (indexType i = 0; i < NA; i++) {
            for (indexType j = 0; j < NB; j++) {
                Cz[m] = Bz[j] - Az[i];
                m++;
            }
        }
	} else {
        for (indexType i = 0; i < NA; i++) {
            for (indexType j = 0; j < NB; j++) {
                Cz[m] = Az[i] - Bz[j];
                m++;
            }
        }
	}
}
//=============================================================================
static ArrayOf
double_vector_matrix_subtraction(const ArrayOf& a, const ArrayOf& b, bool reverse = false)
{
    const double* ptrA = (const double*)a.getDataPointer();
    const double* ptrB = (const double*)b.getDataPointer();
    indexType q = 0;
    Dimensions dimsC = b.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = new_with_exception<double>(Clen, false);
    double* C = (double*)Cp;
	if (reverse)
	{
        for (indexType i = 0; i < dimsC.getRows(); i++) {
            for (indexType j = 0; j < dimsC.getColumns(); j++) {
                indexType m = i + j * a.getDimensions().getRows();
                C[m] = ptrB[m] - ptrA[q];
            }
            q++;
        }
	} else {
        for (indexType i = 0; i < dimsC.getRows(); i++) {
            for (indexType j = 0; j < dimsC.getColumns(); j++) {
                indexType m = i + j * a.getDimensions().getRows();
                C[m] = ptrA[q] - ptrB[m] ;
            }
            q++;
        }
	}
    return ArrayOf(NLS_DOUBLE, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf
doublecomplex_vector_matrix_subtraction(const ArrayOf& a, const ArrayOf& b, bool reverse = false)
{
    Dimensions dimsC = b.getDimensions();
    indexType q = 0;
    indexType Clen = dimsC.getElementCount();
    double* ptrA = (double*)a.getDataPointer();
    double* ptrB = (double*)b.getDataPointer();
    void* Cp = new_with_exception<double>(Clen * 2, false);
    double* C = (double*)Cp;
    doublecomplex* Az = reinterpret_cast<doublecomplex*>(ptrA);
    doublecomplex* Bz = reinterpret_cast<doublecomplex*>(ptrB);
    doublecomplex* Cz = reinterpret_cast<doublecomplex*>(C);
    if (reverse) {
        for (indexType i = 0; i < dimsC.getRows(); i++) {
            for (indexType j = 0; j < dimsC.getColumns(); j++) {
                indexType m = i + j * a.getDimensions().getRows();
                Cz[m] = Bz[m] - Az[q];
            }
            q++;
        }
	} else {
        for (indexType i = 0; i < dimsC.getRows(); i++) {
            for (indexType j = 0; j < dimsC.getColumns(); j++) {
                indexType m = i + j * a.getDimensions().getRows();
                Cz[m] = Az[q] - Bz[m];
            }
            q++;
        }
	}
    return ArrayOf(NLS_DCOMPLEX, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf
double_vector_column_subtraction(const ArrayOf& a, const ArrayOf& b, bool reverse = false)
{
    const double* ptrA = (const double*)a.getDataPointer();
    const double* ptrB = (const double*)b.getDataPointer();
    Dimensions dimsC = b.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = new_with_exception<double>(Clen, false);
    double* C = (double*)Cp;
    if (reverse) {
        for (indexType i = 0; i < dimsC.getRows(); i++) {
            for (indexType j = 0; j < dimsC.getColumns(); j++) {
                indexType m = i + j * b.getDimensions().getRows();
                C[m] = ptrB[m] - ptrA[j];
            }
        }
	} else {
        for (indexType i = 0; i < dimsC.getRows(); i++) {
            for (indexType j = 0; j < dimsC.getColumns(); j++) {
                indexType m = i + j * b.getDimensions().getRows();
                C[m] = ptrA[j] - ptrB[m];
            }
        }
	}
    return ArrayOf(NLS_DOUBLE, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf
doublecomplex_vector_column_subtraction(const ArrayOf& a, const ArrayOf& b, bool reverse = false)
{
    indexType q = 0;
    Dimensions dimsC = b.getDimensions();
    indexType Clen = dimsC.getElementCount();
    double* ptrA = (double*)a.getDataPointer();
    double* ptrB = (double*)b.getDataPointer();
    void* Cp = new_with_exception<double>(Clen * 2, false);
    double* C = (double*)Cp;
    doublecomplex* Az = reinterpret_cast<doublecomplex*>(ptrA);
    doublecomplex* Bz = reinterpret_cast<doublecomplex*>(ptrB);
    doublecomplex* Cz = reinterpret_cast<doublecomplex*>(C);
    if (reverse) {
        for (indexType i = 0; i < dimsC.getRows(); i++) {
            for (indexType j = 0; j < dimsC.getColumns(); j++) {
                indexType m = i + j * b.getDimensions().getRows();
                Cz[m] = Bz[m] - Az[j];
            }
        }
	} else {
        for (indexType i = 0; i < dimsC.getRows(); i++) {
            for (indexType j = 0; j < dimsC.getColumns(); j++) {
                indexType m = i + j * b.getDimensions().getRows();
                Cz[m] = Az[j] - Bz[m];
			}
        }
	}
    return ArrayOf(NLS_DCOMPLEX, dimsC, Cp, false);
}
//=============================================================================
ArrayOf
double_subtraction(ArrayOf a, ArrayOf b)
{
    void* Cp = nullptr;
    if (a.isScalar() && b.isScalar()) {
        double res = (a.getContentAsDoubleScalar() - b.getContentAsDoubleScalar());
        return ArrayOf::doubleConstructor(res);
    }
    Dimensions dimsA = a.getDimensions();
    Dimensions dimsB = b.getDimensions();
    Dimensions dimsC;
    if (a.isEmpty() || b.isEmpty()) {
        if (a.isScalar() || b.isScalar()) {
            if (a.isScalar()) {
                return ArrayOf(b);
            } else {
                return ArrayOf(a);
            }
        } else {
            if (!(SameSizeCheck(dimsA, dimsB))) {
                throw Exception(_W("Size mismatch on arguments to arithmetic operator ") + L"-");
            }
            return ArrayOf(b);
        }
    }
    if (SameSizeCheck(dimsA, dimsB)) {
        return double_matrix_matrix_subtraction(a, b);
    } else {
        if (a.isScalar() || b.isScalar()) {
            if (a.isScalar()) {
                return double_scalar_matrix_subtraction(a, b);
            } else {
                // b.isScalar()
                return double_scalar_matrix_subtraction(b, a, true);
            }
        } else {
            if (a.isVector() || b.isVector()) {
                if (a.isRowVector() && b.isColumnVector()) {
                    dimsC = Dimensions(std::min(dimsA.getMax(), dimsB.getMax()),
                        std::max(dimsA.getMax(), dimsB.getMax()));
                    indexType Clen = dimsC.getElementCount();
                    Cp = new_with_exception<double>(Clen, false);
                    double_vector_subtraction((double*)Cp, (const double*)a.getDataPointer(),
                        dimsA.getElementCount(), (const double*)b.getDataPointer(),
                        dimsB.getElementCount());
                } else if (a.isColumnVector() && b.isRowVector()) {
                    dimsC = Dimensions(std::min(dimsA.getMax(), dimsB.getMax()),
                        std::max(dimsA.getMax(), dimsB.getMax()));
                    indexType Clen = dimsC.getElementCount();
                    Cp = new_with_exception<double>(Clen, false);
                    double_vector_subtraction((double*)Cp, (const double*)b.getDataPointer(),
                        dimsB.getElementCount(), (const double*)a.getDataPointer(),
                        dimsA.getElementCount(), true);
                } else if ((a.isRowVector() && b.isRowVector())
                    || (a.isColumnVector() && b.isColumnVector())) {
                    throw Exception(
                        _W("Size mismatch on arguments to arithmetic operator ") + L"-");
                } else {
                    const double* ptrA = (const double*)a.getDataPointer();
                    const double* ptrB = (const double*)b.getDataPointer();

                    if (dimsA[0] == dimsB[0]) {
                        if (a.isVector()) {
                            return double_vector_matrix_subtraction(a, b);
                        } else {
                            return double_vector_matrix_subtraction(b, a, true);
                        }
                    } else if (dimsA[1] == dimsB[1]) {
                        if (a.isVector()) {
                            return double_vector_column_subtraction(a, b);
                        } else {
                            return double_vector_column_subtraction(b, a, true);
                        }
                    } else {
                        throw Exception(
                            _W("Size mismatch on arguments to arithmetic operator ") + L"-");
                    }
                }
            } else {
                throw Exception(_W("Size mismatch on arguments to arithmetic operator ") + L"-");
            }
        }
    }
    return ArrayOf(NLS_DOUBLE, dimsC, Cp, false);
}
//=============================================================================
ArrayOf
dcomplex_subtraction(ArrayOf a, ArrayOf b)
{
    a.promoteType(NLS_DCOMPLEX);
    b.promoteType(NLS_DCOMPLEX);
    void* Cp = nullptr;
    if (a.isScalar() && b.isScalar()) {
        doublecomplex ca = a.getContentAsDoubleComplexScalar();
        doublecomplex cb = b.getContentAsDoubleComplexScalar();
        doublecomplex res = ca - cb;
        return ArrayOf::dcomplexConstructor(res.real(), res.imag());
    }
    Dimensions dimsA = a.getDimensions();
    Dimensions dimsB = b.getDimensions();
    Dimensions dimsC;
    if (a.isEmpty() || b.isEmpty()) {
        if (a.isScalar() || b.isScalar()) {
            if (a.isScalar()) {
                return ArrayOf(b);
            } else {
                return ArrayOf(a);
            }
        } else {
            if (!(SameSizeCheck(dimsA, dimsB))) {
                throw Exception(_W("Size mismatch on arguments to arithmetic operator ") + L"-");
            }
            return ArrayOf(b);
        }
    }
    if (SameSizeCheck(dimsA, dimsB)) {
        return doublecomplex_matrix_matrix_subtraction(a, b);
    } else {
        if (a.isScalar() || b.isScalar()) {
            if (a.isScalar()) {
                return doublecomplex_scalar_matrix_subtraction(a, b);
            } else {
                // b.isScalar()
                return doublecomplex_scalar_matrix_subtraction(b, a, true);
            }
        } else {
            if (a.isVector() || b.isVector()) {
                if (a.isRowVector() && b.isColumnVector()) {
                    dimsC = Dimensions(std::min(dimsA.getMax(), dimsB.getMax()),
                        std::max(dimsA.getMax(), dimsB.getMax()));
                    indexType Clen = dimsC.getElementCount();
                    Cp = new_with_exception<double>(Clen * 2, false);
                    doublecomplex_vector_subtraction((double*)Cp, (double*)a.getDataPointer(),
                        dimsA.getElementCount(), (double*)b.getDataPointer(),
                        dimsB.getElementCount());
                } else if (a.isColumnVector() && b.isRowVector()) {
                    dimsC = Dimensions(std::min(dimsA.getMax(), dimsB.getMax()),
                        std::max(dimsA.getMax(), dimsB.getMax()));
                    indexType Clen = dimsC.getElementCount();
                    Cp = new_with_exception<double>(Clen * 2, false);
                    doublecomplex_vector_subtraction((double*)Cp, (double*)b.getDataPointer(),
                        dimsB.getElementCount(), (double*)a.getDataPointer(),
                        dimsA.getElementCount(), true);
                } else if ((a.isRowVector() && b.isRowVector())
                    || (a.isColumnVector() && b.isColumnVector())) {
                    throw Exception(
                        _W("Size mismatch on arguments to arithmetic operator ") + L"-");
                } else {
                    double* ptrA = (double*)a.getDataPointer();
                    double* ptrB = (double*)b.getDataPointer();

                    if (dimsA[0] == dimsB[0]) {
                        if (a.isVector()) {
                            return doublecomplex_vector_matrix_subtraction(a, b);
                        } else {
                            return doublecomplex_vector_matrix_subtraction(b, a, true);
                        }
                    } else if (dimsA[1] == dimsB[1]) {
                        if (a.isVector()) {
                            return doublecomplex_vector_column_subtraction(a, b);
                        } else {
                            return doublecomplex_vector_column_subtraction(b, a, true);
                        }
                    } else {
                        throw Exception(
                            _W("Size mismatch on arguments to arithmetic operator ") + L"-");
                    }
                }
            } else {
                throw Exception(_W("Size mismatch on arguments to arithmetic operator ") + L"-");
            }
        }
    }
    return ArrayOf(NLS_DCOMPLEX, dimsC, Cp, false);
}
//=============================================================================
ArrayOf
double_minus_double(ArrayOf a, ArrayOf b)
{
    if (a.isComplex() || b.isComplex()) {
        ArrayOf res = dcomplex_subtraction(a, b);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
        return res;
    }
    return double_subtraction(a, b);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
