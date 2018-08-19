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
#include <Eigen/Dense>
#include "Atan2.hpp"
#include "ArrayOf.hpp"
#include "MatrixCheck.hpp"
#include "Exception.hpp"
#include "nlsTrigonometric_functions_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
static ArrayOf
matrix_matrix_atan2(Class classDestination, const ArrayOf& a, const ArrayOf& b)
{
    Dimensions dimsC = a.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = new_with_exception<T>(Clen, false);
    T* C = (T*)Cp;
    T* ptrA = (T*)a.getDataPointer();
    T* ptrB = (T*)b.getDataPointer();
    for (indexType k = 0; k < Clen; k++) {
        C[k] = atan2(ptrA[k], ptrB[k]);
    }
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
template <class T>
static ArrayOf
scalar_matrix_atan2(Class classDestination, const ArrayOf& a, const ArrayOf& b)
{
    Dimensions dimsC = b.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = new_with_exception<T>(Clen, false);
    T* ptrA = (T*)a.getDataPointer();
    T* ptrB = (T*)b.getDataPointer();
    T* ptrC = (T*)Cp;
    for (indexType k = 0; k < Clen; k++) {
        ptrC[k] = atan2(ptrA[0], ptrB[k]);
    }
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
template <class T>
static ArrayOf
matrix_scalar_atan2(Class classDestination, const ArrayOf& a, const ArrayOf& b)
{
    Dimensions dimsC = a.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = new_with_exception<T>(Clen, false);
    T* ptrA = (T*)a.getDataPointer();
    T* ptrB = (T*)b.getDataPointer();
    T* ptrC = (T*)Cp;
    for (indexType k = 0; k < Clen; k++) {
        ptrC[k] = atan2(ptrA[k], ptrB[0]);
    }
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
template <class T>
static void
vector_column_row_atan2(T* C, const T* A, indexType NA, const T* B, indexType NB)
{
    for (indexType i = 0; i < NA; i++) {
        for (indexType j = 0; j < NB; j++) {
            indexType m = i + j * NA;
            C[m] = atan2(A[i], B[j]);
        }
    }
}
//=============================================================================
template <class T>
static void
vector_row_column_atan2(T* C, const T* A, indexType NA, const T* B, indexType NB)
{
    indexType m = 0;
    for (indexType i = 0; i < NA; i++) {
        for (indexType j = 0; j < NB; j++) {
            C[m] = atan2(A[i], B[j]);
            m++;
        }
    }
}
//=============================================================================
template <class T>
static ArrayOf
vector_matrix_atan2(Class classDestination, const ArrayOf& a, const ArrayOf& b)
{
    const T* ptrA = (const T*)a.getDataPointer();
    const T* ptrB = (const T*)b.getDataPointer();
    indexType q = 0;
    Dimensions dimsC = b.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = new_with_exception<T>(Clen, false);
    T* C = (T*)Cp;
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * a.getDimensions().getRows();
            C[m] = atan2(ptrB[m], ptrA[q]);
        }
        q++;
    }
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
template <class T>
static ArrayOf
vector_column_atan2(Class classDestination, const ArrayOf& a, const ArrayOf& b)
{
    const T* ptrA = (const T*)a.getDataPointer();
    const T* ptrB = (const T*)b.getDataPointer();
    Dimensions dimsC = b.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = new_with_exception<T>(Clen, false);
    T* C = (T*)Cp;
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * b.getDimensions().getRows();
            C[m] = atan2(ptrB[m], ptrA[j]);
        }
    }
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
template <class T>
static ArrayOf
Atan2(Class classDestination, const ArrayOf& A, const ArrayOf& B)
{
    void* Cp = nullptr;
    if (A.isScalar() && B.isScalar()) {
        T* ptrA = (T*)A.getDataPointer();
        T* ptrB = (T*)B.getDataPointer();
        T res = atan2(ptrA[0], ptrB[0]);
        if (classDestination == NLS_SINGLE) {
            return ArrayOf::singleConstructor((single)res);
        } else {
            return ArrayOf::doubleConstructor((double)res);
        }
    }
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    Dimensions dimsC;
    if (A.isEmpty() || B.isEmpty()) {
        if (A.isScalar() || B.isScalar()) {
            if (A.isScalar()) {
                return ArrayOf(B);
            } else {
                return ArrayOf(A);
            }
        } else {
            if (!(SameSizeCheck(dimsA, dimsB))) {
                Error(_W("Size mismatch on arguments to arithmetic operator ") + L"atan2");
            }
            return ArrayOf(B);
        }
    }
    if (SameSizeCheck(dimsA, dimsB)) {
        return matrix_matrix_atan2<T>(classDestination, A, B);
    } else {
        if (A.isScalar() || B.isScalar()) {
            if (A.isScalar()) {
                return scalar_matrix_atan2<T>(classDestination, A, B);
            } else {
                // b.isScalar()
                return matrix_scalar_atan2<T>(classDestination, A, B);
            }
        } else {
            if (A.isVector() || B.isVector()) {
                if (A.isRowVector() && B.isColumnVector()) {
                    dimsC = Dimensions(std::min(dimsA.getMax(), dimsB.getMax()),
                        std::max(dimsA.getMax(), dimsB.getMax()));
                    indexType Clen = dimsC.getElementCount();
                    Cp = new_with_exception<T>(Clen, false);
                    vector_row_column_atan2((T*)Cp, (const T*)A.getDataPointer(),
                        dimsA.getElementCount(), (const T*)B.getDataPointer(),
                        dimsB.getElementCount());
                } else if (A.isColumnVector() && B.isRowVector()) {
                    dimsC = Dimensions(std::min(dimsA.getMax(), dimsB.getMax()),
                        std::max(dimsA.getMax(), dimsB.getMax()));
                    indexType Clen = dimsC.getElementCount();
                    Cp = new_with_exception<T>(Clen, false);
                    vector_column_row_atan2<T>((T*)Cp, (const T*)A.getDataPointer(),
                        dimsA.getElementCount(), (const T*)B.getDataPointer(),
                        dimsB.getElementCount());
                } else if ((A.isRowVector() && B.isRowVector())
                    || (A.isColumnVector() && B.isColumnVector())) {
                    Error(_W("Size mismatch on arguments to arithmetic operator ") + L"atan2");
                } else {
                    const T* ptrA = (const T*)A.getDataPointer();
                    const T* ptrB = (const T*)B.getDataPointer();

                    if (dimsA[0] == dimsB[0]) {
                        if (A.isVector()) {
                            return vector_matrix_atan2<T>(classDestination, A, B);
                        } else {
                            return vector_matrix_atan2<T>(classDestination, B, A);
                        }
                    } else if (dimsA[1] == dimsB[1]) {
                        if (A.isVector()) {
                            return vector_column_atan2<T>(classDestination, A, B);
                        } else {
                            return vector_column_atan2<T>(classDestination, B, A);
                        }
                    } else {
                        Error(_W("Size mismatch on arguments to arithmetic operator ") + L"atan2");
                    }
                }
            } else {
                Error(_W("Size mismatch on arguments to arithmetic operator ") + L"atan2");
            }
        }
    }
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
ArrayOf
Atan2(ArrayOf A, ArrayOf B, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    if (A.isSparse()) {
        needToOverload = true;
    } else {
        if ((A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_SINGLE)
            && (B.getDataClass() == NLS_DOUBLE || B.getDataClass() == NLS_SINGLE)) {
            Class destinationClass;
            if (A.getDataClass() == B.getDataClass()) {
                destinationClass = A.getDataClass();
            } else {
                destinationClass = NLS_SINGLE;
            }
            if (destinationClass == NLS_SINGLE) {
                A.promoteType(NLS_SINGLE);
                B.promoteType(NLS_SINGLE);
                res = Atan2<single>(destinationClass, A, B);
            } else {
                A.promoteType(NLS_DOUBLE);
                B.promoteType(NLS_DOUBLE);
                res = Atan2<double>(destinationClass, A, B);
            }
        } else {
            needToOverload = true;
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
