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
#include <algorithm>
#include "Equals.hpp"
#include "MatrixCheck.hpp"
#include "Exception.hpp"
#include "ClassName.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
matrix_matrix_operator(ArrayOf& A, ArrayOf& B,
    logical (*realRelationOperator)(
        Class commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*complexRelationOperator)(
        Class commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*stringRelationOperator)(
        Class commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB))
{
    ArrayOf res;
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    logical* Cp = new_with_exception<logical>(Clen, false);

    void* ptrA = (void*)A.getDataPointer();
    void* ptrB = (void*)B.getDataPointer();
    Class classA = A.getDataClass();
    for (indexType i = 0; i < dimsC.getElementCount(); i++) {
        switch (classA) {
        case NLS_STRING_ARRAY: {
            Cp[i] = (*stringRelationOperator)(classA, ptrA, ptrB, i, i);
        } break;
        case NLS_LOGICAL: {
            Cp[i] = (*realRelationOperator)(classA, ptrA, ptrB, i, i);
        } break;
        case NLS_UINT8: {
            Cp[i] = (*realRelationOperator)(classA, ptrA, ptrB, i, i);
        } break;
        case NLS_INT8: {
            Cp[i] = (*realRelationOperator)(classA, ptrA, ptrB, i, i);
        } break;
        case NLS_UINT16: {
            Cp[i] = (*realRelationOperator)(classA, ptrA, ptrB, i, i);
        } break;
        case NLS_INT16: {
            Cp[i] = (*realRelationOperator)(classA, ptrA, ptrB, i, i);
        } break;
        case NLS_UINT32: {
            Cp[i] = (*realRelationOperator)(classA, ptrA, ptrB, i, i);
        } break;
        case NLS_INT32: {
            Cp[i] = (*realRelationOperator)(classA, ptrA, ptrB, i, i);
        } break;
        case NLS_UINT64: {
            Cp[i] = (*realRelationOperator)(classA, ptrA, ptrB, i, i);
        } break;
        case NLS_INT64: {
            Cp[i] = (*realRelationOperator)(classA, ptrA, ptrB, i, i);
        } break;
        case NLS_SINGLE: {
            Cp[i] = (*realRelationOperator)(classA, ptrA, ptrB, i, i);
        } break;
        case NLS_DOUBLE: {
            Cp[i] = (*realRelationOperator)(classA, ptrA, ptrB, i, i);
        } break;
        case NLS_SCOMPLEX: {
            Cp[i] = (*complexRelationOperator)(classA, ptrA, ptrB, i, i);
        } break;
        case NLS_DCOMPLEX: {
            Cp[i] = (*complexRelationOperator)(classA, ptrA, ptrB, i, i);
        } break;
        case NLS_CHAR: {
            Cp[i] = (*realRelationOperator)(classA, ptrA, ptrB, i, i);
        } break;
        }
    }
    return ArrayOf(NLS_LOGICAL, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf
scalar_matrix_operator(ArrayOf& A, ArrayOf& B,
    logical (*realRelationOperator)(
        Class commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*complexRelationOperator)(
        Class commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*stringRelationOperator)(
        Class commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB))
{
    Dimensions dimsC = B.getDimensions();
    indexType Clen = dimsC.getElementCount();
    logical* Cp = new_with_exception<logical>(Clen, false);
    void* ptrA = (void*)A.getDataPointer();
    void* ptrB = (void*)B.getDataPointer();
    Class classA = A.getDataClass();
    for (indexType i = 0; i < Clen; i++) {
        switch (classA) {
        case NLS_STRING_ARRAY: {
            Cp[i] = stringRelationOperator(classA, ptrA, ptrB, 0, i);
        } break;
        case NLS_LOGICAL: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, 0, i);
        } break;
        case NLS_UINT8: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, 0, i);
        } break;
        case NLS_INT8: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, 0, i);
        } break;
        case NLS_UINT16: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, 0, i);
        } break;
        case NLS_INT16: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, 0, i);
        } break;
        case NLS_UINT32: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, 0, i);
        } break;
        case NLS_INT32: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, 0, i);
        } break;
        case NLS_UINT64: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, 0, i);
        } break;
        case NLS_INT64: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, 0, i);
        } break;
        case NLS_SINGLE: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, 0, i);
        } break;
        case NLS_DOUBLE: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, 0, i);
        } break;
        case NLS_SCOMPLEX: {
            Cp[i] = complexRelationOperator(classA, ptrA, ptrB, 0, i);
        } break;
        case NLS_DCOMPLEX: {
            Cp[i] = complexRelationOperator(classA, ptrA, ptrB, 0, i);
        } break;
        case NLS_CHAR: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, 0, i);
        } break;
        }
    }
    return ArrayOf(NLS_LOGICAL, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf
matrix_scalar_operator(ArrayOf& A, ArrayOf& B,
    logical (*realRelationOperator)(
        Class commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*complexRelationOperator)(
        Class commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*stringRelationOperator)(
        Class commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB))
{
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    logical* Cp = new_with_exception<logical>(Clen, false);
    void* ptrA = (void*)A.getDataPointer();
    void* ptrB = (void*)B.getDataPointer();
    Class classA = A.getDataClass();
    for (indexType i = 0; i < Clen; i++) {
        switch (classA) {
        case NLS_STRING_ARRAY: {
            Cp[i] = stringRelationOperator(classA, ptrA, ptrB, i, 0);
        } break;
        case NLS_LOGICAL: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, i, 0);
        } break;
        case NLS_UINT8: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, i, 0);
        } break;
        case NLS_INT8: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, i, 0);
        } break;
        case NLS_UINT16: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, i, 0);
        } break;
        case NLS_INT16: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, i, 0);
        } break;
        case NLS_UINT32: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, i, 0);
        } break;
        case NLS_INT32: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, i, 0);
        } break;
        case NLS_UINT64: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, i, 0);
        } break;
        case NLS_INT64: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, i, 0);
        } break;
        case NLS_SINGLE: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, i, 0);
        } break;
        case NLS_DOUBLE: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, i, 0);
        } break;
        case NLS_SCOMPLEX: {
            Cp[i] = complexRelationOperator(classA, ptrA, ptrB, i, 0);
        } break;
        case NLS_DCOMPLEX: {
            Cp[i] = complexRelationOperator(classA, ptrA, ptrB, i, 0);
        } break;
        case NLS_CHAR: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, i, 0);
        } break;
        }
    }
    return ArrayOf(NLS_LOGICAL, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf
vector_row_column_operator(const Dimensions& outputDimensions, ArrayOf& A, ArrayOf& B,
    logical (*realRelationOperator)(
        Class commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*complexRelationOperator)(
        Class commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*stringRelationOperator)(
        Class commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB))
{
    ArrayOf res;
    indexType Clen = outputDimensions.getElementCount();
    logical* Cp = new_with_exception<logical>(Clen, false);
    void* ptrA = (void*)A.getDataPointer();
    void* ptrB = (void*)B.getDataPointer();
    indexType nbElementsA = A.getDimensions().getElementCount();
    indexType nbElementsB = B.getDimensions().getElementCount();
    Class classA = A.getDataClass();
    indexType m = 0;
    for (indexType i = 0; i < nbElementsA; i++) {
        for (indexType j = 0; j < nbElementsB; j++) {
            switch (classA) {
            case NLS_STRING_ARRAY: {
                Cp[m] = stringRelationOperator(classA, ptrA, ptrB, i, j);
            } break;
            case NLS_LOGICAL: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, i, j);
            } break;
            case NLS_UINT8: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, i, j);
            } break;
            case NLS_INT8: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, i, j);
            } break;
            case NLS_UINT16: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, i, j);
            } break;
            case NLS_INT16: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, i, j);
            } break;
            case NLS_UINT32: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, i, j);
            } break;
            case NLS_INT32: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, i, j);
            } break;
            case NLS_UINT64: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, i, j);
            } break;
            case NLS_INT64: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, i, j);
            } break;
            case NLS_SINGLE: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, i, j);
            } break;
            case NLS_DOUBLE: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, i, j);
            } break;
            case NLS_SCOMPLEX: {
                Cp[m] = complexRelationOperator(classA, ptrA, ptrB, i, j);
            } break;
            case NLS_DCOMPLEX: {
                Cp[m] = complexRelationOperator(classA, ptrA, ptrB, i, j);
            } break;
            case NLS_CHAR: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, i, j);
            } break;
            }
            m++;
        }
    }
    return ArrayOf(NLS_LOGICAL, outputDimensions, Cp, false);
}
//=============================================================================
static ArrayOf
vector_column_row_operator(const Dimensions& outputDimensions, ArrayOf& A, ArrayOf& B,
    logical (*realRelationOperator)(
        Class commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*complexRelationOperator)(
        Class commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*stringRelationOperator)(
        Class commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB))
{
    ArrayOf res;
    indexType Clen = outputDimensions.getElementCount();
    logical* Cp = new_with_exception<logical>(Clen, false);
    void* ptrA = (void*)A.getDataPointer();
    void* ptrB = (void*)B.getDataPointer();
    Class classA = A.getDataClass();
    indexType m = 0;
    for (indexType i = 0; i < B.getDimensions().getElementCount(); i++) {
        for (indexType j = 0; j < A.getDimensions().getElementCount(); j++) {
            switch (classA) {
            case NLS_STRING_ARRAY: {
                Cp[m] = stringRelationOperator(classA, ptrA, ptrB, j, i);
            } break;
            case NLS_LOGICAL: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, i);
            } break;
            case NLS_UINT8: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, i);
            } break;
            case NLS_INT8: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, i);
            } break;
            case NLS_UINT16: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, i);
            } break;
            case NLS_INT16: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, i);
            } break;
            case NLS_UINT32: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, i);
            } break;
            case NLS_INT32: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, i);
            } break;
            case NLS_UINT64: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, i);
            } break;
            case NLS_INT64: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, i);
            } break;
            case NLS_SINGLE: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, i);
            } break;
            case NLS_DOUBLE: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, i);
            } break;
            case NLS_SCOMPLEX: {
                Cp[m] = complexRelationOperator(classA, ptrA, ptrB, j, i);
            } break;
            case NLS_DCOMPLEX: {
                Cp[m] = complexRelationOperator(classA, ptrA, ptrB, j, i);
            } break;
            case NLS_CHAR: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, i);
            } break;
            }
            m++;
        }
    }
    return ArrayOf(NLS_LOGICAL, outputDimensions, Cp, false);
}
//=============================================================================
static ArrayOf
vector_matrix_operator(ArrayOf& A, ArrayOf& B,
    logical (*realRelationOperator)(
        Class commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*complexRelationOperator)(
        Class commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*stringRelationOperator)(
        Class commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB))
{
    indexType q = 0;
    Dimensions dimsC = B.getDimensions();
    indexType Clen = dimsC.getElementCount();
    logical* Cp = new_with_exception<logical>(Clen, false);
    void* ptrA = (void*)A.getDataPointer();
    void* ptrB = (void*)B.getDataPointer();
    Class classA = A.getDataClass();
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * A.getDimensions().getRows();
            switch (classA) {
            case NLS_STRING_ARRAY: {
                Cp[m] = stringRelationOperator(classA, ptrA, ptrB, q, m);
            } break;
            case NLS_LOGICAL: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, q, m);
            } break;
            case NLS_UINT8: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, q, m);
            } break;
            case NLS_INT8: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, q, m);
            } break;
            case NLS_UINT16: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, q, m);
            } break;
            case NLS_INT16: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, q, m);
            } break;
            case NLS_UINT32: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, q, m);
            } break;
            case NLS_INT32: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, q, m);
            } break;
            case NLS_UINT64: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, q, m);
            } break;
            case NLS_INT64: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, q, m);
            } break;
            case NLS_SINGLE: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, q, m);
            } break;
            case NLS_DOUBLE: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, q, m);
            } break;
            case NLS_SCOMPLEX: {
                Cp[m] = complexRelationOperator(classA, ptrA, ptrB, q, m);
            } break;
            case NLS_DCOMPLEX: {
                Cp[m] = complexRelationOperator(classA, ptrA, ptrB, q, m);
            } break;
            case NLS_CHAR: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, q, m);
            } break;
            }
        }
        q++;
    }
    return ArrayOf(NLS_LOGICAL, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf
matrix_vector_operator(ArrayOf& A, ArrayOf& B,
    logical (*realRelationOperator)(
        Class commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*complexRelationOperator)(
        Class commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*stringRelationOperator)(
        Class commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB))
{
    indexType q = 0;
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    logical* Cp = new_with_exception<logical>(Clen, false);
    void* ptrA = (void*)A.getDataPointer();
    void* ptrB = (void*)B.getDataPointer();
    Class classA = A.getDataClass();
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * A.getDimensions().getRows();
            switch (classA) {
            case NLS_STRING_ARRAY: {
                Cp[m] = stringRelationOperator(classA, ptrA, ptrB, m, q);
            } break;
            case NLS_LOGICAL: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, q);
            } break;
            case NLS_UINT8: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, q);
            } break;
            case NLS_INT8: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, q);
            } break;
            case NLS_UINT16: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, q);
            } break;
            case NLS_INT16: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, q);
            } break;
            case NLS_UINT32: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, q);
            } break;
            case NLS_INT32: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, q);
            } break;
            case NLS_UINT64: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, q);
            } break;
            case NLS_INT64: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, q);
            } break;
            case NLS_SINGLE: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, q);
            } break;
            case NLS_DOUBLE: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, q);
            } break;
            case NLS_SCOMPLEX: {
                Cp[m] = complexRelationOperator(classA, ptrA, ptrB, m, q);
            } break;
            case NLS_DCOMPLEX: {
                Cp[m] = complexRelationOperator(classA, ptrA, ptrB, m, q);
            } break;
            case NLS_CHAR: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, q);
            } break;
            }
        }
        q++;
    }
    return ArrayOf(NLS_LOGICAL, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf
vector_column_matrix_operator(ArrayOf& A, ArrayOf& B,
    logical (*realRelationOperator)(
        Class commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*complexRelationOperator)(
        Class commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*stringRelationOperator)(
        Class commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB))
{

    Dimensions dimsC = B.getDimensions();
    indexType Clen = dimsC.getElementCount();
    logical* Cp = new_with_exception<logical>(Clen, false);
    void* ptrA = (void*)A.getDataPointer();
    void* ptrB = (void*)B.getDataPointer();
    Class classA = A.getDataClass();
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * A.getDimensions().getColumns();
            switch (classA) {
            case NLS_STRING_ARRAY: {
                Cp[m] = stringRelationOperator(classA, ptrA, ptrB, j, m);
            } break;
            case NLS_LOGICAL: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, m);
            } break;
            case NLS_UINT8: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, m);
            } break;
            case NLS_INT8: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, m);
            } break;
            case NLS_UINT16: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, m);
            } break;
            case NLS_INT16: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, m);
            } break;
            case NLS_UINT32: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, m);
            } break;
            case NLS_INT32: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, m);
            } break;
            case NLS_UINT64: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, m);
            } break;
            case NLS_INT64: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, m);
            } break;
            case NLS_SINGLE: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, m);
            } break;
            case NLS_DOUBLE: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, m);
            } break;
            case NLS_SCOMPLEX: {
                Cp[m] = complexRelationOperator(classA, ptrA, ptrB, j, m);
            } break;
            case NLS_DCOMPLEX: {
                Cp[m] = complexRelationOperator(classA, ptrA, ptrB, j, m);
            } break;
            case NLS_CHAR: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, m);
            } break;
            }
        }
    }
    return ArrayOf(NLS_LOGICAL, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf
matrix_vector_column_operator(ArrayOf& A, ArrayOf& B,
    logical (*realRelationOperator)(
        Class commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*complexRelationOperator)(
        Class commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*stringRelationOperator)(
        Class commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB))
{
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    logical* Cp = new_with_exception<logical>(Clen, false);
    void* ptrA = (void*)A.getDataPointer();
    void* ptrB = (void*)B.getDataPointer();
    Class classA = A.getDataClass();
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * B.getDimensions().getColumns();
            switch (classA) {
            case NLS_STRING_ARRAY: {
                Cp[m] = stringRelationOperator(classA, ptrA, ptrB, m, j);
            } break;
            case NLS_LOGICAL: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, j);
            } break;
            case NLS_UINT8: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, j);
            } break;
            case NLS_INT8: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, j);
            } break;
            case NLS_UINT16: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, j);
            } break;
            case NLS_INT16: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, j);
            } break;
            case NLS_UINT32: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, j);
            } break;
            case NLS_INT32: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, j);
            } break;
            case NLS_UINT64: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, j);
            } break;
            case NLS_INT64: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, j);
            } break;
            case NLS_SINGLE: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, j);
            } break;
            case NLS_DOUBLE: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, j);
            } break;
            case NLS_SCOMPLEX: {
                Cp[m] = complexRelationOperator(classA, ptrA, ptrB, m, j);
            } break;
            case NLS_DCOMPLEX: {
                Cp[m] = complexRelationOperator(classA, ptrA, ptrB, m, j);
            } break;
            case NLS_CHAR: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, j);
            } break;
            }
        }
    }
    return ArrayOf(NLS_LOGICAL, dimsC, Cp, false);
}
//=============================================================================
ArrayOf
relationOperator(ArrayOf& A, ArrayOf& B, const std::wstring& operatorName,
    logical (*realRelationOperator)(
        Class commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*complexRelationOperator)(
        Class commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*stringRelationOperator)(
        Class commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    bool& needToOverload)
{
    needToOverload = false;
    if (A.isSparse() || B.isSparse()) {
        needToOverload = true;
        return ArrayOf();
    }
    bool asStringArray = (A.isStringArray() && B.isStringArray())
        || (A.isStringArray() && B.isRowVectorCharacterArray())
        || (B.isStringArray() && A.isRowVectorCharacterArray());
    if ((A.isReferenceType() || B.isReferenceType()) && !asStringArray) {
        needToOverload = true;
        return ArrayOf();
    }
    Class classCommon = FindCommonType(A, B, false);
    if (asStringArray) {
        if (!A.isStringArray()) {
            A = ArrayOf::toStringArray(A, needToOverload);
        }
        if (!B.isStringArray()) {
            B = ArrayOf::toStringArray(B, needToOverload);
        }
    } else {
        A.promoteType(classCommon);
        B.promoteType(classCommon);
    }

    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    Dimensions dimsC;
    if (A.isEmpty() || B.isEmpty()) {
        if (A.isScalar() || B.isScalar()) {
            if (A.isScalar()) {
                ArrayOf res = ArrayOf::emptyConstructor(dimsB);
                res.promoteType(NLS_LOGICAL);
                return res;
            } else {
                ArrayOf res = ArrayOf::emptyConstructor(dimsA);
                res.promoteType(NLS_LOGICAL);
                return res;
            }
        } else {
            if (!(SameSizeCheck(dimsA, dimsB))) {
                Error(_W("Size mismatch on arguments to arithmetic operator ") + operatorName);
            }
            ArrayOf res = ArrayOf::emptyConstructor(dimsB);
            res.promoteType(NLS_LOGICAL);
            return res;
        }
    }
    if (SameSizeCheck(dimsA, dimsB)) {
        return matrix_matrix_operator(
            A, B, realRelationOperator, complexRelationOperator, stringRelationOperator);
    } else {
        if (A.isScalar() || B.isScalar()) {
            if (A.isScalar()) {
                return scalar_matrix_operator(
                    A, B, realRelationOperator, complexRelationOperator, stringRelationOperator);
            } else {
                return matrix_scalar_operator(
                    A, B, realRelationOperator, complexRelationOperator, stringRelationOperator);
            }
        } else {
            if (A.isVector() || B.isVector()) {
                if (A.isRowVector() && B.isColumnVector()) {
                    dimsC = Dimensions(std::min(dimsA.getMax(), dimsB.getMax()),
                        std::max(dimsA.getMax(), dimsB.getMax()));
                    return vector_row_column_operator(dimsC, A, B, realRelationOperator,
                        complexRelationOperator, stringRelationOperator);
                } else if (A.isColumnVector() && B.isRowVector()) {
                    dimsC = Dimensions(std::min(dimsA.getMax(), dimsB.getMax()),
                        std::max(dimsA.getMax(), dimsB.getMax()));
                    return vector_column_row_operator(dimsC, A, B, realRelationOperator,
                        complexRelationOperator, stringRelationOperator);
                } else if ((A.isRowVector() && B.isRowVector())
                    || (A.isColumnVector() && B.isColumnVector())) {
                    Error(_W("Size mismatch on arguments to arithmetic operator ") + operatorName);
                } else {
                    if (dimsA[1] == dimsB[1]) {
                        if (A.isVector()) {
                            return vector_column_matrix_operator(A, B, realRelationOperator,
                                complexRelationOperator, stringRelationOperator);
                        } else {
                            return matrix_vector_column_operator(A, B, realRelationOperator,
                                complexRelationOperator, stringRelationOperator);
                        }
                    } else if ((dimsA[0] == dimsB[0]) && (dimsA[0] != 1)) {
                        if (A.isVector()) {
                            return vector_matrix_operator(A, B, realRelationOperator,
                                complexRelationOperator, stringRelationOperator);
                        } else {
                            return matrix_vector_operator(A, B, realRelationOperator,
                                complexRelationOperator, stringRelationOperator);
                        }
                    } else {
                        Error(_W("Size mismatch on arguments to arithmetic operator ")
                            + operatorName);
                    }
                }
            } else {
                Error(_W("Size mismatch on arguments to arithmetic operator ") + operatorName);
            }
        }
    }
    needToOverload = true;
    return ArrayOf();
}
//=============================================================================
}
//=============================================================================
