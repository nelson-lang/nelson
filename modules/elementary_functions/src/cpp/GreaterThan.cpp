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
#include "GreaterThan.hpp"
#include "complex_abs.hpp"
#include "MatrixCheck.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
string_compare_is_greater_than(const std::wstring& A, const std::wstring& B)
{
    if (A.empty() || B.empty()) {
        if (A.empty() && B.empty()) {
            return true;
        }
        if (!A.empty()) {
            return true;
        }
        return false;
    }
    return wcscmp(A.c_str(), B.c_str()) > 0;
}
//=============================================================================
static ArrayOf
matrix_matrix_greater_than(ArrayOf& A, ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    logical* Cp = new_with_exception<logical>(Clen, false);

    for (indexType i = 0; i < A.getDimensions().getElementCount(); i++) {
        switch (A.getDataClass()) {
        case NLS_STRING_ARRAY: {
            ArrayOf* elementsA = (ArrayOf*)A.getDataPointer();
            ArrayOf* elementsB = (ArrayOf*)B.getDataPointer();
            if (elementsA[i].isCharacterArray() && elementsB[i].isCharacterArray()) {
                Cp[i] = string_compare_is_greater_than(
                    elementsA[i].getContentAsWideString(), elementsB[i].getContentAsWideString());
            } else {
                Cp[i] = logical(0);
            }
        } break;
        case NLS_LOGICAL: {
            logical* ptrA = (logical*)A.getDataPointer();
            logical* ptrB = (logical*)B.getDataPointer();
            Cp[i] = (ptrA[i] > ptrB[i]);
        } break;
        case NLS_UINT8: {
            uint8* ptrA = (uint8*)A.getDataPointer();
            uint8* ptrB = (uint8*)B.getDataPointer();
            Cp[i] = (ptrA[i] > ptrB[i]);
        } break;
        case NLS_INT8: {
            int8* ptrA = (int8*)A.getDataPointer();
            int8* ptrB = (int8*)B.getDataPointer();
            Cp[i] = (ptrA[i] > ptrB[i]);
        } break;
        case NLS_UINT16: {
            uint16* ptrA = (uint16*)A.getDataPointer();
            uint16* ptrB = (uint16*)B.getDataPointer();
            Cp[i] = (ptrA[i] > ptrB[i]);
        } break;
        case NLS_INT16: {
            int16* ptrA = (int16*)A.getDataPointer();
            int16* ptrB = (int16*)B.getDataPointer();
            Cp[i] = (ptrA[i] > ptrB[i]);
        } break;
        case NLS_UINT32: {
            uint32* ptrA = (uint32*)A.getDataPointer();
            uint32* ptrB = (uint32*)B.getDataPointer();
            Cp[i] = (ptrA[i] > ptrB[i]);
        } break;
        case NLS_INT32: {
            int32* ptrA = (int32*)A.getDataPointer();
            int32* ptrB = (int32*)B.getDataPointer();
            Cp[i] = (ptrA[i] > ptrB[i]);
        } break;
        case NLS_UINT64: {
            uint64* ptrA = (uint64*)A.getDataPointer();
            uint64* ptrB = (uint64*)B.getDataPointer();
            Cp[i] = (ptrA[i] > ptrB[i]);
        } break;
        case NLS_INT64: {
            int64* ptrA = (int64*)A.getDataPointer();
            int64* ptrB = (int64*)B.getDataPointer();
            Cp[i] = (ptrA[i] > ptrB[i]);
        } break;
        case NLS_SINGLE: {
            single* ptrA = (single*)A.getDataPointer();
            single* ptrB = (single*)B.getDataPointer();
            Cp[i] = (ptrA[i] > ptrB[i]);
        } break;
        case NLS_DOUBLE: {
            double* ptrA = (double*)A.getDataPointer();
            double* ptrB = (double*)B.getDataPointer();
            Cp[i] = (ptrA[i] > ptrB[i]);
        } break;
        case NLS_SCOMPLEX: {
            single* ptrA = (single*)A.getDataPointer();
            single* ptrB = (single*)B.getDataPointer();
            Cp[i] = (complex_abs<single>(ptrA[2 * i], ptrA[2 * i + 1])
                > complex_abs<single>(ptrB[2 * i], ptrB[2 * i + 1]));
        } break;
        case NLS_DCOMPLEX: {
            double* ptrA = (double*)A.getDataPointer();
            double* ptrB = (double*)B.getDataPointer();
            Cp[i] = (complex_abs<double>(ptrA[2 * i], ptrA[2 * i + 1])
                > complex_abs<double>(ptrB[2 * i], ptrB[2 * i + 1]));
        } break;
        case NLS_CHAR: {
            charType* ptrA = (charType*)A.getDataPointer();
            charType* ptrB = (charType*)B.getDataPointer();
            Cp[i] = (ptrA[i] > ptrB[i]);
        } break;
        }
    }
    return ArrayOf(NLS_LOGICAL, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf
scalar_matrix_greater_than(ArrayOf& A, ArrayOf& B)
{
    Dimensions dimsC = B.getDimensions();
    indexType Clen = dimsC.getElementCount();
    logical* Cp = new_with_exception<logical>(Clen, false);
    for (indexType i = 0; i < Clen; i++) {
        switch (A.getDataClass()) {
        case NLS_STRING_ARRAY: {
            ArrayOf* elementsA = (ArrayOf*)A.getDataPointer();
            ArrayOf* elementsB = (ArrayOf*)B.getDataPointer();
            if (elementsA[0].isCharacterArray() && elementsB[i].isCharacterArray()) {
                Cp[i] = string_compare_is_greater_than(
                    elementsA[0].getContentAsWideString(), elementsB[i].getContentAsWideString());
            } else {
                Cp[i] = logical(0);
            }
        } break;
        case NLS_LOGICAL: {
            logical* ptrA = (logical*)A.getDataPointer();
            logical* ptrB = (logical*)B.getDataPointer();
            Cp[i] = (ptrA[0] > ptrB[i]);
        } break;
        case NLS_UINT8: {
            uint8* ptrA = (uint8*)A.getDataPointer();
            uint8* ptrB = (uint8*)B.getDataPointer();
            Cp[i] = (ptrA[0] > ptrB[i]);
        } break;
        case NLS_INT8: {
            int8* ptrA = (int8*)A.getDataPointer();
            int8* ptrB = (int8*)B.getDataPointer();
            Cp[i] = (ptrA[0] > ptrB[i]);
        } break;
        case NLS_UINT16: {
            uint16* ptrA = (uint16*)A.getDataPointer();
            uint16* ptrB = (uint16*)B.getDataPointer();
            Cp[i] = (ptrA[0] > ptrB[i]);
        } break;
        case NLS_INT16: {
            int16* ptrA = (int16*)A.getDataPointer();
            int16* ptrB = (int16*)B.getDataPointer();
            Cp[i] = (ptrA[0] > ptrB[i]);
        } break;
        case NLS_UINT32: {
            uint32* ptrA = (uint32*)A.getDataPointer();
            uint32* ptrB = (uint32*)B.getDataPointer();
            Cp[i] = (ptrA[0] > ptrB[i]);
        } break;
        case NLS_INT32: {
            int32* ptrA = (int32*)A.getDataPointer();
            int32* ptrB = (int32*)B.getDataPointer();
            Cp[i] = (ptrA[0] > ptrB[i]);
        } break;
        case NLS_UINT64: {
            uint64* ptrA = (uint64*)A.getDataPointer();
            uint64* ptrB = (uint64*)B.getDataPointer();
            Cp[i] = (ptrA[0] > ptrB[i]);
        } break;
        case NLS_INT64: {
            int64* ptrA = (int64*)A.getDataPointer();
            int64* ptrB = (int64*)B.getDataPointer();
            Cp[i] = (ptrA[0] > ptrB[i]);
        } break;
        case NLS_SINGLE: {
            single* ptrA = (single*)A.getDataPointer();
            single* ptrB = (single*)B.getDataPointer();
            Cp[i] = (ptrA[0] > ptrB[i]);
        } break;
        case NLS_DOUBLE: {
            double* ptrA = (double*)A.getDataPointer();
            double* ptrB = (double*)B.getDataPointer();
            Cp[i] = (ptrA[0] > ptrB[i]);
        } break;
        case NLS_SCOMPLEX: {
            single* ptrA = (single*)A.getDataPointer();
            single* ptrB = (single*)B.getDataPointer();
            Cp[i] = (complex_abs<single>(ptrA[0], ptrA[1])
                > complex_abs<single>(ptrB[2 * i], ptrB[2 * i + 1]));
        } break;
        case NLS_DCOMPLEX: {
            double* ptrA = (double*)A.getDataPointer();
            double* ptrB = (double*)B.getDataPointer();
            Cp[i] = (complex_abs<double>(ptrA[0], ptrA[1])
                > complex_abs<double>(ptrB[2 * i], ptrB[2 * i + 1]));
        } break;
        case NLS_CHAR: {
            charType* ptrA = (charType*)A.getDataPointer();
            charType* ptrB = (charType*)B.getDataPointer();
            Cp[i] = (ptrA[0] > ptrB[i]);
        } break;
        }
    }
    return ArrayOf(NLS_LOGICAL, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf
matrix_scalar_greater_than(ArrayOf& A, ArrayOf& B)
{
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    logical* Cp = new_with_exception<logical>(Clen, false);
    for (indexType i = 0; i < Clen; i++) {
        switch (A.getDataClass()) {
        case NLS_STRING_ARRAY: {
            ArrayOf* elementsA = (ArrayOf*)A.getDataPointer();
            ArrayOf* elementsB = (ArrayOf*)B.getDataPointer();
            if (elementsA[i].isCharacterArray() && elementsB[0].isCharacterArray()) {
                Cp[i] = string_compare_is_greater_than(
                    elementsA[i].getContentAsWideString(), elementsB[0].getContentAsWideString());
            } else {
                Cp[i] = logical(0);
            }
        } break;
        case NLS_LOGICAL: {
            logical* ptrA = (logical*)A.getDataPointer();
            logical* ptrB = (logical*)B.getDataPointer();
            Cp[i] = (ptrA[i] > ptrB[0]);
        } break;
        case NLS_UINT8: {
            uint8* ptrA = (uint8*)A.getDataPointer();
            uint8* ptrB = (uint8*)B.getDataPointer();
            Cp[i] = (ptrA[i] > ptrB[0]);
        } break;
        case NLS_INT8: {
            int8* ptrA = (int8*)A.getDataPointer();
            int8* ptrB = (int8*)B.getDataPointer();
            Cp[i] = (ptrA[i] > ptrB[0]);
        } break;
        case NLS_UINT16: {
            uint16* ptrA = (uint16*)A.getDataPointer();
            uint16* ptrB = (uint16*)B.getDataPointer();
            Cp[i] = (ptrA[i] > ptrB[0]);
        } break;
        case NLS_INT16: {
            int16* ptrA = (int16*)A.getDataPointer();
            int16* ptrB = (int16*)B.getDataPointer();
            Cp[i] = (ptrA[i] > ptrB[0]);
        } break;
        case NLS_UINT32: {
            uint32* ptrA = (uint32*)A.getDataPointer();
            uint32* ptrB = (uint32*)B.getDataPointer();
            Cp[i] = (ptrA[i] > ptrB[0]);
        } break;
        case NLS_INT32: {
            int32* ptrA = (int32*)A.getDataPointer();
            int32* ptrB = (int32*)B.getDataPointer();
            Cp[i] = (ptrA[i] > ptrB[0]);
        } break;
        case NLS_UINT64: {
            uint64* ptrA = (uint64*)A.getDataPointer();
            uint64* ptrB = (uint64*)B.getDataPointer();
            Cp[i] = (ptrA[i] > ptrB[0]);
        } break;
        case NLS_INT64: {
            int64* ptrA = (int64*)A.getDataPointer();
            int64* ptrB = (int64*)B.getDataPointer();
            Cp[i] = (ptrA[i] > ptrB[0]);
        } break;
        case NLS_SINGLE: {
            single* ptrA = (single*)A.getDataPointer();
            single* ptrB = (single*)B.getDataPointer();
            Cp[i] = (ptrA[i] > ptrB[0]);
        } break;
        case NLS_DOUBLE: {
            double* ptrA = (double*)A.getDataPointer();
            double* ptrB = (double*)B.getDataPointer();
            Cp[i] = (ptrA[i] > ptrB[0]);
        } break;
        case NLS_SCOMPLEX: {
            single* ptrA = (single*)A.getDataPointer();
            single* ptrB = (single*)B.getDataPointer();
            Cp[i] = (complex_abs<single>(ptrA[2 * i], ptrA[2 * i + 1])
                > complex_abs<single>(ptrB[0], ptrB[1]));
        } break;
        case NLS_DCOMPLEX: {
            double* ptrA = (double*)A.getDataPointer();
            double* ptrB = (double*)B.getDataPointer();
            Cp[i] = (complex_abs<double>(ptrA[2 * i], ptrA[2 * i + 1])
                > complex_abs<double>(ptrB[0], ptrB[1]));
        } break;
        case NLS_CHAR: {
            charType* ptrA = (charType*)A.getDataPointer();
            charType* ptrB = (charType*)B.getDataPointer();
            Cp[i] = (ptrA[i] > ptrB[0]);
        } break;
        }
    }
    return ArrayOf(NLS_LOGICAL, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf
vector_row_column_greater_than(const Dimensions& outputDimensions, ArrayOf& A, ArrayOf& B)
{
    ArrayOf res;
    indexType Clen = outputDimensions.getElementCount();
    logical* Cp = new_with_exception<logical>(Clen, false);

    indexType m = 0;
    for (indexType i = 0; i < A.getDimensions().getElementCount(); i++) {
        for (indexType j = 0; j < B.getDimensions().getElementCount(); j++) {
            switch (A.getDataClass()) {
            case NLS_STRING_ARRAY: {
                ArrayOf* elementsA = (ArrayOf*)A.getDataPointer();
                ArrayOf* elementsB = (ArrayOf*)B.getDataPointer();
                if (elementsA[i].isCharacterArray() && elementsB[j].isCharacterArray()) {
                    Cp[m] = string_compare_is_greater_than(elementsA[i].getContentAsWideString(),
                        elementsB[j].getContentAsWideString());
                } else {
                    Cp[m] = logical(0);
                }
            } break;
            case NLS_LOGICAL: {
                logical* ptrA = (logical*)A.getDataPointer();
                logical* ptrB = (logical*)B.getDataPointer();
                Cp[m] = (ptrA[i] > ptrB[j]);
            } break;
            case NLS_UINT8: {
                uint8* ptrA = (uint8*)A.getDataPointer();
                uint8* ptrB = (uint8*)B.getDataPointer();
                Cp[m] = (ptrA[i] > ptrB[j]);
            } break;
            case NLS_INT8: {
                int8* ptrA = (int8*)A.getDataPointer();
                int8* ptrB = (int8*)B.getDataPointer();
                Cp[m] = (ptrA[i] > ptrB[j]);
            } break;
            case NLS_UINT16: {
                uint16* ptrA = (uint16*)A.getDataPointer();
                uint16* ptrB = (uint16*)B.getDataPointer();
                Cp[m] = (ptrA[i] > ptrB[j]);
            } break;
            case NLS_INT16: {
                int16* ptrA = (int16*)A.getDataPointer();
                int16* ptrB = (int16*)B.getDataPointer();
                Cp[m] = (ptrA[i] > ptrB[j]);
            } break;
            case NLS_UINT32: {
                uint32* ptrA = (uint32*)A.getDataPointer();
                uint32* ptrB = (uint32*)B.getDataPointer();
                Cp[m] = (ptrA[i] > ptrB[j]);
            } break;
            case NLS_INT32: {
                int32* ptrA = (int32*)A.getDataPointer();
                int32* ptrB = (int32*)B.getDataPointer();
                Cp[m] = (ptrA[i] > ptrB[j]);
            } break;
            case NLS_UINT64: {
                uint64* ptrA = (uint64*)A.getDataPointer();
                uint64* ptrB = (uint64*)B.getDataPointer();
                Cp[m] = (ptrA[i] > ptrB[j]);
            } break;
            case NLS_INT64: {
                int64* ptrA = (int64*)A.getDataPointer();
                int64* ptrB = (int64*)B.getDataPointer();
                Cp[m] = (ptrA[i] > ptrB[j]);
            } break;
            case NLS_SINGLE: {
                single* ptrA = (single*)A.getDataPointer();
                single* ptrB = (single*)B.getDataPointer();
                Cp[m] = (ptrA[i] > ptrB[j]);
            } break;
            case NLS_DOUBLE: {
                double* ptrA = (double*)A.getDataPointer();
                double* ptrB = (double*)B.getDataPointer();
                Cp[m] = (ptrA[i] > ptrB[j]);
            } break;
            case NLS_SCOMPLEX: {
                single* ptrA = (single*)A.getDataPointer();
                single* ptrB = (single*)B.getDataPointer();
                Cp[m] = (complex_abs<single>(ptrA[2 * i], ptrA[2 * i + 1])
                    > complex_abs<single>(ptrB[2 * j], ptrB[2 * j + 1]));
            } break;
            case NLS_DCOMPLEX: {
                double* ptrA = (double*)A.getDataPointer();
                double* ptrB = (double*)B.getDataPointer();
                Cp[m] = (complex_abs<double>(ptrA[2 * i], ptrA[2 * i + 1])
                    > complex_abs<double>(ptrB[2 * j], ptrB[2 * j + 1]));
            } break;
            case NLS_CHAR: {
                charType* ptrA = (charType*)A.getDataPointer();
                charType* ptrB = (charType*)B.getDataPointer();
                Cp[m] = (ptrA[i] > ptrB[j]);
            } break;
            }
            m++;
        }
    }
    return ArrayOf(NLS_LOGICAL, outputDimensions, Cp, false);
}
//=============================================================================
static ArrayOf
vector_column_row_greater_than(const Dimensions& outputDimensions, ArrayOf& A, ArrayOf& B)
{
    ArrayOf res;
    indexType Clen = outputDimensions.getElementCount();
    logical* Cp = new_with_exception<logical>(Clen, false);

    indexType m = 0;
    for (indexType i = 0; i < B.getDimensions().getElementCount(); i++) {
        for (indexType j = 0; j < A.getDimensions().getElementCount(); j++) {
            switch (A.getDataClass()) {
            case NLS_STRING_ARRAY: {
                ArrayOf* elementsA = (ArrayOf*)A.getDataPointer();
                ArrayOf* elementsB = (ArrayOf*)B.getDataPointer();
                if (elementsA[j].isCharacterArray() && elementsB[i].isCharacterArray()) {
                    Cp[m] = string_compare_is_greater_than(elementsA[j].getContentAsWideString(),
                        elementsB[i].getContentAsWideString());
                } else {
                    Cp[m] = logical(0);
                }
            } break;
            case NLS_LOGICAL: {
                logical* ptrA = (logical*)A.getDataPointer();
                logical* ptrB = (logical*)B.getDataPointer();
                Cp[m] = (ptrA[j] > ptrB[i]);
            } break;
            case NLS_UINT8: {
                uint8* ptrA = (uint8*)A.getDataPointer();
                uint8* ptrB = (uint8*)B.getDataPointer();
                Cp[m] = (ptrA[j] > ptrB[i]);
            } break;
            case NLS_INT8: {
                int8* ptrA = (int8*)A.getDataPointer();
                int8* ptrB = (int8*)B.getDataPointer();
                Cp[m] = (ptrA[j] > ptrB[i]);
            } break;
            case NLS_UINT16: {
                uint16* ptrA = (uint16*)A.getDataPointer();
                uint16* ptrB = (uint16*)B.getDataPointer();
                Cp[m] = (ptrA[j] > ptrB[i]);
            } break;
            case NLS_INT16: {
                int16* ptrA = (int16*)A.getDataPointer();
                int16* ptrB = (int16*)B.getDataPointer();
                Cp[m] = (ptrA[j] > ptrB[i]);
            } break;
            case NLS_UINT32: {
                uint32* ptrA = (uint32*)A.getDataPointer();
                uint32* ptrB = (uint32*)B.getDataPointer();
                Cp[m] = (ptrA[j] > ptrB[i]);
            } break;
            case NLS_INT32: {
                int32* ptrA = (int32*)A.getDataPointer();
                int32* ptrB = (int32*)B.getDataPointer();
                Cp[m] = (ptrA[j] > ptrB[i]);
            } break;
            case NLS_UINT64: {
                uint64* ptrA = (uint64*)A.getDataPointer();
                uint64* ptrB = (uint64*)B.getDataPointer();
                Cp[m] = (ptrA[j] > ptrB[i]);
            } break;
            case NLS_INT64: {
                int64* ptrA = (int64*)A.getDataPointer();
                int64* ptrB = (int64*)B.getDataPointer();
                Cp[m] = (ptrA[j] > ptrB[i]);
            } break;
            case NLS_SINGLE: {
                single* ptrA = (single*)A.getDataPointer();
                single* ptrB = (single*)B.getDataPointer();
                Cp[m] = (ptrA[j] > ptrB[i]);
            } break;
            case NLS_DOUBLE: {
                double* ptrA = (double*)A.getDataPointer();
                double* ptrB = (double*)B.getDataPointer();
                Cp[m] = (ptrA[j] > ptrB[i]);
            } break;
            case NLS_SCOMPLEX: {
                single* ptrA = (single*)A.getDataPointer();
                single* ptrB = (single*)B.getDataPointer();
                Cp[m] = (complex_abs<single>(ptrA[2 * j], ptrA[2 * j + 1])
                    > complex_abs<single>(ptrB[2 * i], ptrB[2 * i + 1]));
            } break;
            case NLS_DCOMPLEX: {
                double* ptrA = (double*)A.getDataPointer();
                double* ptrB = (double*)B.getDataPointer();
                Cp[m] = (complex_abs<double>(ptrA[2 * j], ptrA[2 * j + 1])
                    > complex_abs<double>(ptrB[2 * i], ptrB[2 * i + 1]));
            } break;
            case NLS_CHAR: {
                charType* ptrA = (charType*)A.getDataPointer();
                charType* ptrB = (charType*)B.getDataPointer();
                Cp[m] = (ptrA[j] > ptrB[i]);
            } break;
            }
            m++;
        }
    }
    return ArrayOf(NLS_LOGICAL, outputDimensions, Cp, false);
}
//=============================================================================
static ArrayOf
vector_matrix_greater_than(ArrayOf& A, ArrayOf& B)
{
    indexType q = 0;
    Dimensions dimsC = B.getDimensions();
    indexType Clen = dimsC.getElementCount();
    logical* Cp = new_with_exception<logical>(Clen, false);
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * A.getDimensions().getRows();
            switch (A.getDataClass()) {
            case NLS_STRING_ARRAY: {
                ArrayOf* elementsA = (ArrayOf*)A.getDataPointer();
                ArrayOf* elementsB = (ArrayOf*)B.getDataPointer();
                if (elementsA[m].isCharacterArray() && elementsB[q].isCharacterArray()) {
                    Cp[m] = string_compare_is_greater_than(elementsA[q].getContentAsWideString(),
                        elementsB[m].getContentAsWideString());
                } else {
                    Cp[i] = logical(0);
                }
            } break;
            case NLS_LOGICAL: {
                logical* ptrA = (logical*)A.getDataPointer();
                logical* ptrB = (logical*)B.getDataPointer();
                Cp[m] = (ptrA[q] > ptrB[m]);
            } break;
            case NLS_UINT8: {
                uint8* ptrA = (uint8*)A.getDataPointer();
                uint8* ptrB = (uint8*)B.getDataPointer();
                Cp[m] = (ptrA[q] > ptrB[m]);
            } break;
            case NLS_INT8: {
                int8* ptrA = (int8*)A.getDataPointer();
                int8* ptrB = (int8*)B.getDataPointer();
                Cp[m] = (ptrA[q] > ptrB[m]);
            } break;
            case NLS_UINT16: {
                uint16* ptrA = (uint16*)A.getDataPointer();
                uint16* ptrB = (uint16*)B.getDataPointer();
                Cp[m] = (ptrA[q] > ptrB[m]);
            } break;
            case NLS_INT16: {
                int16* ptrA = (int16*)A.getDataPointer();
                int16* ptrB = (int16*)B.getDataPointer();
                Cp[m] = (ptrA[q] > ptrB[m]);
            } break;
            case NLS_UINT32: {
                uint32* ptrA = (uint32*)A.getDataPointer();
                uint32* ptrB = (uint32*)B.getDataPointer();
                Cp[m] = (ptrA[q] > ptrB[m]);
            } break;
            case NLS_INT32: {
                int32* ptrA = (int32*)A.getDataPointer();
                int32* ptrB = (int32*)B.getDataPointer();
                Cp[m] = (ptrA[q] > ptrB[m]);
            } break;
            case NLS_UINT64: {
                uint64* ptrA = (uint64*)A.getDataPointer();
                uint64* ptrB = (uint64*)B.getDataPointer();
                Cp[m] = (ptrA[q] > ptrB[m]);
            } break;
            case NLS_INT64: {
                int64* ptrA = (int64*)A.getDataPointer();
                int64* ptrB = (int64*)B.getDataPointer();
                Cp[m] = (ptrA[q] > ptrB[m]);
            } break;
            case NLS_SINGLE: {
                single* ptrA = (single*)A.getDataPointer();
                single* ptrB = (single*)B.getDataPointer();
                Cp[m] = (ptrA[q] > ptrB[m]);
            } break;
            case NLS_DOUBLE: {
                double* ptrA = (double*)A.getDataPointer();
                double* ptrB = (double*)B.getDataPointer();
                Cp[m] = (ptrA[q] > ptrB[m]);
            } break;
            case NLS_SCOMPLEX: {
                single* ptrA = (single*)A.getDataPointer();
                single* ptrB = (single*)B.getDataPointer();
                Cp[m] = (complex_abs<single>(ptrA[2 * q], ptrA[2 * q + 1])
                    > complex_abs<single>(ptrB[2 * m], ptrB[2 * m + 1]));
            } break;
            case NLS_DCOMPLEX: {
                double* ptrA = (double*)A.getDataPointer();
                double* ptrB = (double*)B.getDataPointer();
                Cp[m] = (complex_abs<double>(ptrA[2 * q], ptrA[2 * q + 1])
                    > complex_abs<double>(ptrB[2 * m], ptrB[2 * m + 1]));
            } break;
            case NLS_CHAR: {
                charType* ptrA = (charType*)A.getDataPointer();
                charType* ptrB = (charType*)B.getDataPointer();
                Cp[m] = (ptrA[q] > ptrB[m]);
            } break;
            }
        }
        q++;
    }
    return ArrayOf(NLS_LOGICAL, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf
matrix_vector_greater_than(ArrayOf& A, ArrayOf& B)
{
    indexType q = 0;
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    logical* Cp = new_with_exception<logical>(Clen, false);
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * B.getDimensions().getRows();
            switch (A.getDataClass()) {
            case NLS_STRING_ARRAY: {
                ArrayOf* elementsA = (ArrayOf*)A.getDataPointer();
                ArrayOf* elementsB = (ArrayOf*)B.getDataPointer();
                if (elementsA[q].isCharacterArray() && elementsB[m].isCharacterArray()) {
                    Cp[m] = string_compare_is_greater_than(elementsA[q].getContentAsWideString(),
                        elementsB[m].getContentAsWideString());
                } else {
                    Cp[i] = logical(0);
                }
            } break;
            case NLS_LOGICAL: {
                logical* ptrA = (logical*)A.getDataPointer();
                logical* ptrB = (logical*)B.getDataPointer();
                Cp[m] = (ptrA[q] > ptrB[m]);
            } break;
            case NLS_UINT8: {
                uint8* ptrA = (uint8*)A.getDataPointer();
                uint8* ptrB = (uint8*)B.getDataPointer();
                Cp[m] = (ptrA[q] > ptrB[m]);
            } break;
            case NLS_INT8: {
                int8* ptrA = (int8*)A.getDataPointer();
                int8* ptrB = (int8*)B.getDataPointer();
                Cp[m] = (ptrA[q] > ptrB[m]);
            } break;
            case NLS_UINT16: {
                uint16* ptrA = (uint16*)A.getDataPointer();
                uint16* ptrB = (uint16*)B.getDataPointer();
                Cp[m] = (ptrA[q] > ptrB[m]);
            } break;
            case NLS_INT16: {
                int16* ptrA = (int16*)A.getDataPointer();
                int16* ptrB = (int16*)B.getDataPointer();
                Cp[m] = (ptrA[q] > ptrB[m]);
            } break;
            case NLS_UINT32: {
                uint32* ptrA = (uint32*)A.getDataPointer();
                uint32* ptrB = (uint32*)B.getDataPointer();
                Cp[m] = (ptrA[q] > ptrB[m]);
            } break;
            case NLS_INT32: {
                int32* ptrA = (int32*)A.getDataPointer();
                int32* ptrB = (int32*)B.getDataPointer();
                Cp[m] = (ptrA[q] > ptrB[m]);
            } break;
            case NLS_UINT64: {
                uint64* ptrA = (uint64*)A.getDataPointer();
                uint64* ptrB = (uint64*)B.getDataPointer();
                Cp[m] = (ptrA[q] > ptrB[m]);
            } break;
            case NLS_INT64: {
                int64* ptrA = (int64*)A.getDataPointer();
                int64* ptrB = (int64*)B.getDataPointer();
                Cp[m] = (ptrA[q] > ptrB[m]);
            } break;
            case NLS_SINGLE: {
                single* ptrA = (single*)A.getDataPointer();
                single* ptrB = (single*)B.getDataPointer();
                Cp[m] = (ptrA[q] > ptrB[m]);
            } break;
            case NLS_DOUBLE: {
                double* ptrA = (double*)A.getDataPointer();
                double* ptrB = (double*)B.getDataPointer();
                Cp[m] = (ptrA[q] > ptrB[m]);
            } break;
            case NLS_SCOMPLEX: {
                single* ptrA = (single*)A.getDataPointer();
                single* ptrB = (single*)B.getDataPointer();
                Cp[m] = (complex_abs<single>(ptrA[2 * q], ptrA[2 * q + 1])
                    > complex_abs<single>(ptrB[2 * m], ptrB[2 * m + 1]));
            } break;
            case NLS_DCOMPLEX: {
                double* ptrA = (double*)A.getDataPointer();
                double* ptrB = (double*)B.getDataPointer();
                Cp[m] = (complex_abs<double>(ptrA[2 * q], ptrA[2 * q + 1])
                    > complex_abs<double>(ptrB[2 * m], ptrB[2 * m + 1]));
            } break;
            case NLS_CHAR: {
                charType* ptrA = (charType*)A.getDataPointer();
                charType* ptrB = (charType*)B.getDataPointer();
                Cp[m] = (ptrA[q] > ptrB[m]);
            } break;
            }
        }
        q++;
    }
    return ArrayOf(NLS_LOGICAL, dimsC, Cp, false);
}
//=============================================================================
ArrayOf
GreaterThan(ArrayOf& A, ArrayOf& B, bool& needToOverload)
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
                ArrayOf res = ArrayOf::emptyConstructor(B.getDimensions());
                res.promoteType(NLS_LOGICAL);
                return res;
            } else {
                ArrayOf res = ArrayOf::emptyConstructor(A.getDimensions());
                res.promoteType(NLS_LOGICAL);
                return res;
            }
        } else {
            if (!(SameSizeCheck(dimsA, dimsB))) {
                Error(_W("Size mismatch on arguments to arithmetic operator ") + L">");
            }
            ArrayOf res = ArrayOf::emptyConstructor(B.getDimensions());
            res.promoteType(NLS_LOGICAL);
            return res;
        }
    }
    if (SameSizeCheck(dimsA, dimsB)) {
        return matrix_matrix_greater_than(A, B);
    } else {
        if (A.isScalar() || B.isScalar()) {
            if (A.isScalar()) {
                return scalar_matrix_greater_than(A, B);
            } else {
                return matrix_scalar_greater_than(A, B);
            }
        } else {
            if (A.isVector() || B.isVector()) {
                if (A.isRowVector() && B.isColumnVector()) {
                    dimsC = Dimensions(std::min(dimsA.getMax(), dimsB.getMax()),
                        std::max(dimsA.getMax(), dimsB.getMax()));
                    return vector_row_column_greater_than(dimsC, A, B);
                } else if (A.isColumnVector() && B.isRowVector()) {
                    dimsC = Dimensions(std::min(dimsA.getMax(), dimsB.getMax()),
                        std::max(dimsA.getMax(), dimsB.getMax()));
                    return vector_column_row_greater_than(dimsC, A, B);
                } else if ((A.isRowVector() && B.isRowVector())
                    || (A.isColumnVector() && B.isColumnVector())) {
                    Error(_W("Size mismatch on arguments to arithmetic operator ") + L">");
                } else {
                    if ((dimsA[0] == dimsB[0]) && (dimsA[0] != 1)) {
                        if (A.isVector()) {
                            return vector_matrix_greater_than(A, B);
                        } else {
                            return matrix_vector_greater_than(A, B);
                        }
                    } else {
                        Error(_W("Size mismatch on arguments to arithmetic operator ") + L">");
                    }
                }
            } else {
                Error(_W("Size mismatch on arguments to arithmetic operator ") + L">");
            }
        }
    }
    needToOverload = true;
    return ArrayOf();
}
//=============================================================================
}
//=============================================================================
