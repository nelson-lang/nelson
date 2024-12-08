//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StringJoin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
StringJoinCharacters(const ArrayOf& A, const ArrayOf& delimiters, size_t dimension);
//=============================================================================
static ArrayOf
StringJoinCellCharacters(const ArrayOf& A, const ArrayOf& delimiters, size_t dimension);
static ArrayOf
StringJoinCellCharactersScalarDelimiter(
    const ArrayOf& A, const ArrayOf& delimiter, size_t dimension);
static ArrayOf
StringJoinCellCharactersRowVectorDelimiter(
    const ArrayOf& A, const ArrayOf& delimiters, size_t dimension);
static ArrayOf
StringJoinCellCharactersColumnVectorDelimiter(
    const ArrayOf& A, const ArrayOf& delimiters, size_t dimension);
static ArrayOf
StringJoinCellCharactersMatrixDelimiter(
    const ArrayOf& A, const ArrayOf& delimiters, size_t dimension);
//=============================================================================
static ArrayOf
StringJoinStringArray(const ArrayOf& A, const ArrayOf& delimiters, size_t dimension);
static ArrayOf
StringJoinStringArrayScalarDelimiter(const ArrayOf& A, const ArrayOf& delimiter, size_t dimension);
static ArrayOf
StringJoinStringArrayRowVectorDelimiter(
    const ArrayOf& A, const ArrayOf& delimiters, size_t dimension);
static ArrayOf
StringJoinStringArrayColumnVectorDelimiter(
    const ArrayOf& A, const ArrayOf& delimiters, size_t dimension);
static ArrayOf
StringJoinStringArrayMatrixDelimiter(const ArrayOf& A, const ArrayOf& delimiters, size_t dimension);
//=============================================================================
static bool
validateDelimiterDimensions(const ArrayOf& input, const ArrayOf& delimiters, size_t dimension);
//=============================================================================
ArrayOf
StringJoin(const ArrayOf& A, const ArrayOf& delimiters, size_t dimension)
{
    if ((dimension > 2 || dimension < 1)
        || !validateDelimiterDimensions(A, delimiters, dimension)) {
        Error(_W("Invalid delimiter dimensions."));
    }
    if (!delimiters.isStringArray()) {
        Error(_W("Invalid delimiter type."));
    }
    switch (A.getDataClass()) {
    case NLS_CHAR: {
        return StringJoinCharacters(A, delimiters, dimension);
    } break;
    case NLS_CELL_ARRAY: {
        return StringJoinCellCharacters(A, delimiters, dimension);
    } break;
    case NLS_STRING_ARRAY: {
        return StringJoinStringArray(A, delimiters, dimension);
    } break;
    default: {
        Error(_W("Type not supported."));
    } break;
    }
    return {};
}
//=============================================================================
bool
validateDelimiterDimensions(const ArrayOf& input, const ArrayOf& delimiters, size_t dimension)
{
    Dimensions inputDims = input.getDimensions();
    Dimensions delimDims = delimiters.getDimensions();

    // Case 1: Scalar delimiter
    if (delimiters.isScalar()) {
        return true;
    }

    // Case 2: Vector delimiter
    if (delimiters.isRowVector()) {
        if (dimension == 1) {
            return (delimDims.getColumns() == inputDims.getRows() - 1);
        }
        if (dimension == 2) {
            return (delimDims.getColumns() == inputDims.getColumns() - 1);
        }
    }
    if (delimiters.isColumnVector()) {
        if (dimension == 1) {
            return (delimDims.getRows() == inputDims.getRows() - 1);
        }
        if (dimension == 2) {
            return delimDims.getColumns() == inputDims.getColumns() - 1;
        }
    }

    // Case 3: Matrix delimiter
    if (!delimiters.isVector()) {
        if (dimension == 1) {
            return (delimDims.getRows() == inputDims.getRows() - 1
                && delimDims.getColumns() == inputDims.getColumns());
        }
        if (dimension == 2) {
            return (delimDims.getRows() == inputDims.getRows()
                && delimDims.getColumns() == inputDims.getColumns() - 1);
        }
    }

    return false;
}
//=============================================================================
ArrayOf
StringJoinCharacters(const ArrayOf& A, const ArrayOf& delimiters, size_t dimension)
{
    if (A.isEmpty()) {
        return ArrayOf::characterArrayConstructor("");
    }
    std::wstring strdelimiter;
    if ((delimiters.isCellArrayOfCharacterVectors() && delimiters.isScalar())
        || delimiters.isScalarStringArray() || delimiters.isRowVectorCharacterArray()) {
        strdelimiter = delimiters.getContentAsWideString();
    } else {
        Error(_W("Invalid delimiter dimensions."));
    }
    return A;
}
//=============================================================================
ArrayOf
StringJoinCellCharacters(const ArrayOf& A, const ArrayOf& delimiters, size_t dimension)
{
    if (A.isEmpty()) {
        Dimensions dims(0, 1);
        return ArrayOf::emptyCell(dims);
    }
    if (delimiters.isScalar()) {
        return StringJoinCellCharactersScalarDelimiter(A, delimiters, dimension);
    }
    if (delimiters.isVector()) {
        if (delimiters.isRowVector()) {
            return StringJoinCellCharactersRowVectorDelimiter(A, delimiters, dimension);
        }
        return StringJoinCellCharactersColumnVectorDelimiter(A, delimiters, dimension);
    }
    return StringJoinCellCharactersMatrixDelimiter(A, delimiters, dimension);
}
//=============================================================================
ArrayOf
StringJoinStringArrayScalarDelimiter(const ArrayOf& A, const ArrayOf& delimiter, size_t dimension)
{
    ArrayOf* strs = (ArrayOf*)A.getDataPointer();
    ArrayOf* delim = (ArrayOf*)delimiter.getDataPointer();
    size_t nbRows = A.getRows();
    size_t nbCols = A.getColumns();

    if (dimension == 1) {
        // Join by rows - output will have nbCols columns
        Dimensions dims(1, nbCols);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, nbCols);
        ArrayOf strArray = ArrayOf(NLS_STRING_ARRAY, dims, elements);

        // Process each column
#ifdef WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType c = 0; c < (ompIndexType)nbCols; c++) {
            std::wstring joined;
            bool isMissing = false;
            // Join rows within this column
            for (size_t r = 0; r < nbRows && !isMissing; r++) {
                if (r > 0) {
                    if (delim[0].isRowVectorCharacterArray()) {
                        joined += delim[0].getContentAsWideString();
                    } else {
                        isMissing = true;
                    }
                }
                isMissing = isMissing || !strs[r + c * nbRows].isRowVectorCharacterArray();
                if (!isMissing) {
                    joined += strs[r + c * nbRows].getContentAsWideString();
                }
            }
            elements[c] = isMissing ? ArrayOf::doubleConstructor(std::nan("NaN"))
                                    : ArrayOf::characterArrayConstructor(joined);
        }
        return strArray;
    } else {
        // Join by columns - output will have nbRows rows
        Dimensions dims(nbRows, 1);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, nbRows);
        ArrayOf strArray = ArrayOf(NLS_STRING_ARRAY, dims, elements);

        // Process each row
#ifdef WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType r = 0; r < (ompIndexType)nbRows; r++) {
            std::wstring joined;
            bool isMissing = false;
            // Join columns within this row
            for (size_t c = 0; c < nbCols && !isMissing; c++) {
                if (c > 0) {
                    if (delim[0].isRowVectorCharacterArray()) {
                        joined += delim[0].getContentAsWideString();
                    } else {
                        isMissing = true;
                    }
                }
                isMissing = isMissing || !strs[r + c * nbRows].isRowVectorCharacterArray();
                if (!isMissing) {
                    joined += strs[r + c * nbRows].getContentAsWideString();
                }
            }
            elements[r] = isMissing ? ArrayOf::doubleConstructor(std::nan("NaN"))
                                    : ArrayOf::characterArrayConstructor(joined);
        }
        return strArray;
    }
}
//=============================================================================
ArrayOf
StringJoinStringArrayRowVectorDelimiter(
    const ArrayOf& A, const ArrayOf& delimiters, size_t dimension)
{
    ArrayOf* strs = (ArrayOf*)A.getDataPointer();
    ArrayOf* delim = (ArrayOf*)delimiters.getDataPointer();
    size_t nbRows = A.getRows();
    size_t nbCols = A.getColumns();

    if (dimension == 1) {
        // Join by rows - output will have nbCols columns
        Dimensions dims(1, nbCols);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, nbCols);
        ArrayOf strArray = ArrayOf(NLS_STRING_ARRAY, dims, elements);

        // Process each column
#ifdef WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType c = 0; c < (ompIndexType)nbCols; c++) {
            std::wstring joined;
            bool isMissing = false;
            // Join rows within this column
            for (size_t r = 0; r < nbRows && !isMissing; r++) {
                if (r > 0) {
                    if (delim[r - 1].isRowVectorCharacterArray()) {
                        joined += delim[r - 1].getContentAsWideString();
                    } else {
                        isMissing = true;
                    }
                }
                isMissing = isMissing || !strs[r + c * nbRows].isRowVectorCharacterArray();
                if (!isMissing) {
                    joined += strs[r + c * nbRows].getContentAsWideString();
                }
            }
            elements[c] = isMissing ? ArrayOf::doubleConstructor(std::nan("NaN"))
                                    : ArrayOf::characterArrayConstructor(joined);
        }
        return strArray;
    } else /* dimension == 2 */ {
        // Join by columns - output will have nbRows rows
        Dimensions dims(nbRows, 1);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, nbRows);
        ArrayOf strArray = ArrayOf(NLS_STRING_ARRAY, dims, elements);

        // Process each row
#ifdef WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType r = 0; r < (ompIndexType)nbRows; r++) {
            std::wstring joined;
            bool isMissing = false;
            // Join columns within this row
            for (size_t c = 0; c < nbCols && !isMissing; c++) {
                if (c > 0) {
                    if (delim[c - 1].isRowVectorCharacterArray()) {
                        joined += delim[c - 1].getContentAsWideString();
                    } else {
                        isMissing = true;
                    }
                }
                isMissing = isMissing || !strs[r + c * nbRows].isRowVectorCharacterArray();
                if (!isMissing) {
                    joined += strs[r + c * nbRows].getContentAsWideString();
                }
            }
            elements[r] = isMissing ? ArrayOf::doubleConstructor(std::nan("NaN"))
                                    : ArrayOf::characterArrayConstructor(joined);
        }
        return strArray;
    }
    return {};
}
//=============================================================================
ArrayOf
StringJoinStringArrayColumnVectorDelimiter(
    const ArrayOf& A, const ArrayOf& delimiters, size_t dimension)
{
    ArrayOf* strs = (ArrayOf*)A.getDataPointer();
    ArrayOf* delim = (ArrayOf*)delimiters.getDataPointer();
    size_t nbRows = A.getRows();
    size_t nbCols = A.getColumns();

    if (dimension == 1) {
        // Join by rows - output will have nbCols columns
        Dimensions dims(1, nbCols);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, nbCols);
        ArrayOf strArray = ArrayOf(NLS_STRING_ARRAY, dims, elements);
#ifdef WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType c = 0; c < (ompIndexType)nbCols; c++) {
            std::wstring joined;
            bool isMissing = false;
            for (size_t r = 0; r < nbRows && !isMissing; r++) {
                if (r > 0) {
                    if (delim[r - 1].isRowVectorCharacterArray()) {
                        joined += delim[r - 1].getContentAsWideString();
                    } else {
                        isMissing = true;
                    }
                }
                isMissing = isMissing || !strs[r + c * nbRows].isRowVectorCharacterArray();
                if (!isMissing) {
                    joined += strs[r + c * nbRows].getContentAsWideString();
                }
            }
            elements[c] = isMissing ? ArrayOf::doubleConstructor(std::nan("NaN"))
                                    : ArrayOf::characterArrayConstructor(joined);
        }
        return strArray;

    } else /* dimension == 2 */ {
        // Join by columns - output will have nbRows rows
        Dimensions dims(nbRows, 1);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, nbRows);
        ArrayOf strArray = ArrayOf(NLS_STRING_ARRAY, dims, elements);
#ifdef WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType r = 0; r < (ompIndexType)nbRows; r++) {
            std::wstring joined;
            bool isMissing = false;
            for (size_t c = 0; c < nbCols && !isMissing; c++) {
                if (c > 0) {
                    if (delim[c - 1].isRowVectorCharacterArray()) {
                        joined += delim[c - 1].getContentAsWideString();
                    } else {
                        isMissing = true;
                    }
                }
                isMissing = isMissing || !strs[r + c * nbRows].isRowVectorCharacterArray();
                if (!isMissing) {
                    joined += strs[r + c * nbRows].getContentAsWideString();
                }
            }
            elements[r] = isMissing ? ArrayOf::doubleConstructor(std::nan("NaN"))
                                    : ArrayOf::characterArrayConstructor(joined);
        }
        return strArray;
    }
}
//=============================================================================
ArrayOf
StringJoinStringArrayMatrixDelimiter(const ArrayOf& A, const ArrayOf& delimiters, size_t dimension)
{
    ArrayOf* strs = (ArrayOf*)A.getDataPointer();
    ArrayOf* delims = (ArrayOf*)delimiters.getDataPointer();
    size_t nbRows = A.getRows();
    size_t nbCols = A.getColumns();

    if (dimension == 1) {
        // Join by rows - output will have nbCols columns
        Dimensions dims(1, nbCols);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, nbCols);
        ArrayOf strArray = ArrayOf(NLS_STRING_ARRAY, dims, elements);

#ifdef WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType c = 0; c < (ompIndexType)nbCols; c++) {
            std::wstring joined;
            bool isMissing = false;
            for (size_t r = 0; r < nbRows && !isMissing; r++) {
                if (r > 0) {
                    if (delims[(r - 1) + c * (nbRows - 1)].isRowVectorCharacterArray()) {
                        joined += delims[(r - 1) + c * (nbRows - 1)].getContentAsWideString();
                    } else {
                        isMissing = true;
                    }
                }
                isMissing = isMissing || !strs[r + c * nbRows].isRowVectorCharacterArray();
                if (!isMissing) {
                    joined += strs[r + c * nbRows].getContentAsWideString();
                }
            }
            elements[c] = isMissing ? ArrayOf::doubleConstructor(std::nan("NaN"))
                                    : ArrayOf::characterArrayConstructor(joined);
        }
        return strArray;

    } else /* dimension == 2 */ {
        // Join by columns - output will have nbRows rows
        Dimensions dims(nbRows, 1);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, nbRows);
        ArrayOf strArray = ArrayOf(NLS_STRING_ARRAY, dims, elements);
#ifdef WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType r = 0; r < (ompIndexType)nbRows; r++) {
            std::wstring joined;
            bool isMissing = false;
            for (size_t c = 0; c < nbCols && !isMissing; c++) {
                if (c > 0) {
                    if (delims[r + (c - 1) * nbRows].isRowVectorCharacterArray()) {
                        joined += delims[r + (c - 1) * nbRows].getContentAsWideString();

                    } else {
                        isMissing = true;
                    }
                }
                isMissing = !isMissing && !strs[r + c * nbRows].isRowVectorCharacterArray();
                if (!isMissing) {
                    joined += strs[r + c * nbRows].getContentAsWideString();
                }
            }
            elements[r] = isMissing ? ArrayOf::doubleConstructor(std::nan("NaN"))
                                    : ArrayOf::characterArrayConstructor(joined);
        }
        return strArray;
    }
}
//=============================================================================
ArrayOf
StringJoinStringArray(const ArrayOf& A, const ArrayOf& delimiters, size_t dimension)
{
    if (A.isEmpty()) {
        Dimensions dims(0, 1);
        return ArrayOf::stringArrayConstructor(wstringVector(), dims);
    }
    if (delimiters.isScalar()) {
        return StringJoinStringArrayScalarDelimiter(A, delimiters, dimension);
    }
    if (delimiters.isVector()) {
        if (delimiters.isRowVector()) {
            return StringJoinStringArrayRowVectorDelimiter(A, delimiters, dimension);
        }
        return StringJoinStringArrayColumnVectorDelimiter(A, delimiters, dimension);
    }
    return StringJoinStringArrayMatrixDelimiter(A, delimiters, dimension);
}
//=============================================================================
ArrayOf
StringJoinCellCharactersScalarDelimiter(
    const ArrayOf& A, const ArrayOf& delimiter, size_t dimension)
{
    ArrayOf* strs = (ArrayOf*)A.getDataPointer();
    ArrayOf* delim = (ArrayOf*)delimiter.getDataPointer();
    size_t nbRows = A.getRows();
    size_t nbCols = A.getColumns();

    if (dimension == 1) {
        // Join by rows - output will have nbCols columns
        Dimensions dims(1, nbCols);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbCols);
        ArrayOf strArray = ArrayOf(NLS_CELL_ARRAY, dims, elements);

        // Process each column
#ifdef WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType c = 0; c < (ompIndexType)nbCols; c++) {
            std::wstring joined;
            bool isMissing = false;
            // Join rows within this column
            for (size_t r = 0; r < nbRows && !isMissing; r++) {
                if (r > 0) {
                    if (delim[0].isRowVectorCharacterArray()) {
                        joined += delim[0].getContentAsWideString();
                    } else {
                        isMissing = true;
                        joined = L"";
                    }
                }
                if (isMissing) {
                    joined = L"";
                } else {
                    joined += strs[r + c * nbRows].getContentAsWideString();
                }
            }
            elements[c] = ArrayOf::characterArrayConstructor(joined);
        }
        return strArray;
    } else {
        // Join by columns - output will have nbRows rows
        Dimensions dims(nbRows, 1);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbRows);
        ArrayOf strArray = ArrayOf(NLS_CELL_ARRAY, dims, elements);

        // Process each row
#ifdef WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType r = 0; r < (ompIndexType)nbRows; r++) {
            std::wstring joined;
            bool isMissing = false;
            // Join columns within this row
            for (size_t c = 0; c < nbCols && !isMissing; c++) {
                if (c > 0) {
                    if (delim[0].isRowVectorCharacterArray()) {
                        joined += delim[0].getContentAsWideString();
                    } else {
                        isMissing = true;
                    }
                }
                if (!isMissing) {
                    joined += strs[r + c * nbRows].getContentAsWideString();
                } else {
                    joined = L"";
                }
            }
            elements[r] = ArrayOf::characterArrayConstructor(joined);
        }
        return strArray;
    }
}
//=============================================================================
ArrayOf
StringJoinCellCharactersRowVectorDelimiter(
    const ArrayOf& A, const ArrayOf& delimiters, size_t dimension)
{
    ArrayOf* strs = (ArrayOf*)A.getDataPointer();
    ArrayOf* delim = (ArrayOf*)delimiters.getDataPointer();
    size_t nbRows = A.getRows();
    size_t nbCols = A.getColumns();

    if (dimension == 1) {
        // Join by rows - output will have nbCols columns
        Dimensions dims(1, nbCols);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbCols);
        ArrayOf strArray = ArrayOf(NLS_CELL_ARRAY, dims, elements);

        // Process each column
#ifdef WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType c = 0; c < (ompIndexType)nbCols; c++) {
            std::wstring joined;
            bool isMissing = false;
            // Join rows within this column
            for (size_t r = 0; r < nbRows && !isMissing; r++) {
                if (r > 0) {
                    if (delim[r - 1].isRowVectorCharacterArray()) {
                        joined += delim[r - 1].getContentAsWideString();
                    } else {
                        joined = L"";
                        isMissing = true;
                    }
                }
                if (!isMissing) {
                    joined += strs[r + c * nbRows].getContentAsWideString();
                } else {
                    joined = L"";
                }
            }
            elements[c] = ArrayOf::characterArrayConstructor(joined);
        }
        return strArray;
    } else /* dimension == 2 */ {
        // Join by columns - output will have nbRows rows
        Dimensions dims(nbRows, 1);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbRows);
        ArrayOf strArray = ArrayOf(NLS_CELL_ARRAY, dims, elements);

        // Process each row
#ifdef WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType r = 0; r < (ompIndexType)nbRows; r++) {
            std::wstring joined;
            bool isMissing = false;
            // Join columns within this row
            for (size_t c = 0; c < nbCols && !isMissing; c++) {
                if (c > 0) {
                    if (delim[c - 1].isRowVectorCharacterArray()) {
                        joined += delim[c - 1].getContentAsWideString();
                    } else {
                        joined = L"";
                    }
                }
                if (!isMissing) {
                    joined += strs[r + c * nbRows].getContentAsWideString();
                } else {
                    joined = L"";
                }
            }
            elements[r] = ArrayOf::characterArrayConstructor(joined);
        }
        return strArray;
    }
    return {};
}
//=============================================================================
ArrayOf
StringJoinCellCharactersColumnVectorDelimiter(
    const ArrayOf& A, const ArrayOf& delimiters, size_t dimension)
{
    ArrayOf* strs = (ArrayOf*)A.getDataPointer();
    ArrayOf* delim = (ArrayOf*)delimiters.getDataPointer();
    size_t nbRows = A.getRows();
    size_t nbCols = A.getColumns();

    if (dimension == 1) {
        // Join by rows - output will have nbCols columns
        Dimensions dims(1, nbCols);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbCols);
        ArrayOf strArray = ArrayOf(NLS_CELL_ARRAY, dims, elements);

#ifdef WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType c = 0; c < (ompIndexType)nbCols; c++) {
            std::wstring joined;
            bool isMissing = false;
            for (size_t r = 0; r < nbRows && !isMissing; r++) {
                if (r > 0) {
                    if (delim[r - 1].isRowVectorCharacterArray()) {
                        joined += delim[r - 1].getContentAsWideString();
                    } else {
                        isMissing = true;
                        joined = L"";
                    }
                }
                if (!isMissing) {
                    joined += strs[r + c * nbRows].getContentAsWideString();
                } else {
                    joined = L"";
                }
            }
            elements[c] = ArrayOf::characterArrayConstructor(joined);
        }
        return strArray;
    } else /* dimension == 2 */ {
        // Join by columns - output will have nbRows rows
        Dimensions dims(nbRows, 1);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbRows);
        ArrayOf strArray = ArrayOf(NLS_CELL_ARRAY, dims, elements);
#ifdef WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType r = 0; r < (ompIndexType)nbRows; r++) {
            std::wstring joined;
            bool isMissing = false;
            for (size_t c = 0; c < nbCols && !isMissing; c++) {
                if (c > 0) {
                    if (delim[c - 1].isRowVectorCharacterArray()) {
                        joined += delim[c - 1].getContentAsWideString();
                        isMissing = true;
                    } else {
                        joined = L"";
                    }
                }
                if (!isMissing) {
                    joined += strs[r + c * nbRows].getContentAsWideString();
                } else {
                    joined = L"";
                }
            }
            elements[r] = ArrayOf::characterArrayConstructor(joined);
        }
        return strArray;
    }
}
//=============================================================================
ArrayOf
StringJoinCellCharactersMatrixDelimiter(
    const ArrayOf& A, const ArrayOf& delimiters, size_t dimension)
{
    ArrayOf* strs = (ArrayOf*)A.getDataPointer();
    ArrayOf* delims = (ArrayOf*)delimiters.getDataPointer();
    size_t nbRows = A.getRows();
    size_t nbCols = A.getColumns();

    if (dimension == 1) {
        // Join by rows - output will have nbCols columns
        Dimensions dims(1, nbCols);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbCols);
        ArrayOf strArray = ArrayOf(NLS_CELL_ARRAY, dims, elements);
#ifdef WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType c = 0; c < (ompIndexType)nbCols; c++) {
            std::wstring joined;
            bool isMissing = false;
            for (size_t r = 0; r < nbRows && !isMissing; r++) {
                if (r > 0) {
                    if (delims[(r - 1) + c * (nbRows - 1)].isRowVectorCharacterArray()) {
                        joined += delims[(r - 1) + c * (nbRows - 1)].getContentAsWideString();
                    } else {
                        isMissing = true;
                        joined = L"";
                    }
                }
                if (!isMissing) {
                    joined += strs[r + c * nbRows].getContentAsWideString();
                } else {
                    joined = L"";
                }
            }
            elements[c] = ArrayOf::characterArrayConstructor(joined);
        }
        return strArray;

    } else /* dimension == 2 */ {
        Dimensions dims(nbRows, 1);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbRows);
        ArrayOf strArray = ArrayOf(NLS_CELL_ARRAY, dims, elements);
#ifdef WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType r = 0; r < (ompIndexType)nbRows; r++) {
            std::wstring joined;
            bool isMissing = false;
            for (size_t c = 0; c < nbCols && !isMissing; c++) {
                if (c > 0) {
                    if (delims[r + (c - 1) * nbRows].isRowVectorCharacterArray()) {
                        joined += delims[r + (c - 1) * nbRows].getContentAsWideString();
                    } else {
                        isMissing = true;
                        joined = L"";
                    }
                }
                if (!isMissing) {
                    joined += strs[r + c * nbRows].getContentAsWideString();
                } else {
                    joined = L"";
                }
            }
            elements[r] = ArrayOf::characterArrayConstructor(joined);
        }
        return strArray;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
