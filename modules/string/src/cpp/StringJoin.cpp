//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StringJoin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
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
        Error(L"Nelson:string:invalidDelimiterDimensions", L"Invalid delimiter dimensions.");
    }
    if (!delimiters.isStringArray()) {
        Error(L"Nelson:string:invalidDelimiterType", L"Invalid delimiter type.");
    }
    switch (A.dataClass()) {
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
        Error(L"Nelson:string:typeSupported", L"Type not supported.");
    } break;
    }
    return {};
}
//=============================================================================
bool
validateDelimiterDimensions(const ArrayOf& input, const ArrayOf& delimiters, size_t dimension)
{
    Dimensions inputDims = input.dimensions();
    Dimensions delimDims = delimiters.dimensions();

    if (delimiters.isScalar()) {
        return true;
    }

    if (delimiters.isRowVector()) {
        if (dimension == 1) {
            return (delimDims.columns() == inputDims.rows() - 1);
        }
        if (dimension == 2) {
            return (delimDims.columns() == inputDims.columns() - 1);
        }
    }
    if (delimiters.isColumnVector()) {
        if (dimension == 1) {
            return (delimDims.rows() == inputDims.rows() - 1);
        }
        if (dimension == 2) {
            return delimDims.columns() == inputDims.columns() - 1;
        }
    }

    if (!delimiters.isVector()) {
        if (dimension == 1) {
            return (delimDims.rows() == inputDims.rows() - 1
                && delimDims.columns() == inputDims.columns());
        }
        if (dimension == 2) {
            return (delimDims.rows() == inputDims.rows()
                && delimDims.columns() == inputDims.columns() - 1);
        }
    }

    return false;
}
//=============================================================================
ArrayOf
StringJoinCharacters(const ArrayOf& A, const ArrayOf& delimiters, size_t dimension)
{
    if (A.isEmpty()) {
        return ArrayOf::characterArray("");
    }
    std::wstring strdelimiter;
    if ((delimiters.isCellArrayOfCharacterVectors() && delimiters.isScalar())
        || delimiters.isScalarStringArray() || delimiters.isRowVectorCharacterArray()) {
        strdelimiter = delimiters.asWideString();
    } else {
        Error(L"Nelson:string:invalidDelimiterDimensions", L"Invalid delimiter dimensions.");
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
    ArrayOf* strs = (ArrayOf*)A.dataPointer();
    ArrayOf* delim = (ArrayOf*)delimiter.dataPointer();
    size_t nbRows = A.rows();
    size_t nbCols = A.columns();

    if (dimension == 1) {
        Dimensions dims(1, nbCols);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, nbCols);
        ArrayOf strArray = ArrayOf(NLS_STRING_ARRAY, dims, elements);

        OMP_PARALLEL_FOR_LOOP(nbCols)
        for (ompIndexType c = 0; c < (ompIndexType)nbCols; c++) {
            std::wstring joined;
            bool isMissing = false;
            for (size_t r = 0; r < nbRows && !isMissing; r++) {
                if (r > 0) {
                    if (delim[0].isRowVectorCharacterArray()) {
                        joined += delim[0].asWideString();
                    } else {
                        isMissing = true;
                    }
                }
                isMissing = isMissing || !strs[r + c * nbRows].isRowVectorCharacterArray();
                if (!isMissing) {
                    joined += strs[r + c * nbRows].asWideString();
                }
            }
            elements[c]
                = isMissing ? ArrayOf::doubleScalar(std::nan("")) : ArrayOf::characterArray(joined);
        }
        return strArray;
    } else {
        Dimensions dims(nbRows, 1);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, nbRows);
        ArrayOf strArray = ArrayOf(NLS_STRING_ARRAY, dims, elements);

        OMP_PARALLEL_FOR_LOOP(nbRows)
        for (ompIndexType r = 0; r < (ompIndexType)nbRows; r++) {
            std::wstring joined;
            bool isMissing = false;
            for (size_t c = 0; c < nbCols && !isMissing; c++) {
                if (c > 0) {
                    if (delim[0].isRowVectorCharacterArray()) {
                        joined += delim[0].asWideString();
                    } else {
                        isMissing = true;
                    }
                }
                isMissing = isMissing || !strs[r + c * nbRows].isRowVectorCharacterArray();
                if (!isMissing) {
                    joined += strs[r + c * nbRows].asWideString();
                }
            }
            elements[r]
                = isMissing ? ArrayOf::doubleScalar(std::nan("")) : ArrayOf::characterArray(joined);
        }
        return strArray;
    }
}
//=============================================================================
ArrayOf
StringJoinStringArrayRowVectorDelimiter(
    const ArrayOf& A, const ArrayOf& delimiters, size_t dimension)
{
    ArrayOf* strs = (ArrayOf*)A.dataPointer();
    ArrayOf* delim = (ArrayOf*)delimiters.dataPointer();
    size_t nbRows = A.rows();
    size_t nbCols = A.columns();

    if (dimension == 1) {
        Dimensions dims(1, nbCols);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, nbCols);
        ArrayOf strArray = ArrayOf(NLS_STRING_ARRAY, dims, elements);

        OMP_PARALLEL_FOR_LOOP(nbCols)
        for (ompIndexType c = 0; c < (ompIndexType)nbCols; c++) {
            std::wstring joined;
            bool isMissing = false;
            for (size_t r = 0; r < nbRows && !isMissing; r++) {
                if (r > 0) {
                    if (delim[r - 1].isRowVectorCharacterArray()) {
                        joined += delim[r - 1].asWideString();
                    } else {
                        isMissing = true;
                    }
                }
                isMissing = isMissing || !strs[r + c * nbRows].isRowVectorCharacterArray();
                if (!isMissing) {
                    joined += strs[r + c * nbRows].asWideString();
                }
            }
            elements[c]
                = isMissing ? ArrayOf::doubleScalar(std::nan("")) : ArrayOf::characterArray(joined);
        }
        return strArray;
    } else /* dimension == 2 */ {
        Dimensions dims(nbRows, 1);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, nbRows);
        ArrayOf strArray = ArrayOf(NLS_STRING_ARRAY, dims, elements);

        OMP_PARALLEL_FOR_LOOP(nbRows)
        for (ompIndexType r = 0; r < (ompIndexType)nbRows; r++) {
            std::wstring joined;
            bool isMissing = false;
            for (size_t c = 0; c < nbCols && !isMissing; c++) {
                if (c > 0) {
                    if (delim[c - 1].isRowVectorCharacterArray()) {
                        joined += delim[c - 1].asWideString();
                    } else {
                        isMissing = true;
                    }
                }
                isMissing = isMissing || !strs[r + c * nbRows].isRowVectorCharacterArray();
                if (!isMissing) {
                    joined += strs[r + c * nbRows].asWideString();
                }
            }
            elements[r]
                = isMissing ? ArrayOf::doubleScalar(std::nan("")) : ArrayOf::characterArray(joined);
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
    ArrayOf* strs = (ArrayOf*)A.dataPointer();
    ArrayOf* delim = (ArrayOf*)delimiters.dataPointer();
    size_t nbRows = A.rows();
    size_t nbCols = A.columns();

    if (dimension == 1) {
        Dimensions dims(1, nbCols);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, nbCols);
        ArrayOf strArray = ArrayOf(NLS_STRING_ARRAY, dims, elements);
        OMP_PARALLEL_FOR_LOOP(nbCols)
        for (ompIndexType c = 0; c < (ompIndexType)nbCols; c++) {
            std::wstring joined;
            bool isMissing = false;
            for (size_t r = 0; r < nbRows && !isMissing; r++) {
                if (r > 0) {
                    if (delim[r - 1].isRowVectorCharacterArray()) {
                        joined += delim[r - 1].asWideString();
                    } else {
                        isMissing = true;
                    }
                }
                isMissing = isMissing || !strs[r + c * nbRows].isRowVectorCharacterArray();
                if (!isMissing) {
                    joined += strs[r + c * nbRows].asWideString();
                }
            }
            elements[c]
                = isMissing ? ArrayOf::doubleScalar(std::nan("")) : ArrayOf::characterArray(joined);
        }
        return strArray;

    } else /* dimension == 2 */ {
        Dimensions dims(nbRows, 1);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, nbRows);
        ArrayOf strArray = ArrayOf(NLS_STRING_ARRAY, dims, elements);
        OMP_PARALLEL_FOR_LOOP(nbRows)
        for (ompIndexType r = 0; r < (ompIndexType)nbRows; r++) {
            std::wstring joined;
            bool isMissing = false;
            for (size_t c = 0; c < nbCols && !isMissing; c++) {
                if (c > 0) {
                    if (delim[c - 1].isRowVectorCharacterArray()) {
                        joined += delim[c - 1].asWideString();
                    } else {
                        isMissing = true;
                    }
                }
                isMissing = isMissing || !strs[r + c * nbRows].isRowVectorCharacterArray();
                if (!isMissing) {
                    joined += strs[r + c * nbRows].asWideString();
                }
            }
            elements[r]
                = isMissing ? ArrayOf::doubleScalar(std::nan("")) : ArrayOf::characterArray(joined);
        }
        return strArray;
    }
}
//=============================================================================
ArrayOf
StringJoinStringArrayMatrixDelimiter(const ArrayOf& A, const ArrayOf& delimiters, size_t dimension)
{
    ArrayOf* strs = (ArrayOf*)A.dataPointer();
    ArrayOf* delims = (ArrayOf*)delimiters.dataPointer();
    size_t nbRows = A.rows();
    size_t nbCols = A.columns();

    if (dimension == 1) {
        Dimensions dims(1, nbCols);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, nbCols);
        ArrayOf strArray = ArrayOf(NLS_STRING_ARRAY, dims, elements);
        OMP_PARALLEL_FOR_LOOP(nbCols)
        for (ompIndexType c = 0; c < (ompIndexType)nbCols; c++) {
            std::wstring joined;
            bool isMissing = false;
            for (size_t r = 0; r < nbRows && !isMissing; r++) {
                if (r > 0) {
                    if (delims[(r - 1) + c * (nbRows - 1)].isRowVectorCharacterArray()) {
                        joined += delims[(r - 1) + c * (nbRows - 1)].asWideString();
                    } else {
                        isMissing = true;
                    }
                }
                isMissing = isMissing || !strs[r + c * nbRows].isRowVectorCharacterArray();
                if (!isMissing) {
                    joined += strs[r + c * nbRows].asWideString();
                }
            }
            elements[c]
                = isMissing ? ArrayOf::doubleScalar(std::nan("")) : ArrayOf::characterArray(joined);
        }
        return strArray;

    } else /* dimension == 2 */ {
        Dimensions dims(nbRows, 1);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, nbRows);
        ArrayOf strArray = ArrayOf(NLS_STRING_ARRAY, dims, elements);
        OMP_PARALLEL_FOR_LOOP(nbRows)
        for (ompIndexType r = 0; r < (ompIndexType)nbRows; r++) {
            std::wstring joined;
            bool isMissing = false;
            for (size_t c = 0; c < nbCols && !isMissing; c++) {
                if (c > 0) {
                    if (delims[r + (c - 1) * nbRows].isRowVectorCharacterArray()) {
                        joined += delims[r + (c - 1) * nbRows].asWideString();

                    } else {
                        isMissing = true;
                    }
                }
                isMissing = !isMissing && !strs[r + c * nbRows].isRowVectorCharacterArray();
                if (!isMissing) {
                    joined += strs[r + c * nbRows].asWideString();
                }
            }
            elements[r]
                = isMissing ? ArrayOf::doubleScalar(std::nan("")) : ArrayOf::characterArray(joined);
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
        return ArrayOf::stringArray(wstringVector(), dims);
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
    ArrayOf* strs = (ArrayOf*)A.dataPointer();
    ArrayOf* delim = (ArrayOf*)delimiter.dataPointer();
    size_t nbRows = A.rows();
    size_t nbCols = A.columns();

    if (dimension == 1) {
        Dimensions dims(1, nbCols);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbCols);
        ArrayOf strArray = ArrayOf(NLS_CELL_ARRAY, dims, elements);

        OMP_PARALLEL_FOR_LOOP(nbCols)
        for (ompIndexType c = 0; c < (ompIndexType)nbCols; c++) {
            std::wstring joined;
            bool isMissing = false;
            for (size_t r = 0; r < nbRows && !isMissing; r++) {
                if (r > 0) {
                    if (delim[0].isRowVectorCharacterArray()) {
                        joined += delim[0].asWideString();
                    } else {
                        isMissing = true;
                        joined = L"";
                    }
                }
                if (isMissing) {
                    joined = L"";
                } else {
                    joined += strs[r + c * nbRows].asWideString();
                }
            }
            elements[c] = ArrayOf::characterArray(joined);
        }
        return strArray;
    } else {
        Dimensions dims(nbRows, 1);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbRows);
        ArrayOf strArray = ArrayOf(NLS_CELL_ARRAY, dims, elements);

        OMP_PARALLEL_FOR_LOOP(nbRows)
        for (ompIndexType r = 0; r < (ompIndexType)nbRows; r++) {
            std::wstring joined;
            bool isMissing = false;
            for (size_t c = 0; c < nbCols && !isMissing; c++) {
                if (c > 0) {
                    if (delim[0].isRowVectorCharacterArray()) {
                        joined += delim[0].asWideString();
                    } else {
                        isMissing = true;
                    }
                }
                if (!isMissing) {
                    joined += strs[r + c * nbRows].asWideString();
                } else {
                    joined = L"";
                }
            }
            elements[r] = ArrayOf::characterArray(joined);
        }
        return strArray;
    }
}
//=============================================================================
ArrayOf
StringJoinCellCharactersRowVectorDelimiter(
    const ArrayOf& A, const ArrayOf& delimiters, size_t dimension)
{
    ArrayOf* strs = (ArrayOf*)A.dataPointer();
    ArrayOf* delim = (ArrayOf*)delimiters.dataPointer();
    size_t nbRows = A.rows();
    size_t nbCols = A.columns();

    if (dimension == 1) {
        Dimensions dims(1, nbCols);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbCols);
        ArrayOf strArray = ArrayOf(NLS_CELL_ARRAY, dims, elements);

        OMP_PARALLEL_FOR_LOOP(nbCols)
        for (ompIndexType c = 0; c < (ompIndexType)nbCols; c++) {
            std::wstring joined;
            bool isMissing = false;
            for (size_t r = 0; r < nbRows && !isMissing; r++) {
                if (r > 0) {
                    if (delim[r - 1].isRowVectorCharacterArray()) {
                        joined += delim[r - 1].asWideString();
                    } else {
                        joined = L"";
                        isMissing = true;
                    }
                }
                if (!isMissing) {
                    joined += strs[r + c * nbRows].asWideString();
                } else {
                    joined = L"";
                }
            }
            elements[c] = ArrayOf::characterArray(joined);
        }
        return strArray;
    } else /* dimension == 2 */ {
        Dimensions dims(nbRows, 1);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbRows);
        ArrayOf strArray = ArrayOf(NLS_CELL_ARRAY, dims, elements);

        OMP_PARALLEL_FOR_LOOP(nbRows)
        for (ompIndexType r = 0; r < (ompIndexType)nbRows; r++) {
            std::wstring joined;
            bool isMissing = false;
            for (size_t c = 0; c < nbCols && !isMissing; c++) {
                if (c > 0) {
                    if (delim[c - 1].isRowVectorCharacterArray()) {
                        joined += delim[c - 1].asWideString();
                    } else {
                        joined = L"";
                    }
                }
                if (!isMissing) {
                    joined += strs[r + c * nbRows].asWideString();
                } else {
                    joined = L"";
                }
            }
            elements[r] = ArrayOf::characterArray(joined);
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
    ArrayOf* strs = (ArrayOf*)A.dataPointer();
    ArrayOf* delim = (ArrayOf*)delimiters.dataPointer();
    size_t nbRows = A.rows();
    size_t nbCols = A.columns();

    if (dimension == 1) {
        Dimensions dims(1, nbCols);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbCols);
        ArrayOf strArray = ArrayOf(NLS_CELL_ARRAY, dims, elements);

        OMP_PARALLEL_FOR_LOOP(nbCols)
        for (ompIndexType c = 0; c < (ompIndexType)nbCols; c++) {
            std::wstring joined;
            bool isMissing = false;
            for (size_t r = 0; r < nbRows && !isMissing; r++) {
                if (r > 0) {
                    if (delim[r - 1].isRowVectorCharacterArray()) {
                        joined += delim[r - 1].asWideString();
                    } else {
                        isMissing = true;
                        joined = L"";
                    }
                }
                if (!isMissing) {
                    joined += strs[r + c * nbRows].asWideString();
                } else {
                    joined = L"";
                }
            }
            elements[c] = ArrayOf::characterArray(joined);
        }
        return strArray;
    } else /* dimension == 2 */ {
        Dimensions dims(nbRows, 1);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbRows);
        ArrayOf strArray = ArrayOf(NLS_CELL_ARRAY, dims, elements);
        OMP_PARALLEL_FOR_LOOP(nbRows)
        for (ompIndexType r = 0; r < (ompIndexType)nbRows; r++) {
            std::wstring joined;
            bool isMissing = false;
            for (size_t c = 0; c < nbCols && !isMissing; c++) {
                if (c > 0) {
                    if (delim[c - 1].isRowVectorCharacterArray()) {
                        joined += delim[c - 1].asWideString();
                        isMissing = true;
                    } else {
                        joined = L"";
                    }
                }
                if (!isMissing) {
                    joined += strs[r + c * nbRows].asWideString();
                } else {
                    joined = L"";
                }
            }
            elements[r] = ArrayOf::characterArray(joined);
        }
        return strArray;
    }
}
//=============================================================================
ArrayOf
StringJoinCellCharactersMatrixDelimiter(
    const ArrayOf& A, const ArrayOf& delimiters, size_t dimension)
{
    ArrayOf* strs = (ArrayOf*)A.dataPointer();
    ArrayOf* delims = (ArrayOf*)delimiters.dataPointer();
    size_t nbRows = A.rows();
    size_t nbCols = A.columns();

    if (dimension == 1) {
        Dimensions dims(1, nbCols);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbCols);
        ArrayOf strArray = ArrayOf(NLS_CELL_ARRAY, dims, elements);
        OMP_PARALLEL_FOR_LOOP(nbCols)
        for (ompIndexType c = 0; c < (ompIndexType)nbCols; c++) {
            std::wstring joined;
            bool isMissing = false;
            for (size_t r = 0; r < nbRows && !isMissing; r++) {
                if (r > 0) {
                    if (delims[(r - 1) + c * (nbRows - 1)].isRowVectorCharacterArray()) {
                        joined += delims[(r - 1) + c * (nbRows - 1)].asWideString();
                    } else {
                        isMissing = true;
                        joined = L"";
                    }
                }
                if (!isMissing) {
                    joined += strs[r + c * nbRows].asWideString();
                } else {
                    joined = L"";
                }
            }
            elements[c] = ArrayOf::characterArray(joined);
        }
        return strArray;

    } else /* dimension == 2 */ {
        Dimensions dims(nbRows, 1);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbRows);
        ArrayOf strArray = ArrayOf(NLS_CELL_ARRAY, dims, elements);
        OMP_PARALLEL_FOR_LOOP(nbRows)
        for (ompIndexType r = 0; r < (ompIndexType)nbRows; r++) {
            std::wstring joined;
            bool isMissing = false;
            for (size_t c = 0; c < nbCols && !isMissing; c++) {
                if (c > 0) {
                    if (delims[r + (c - 1) * nbRows].isRowVectorCharacterArray()) {
                        joined += delims[r + (c - 1) * nbRows].asWideString();
                    } else {
                        isMissing = true;
                        joined = L"";
                    }
                }
                if (!isMissing) {
                    joined += strs[r + c * nbRows].asWideString();
                } else {
                    joined = L"";
                }
            }
            elements[r] = ArrayOf::characterArray(joined);
        }
        return strArray;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
