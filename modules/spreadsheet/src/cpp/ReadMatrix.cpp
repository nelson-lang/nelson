//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <rapidcsv.h>
#include <complex>
#include <fast_float/fast_float.h>
#include "ReadMatrix.hpp"
#include "characters_encoding.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#if WITH_OPENMP
#include <omp.h>
#endif
#include "CSVTypeConverters.hpp"
#include "ReadLinesFromFile.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
ReadMatrix(const std::wstring& filename, const detectImportOptions& options, NelsonType OutputType,
    std::string& errorMessage)
{
    char separator = options.Delimiter[0][0];
    bool pHasCR = false;
    rapidcsv::SeparatorParams separatorParams
        = rapidcsv::SeparatorParams(separator, true, pHasCR, false, false);

    rapidcsv::ConverterParams converterParams;
    converterParams.mHasDefaultConverter = false;
    converterParams.mNumericLocale = false;

    rapidcsv::LineReaderParams lineReaderParams;
    lineReaderParams.mSkipCommentLines = !options.CommentStyle.empty();
    if (options.CommentStyle.empty()) {
        lineReaderParams.mCommentPrefix = '\0';
        lineReaderParams.mSkipCommentLines = false;
    } else {
        lineReaderParams.mCommentPrefix = options.CommentStyle[0][0];
        lineReaderParams.mSkipCommentLines = true;
    }
    lineReaderParams.mSkipEmptyLines = options.EmptyLineRule == "skip";

    rapidcsv::LabelParams labelParams(-1, -1);
    try {
        std::stringstream stream = readLinesFromFile(filename, options);
        rapidcsv::Document doc(
            stream, labelParams, separatorParams, converterParams, lineReaderParams);
        size_t nbRows = doc.GetRowCount();
        size_t nbColumns = doc.GetColumnCount();
        size_t nbElements = nbRows
            * (options.VariableNames.size() > nbColumns ? options.VariableNames.size() : nbColumns);

        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbElements);
        Dimensions dims(nbRows,
            options.VariableNames.size() > nbColumns ? options.VariableNames.size() : nbColumns);
        ArrayOf result = ArrayOf(NLS_CELL_ARRAY, dims, elements);

        ompIndexType nbAvailableElements = (ompIndexType)(nbColumns * nbRows);

        switch (OutputType) {
        case NLS_DOUBLE: {
            if (doc.IsIJLastChar()) {
                std::complex<double>* ptr = (std::complex<double>*)ArrayOf::allocateArrayOf(
                    NLS_DCOMPLEX, dims.getElementCount());
                ArrayOf matrix = ArrayOf(NLS_DCOMPLEX, dims, ptr);
                OMP_PARALLEL_FOR_LOOP(nbAvailableElements)
                for (ompIndexType index = 0; index < nbAvailableElements; ++index) {
                    size_t i = index / nbRows;
                    size_t j = index % nbRows;
                    ptr[index]
                        = doc.GetCell<std::complex<double>>(i, j, ConvertStringToDoubleComplex);
                }
                return matrix;
            } else {
                double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dims.getElementCount());
                ArrayOf matrix = ArrayOf(NLS_DOUBLE, dims, ptr);
                OMP_PARALLEL_FOR_LOOP(nbAvailableElements)
                for (ompIndexType index = 0; index < nbAvailableElements; ++index) {
                    size_t i = index / nbRows;
                    size_t j = index % nbRows;
                    ptr[index] = doc.GetCell<double>(i, j, ConvertStringToDouble);
                }

                return matrix;
            }
        } break;
        case NLS_SINGLE: {
            if (doc.IsIJLastChar()) {
                std::complex<single>* ptr = (std::complex<single>*)ArrayOf::allocateArrayOf(
                    NLS_SCOMPLEX, dims.getElementCount());
                ArrayOf matrix = ArrayOf(NLS_SCOMPLEX, dims, ptr);
                OMP_PARALLEL_FOR_LOOP(nbAvailableElements)
                for (ompIndexType index = 0; index < nbAvailableElements; ++index) {
                    size_t i = index / nbRows;
                    size_t j = index % nbRows;
                    ptr[index]
                        = doc.GetCell<std::complex<single>>(i, j, ConvertStringToSingleComplex);
                }
                return matrix;
            } else {
                single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, dims.getElementCount());
                ArrayOf matrix = ArrayOf(NLS_SINGLE, dims, ptr);
                OMP_PARALLEL_FOR_LOOP(nbAvailableElements)
                for (ompIndexType index = 0; index < nbAvailableElements; ++index) {
                    size_t i = index / nbRows;
                    size_t j = index % nbRows;
                    ptr[index] = doc.GetCell<single>(i, j, ConvertStringToSingle);
                }
                return matrix;
            }
        } break;
        case NLS_STRING_ARRAY: {
            ArrayOf* ptr
                = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, dims.getElementCount());
            ArrayOf matrix = ArrayOf(NLS_STRING_ARRAY, dims, ptr);
            OMP_PARALLEL_FOR_LOOP(nbAvailableElements)
            for (ompIndexType index = 0; index < nbAvailableElements; ++index) {
                size_t i = index / nbRows;
                size_t j = index % nbRows;
                ptr[index] = ArrayOf::characterArrayConstructor(doc.GetCell<std::string>(i, j));
            }
            return matrix;

        } break;
        case NLS_CELL_ARRAY: {
            ArrayOf* ptr
                = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, dims.getElementCount());
            ArrayOf matrix = ArrayOf(NLS_CELL_ARRAY, dims, ptr);
            OMP_PARALLEL_FOR_LOOP(nbAvailableElements)
            for (ompIndexType index = 0; index < nbAvailableElements; ++index) {
                size_t i = index / nbRows;
                size_t j = index % nbRows;
                ptr[index] = ArrayOf::characterArrayConstructor(doc.GetCell<std::string>(i, j));
            }
            return matrix;
        } break;
        case NLS_INT8: {
            int8* ptr = (int8*)ArrayOf::allocateArrayOf(NLS_INT8, dims.getElementCount());
            ArrayOf matrix = ArrayOf(NLS_INT8, dims, ptr);
            OMP_PARALLEL_FOR_LOOP(nbAvailableElements)
            for (ompIndexType index = 0; index < nbAvailableElements; ++index) {
                size_t i = index / nbRows;
                size_t j = index % nbRows;
                ptr[index] = doc.GetCell<int8>(i, j, ConvertStringToInt8);
            }
            return matrix;
        } break;
        case NLS_INT16: {
            int16* ptr = (int16*)ArrayOf::allocateArrayOf(NLS_INT16, dims.getElementCount());
            ArrayOf matrix = ArrayOf(NLS_INT16, dims, ptr);
            OMP_PARALLEL_FOR_LOOP(nbAvailableElements)
            for (ompIndexType index = 0; index < nbAvailableElements; ++index) {
                size_t i = index / nbRows;
                size_t j = index % nbRows;
                ptr[index] = doc.GetCell<int16>(i, j, ConvertStringToInt16);
            }
            return matrix;
        } break;
        case NLS_INT32: {
            int32* ptr = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, dims.getElementCount());
            ArrayOf matrix = ArrayOf(NLS_INT32, dims, ptr);
            OMP_PARALLEL_FOR_LOOP(nbAvailableElements)
            for (ompIndexType index = 0; index < nbAvailableElements; ++index) {
                size_t i = index / nbRows;
                size_t j = index % nbRows;
                ptr[index] = doc.GetCell<int32>(i, j, ConvertStringToInt32);
            }
            return matrix;
        } break;
        case NLS_INT64: {
            int64* ptr = (int64*)ArrayOf::allocateArrayOf(NLS_INT64, dims.getElementCount());
            ArrayOf matrix = ArrayOf(NLS_INT64, dims, ptr);
            OMP_PARALLEL_FOR_LOOP(nbAvailableElements)
            for (ompIndexType index = 0; index < nbAvailableElements; ++index) {
                size_t i = index / nbRows;
                size_t j = index % nbRows;
                ptr[index] = doc.GetCell<int64>(i, j, ConvertStringToInt64);
            }
            return matrix;
        } break;
        case NLS_UINT8: {
            uint8* ptr = (uint8*)ArrayOf::allocateArrayOf(NLS_UINT8, dims.getElementCount());
            ArrayOf matrix = ArrayOf(NLS_UINT8, dims, ptr);
            OMP_PARALLEL_FOR_LOOP(nbAvailableElements)
            for (ompIndexType index = 0; index < nbAvailableElements; ++index) {
                size_t i = index / nbRows;
                size_t j = index % nbRows;
                ptr[index] = doc.GetCell<uint8>(i, j, ConvertStringToUInt8);
            }
            return matrix;
        } break;
        case NLS_UINT16: {
            uint16* ptr = (uint16*)ArrayOf::allocateArrayOf(NLS_UINT16, dims.getElementCount());
            ArrayOf matrix = ArrayOf(NLS_UINT16, dims, ptr);
            OMP_PARALLEL_FOR_LOOP(nbAvailableElements)
            for (ompIndexType index = 0; index < nbAvailableElements; ++index) {
                size_t i = index / nbRows;
                size_t j = index % nbRows;
                ptr[index] = doc.GetCell<uint16>(i, j, ConvertStringToUInt16);
            }
            return matrix;
        } break;
        case NLS_UINT32: {
            uint32* ptr = (uint32*)ArrayOf::allocateArrayOf(NLS_UINT32, dims.getElementCount());
            ArrayOf matrix = ArrayOf(NLS_UINT32, dims, ptr);
            OMP_PARALLEL_FOR_LOOP(nbAvailableElements)
            for (ompIndexType index = 0; index < nbAvailableElements; ++index) {
                size_t i = index / nbRows;
                size_t j = index % nbRows;
                ptr[index] = doc.GetCell<uint32>(i, j, ConvertStringToUInt32);
            }
            return matrix;
        } break;
        case NLS_UINT64: {
            uint64* ptr = (uint64*)ArrayOf::allocateArrayOf(NLS_UINT64, dims.getElementCount());
            ArrayOf matrix = ArrayOf(NLS_UINT64, dims, ptr);
            OMP_PARALLEL_FOR_LOOP(nbAvailableElements)
            for (ompIndexType index = 0; index < nbAvailableElements; ++index) {
                size_t i = index / nbRows;
                size_t j = index % nbRows;
                ptr[index] = doc.GetCell<uint64>(i, j, ConvertStringToUInt64);
            }
            return matrix;
        } break;
        default: {
        } break;
        }
    } catch (const std::exception& e) {
        errorMessage = e.what();
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
