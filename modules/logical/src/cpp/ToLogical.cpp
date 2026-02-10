//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ToLogical.hpp"
#include "SparseDynamicFunctions.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
floatingNumberToLogical(const ArrayOf& A)
{
    if (A.isDoubleClass()) {
        if (A.isSparse()) {
            void* pLogical = TypeConvertSparseDynamicFunction(
                NLS_LOGICAL, A.getRows(), A.getColumns(), A.getSparseDataPointer(), NLS_DOUBLE);
            return ArrayOf(NLS_LOGICAL, A.getDimensions(), pLogical, true);
        }
    } else {
        raiseError(L"Nelson:logical:ERROR_CONVERSION_TO_LOGICAL_FROM_SINGLE_IS_NOT_POSSIBLE",
            ERROR_CONVERSION_TO_LOGICAL_FROM_SINGLE_IS_NOT_POSSIBLE);
    }
    logical* pLogical = static_cast<logical*>(
        ArrayOf::allocateArrayOf(NLS_LOGICAL, A.getElementCount(), stringVector(), false));
    ArrayOf r = ArrayOf(NLS_LOGICAL, A.getDimensions(), pLogical, false);
    if (A.isDoubleClass()) {
        auto* ptrReal = (double*)A.getDataPointer();
        for (indexType k = 0; k < A.getElementCount(); k++) {
            if (std::isnan(ptrReal[k])) {
                raiseError(L"Nelson:logical:ERROR_CONVERSION_TO_LOGICAL_WITH_NAN_IS_NOT_POSSIBLE",
                    ERROR_CONVERSION_TO_LOGICAL_WITH_NAN_IS_NOT_POSSIBLE);
            }
            pLogical[k] = static_cast<logical>(ptrReal[k] != 0.0);
        }

    } else {
        auto* ptrReal = (single*)A.getDataPointer();
        for (indexType k = 0; k < A.getElementCount(); k++) {
            if (std::isnan(ptrReal[k])) {
                raiseError(L"Nelson:logical:ERROR_CONVERSION_TO_LOGICAL_WITH_NAN_IS_NOT_POSSIBLE",
                    ERROR_CONVERSION_TO_LOGICAL_WITH_NAN_IS_NOT_POSSIBLE);
            }
            pLogical[k] = static_cast<logical>(ptrReal[k] != 0.0);
        }
    }
    return r;
}
//=============================================================================
template <class T>
static ArrayOf
integerToLogical(const ArrayOf& A)
{
    ArrayOf r;
    if (A.isSparse()) {
        raiseError(
            L"Nelson:logical:ERROR_CONVERSION_TO_LOGICAL_FROM_SPARSE_INTEGER_IS_NOT_POSSIBLE",
            ERROR_CONVERSION_TO_LOGICAL_FROM_SPARSE_INTEGER_IS_NOT_POSSIBLE);
    } else {
        logical* pLogical = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, A.getElementCount(), stringVector(), false));
        r = ArrayOf(NLS_LOGICAL, A.getDimensions(), pLogical, false);
        auto* ptrInt = (T*)A.getDataPointer();
        ompIndexType elementCount = A.getElementCount();
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; k++) {
            pLogical[k] = static_cast<logical>(ptrInt[k] != 0);
        }
    }
    return r;
}
//=============================================================================
ArrayOf
ToLogical(ArrayOf A)
{
    switch (A.getDataClass()) {
    case NLS_LOGICAL: {
        ArrayOf r(A);
        r.ensureSingleOwner();
        return r;
    } break;
    case NLS_GO_HANDLE: {
        raiseError(
            L"Nelson:logical:ERROR_CONVERSION_TO_LOGICAL_FROM_GRAPHICS_OBJECT_IS_NOT_POSSIBLE",
            ERROR_CONVERSION_TO_LOGICAL_FROM_GRAPHICS_OBJECT_IS_NOT_POSSIBLE);
    } break;
    case NLS_HANDLE: {
        raiseError(L"Nelson:logical:ERROR_CONVERSION_TO_LOGICAL_FROM_HANDLE_IS_NOT_POSSIBLE",
            ERROR_CONVERSION_TO_LOGICAL_FROM_HANDLE_IS_NOT_POSSIBLE);
    } break;
    case NLS_STRING_ARRAY: {
        raiseError(L"Nelson:logical:ERROR_CONVERSION_TO_LOGICAL_FROM_STRING_IS_NOT_POSSIBLE",
            ERROR_CONVERSION_TO_LOGICAL_FROM_STRING_IS_NOT_POSSIBLE);
    } break;
    case NLS_CELL_ARRAY: {
        raiseError(L"Nelson:logical:ERROR_CONVERSION_TO_LOGICAL_FROM_CELL_IS_NOT_POSSIBLE",
            ERROR_CONVERSION_TO_LOGICAL_FROM_CELL_IS_NOT_POSSIBLE);
        ;
    } break;
    case NLS_FUNCTION_HANDLE: {
        raiseError(
            L"Nelson:logical:ERROR_CONVERSION_TO_LOGICAL_FROM_FUNCTION_HANDLE_IS_NOT_POSSIBLE",
            ERROR_CONVERSION_TO_LOGICAL_FROM_FUNCTION_HANDLE_IS_NOT_POSSIBLE);
    } break;
    case NLS_CLASS_ARRAY: {
        raiseError(L"Nelson:logical:ERROR_UNDEFINED_FUNCTION_LOGICAL_FOR_INPUT_ARGUMENTS_OF_TYPE",
            ERROR_UNDEFINED_FUNCTION_LOGICAL_FOR_INPUT_ARGUMENTS_OF_TYPE,
            utf8_to_wstring(A.getClassType()));
    } break;
    case NLS_STRUCT_ARRAY: {
        raiseError(L"Nelson:logical:ERROR_CONVERSION_TO_LOGICAL_FROM_STRUCT_IS_NOT_POSSIBLE",
            ERROR_CONVERSION_TO_LOGICAL_FROM_STRUCT_IS_NOT_POSSIBLE);
    } break;
    case NLS_DCOMPLEX:
    case NLS_SCOMPLEX: {
        raiseError(L"Nelson:logical:ERROR_CONVERSION_TO_LOGICAL_FROM_COMPLEX_IS_NOT_POSSIBLE",
            ERROR_CONVERSION_TO_LOGICAL_FROM_COMPLEX_IS_NOT_POSSIBLE);
    } break;
    case NLS_DOUBLE:
    case NLS_SINGLE: {
        return floatingNumberToLogical(A);
    } break;
    case NLS_UINT8: {
        return integerToLogical<uint8>(A);
    } break;
    case NLS_INT8: {
        return integerToLogical<int8>(A);
    } break;
    case NLS_UINT16: {
        return integerToLogical<uint16>(A);
    } break;
    case NLS_INT16: {
        return integerToLogical<int16>(A);
    } break;
    case NLS_UINT32: {
        return integerToLogical<uint32>(A);
    } break;
    case NLS_INT32: {
        return integerToLogical<int32>(A);
    } break;
    case NLS_UINT64: {
        return integerToLogical<uint64>(A);
    } break;
    case NLS_INT64: {
        return integerToLogical<int64>(A);
    } break;
    default: {
        raiseError(L"Nelson:logical:ERROR_INVALID_CONVERSION", ERROR_INVALID_CONVERSION);
    } break;
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
