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
#include "SparseType.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
floatingNumberToLogical(const ArrayOf& A)
{
    if (A.isSparse()) {
        void* pLogical = Eigen_TypeConvertSparse(
            NLS_LOGICAL, A.rows(), A.columns(), A.sparseDataPointer(), A.dataClass());
        return ArrayOf(NLS_LOGICAL, A.dimensions(), pLogical, true);
    }
    logical* pLogical = static_cast<logical*>(
        ArrayOf::allocateArrayOf(NLS_LOGICAL, A.elementCount(), stringVector(), false));
    ArrayOf r = ArrayOf(NLS_LOGICAL, A.dimensions(), pLogical, false);
    if (A.isDoubleClass()) {
        auto* ptrReal = (double*)A.dataPointer();
        for (indexType k = 0; k < A.elementCount(); k++) {
            if (std::isnan(ptrReal[k])) {
                Error(L"Nelson:logical:nanNotAllowed",
                    L"Conversion to logical with NaN is not possible.");
            }
            pLogical[k] = static_cast<logical>(ptrReal[k] != 0.0);
        }

    } else {
        auto* ptrReal = (single*)A.dataPointer();
        for (indexType k = 0; k < A.elementCount(); k++) {
            if (std::isnan(ptrReal[k])) {
                Error(L"Nelson:logical:nanNotAllowed",
                    L"Conversion to logical with NaN is not possible.");
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
        Error(L"Nelson:logical:cannotConvertFromSparseInteger",
            L"Conversion to logical from sparse integer is not possible.");
    } else {
        logical* pLogical = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, A.elementCount(), stringVector(), false));
        r = ArrayOf(NLS_LOGICAL, A.dimensions(), pLogical, false);
        auto* ptrInt = (T*)A.dataPointer();
        ompIndexType elementCount = A.elementCount();
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
    switch (A.dataClass()) {
    case NLS_LOGICAL: {
        ArrayOf r(A);
        r.ensureSingleOwner();
        return r;
    } break;
    case NLS_GO_HANDLE: {
        Error(L"Nelson:logical:cannotConvert:conversionLogicalFromGraphicsObjectNot",
            L"Conversion to logical from graphics_object is not possible.");
    } break;
    case NLS_HANDLE: {
        Error(L"Nelson:logical:cannotConvert:conversionLogicalFromHandleNotPossible",
            L"Conversion to logical from handle is not possible.");
    } break;
    case NLS_STRING_ARRAY: {
        Error(L"Nelson:logical:cannotConvert:conversionLogicalFromStringNotPossible",
            L"Conversion to logical from string is not possible.");
    } break;
    case NLS_CELL_ARRAY: {
        Error(L"Nelson:logical:cannotConvert", L"Conversion to logical from cell is not possible.");
    } break;
    case NLS_FUNCTION_HANDLE: {
        Error(L"Nelson:logical:cannotConvert:conversionLogicalFromFunctionHandleNot",
            L"Conversion to logical from function_handle is not possible.");
    } break;
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY: {
        Error("Nelson:logical:conversionNotDefined",
            "Undefined function '%s' for input arguments of type '%s'.", std::string("logical"),
            A.classType());
    } break;
    case NLS_STRUCT_ARRAY: {
        Error(L"Nelson:logical:cannotConvert:conversionLogicalFromStructNotPossible",
            L"Conversion to logical from struct is not possible.");
    } break;
    case NLS_DCOMPLEX:
    case NLS_SCOMPLEX: {
        Error(L"Nelson:logical:cannotConvert:conversionLogicalFromComplexNotPossible",
            L"Conversion to logical from complex is not possible.");
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
        Error(L"Nelson:logical:invalidConversion", L"Invalid conversion.");
    } break;
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
