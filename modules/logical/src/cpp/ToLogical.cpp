//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ToLogical.hpp"
#include "SparseDynamicFunctions.hpp"
#include "nlsBuildConfig.h"
#include "Error.hpp"
#include "i18n.hpp"
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
        Error(_W("Conversion to logical from single is not possible."));
    }
    logical* pLogical = static_cast<logical*>(
        ArrayOf::allocateArrayOf(NLS_LOGICAL, A.getElementCount(), stringVector(), false));
    ArrayOf r = ArrayOf(NLS_LOGICAL, A.getDimensions(), pLogical, false);
    if (A.isDoubleClass()) {
        auto* ptrReal = (double*)A.getDataPointer();
        for (indexType k = 0; k < A.getElementCount(); k++) {
            if (std::isnan(ptrReal[k])) {
                Error(_W("Conversion to logical with NaN is not possible."));
            }
            pLogical[k] = static_cast<logical>(ptrReal[k] != 0.0);
        }

    } else {
        auto* ptrReal = (single*)A.getDataPointer();
        for (indexType k = 0; k < A.getElementCount(); k++) {
            if (std::isnan(ptrReal[k])) {
                Error(_W("Conversion to logical with NaN is not possible."));
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
        Error(_W("Conversion to logical from sparse integer is not possible."));
    } else {
        logical* pLogical = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, A.getElementCount(), stringVector(), false));
        r = ArrayOf(NLS_LOGICAL, A.getDimensions(), pLogical, false);
        auto* ptrInt = (T*)A.getDataPointer();
#if WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); k++) {
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
        Error(_W("Conversion to logical from graphics_object is not possible."));
    } break;
    case NLS_HANDLE: {
        Error(_W("Conversion to logical from handle is not possible."));
    } break;
    case NLS_STRING_ARRAY: {
        Error(_W("Conversion to logical from string is not possible."));
    } break;
    case NLS_CELL_ARRAY: {
        Error(_W("Conversion to logical from cell is not possible."));
    } break;
    case NLS_FUNCTION_HANDLE: {
        Error(_W("Conversion to logical from function_handle is not possible."));
    } break;
    case NLS_CLASS_ARRAY: {
        Error(_("Undefined function 'logical' for input arguments of type '") + A.getClassType()
            + "'.");
    } break;
    case NLS_STRUCT_ARRAY: {
        Error(_W("Conversion to logical from struct is not possible."));
    } break;
    case NLS_DCOMPLEX:
    case NLS_SCOMPLEX: {
        Error(_W("Conversion to logical from complex is not possible."));
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
        Error(_W("Invalid conversion."));
    } break;
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
