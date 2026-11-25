//=============================================================================
// Copyright (c) 2019-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#if _MSC_VER
#pragma warning(disable : 4018)
#endif
//=============================================================================
#include <algorithm>
#include <cmath>
#include <limits>
#include <type_traits>
#include <typeinfo>
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "Dimensions.hpp"
#include "Types.hpp"
#include "SparseType.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "ArrayOf_PromoteTypeKernels.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void*
promoteNumericData(NelsonType srcClass, NelsonType dstClass, const void* src, indexType count);
//=============================================================================
void
ArrayOf::promoteType(NelsonType dstClass, const stringVector& fNames)
{
    if (hasScalarStorage()) {
        if (dstClass == scalarType_) {
            return;
        }
        materializeScalar();
    }
    if (dp->dataClass != dstClass && !isEmpty()) {
        bool allowedCases = ((dstClass == NLS_CLASS_ARRAY) && (dp->dataClass == NLS_STRUCT_ARRAY))
            || ((dstClass == NLS_CLASSDEF_ARRAY) && (dp->dataClass == NLS_STRUCT_ARRAY))
            || ((dstClass == NLS_STRUCT_ARRAY) && (dp->dataClass == NLS_CLASS_ARRAY))
            || ((dstClass == NLS_STRUCT_ARRAY) && (dp->dataClass == NLS_CLASSDEF_ARRAY))
            || ((dstClass == NLS_FUNCTION_HANDLE) && (dp->dataClass == NLS_STRUCT_ARRAY))
            || ((dstClass == NLS_STRUCT_ARRAY) && (dp->dataClass == NLS_FUNCTION_HANDLE));
        if ((dstClass == NLS_STRING_ARRAY) || (dstClass == NLS_CELL_ARRAY)
            || (dp->dataClass <= NLS_CHAR && dstClass > NLS_CHAR) && !allowedCases) {
            Error(L"Nelson:types:conversionNotSupported:cannotConvertBaseTypesReferenceTypes",
                L"Cannot convert base types to reference types.");
        }
    }

    if (isEmpty() && (!isSparse() || dp->data() == nullptr)) {
        dp = dp->putData(dstClass, dp->dimensions_, nullptr, isSparse(), fNames);
        return;
    }
    if (isSparse()) {
        dp = dp->putData(dstClass, dp->dimensions_,
            Eigen_TypeConvertSparse(
                dstClass, dp->dimensions_[0], dp->dimensions_[1], dp->data(), dp->dataClass),
            true);
        return;
    }
    indexType count = elementCount();
    void* dstPtr = nullptr;
    switch (dp->dataClass) {
    case NLS_HANDLE: {
        if (dstClass == NLS_HANDLE) {
            return;
        }
        Error(L"Nelson:types:conversionNotSupported:cannotConvertValueAnyOtherType",
            L"Cannot convert %s to any other type.", L"handle-arrays");
    } break;
    case NLS_GO_HANDLE: {
        if (dstClass == NLS_GO_HANDLE) {
            return;
        }
        Error(L"Nelson:types:conversionNotSupported:cannotConvertValueAnyOtherType",
            L"Cannot convert %s to any other type.", L"graphic handle-arrays");
    } break;
    case NLS_CELL_ARRAY: {
        if (dstClass == NLS_CELL_ARRAY) {
            return;
        }
        Error(L"Nelson:types:conversionNotSupported:cannotConvertValueAnyOtherType",
            L"Cannot convert %s to any other type.", L"cell-arrays");

    } break;
    case NLS_FUNCTION_HANDLE: {
        if (dstClass == NLS_STRUCT_ARRAY) {
            return;
        }
        Error(L"Nelson:types:conversionNotSupported:cannotConvertValueAnyOtherType",
            L"Cannot convert %s to any other type.", L"function_handle");

    } break;
    case NLS_CLASS_ARRAY: {
        if (dstClass == NLS_CLASS_ARRAY) {
            if (dp->classTypeName() != classType()) {
                Error(L"Nelson:types:conversionNotSupported:cannotCombineClassesDifferentTypes",
                    L"Cannot combine classes with different types.");
            }
            return;
        }
        if (dstClass == NLS_STRUCT_ARRAY) {
            // The class flip mutates Data in place: detach shared storage first.
            ensureSingleOwner();
            dp->promoteClassToStruct();
            return;
        }
        Error(L"Nelson:types:conversionNotSupported:cannotConvertValueAnyOtherType",
            L"Cannot convert %s to any other type.", L"class");
    } break;
    case NLS_CLASSDEF_ARRAY: {
        if (dstClass == NLS_CLASSDEF_ARRAY) {
            if (dp->classTypeName() != classType()) {
                Error(L"Nelson:types:conversionNotSupported:cannotCombineClassesDifferentTypes",
                    L"Cannot combine classes with different types.");
            }
            return;
        }
        if (dstClass == NLS_STRUCT_ARRAY) {
            ensureSingleOwner();
            dp->promoteClassToStruct();
            return;
        }
        Error(L"Nelson:types:conversionNotSupported:cannotConvertValueAnyOtherType",
            L"Cannot convert %s to any other type.", L"class");
    } break;
    case NLS_STRUCT_ARRAY: {
        if (dstClass == NLS_STRUCT_ARRAY) {
            // TODO: Generalize this code to allow for one more field in destination
            // than in source...
            const stringVector& fieldNames = dp->fieldNames();
            if (fieldNames.size() > fNames.size()) {
                Error(L"Nelson:types:structFieldsMismatch",
                    L"Cannot combine structures with different fields if the combination requires "
                    L"fields to be deleted from one of the structures.");
            }
            // Every field name in fieldnames must be present in fnames.
            int extraCount = 0;
            int matchCount = 0;
            indexType i;
            for (i = 0; i < (int)fNames.size(); i++) {
                int64 ndx = fieldIndex(fNames[i]);
                if (ndx == -1) {
                    extraCount++;
                } else {
                    matchCount++;
                }
            }
            if (matchCount != fieldNames.size()) {
                Error(L"Nelson:types:structFieldsMismatch",
                    L"Cannot combine structures with different fields if the combination requires "
                    L"fields to be deleted from one of the structures.");
            }
            void* dstPtr = allocateArrayOf(dp->dataClass, elementCount(), fNames, false);
            const ArrayOf* src_rp = (const ArrayOf*)dp->data();
            ArrayOf* dst_rp = (ArrayOf*)dstPtr;
            indexType elCount(elementCount());
            indexType fieldCount(fieldNames.size());
            indexType newFieldCount(fNames.size());
            ;
            for (i = 0; i < fieldCount; i++) {
                int64 newNdx = fieldIndexFromList(fieldNames[i], fNames);
                for (indexType j = 0; j < elCount; j++) {
                    dst_rp[j * newFieldCount + newNdx] = src_rp[j * fieldCount + i];
                }
            }
            dp = dp->putData(dp->dataClass, dp->dimensions_, dstPtr, false, fNames);
            return;
        }
        if (dstClass == NLS_CLASS_ARRAY) {
            // The class flip mutates Data in place: detach shared storage first.
            ensureSingleOwner();
            dp->promoteStructToClass();
            return;
        }
        if (dstClass == NLS_CLASSDEF_ARRAY) {
            ensureSingleOwner();
            dp->promoteStructToClassdef();
            return;
        }
        if (dstClass == NLS_FUNCTION_HANDLE) {
            ensureSingleOwner();
            dp->promoteStructToFunctionHandle();
            return;
        }

        Error(L"Nelson:types:conversionNotSupported:cannotConvertValueAnyOtherType",
            L"Cannot convert %s to any other type.", L"struct-arrays");

    } break;
    case NLS_STRING_ARRAY: {
        if (dstClass == NLS_STRING_ARRAY) {
            return;
        }
        Error(L"Nelson:types:conversionNotSupported:cannotConvertValueAnyOtherType",
            L"Cannot convert %s to any other type.", L"string-arrays");

    } break;
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
    case NLS_CHAR:
    case NLS_MISSING_ARRAY: {
        dstPtr = promoteNumericData(dp->dataClass, dstClass, dp->data(), count);
        if (dstPtr == nullptr) {
            return;
        }
    } break;
    default: {
        Error(L"Nelson:types:typeNotManaged", L"Type not supported.");
    } break;
    }
    replaceData(dstClass, dp->dimensions_, dstPtr);
}
//=============================================================================
void
ArrayOf::promoteType(NelsonType dstClass)
{
    stringVector dummy;
    promoteType(dstClass, dummy);
}
//=============================================================================
bool
ArrayOf::canBePromotedTo(NelsonType dstClass)
{
    if (isEmpty()) {
        return true;
    }
    const NelsonType sourceClass = dataClass();
    if (sourceClass != dstClass) {
        if ((dstClass == NLS_STRING_ARRAY) || (dstClass == NLS_CELL_ARRAY)
            || (dstClass == NLS_STRUCT_ARRAY) || (dstClass == NLS_CLASS_ARRAY)
            || (dstClass == NLS_CLASSDEF_ARRAY) || (dstClass == NLS_FUNCTION_HANDLE)) {
            return false;
        }
    }
    if (sourceClass == dstClass) {
        return true;
    }
    if (isSparse()) {
        if ((sourceClass == NLS_SINGLE) || (sourceClass == NLS_SCOMPLEX)
            || (sourceClass == NLS_DOUBLE) || (sourceClass == NLS_DCOMPLEX)
            || (sourceClass == NLS_LOGICAL)) {
            return true;
        }
    }
    switch (sourceClass) {
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_STRUCT_ARRAY:
    case NLS_STRING_ARRAY: {
        return false;
    } break;
    case NLS_LOGICAL: {
        switch (dstClass) {
        case NLS_LOGICAL:
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX: {
            return true;
        } break;
        default: {
            return false;
        } break;
        }
    } break;
    case NLS_UINT8: {
        switch (dstClass) {
        case NLS_LOGICAL:
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        case NLS_CHAR: {
            return true;
        } break;
        default: {
            return false;
        } break;
        }
    } break;
    case NLS_INT8: {
        switch (dstClass) {
        case NLS_LOGICAL:
        case NLS_INT8:
        case NLS_UINT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        case NLS_CHAR: {
            return true;
        } break;
        default: {
            return false;
        } break;
        }
    } break;
    case NLS_UINT16: {
        switch (dstClass) {
        case NLS_LOGICAL:
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        case NLS_CHAR: {
            return true;
        } break;
        default: {
            return true;
        } break;
        }
    } break;
    case NLS_INT16: {
        switch (dstClass) {
        case NLS_LOGICAL:
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_INT16:
        case NLS_UINT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        case NLS_CHAR: {
            return true;
        } break;
        default: {
            return false;
        } break;
        }
    } break;
    case NLS_UINT32: {
        switch (dstClass) {
        case NLS_LOGICAL:
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        case NLS_CHAR: {
            return true;
        } break;
        default: {
            return false;
        } break;
        }
    } break;
    case NLS_INT32: {
        switch (dstClass) {
        case NLS_LOGICAL:
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_INT32:
        case NLS_UINT32:
        case NLS_UINT64:
        case NLS_INT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        case NLS_CHAR: {
            return true;
        } break;
        default: {
            return false;
        } break;
        }
    } break;
    case NLS_UINT64: {
        switch (dstClass) {
        case NLS_LOGICAL:
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        case NLS_CHAR: {
            return true;
        } break;
        default: {
            return false;
        } break;
        }
    } break;
    case NLS_INT64: {
        switch (dstClass) {
        case NLS_LOGICAL:
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_INT64:
        case NLS_UINT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        case NLS_CHAR: {
            return true;
        } break;
        default: {
            return false;
        } break;
        }
    } break;
    case NLS_SINGLE: {
        switch (dstClass) {
        case NLS_LOGICAL:
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        case NLS_CHAR: {
            return true;
        } break;
        default: {
            return false;
        } break;
        }
    } break;
    case NLS_DOUBLE: {
        switch (dstClass) {
        case NLS_LOGICAL:
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        case NLS_CHAR: {
            return true;
        } break;
        default: {
            return false;
        } break;
        }
    } break;
    case NLS_SCOMPLEX: {
        switch (dstClass) {
        case NLS_LOGICAL:
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        case NLS_CHAR: {
            return true;
        } break;
        default: {
            return false;
        } break;
        }
    } break;
    case NLS_DCOMPLEX: {
        switch (dstClass) {
        case NLS_LOGICAL:
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_DCOMPLEX:
        case NLS_SCOMPLEX:
        case NLS_CHAR: {
            return true;
        } break;
        default: {
            return false;
        } break;
        }
    } break;
    case NLS_CHAR: {
        switch (dstClass) {
        case NLS_LOGICAL:
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        case NLS_CHAR: {
            return true;
        } break;
        default: {
            return false;
        } break;
        }
    } break;
    default: {
        return false;
    } break;
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
