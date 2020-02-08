//=============================================================================
// Copyright (c) 2019-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <limits>
#include <Eigen/Dense>
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "Dimensions.hpp"
#include "Types.hpp"
#include "SparseDynamicFunctions.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <typename TIN, typename TOUT>
inline TOUT
numeric_cast(TIN value)
{
    const bool positive_overflow_possible
        = std::numeric_limits<TOUT>::max() < std::numeric_limits<TIN>::max();
    const bool negative_overflow_possible = std::numeric_limits<TIN>::is_signed
        || (std::numeric_limits<TOUT>::lowest() > std::numeric_limits<TIN>::lowest());

    // unsigned <-- unsigned
    if ((!std::numeric_limits<TOUT>::is_signed) && (!std::numeric_limits<TIN>::is_signed)) {
        if (positive_overflow_possible && (value > std::numeric_limits<TOUT>::max())) {
            return std::numeric_limits<TOUT>::max();
        }
    }
    // unsigned <-- signed
    else if ((!std::numeric_limits<TOUT>::is_signed) && std::numeric_limits<TIN>::is_signed) {
        if (positive_overflow_possible && (value > std::numeric_limits<TOUT>::max())) {
            return std::numeric_limits<TOUT>::max();
        } else if (negative_overflow_possible && (value < 0)) {
            return std::numeric_limits<TOUT>::min();
        }
    }
    // signed <-- unsigned
    else if (std::numeric_limits<TOUT>::is_signed && (!std::numeric_limits<TIN>::is_signed)) {
        if (positive_overflow_possible && (value > std::numeric_limits<TOUT>::max())) {
            return std::numeric_limits<TOUT>::max();
        }
    }
    // signed <-- signed
    else if (std::numeric_limits<TOUT>::is_signed && std::numeric_limits<TIN>::is_signed) {
        if (positive_overflow_possible && (value > std::numeric_limits<TOUT>::max())) {
            return std::numeric_limits<TOUT>::max();
        } else if (negative_overflow_possible && (value < std::numeric_limits<TOUT>::lowest())) {
            return std::numeric_limits<TOUT>::min();
        }
    }
    return static_cast<TOUT>(value);
}
//=============================================================================
template <class TIN, class TOUT>
void
saturate(Class classIn, Class classOut, const void* pIn, void* pOut, indexType count)
{
    const TIN* sp = (const TIN*)pIn;
    TOUT* qp = (TOUT*)pOut;
    if (classIn == classOut) {
        for (indexType i = 0; i < count; i++) {
            qp[i] = (TOUT)sp[i];
        }
    } else {
        bool checkNaN = false;
        if (typeid(TOUT) != typeid(single) || typeid(TOUT) != typeid(double)) {
            if (typeid(TIN) == typeid(single) || typeid(TIN) == typeid(double)) {
                checkNaN = true;
            }
        }
        if (checkNaN) {
            for (indexType i = 0; i < count; i++) {
                if (std::isnan((double)sp[i])) {
                    qp[i] = (TOUT)0;
                } else {
                    qp[i] = (TOUT)numeric_cast<TIN, TOUT>(sp[i]);
                }
            }
        } else {
            for (indexType i = 0; i < count; i++) {
                qp[i] = numeric_cast<TIN, TOUT>(sp[i]);
            }
        }
    }
}
//=============================================================================
/**
 * Promote our data to a new type.
 *
 * Copy data from our data array to the specified
 * array, converting the data as we go.  We can only
 * convert data to or from base types.  So if the source
 * or destination types are reference types, we cannot
 * perform the conversion.
 *
 * For the remaining types, we have a matrix of
 * possibilities.  Here we list the conversion rules.
 *
 * Source type
 *  - string
 *    - logical dest = (source == 0) ? 0 : 1
 *    - real dest = (double) source
 *    - complex dest = (double) source
 *  - logical
 *    - string dest = (char) source
 *    - real   dest = (double) source
 *    - complex dest = (double) source
 *  - real
 *    - string dest = (char) source
 *    - logical dest = (source == 0) ? 0 : 1
 *    - complex dest = (double) source
 *  - complex
 *    - string dest = (char) real(source)
 *    - logical dest = (real(source) == 0 && imag(source) == 0) ? 0:1
 *    - real dest = real(source)
 */
//=============================================================================
#undef caseMacro
//=============================================================================
#define caseMacro(caseLabel, dpType, convCode)                                                     \
    case caseLabel: {                                                                              \
        dpType* qp = (dpType*)dstPtr;                                                              \
        for (indexType i = 0; i < count; i++)                                                      \
            convCode;                                                                              \
    } break;
//=============================================================================
void
ArrayOf::promoteType(Class dstClass, stringVector fNames)
{
    indexType elCount = 0;
    void* dstPtr = nullptr;
    if (isEmpty()) {
        dp = dp->putData(dstClass, dp->dimensions, NULL, isSparse(), fNames);
        return;
    }
    if (dp->dataClass == NLS_HANDLE)
        if (dstClass == NLS_HANDLE) {
            return;
        } else {
            Error(_W("Cannot convert handle-arrays to any other type."));
        }
    // Handle the reference types.
    // Cell arrays can be promoted with no effort to cell arrays.
    if (dp->dataClass == NLS_CELL_ARRAY)
        if (dstClass == NLS_CELL_ARRAY) {
            return;
        } else {
            Error(_W("Cannot convert cell-arrays to any other type."));
        }
    if (dp->dataClass == NLS_STRING_ARRAY)
        if (dstClass == NLS_STRING_ARRAY) {
            return;
        } else {
            Error(_W("Cannot convert string-arrays to any other type."));
        }
    // Structure arrays can be promoted to structure arrays with different
    // field structures, but have to be rearranged.
    if (dp->dataClass == NLS_STRUCT_ARRAY)
        if (dstClass == NLS_STRUCT_ARRAY) {
            // TODO: Generalize this code to allow for one more field in destination
            // than in source...
            if (dp->fieldNames.size() > fNames.size()) {
                Error(_W("Cannot combine structures with different fields if the "
                         "combination "
                         "requires fields to be deleted from one of the structures."));
            }
            // We are promoting a struct array to a struct array.
            // To do so, we have to make sure that the field names work out.
            // The only thing we must check for is that every field name
            // in fieldnames is present in fnames.
            int extraCount = 0;
            int matchCount = 0;
            indexType i;
            for (i = 0; i < (int)fNames.size(); i++) {
                int64 ndx = getFieldIndex(fNames[i]);
                if (ndx == -1) {
                    extraCount++;
                } else {
                    matchCount++;
                }
            }
            // Now, matchCount should be equal to the size of fieldNames
            if (matchCount != dp->fieldNames.size()) {
                Error(_W("Cannot combine structures with different fields if the "
                         "combination "
                         "requires fields to be deleted from one of the structures."));
            }
            void* dstPtr = allocateArrayOf(dp->dataClass, getLength(), fNames, false);
            const ArrayOf* src_rp = (const ArrayOf*)dp->getData();
            ArrayOf* dst_rp = (ArrayOf*)dstPtr;
            indexType elCount(getLength());
            indexType fieldCount(dp->fieldNames.size());
            indexType newFieldCount(fNames.size());
            ;
            // Now we have to copy our existing fields into the new order...
            for (i = 0; i < fieldCount; i++) {
                int64 newNdx = getFieldIndexFromList(dp->fieldNames[i], fNames);
                for (indexType j = 0; j < elCount; j++) {
                    dst_rp[j * newFieldCount + newNdx] = src_rp[j * fieldCount + i];
                }
            }
            dp = dp->putData(dp->dataClass, dp->dimensions, dstPtr, false, fNames);
            return;
        } else {
            Error(_W("Cannot convert struct-arrays to any other type."));
        }
    // Catch attempts to convert data types to reference types.
    if ((dstClass == NLS_STRING_ARRAY) || (dstClass == NLS_CELL_ARRAY)
        || (dstClass == NLS_STRUCT_ARRAY)) {
        Error(_W("Cannot convert base types to reference types."));
    }
    // Do nothing for promoting to same class (no-op).
    if (isSparse()) {
        dp = dp->putData(dstClass, dp->dimensions,
            TypeConvertSparseDynamicFunction(
                dp->dataClass, dp->dimensions[0], dp->dimensions[1], dp->getData(), dstClass),
            true);
        return;
    }
    if (dstClass == dp->dataClass) {
        return;
    }
    elCount = getLength();
    // We have to promote...
    dstPtr = allocateArrayOf(dstClass, elCount, stringVector(), true);
    indexType count = elCount;
    switch (dp->dataClass) {
    case NLS_CHAR: {
        charType* sp = (charType*)dp->getData();
        switch (dstClass) {
            caseMacro(NLS_LOGICAL, logical, qp[i] = (sp[i] == 0) ? 0 : 1);
            caseMacro(NLS_SINGLE, single, qp[i] = (single)sp[i]);
            caseMacro(NLS_DOUBLE, double, qp[i] = (double)sp[i]);
            caseMacro(NLS_SCOMPLEX, single, qp[i << 1] = (single)sp[i]);
            caseMacro(NLS_DCOMPLEX, double, qp[i << 1] = (double)sp[i]);
            caseMacro(NLS_UINT8, uint8, qp[i] = (uint8)sp[i]);
            caseMacro(NLS_INT8, int8, qp[i] = (int8)sp[i]);
            caseMacro(NLS_UINT16, uint16, qp[i] = (uint16)sp[i]);
            caseMacro(NLS_INT16, int16, qp[i] = (int16)sp[i]);
            caseMacro(NLS_UINT32, uint32, qp[i] = (uint32)sp[i]);
            caseMacro(NLS_INT32, int32, qp[i] = (int32)sp[i]);
            caseMacro(NLS_UINT64, uint64, qp[i] = (uint64)sp[i]);
            caseMacro(NLS_INT64, int64, qp[i] = (int64)sp[i]);
        default: {
        } break;
        }
    } break;
    case NLS_LOGICAL: {
        const logical* sp = (const logical*)dp->getData();
        switch (dstClass) {
            caseMacro(NLS_CHAR, charType, qp[i] = (charType)sp[i]);
            caseMacro(NLS_SINGLE, single, qp[i] = (single)sp[i]);
            caseMacro(NLS_DOUBLE, double, qp[i] = (double)sp[i]);
            caseMacro(NLS_SCOMPLEX, single, qp[i << 1] = (single)sp[i]);
            caseMacro(NLS_DCOMPLEX, double, qp[i << 1] = (double)sp[i]);
            caseMacro(NLS_UINT8, uint8, qp[i] = (uint8)sp[i]);
            caseMacro(NLS_INT8, int8, qp[i] = (int8)sp[i]);
            caseMacro(NLS_UINT16, uint16, qp[i] = (uint16)sp[i]);
            caseMacro(NLS_INT16, int16, qp[i] = (int16)sp[i]);
            caseMacro(NLS_UINT32, uint32, qp[i] = (uint32)sp[i]);
            caseMacro(NLS_INT32, int32, qp[i] = (int32)sp[i]);
            caseMacro(NLS_UINT64, uint64, qp[i] = (uint64)sp[i]);
            caseMacro(NLS_INT64, int64, qp[i] = (int64)sp[i]);
        default: {
        } break;
        }
    } break;
    case NLS_UINT8: {
        const uint8* sp = (const uint8*)dp->getData();
        switch (dstClass) {
            caseMacro(NLS_CHAR, charType, qp[i] = (charType)sp[i]);
            caseMacro(NLS_LOGICAL, logical, qp[i] = (sp[i] == 0) ? 0 : 1);
            caseMacro(NLS_SINGLE, single, qp[i] = (single)sp[i]);
            caseMacro(NLS_DOUBLE, double, qp[i] = (double)sp[i]);
            caseMacro(NLS_SCOMPLEX, single, qp[i << 1] = (single)sp[i]);
            caseMacro(NLS_DCOMPLEX, double, qp[i << 1] = (double)sp[i]);
        case NLS_INT8: {
            saturate<uint8, int8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT16: {
            saturate<uint8, uint16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT16: {
            saturate<uint8, int16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT32: {
            saturate<uint8, uint32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT32: {
            saturate<uint8, int32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT64: {
            saturate<uint8, uint64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT64: {
            saturate<uint8, int64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        default: {
        } break;
        }
    } break;
    case NLS_INT8: {
        const int8* sp = (const int8*)dp->getData();
        switch (dstClass) {
            caseMacro(NLS_CHAR, charType, qp[i] = (charType)sp[i]);
            caseMacro(NLS_LOGICAL, logical, qp[i] = (sp[i] == 0) ? 0 : 1);
            caseMacro(NLS_SINGLE, single, qp[i] = (single)sp[i]);
            caseMacro(NLS_DOUBLE, double, qp[i] = (double)sp[i]);
            caseMacro(NLS_SCOMPLEX, single, qp[i << 1] = (single)sp[i]);
            caseMacro(NLS_DCOMPLEX, double, qp[i << 1] = (double)sp[i]);
        case NLS_UINT8: {
            saturate<int8, uint8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT16: {
            saturate<int8, uint16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT16: {
            saturate<int8, int16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT32: {
            saturate<int8, uint32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT32: {
            saturate<int8, int32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT64: {
            saturate<int8, uint64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT64: {
            saturate<int8, int64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        default: {
        } break;
        }
    } break;
    case NLS_UINT16: {
        const uint16* sp = (const uint16*)dp->getData();
        switch (dstClass) {
            caseMacro(NLS_CHAR, charType, qp[i] = (charType)sp[i]);
            caseMacro(NLS_LOGICAL, logical, qp[i] = (sp[i] == 0) ? 0 : 1);
            caseMacro(NLS_SINGLE, single, qp[i] = (single)sp[i]);
            caseMacro(NLS_DOUBLE, double, qp[i] = (double)sp[i]);
            caseMacro(NLS_SCOMPLEX, single, qp[i << 1] = (single)sp[i]);
            caseMacro(NLS_DCOMPLEX, double, qp[i << 1] = (double)sp[i]);
        case NLS_UINT8: {
            saturate<uint16, uint8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT8: {
            saturate<uint16, int8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT16: {
            saturate<uint16, int16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT32: {
            saturate<uint16, uint32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT32: {
            saturate<uint16, int32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT64: {
            saturate<uint16, uint64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT64: {
            saturate<uint16, int64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        default: {
        } break;
        }
    } break;
    case NLS_INT16: {
        const int16* sp = (const int16*)dp->getData();
        switch (dstClass) {
            caseMacro(NLS_CHAR, charType, qp[i] = (charType)sp[i]);
            caseMacro(NLS_LOGICAL, logical, qp[i] = (sp[i] == 0) ? 0 : 1);
            caseMacro(NLS_SINGLE, single, qp[i] = (single)sp[i]);
            caseMacro(NLS_DOUBLE, double, qp[i] = (double)sp[i]);
            caseMacro(NLS_SCOMPLEX, single, qp[i << 1] = (single)sp[i]);
            caseMacro(NLS_DCOMPLEX, double, qp[i << 1] = (double)sp[i]);
        case NLS_UINT8: {
            saturate<int16, uint8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT8: {
            saturate<int16, int8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT16: {
            saturate<int16, uint16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT32: {
            saturate<int16, uint32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT32: {
            saturate<int16, int32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT64: {
            saturate<int16, uint64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT64: {
            saturate<int16, int64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        default: {
        } break;
        }
    } break;
    case NLS_UINT32: {
        const uint32* sp = (const uint32*)dp->getData();
        switch (dstClass) {
            caseMacro(NLS_CHAR, charType, qp[i] = (charType)sp[i]);
            caseMacro(NLS_LOGICAL, logical, qp[i] = (sp[i] == 0) ? 0 : 1);
            caseMacro(NLS_SINGLE, single, qp[i] = (single)sp[i]);
            caseMacro(NLS_DOUBLE, double, qp[i] = (double)sp[i]);
            caseMacro(NLS_SCOMPLEX, single, qp[i << 1] = (single)sp[i]);
            caseMacro(NLS_DCOMPLEX, double, qp[i << 1] = (double)sp[i]);
        case NLS_INT8: {
            saturate<uint32, int8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT8: {
            saturate<uint32, uint8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT16: {
            saturate<uint32, uint16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT16: {
            saturate<uint32, int16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT32: {
            saturate<uint32, int32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT64: {
            saturate<uint32, uint64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT64: {
            saturate<uint32, int64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        default: {
        } break;
        }
    } break;
    case NLS_INT32: {
        const int32* sp = (const int32*)dp->getData();
        switch (dstClass) {
            caseMacro(NLS_CHAR, charType, qp[i] = (charType)sp[i]);
            caseMacro(NLS_LOGICAL, logical, qp[i] = (sp[i] == 0) ? 0 : 1);
            caseMacro(NLS_SINGLE, single, qp[i] = (single)sp[i]);
            caseMacro(NLS_DOUBLE, double, qp[i] = (double)sp[i]);
            caseMacro(NLS_SCOMPLEX, single, qp[i << 1] = (single)sp[i]);
            caseMacro(NLS_DCOMPLEX, double, qp[i << 1] = (double)sp[i]);
        case NLS_UINT8: {
            saturate<int32, uint8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT8: {
            saturate<int32, int8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT16: {
            saturate<int32, uint16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT16: {
            saturate<int32, int16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT32: {
            saturate<int32, uint32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT64: {
            saturate<int32, uint64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT64: {
            saturate<int32, int64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        default: {
        } break;
        }
    } break;
    case NLS_INT64: {
        const int64* sp = (const int64*)dp->getData();
        switch (dstClass) {
            caseMacro(NLS_CHAR, charType, qp[i] = (charType)sp[i]);
            caseMacro(NLS_LOGICAL, logical, qp[i] = (sp[i] == 0) ? 0 : 1);
            caseMacro(NLS_SINGLE, single, qp[i] = (single)sp[i]);
            caseMacro(NLS_DOUBLE, double, qp[i] = (double)sp[i]);
            caseMacro(NLS_SCOMPLEX, single, qp[i << 1] = (single)sp[i]);
            caseMacro(NLS_DCOMPLEX, double, qp[i << 1] = (double)sp[i]);
        case NLS_UINT8: {
            saturate<int64, uint8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT8: {
            saturate<int64, int8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT16: {
            saturate<int64, uint16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT16: {
            saturate<int64, int16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT32: {
            saturate<int64, uint32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT32: {
            saturate<int64, int32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT64: {
            saturate<int64, uint64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        default: {
        } break;
        }
    } break;
    case NLS_UINT64: {
        const uint64* sp = (const uint64*)dp->getData();
        switch (dstClass) {
            caseMacro(NLS_CHAR, charType, qp[i] = (charType)sp[i]);
            caseMacro(NLS_LOGICAL, logical, qp[i] = (sp[i] == 0) ? 0 : 1);
            caseMacro(NLS_SINGLE, single, qp[i] = (single)sp[i]);
            caseMacro(NLS_DOUBLE, double, qp[i] = (double)sp[i]);
            caseMacro(NLS_SCOMPLEX, single, qp[i << 1] = (single)sp[i]);
            caseMacro(NLS_DCOMPLEX, double, qp[i << 1] = (double)sp[i]);
        case NLS_UINT8: {
            saturate<uint64, uint8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT8: {
            saturate<uint64, int8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT16: {
            saturate<uint64, uint16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT16: {
            saturate<uint64, int16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT32: {
            saturate<uint64, uint32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT32: {
            saturate<uint64, int32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT64: {
            saturate<uint64, int64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        default: {
        } break;
        }
    } break;
    case NLS_SINGLE: {
        const single* sp = (const single*)dp->getData();
        switch (dstClass) {
            caseMacro(NLS_CHAR, charType, qp[i] = (charType)sp[i]);
            caseMacro(NLS_LOGICAL, logical, qp[i] = (sp[i] == 0) ? 0 : 1);
            caseMacro(NLS_DOUBLE, double, qp[i] = (double)sp[i]);
            caseMacro(NLS_SCOMPLEX, single, qp[i << 1] = (single)sp[i]);
            caseMacro(NLS_DCOMPLEX, double, qp[i << 1] = (double)sp[i]);
        case NLS_UINT8: {
            saturate<single, uint8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT8: {
            saturate<single, int8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT16: {
            saturate<single, uint16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT16: {
            saturate<single, int16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT32: {
            saturate<single, uint32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT32: {
            saturate<single, int32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT64: {
            saturate<single, int64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT64: {
            saturate<single, uint64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        default: {
        } break;
        }
    } break;
    case NLS_DOUBLE: {
        const double* sp = (const double*)dp->getData();
        switch (dstClass) {
            caseMacro(NLS_CHAR, charType, qp[i] = (charType)sp[i]);
            caseMacro(NLS_LOGICAL, logical, qp[i] = (sp[i] == 0) ? 0 : 1);
            caseMacro(NLS_SINGLE, single, qp[i] = (single)sp[i]);
            caseMacro(NLS_SCOMPLEX, single, qp[i << 1] = (single)sp[i]);
            caseMacro(NLS_DCOMPLEX, double, qp[i << 1] = (double)sp[i]);
        case NLS_UINT8: {
            saturate<double, uint8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT8: {
            saturate<double, int8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT16: {
            saturate<double, uint16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT16: {
            saturate<double, int16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT32: {
            saturate<double, uint32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT32: {
            saturate<double, int32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT64: {
            saturate<double, int64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT64: {
            saturate<double, uint64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        default: {
        } break;
        }
    } break;
    case NLS_SCOMPLEX: {
        const single* sp = (const single*)dp->getData();
        switch (dstClass) {
            caseMacro(NLS_CHAR, charType, qp[i] = (charType)sp[i << 1]);
            caseMacro(NLS_LOGICAL, logical,
                qp[i] = ((sp[i << 1] == 0.0) && (sp[(i << 1) + 1] == 0.0)) ? 0 : 1);
        case NLS_SINGLE: {
            singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)sp);
            single* qp = (single*)dstPtr;
            Eigen::Map<Eigen::MatrixXcf> matA(Az, 1, dp->dimensions.getElementCount());
            Eigen::Map<Eigen::MatrixXf> matB(qp, 1, dp->dimensions.getElementCount());
            matB = matA.real();
        } break;
            caseMacro(NLS_DOUBLE, double, qp[i] = (double)sp[i << 1]);
            caseMacro(NLS_DCOMPLEX, double, {
                qp[i << 1] = (double)sp[i << 1];
                qp[(i << 1) + 1] = (double)sp[(i << 1) + 1];
            });
        case NLS_UINT8: {
            saturate<single, uint8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        default: {
        } break;
        }
    } break;
    case NLS_DCOMPLEX: {
        const double* sp = (const double*)dp->getData();
        switch (dstClass) {
            caseMacro(NLS_CHAR, charType, qp[i] = (charType)sp[i << 1]);
            caseMacro(NLS_LOGICAL, logical,
                qp[i] = ((sp[i << 1] == 0.0) && (sp[(i << 1) + 1] == 0.0)) ? 0 : 1);
            caseMacro(NLS_SINGLE, single, qp[i] = (single)sp[i << 1]);
        case NLS_DOUBLE: {
            doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)sp);
            double* qp = (double*)dstPtr;
            Eigen::Map<Eigen::MatrixXcd> matA(Az, 1, dp->dimensions.getElementCount());
            Eigen::Map<Eigen::MatrixXd> matB(qp, 1, dp->dimensions.getElementCount());
            matB = matA.real();
        } break;
            caseMacro(NLS_SCOMPLEX, single, {
                qp[i << 1] = (single)sp[i << 1];
                qp[(i << 1) + 1] = (single)sp[(i << 1) + 1];
            });
            caseMacro(NLS_UINT8, uint8, qp[i] = (uint8)sp[i << 1]);
            caseMacro(NLS_INT8, int8, qp[i] = (int8)sp[i << 1]);
            caseMacro(NLS_UINT16, uint16, qp[i] = (uint16)sp[i << 1]);
            caseMacro(NLS_INT16, int16, qp[i] = (int16)sp[i << 1]);
            caseMacro(NLS_UINT32, uint32, qp[i] = (uint32)sp[i << 1]);
            caseMacro(NLS_INT32, int32, qp[i] = (int32)sp[i << 1]);
            caseMacro(NLS_UINT64, uint64, qp[i] = (uint64)sp[i << 1]);
            caseMacro(NLS_INT64, int64, qp[i] = (int64)sp[i << 1]);
        default: {
        } break;
        }
    } break;
    }
    dp = dp->putData(dstClass, dp->dimensions, dstPtr);
}
//=============================================================================
void
ArrayOf::promoteType(Class dstClass)
{
    stringVector dummy;
    promoteType(dstClass, dummy);
}
//=============================================================================
}
//=============================================================================
