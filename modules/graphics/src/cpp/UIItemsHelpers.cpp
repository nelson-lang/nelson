//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include "UIItemsHelpers.h"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
scalarEquals(const ArrayOf& left, const ArrayOf& right)
{
    if ((left.isRowVectorCharacterArray() || left.isScalarStringArray())
        && (right.isRowVectorCharacterArray() || right.isScalarStringArray())) {
        return ArrayOf(left).asWideString() == ArrayOf(right).asWideString();
    }
    if (left.isNumeric() && left.isScalar() && right.isNumeric() && right.isScalar()) {
        return ArrayOf(left).asDoubleScalar() == ArrayOf(right).asDoubleScalar();
    }
    if (left.isLogical() && left.isScalar() && right.isNumeric() && right.isScalar()) {
        return ArrayOf(left).asDoubleScalar() == ArrayOf(right).asDoubleScalar();
    }
    return false;
}
//=============================================================================
int
uiItemsIndexOfValue(const ArrayOf& value, const wstringVector& items, const ArrayOf& itemsData)
{
    bool useItemsData = !itemsData.isEmpty();
    if (useItemsData) {
        if (itemsData.isCell()) {
            const ArrayOf* elements = (const ArrayOf*)itemsData.dataPointer();
            indexType count = std::min((indexType)items.size(), itemsData.elementCount());
            for (indexType k = 0; k < count; ++k) {
                if (scalarEquals(value, elements[k])) {
                    return (int)k + 1;
                }
            }
        } else if (itemsData.isNumeric() && value.isNumeric() && value.isScalar()) {
            ArrayOf asDouble(itemsData);
            asDouble.promoteType(NLS_DOUBLE);
            const double* values = (const double*)asDouble.dataPointer();
            double v = ArrayOf(value).asDoubleScalar();
            indexType count = std::min((indexType)items.size(), itemsData.elementCount());
            for (indexType k = 0; k < count; ++k) {
                if (values[k] == v) {
                    return (int)k + 1;
                }
            }
        }
        return 0;
    }
    if (value.isRowVectorCharacterArray() || value.isScalarStringArray()) {
        std::wstring text = ArrayOf(value).asWideString();
        for (size_t k = 0; k < items.size(); ++k) {
            if (items[k] == text) {
                return (int)k + 1;
            }
        }
    }
    return 0;
}
//=============================================================================
ArrayOf
uiItemsValueAtIndex(int index, const wstringVector& items, const ArrayOf& itemsData)
{
    if (index < 1) {
        return ArrayOf::characterArray(L"");
    }
    bool useItemsData = !itemsData.isEmpty();
    if (useItemsData) {
        if ((indexType)index <= itemsData.elementCount()) {
            if (itemsData.isCell()) {
                const ArrayOf* elements = (const ArrayOf*)itemsData.dataPointer();
                return elements[index - 1];
            }
            if (itemsData.isNumeric()) {
                ArrayOf asDouble(itemsData);
                asDouble.promoteType(NLS_DOUBLE);
                const double* values = (const double*)asDouble.dataPointer();
                return ArrayOf::doubleScalar(values[index - 1]);
            }
        }
        return ArrayOf::characterArray(L"");
    }
    if ((size_t)index <= items.size()) {
        return ArrayOf::characterArray(items[index - 1]);
    }
    return ArrayOf::characterArray(L"");
}
//=============================================================================
} // namespace Nelson
//=============================================================================
