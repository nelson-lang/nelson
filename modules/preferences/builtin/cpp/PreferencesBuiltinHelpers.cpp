//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "PreferencesBuiltinHelpers.hpp"
#include <cwctype>
#include "ArrayOfView.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson::PreferencesGateway {
//=============================================================================
ArrayOf
makeCellArray(const ArrayOfVector& values, const Dimensions& dimensions)
{
    ArrayOf* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, dimensions.elementCount(), stringVector(), false));
    for (indexType k = 0; k < dimensions.elementCount(); ++k) {
        elements[k] = values[k];
    }
    return ArrayOf(NLS_CELL_ARRAY, dimensions, elements);
}
//=============================================================================
ArrayOf
makeLogicalArray(const std::vector<bool>& values, const Dimensions& dimensions)
{
    logical* elements = static_cast<logical*>(
        ArrayOf::allocateArrayOf(NLS_LOGICAL, dimensions.elementCount(), stringVector(), false));
    for (indexType k = 0; k < dimensions.elementCount(); ++k) {
        elements[k] = values[k] ? 1 : 0;
    }
    return ArrayOf(NLS_LOGICAL, dimensions, elements);
}
//=============================================================================
ArrayOfVector
valuesArgumentToVector(
    const Nelson::Preferences::PreferenceNames& preferences, const ArrayOf& value)
{
    if (preferences.scalar) {
        ArrayOfVector values;
        values << value;
        return values;
    }
    if (!value.isCell() || value.elementCount() != preferences.names.size()) {
        Error(L"Nelson:preferences:valueCellArrayExpected",
            L"Value must be a cell array with one value for each preference.");
    }
    ArrayOfView<ArrayOf> cells(value, ArrayOfViewAssumeValidated {});
    ArrayOfVector values;
    for (indexType k = 0; k < value.elementCount(); ++k) {
        values << cells[k];
    }
    return values;
}
//=============================================================================
std::wstring
lowerPreferenceText(const std::wstring& text)
{
    std::wstring result = text;
    for (wchar_t& ch : result) {
        ch = static_cast<wchar_t>(std::towlower(ch));
    }
    return result;
}
//=============================================================================
bool
isAskPreferenceValue(const ArrayOf& value)
{
    if (!(value.isRowVectorCharacterArray() || value.isScalarStringArray())) {
        return false;
    }
    return lowerPreferenceText(value.asWideString()) == L"ask";
}
//=============================================================================
} // namespace Nelson::PreferencesGateway
//=============================================================================
