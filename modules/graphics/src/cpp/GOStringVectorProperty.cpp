//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOStringVectorProperty.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "GOHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
GOStringVectorProperty::get()
{
    if (_returnAsStringRow) {
        Dimensions dims(1, _data.size());
        return ArrayOf::stringArray(_data, dims);
    }
    if (_data.empty() && _emptyReturnsChar) {
        return ArrayOf::characterArray(std::wstring());
    }
    if (_data.size() == 1 && !_returnAsCell) {
        return ArrayOf::characterArray(_data[0]);
    }
    Dimensions dims(_data.size(), 1);
    if (_returnAsRowCell) {
        dims = Dimensions(1, _data.size());
    }
    ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, _data.size());
    ArrayOf resultAsCell = ArrayOf(NLS_CELL_ARRAY, dims, elements);
    for (indexType k = 0; k < dims.elementCount(); k++) {
        elements[k] = ArrayOf::characterArray(_data[k]);
    }
    return resultAsCell;
}
//=============================================================================
void
GOStringVectorProperty::set(ArrayOf arg)
{
    _data.clear();
    ArrayOf _arg = uniformizeStringVector(arg, _data, _pipeDelimited);
    GOGenericProperty::set(_arg);
}
//=============================================================================
std::vector<std::wstring>
GOStringVectorProperty::data()
{
    return _data;
}
//=============================================================================
void
GOStringVectorProperty::data(const std::wstring& m)
{
    _data = { m };
}
//=============================================================================
void
GOStringVectorProperty::data(const std::vector<std::wstring>& m)
{
    _data = m;
}
//=============================================================================
std::wstring
GOStringVectorProperty::toWideString()
{
    if (_data.size() == 1) {
        return _data[0];
    }
    return L"{1x" + std::to_wstring(_data.size()) + L" cell}";
}
//=============================================================================
} // namespace Nelson
//=============================================================================
