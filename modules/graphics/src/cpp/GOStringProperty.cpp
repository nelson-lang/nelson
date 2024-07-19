//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOStringProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
GOStringProperty::data()
{
    return _data;
}
//=============================================================================
void
GOStringProperty::data(const std::wstring& m)
{
    _data = m;
    setModified(true);
}
//=============================================================================
bool
GOStringProperty::isEqual(const std::wstring& m)
{
    return (_data == m);
}
//=============================================================================
std::wstring
GOStringProperty::toWideString()
{
    return L"'" + _data + L"'";
}
//=============================================================================
ArrayOf
GOStringProperty::get()
{
    return ArrayOf::characterArrayConstructor(_data);
}
//=============================================================================
void
GOStringProperty::set(ArrayOf arg)
{
    GOGenericProperty::set(arg);
    _data = arg.getContentAsWideString();
}
//=============================================================================
}
//=============================================================================
