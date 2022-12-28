//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
GOStringVector::get()
{
    if (_data.size() == 0) {
        return ArrayOf::characterArrayConstructor("");
    }
    std::wstring retval;
    for (unsigned i = 0; i < _data.size() - 1; i++) {
        retval.append(_data[i]);
        retval.append(L"|");
    }
    retval.append(_data.back());
    return ArrayOf::characterArrayConstructor(retval);
}
//=============================================================================
void
GOStringVector::set(ArrayOf arg)
{
    if (!arg.isRowVectorCharacterArray()) {
        Error(_W("Expecting a '|' - delimited list of strings for property argument."));
    }
    GOGenericProperty::set(arg);
    std::wstring args(arg.getContentAsWideString());
    _data.clear();
    Tokenize(args, _data, L"|");
}
//=============================================================================
std::vector<std::wstring>
GOStringVector::data()
{
    return _data;
}
//=============================================================================
void
GOStringVector::data(std::vector<std::wstring> m)
{
    _data = m;
}
//=============================================================================
std::wstring
GOStringVector::toWideString()
{
    return L"<>";
}
//=============================================================================
};
//=============================================================================
