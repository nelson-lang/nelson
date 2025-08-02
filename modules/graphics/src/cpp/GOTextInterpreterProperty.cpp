//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOTextInterpreterProperty.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* TEXT_INTERPRETER_DICT[5]
    = { GO_PROPERTY_VALUE_TEX_STR, GO_PROPERTY_VALUE_NONE_STR, 0 };
//=============================================================================
GOTextInterpreterProperty::GOTextInterpreterProperty()
    : GORestrictedStringProperty(TEXT_INTERPRETER_DICT)
{
}
//=============================================================================
TEXT_INTERPRETER_FORMAT
GOTextInterpreterProperty::getAsEnum()
{
    std::wstring value = this->data();
    if (value == GO_PROPERTY_VALUE_TEX_STR) {
        return TEXT_INTERPRETER_FORMAT::TEX_MARKUP;
    }
    return TEXT_INTERPRETER_FORMAT::NONE;
}
//=============================================================================
}
//=============================================================================
