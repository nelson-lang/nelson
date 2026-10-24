//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlBuiltinHelpers.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include <algorithm>
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson::XmlGateway {
//=============================================================================
std::string
normalizedXmlNameValueName(const ArrayOf& name)
{
    bool isName = name.isScalarStringArray() || name.isRowVectorCharacterArray();
    if (!isName) {
        Error(L"Nelson:xml:invalidParameterName",
            L"Invalid parameter name. The parameter name must be a nonempty string or character "
            L"array.");
    }
    std::string value = wstring_to_utf8(name.asWideString());
    std::transform(
        value.begin(), value.end(), value.begin(), [](unsigned char c) { return std::tolower(c); });
    return value;
}
//=============================================================================
std::string
xmlOptionString(const ArrayOf& value)
{
    std::string text = wstring_to_utf8(value.asWideString());
    std::transform(
        text.begin(), text.end(), text.begin(), [](unsigned char c) { return std::tolower(c); });
    return text;
}
//=============================================================================
} // namespace Nelson::XmlGateway
//=============================================================================
