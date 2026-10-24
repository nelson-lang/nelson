//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "msgboxBuiltin.hpp"
#include "MessageBoxBuiltinHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static std::wstring
messageTextFromArray(const ArrayOf& value);
//=============================================================================
static bool
isSupportedValue(const wstringVector& values, const std::wstring& value);
//=============================================================================
ArrayOfVector
msgboxBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 4);
    std::wstring text = messageTextFromArray(argIn[0]);
    std::wstring title;
    std::wstring icon;
    std::wstring mode;
    if (argIn.size() > 1) {
        std::wstring titleOrMode = argIn[1].asWideString();
        isSupportedValue(getSupportedMessageBoxModes(), titleOrMode) ? mode = titleOrMode
                                                                     : title = titleOrMode;
    }
    if (argIn.size() > 2) {
        std::wstring iconOrMode = argIn[2].asWideString();
        bool isMode = isSupportedValue(getSupportedMessageBoxModes(), iconOrMode);
        bool isIcon = isSupportedValue(getSupportedMessageBoxIcons(), iconOrMode);
        if (!isMode && !isIcon) {
            Error(L"Nelson:msgbox:invalidIcon",
                L"Wrong value for #%d argument. A valid icon or mode expected.", 3);
        }
        if (isMode) {
            mode = iconOrMode;
        }
        if (isIcon) {
            icon = iconOrMode;
        }
    }
    if (argIn.size() > 3) {
        mode = argIn[3].asWideString();
        if (!isSupportedValue(getSupportedMessageBoxModes(), mode)) {
            Error(L"Nelson:msgbox:invalidMode",
                L"Wrong value for #%d argument. A valid mode expected.", 4);
        }
    }
    retval << MessageBox(text, title, icon, mode);
    return retval;
}
//=============================================================================
static std::wstring
messageTextFromArray(const ArrayOf& value)
{
    if (value.isRowVectorCharacterArray()) {
        return value.asWideString();
    }
    if (!value.isStringArray() && !value.isCellArrayOfCharacterVectors()) {
        Error(L"Nelson:msgbox:invalidType", L"char, cell, or string expected.");
    }
    wstringVector lines
        = value.isStringArray() ? value.asWideStringVector(false) : value.asWideStringVector();
    std::wstring text = lines.empty() ? L"" : lines[0];
    for (size_t k = 1; k < lines.size(); ++k) {
        text += L"\n";
        text += lines[k];
    }
    return text;
}
//=============================================================================
static bool
isSupportedValue(const wstringVector& values, const std::wstring& value)
{
    return std::find(values.begin(), values.end(), value) != values.end();
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
