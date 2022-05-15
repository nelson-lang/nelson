//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "msgboxBuiltin.hpp"
#include "Error.hpp"
#include "MessageBox.hpp"
#include "IsCellOfStrings.hpp"
#include "Warning.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GuiGateway::msgboxBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 4);
    std::wstring text;
    std::wstring title;
    std::wstring icon;
    std::wstring mode;

    ArrayOf param1 = argIn[0];
    if (param1.isRowVectorCharacterArray()) {
        text = param1.getContentAsWideString();
    } else if (param1.isStringArray()) {
        wstringVector lines = param1.getContentAsWideStringVector(false);
        text = lines[0];
        for (size_t k = 1; k < lines.size(); ++k) {
            text += L"\n";
            text += lines[k];
        }
    } else if (IsCellOfString(param1)) {
        wstringVector lines = param1.getContentAsWideStringVector();
        text = lines[0];
        for (size_t k = 1; k < lines.size(); ++k) {
            text += L"\n";
            text += lines[k];
        }
    } else {
        Error(_W("char, cell, or string expected."));
    }
    if (argIn.size() > 1) {
        ArrayOf param2 = argIn[1];
        std::wstring titleOrMode = param2.getContentAsWideString();
        wstringVector supportedModes = getSupportedMessageBoxModes();
        bool isMode = std::find(supportedModes.begin(), supportedModes.end(), titleOrMode)
            != supportedModes.end();
        isMode ? mode = titleOrMode : title = titleOrMode;
    }
    if (argIn.size() > 2) {
        ArrayOf param3 = argIn[2];
        std::wstring iconOrMode = param3.getContentAsWideString();
        wstringVector supportedModes = getSupportedMessageBoxModes();
        wstringVector supportedIcons = getSupportedMessageBoxIcons();
        bool isMode = std::find(supportedModes.begin(), supportedModes.end(), iconOrMode)
            != supportedModes.end();
        bool isIcon = std::find(supportedIcons.begin(), supportedIcons.end(), iconOrMode)
            != supportedIcons.end();
        if (isMode || isIcon) {
            if (isMode) {
                mode = iconOrMode;
            }
            if (isIcon) {
                icon = iconOrMode;
            }
        } else {
            Warning(_W("Invalid character vector for icon in 'msgbox'."));
        }
    }
    if (argIn.size() > 3) {
        ArrayOf param4 = argIn[3];
        mode = param4.getContentAsWideString();
    }

    retval << MessageBox(text, title, icon, mode);
    return retval;
}
//=============================================================================
