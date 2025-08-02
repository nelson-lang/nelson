//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOUIControl.h"
//=============================================================================
namespace Nelson {
//=============================================================================
void
GOUIControl::tokenize(
    const std::wstring& str, std::vector<std::wstring>& tokens, const std::wstring& delimiters)
{
    std::wstring::size_type lastPos = str.find_first_not_of(delimiters, 0);
    std::wstring::size_type pos = str.find_first_of(delimiters, lastPos);

    while (std::wstring::npos != pos || std::wstring::npos != lastPos) {
        tokens.push_back(str.substr(lastPos, pos - lastPos));
        lastPos = str.find_first_not_of(delimiters, pos);
        pos = str.find_first_of(delimiters, lastPos);
    }
}
//=============================================================================
}
//=============================================================================
