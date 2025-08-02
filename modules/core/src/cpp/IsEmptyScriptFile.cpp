//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
//=============================================================================
#include "IsEmptyScriptFile.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
bool
IsEmptyScriptFile(const std::wstring& filename)
{
    FILE* fr;
#ifdef _MSC_VER
    fr = _wfopen(filename.c_str(), L"rt");
#else
    fr = fopen(wstring_to_utf8(filename).c_str(), "rt");
#endif
    if (fr != nullptr) {
        int ch;
        while (ch = getc(fr)) {
            if (ch == EOF) {
                break;
            }
            bool isCharManaged = (ch == ' ') || (ch == '\r') || (ch == '\n');
            if (!isCharManaged) {
                fclose(fr);
                return false;
            }
        }
        fclose(fr);
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
