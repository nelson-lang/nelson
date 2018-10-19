//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
//=============================================================================
#include "IsEmptyScriptFile.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
bool
IsEmptyScriptFile(std::wstring filename)
{
    FILE* fr;
#ifdef _MSC_VER
    fr = _wfopen(filename.c_str(), L"rt");
#else
    fr = fopen(wstring_to_utf8(filename).c_str(), "rt");
#endif
    if (fr) {
        int ch;
        while (EOF != (ch = getc(fr))) {
            bool isCharManaged = (ch == ' ') || (ch == '\r') || (ch == '\n');
            if (!isCharManaged) {
                fclose(fr);
                return false;
            }
        }
        fclose(fr);
        return true;
    } else {
        return true;
    }
    return false;
}
}
//=============================================================================
