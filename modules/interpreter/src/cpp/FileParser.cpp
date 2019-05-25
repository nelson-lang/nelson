//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
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
#include "FileParser.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring wfilename;
//=============================================================================
void
setParserFilename(const std::string& filename)
{
    wfilename = utf8_to_wstring(filename);
}
//=============================================================================
void
setParserFilename(const std::wstring& filename)
{
    wfilename = filename;
}
//=============================================================================
std::string
getParserFilenameU()
{
    return wstring_to_utf8(wfilename);
}
//=============================================================================
std::wstring
getParserFilenameW()
{
    return wfilename;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
