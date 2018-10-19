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
#include "FileStream.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
FileStream::FileStream(const std::wstring filename, const std::wstring accessmode)
{
#ifdef _MSC_VER
    fp = _wfopen(filename.c_str(), accessmode.c_str());
#else
    fp = fopen(wstring_to_utf8(filename).c_str(), wstring_to_utf8(accessmode).c_str());
#endif
    if (fp == NULL) {
        Error(_W("Unable to open file ") + filename);
    }
    autoclose = true;
}
//=============================================================================
FileStream::FileStream(FILE* afp)
{
    fp = afp;
    autoclose = false;
}
//=============================================================================
FileStream::~FileStream()
{
    if (autoclose) {
        fclose(fp);
    }
}
//=============================================================================
void
FileStream::writeBytes(const void* data, int len)
{
    if (fp) {
        fwrite(data, sizeof(char), len, fp);
    }
}
//=============================================================================
void
FileStream::readBytes(void* data, int len)
{
    if (fp) {
        fread(data, sizeof(char), len, fp);
    }
}
//=============================================================================
}
//=============================================================================
