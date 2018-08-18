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
#include "FileFunc.hpp"
#include "characters_encoding.hpp"
#include <fstream>
#include <iosfwd>
//=============================================================================
namespace Nelson {
FileFunc::FileFunc(const std::wstring& directory, const std::wstring& name)
{
    _nlf_fullfilename = directory + L"/" + name + L".nlf";
    _name = name;
    std::ifstream inFile;
#ifdef _MSC_VER
    inFile.open(_nlf_fullfilename);
#else
    inFile.open(wstring_to_utf8(_nlf_fullfilename));
#endif
    if (inFile.is_open()) {
        std::string content = std::string(
            (std::istreambuf_iterator<char>(inFile)), (std::istreambuf_iterator<char>()));
        _hashid = std::hash<std::string>()(content);
        inFile.close();
    } else {
        _hashid = 0;
    }
}
//=============================================================================
FileFunc::~FileFunc()
{
    _nlf_fullfilename = L"";
    _name = L"";
    _hashid = 0;
}
//=============================================================================
std::wstring
FileFunc::getFilename()
{
    return _nlf_fullfilename;
}
//=============================================================================
std::wstring
FileFunc::getName()
{
    return _name;
}
//=============================================================================
size_t
FileFunc::getHashID()
{
    return _hashid;
}
//=============================================================================
}
//=============================================================================
