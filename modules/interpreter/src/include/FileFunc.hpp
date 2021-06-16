//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
enum class FileFunctionType
{
    M = 0,
    NLF = 1,
    NLS = 2,
    UNKNOWN = 3
};
//=============================================================================
class FileFunc
{
private:
    std::wstring _fullfilename;
    std::wstring _name;
    size_t _hashid;
    FileFunctionType _ftype;

public:
    FileFunc(const std::wstring& directory, const std::wstring& name, FileFunctionType ftype);
    ~FileFunc();
    std::wstring
    getFilename();
    std::wstring
    getName();
    size_t
    getHashID();
    FileFunctionType
    getFileFunctionType();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
