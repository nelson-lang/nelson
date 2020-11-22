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
#include "FileParts.hpp"
#include <boost/filesystem.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
FilePartsPath(const std::wstring& fullpath)
{
    std::wstring res;
    boost::filesystem::path pathToSplit = fullpath;
    if (pathToSplit.has_parent_path()) {
        res = pathToSplit.parent_path().generic_wstring();
    }
    return res;
}
//=============================================================================
std::wstring
FilePartsFilename(const std::wstring& fullpath)
{
    std::wstring res;
    boost::filesystem::path pathToSplit = fullpath;
    if (pathToSplit.has_filename()) {
        res = pathToSplit.stem().generic_wstring();
    }
    if (res == L".") {
        res = L"";
    }
    if (res == L"/" || res == L"\\") {
        res = L"";
    }
    return res;
}
//=============================================================================
std::wstring
FilePartsExtension(const std::wstring& fullpath)
{
    std::wstring res;
    boost::filesystem::path pathToSplit = fullpath;
    if (pathToSplit.has_extension()) {
        res = pathToSplit.extension().generic_wstring();
    }
    return res;
}
//=============================================================================
void
FileParts(const std::wstring& fullpath, std::wstring& path, std::wstring& filename,
    std::wstring& extension)
{
    path = FilePartsPath(fullpath);
    filename = FilePartsFilename(fullpath);
    if (path == L"" && (filename.size() > 1 && filename[1] == L':')) {
        path = filename;
        filename = L"";
    }
    extension = FilePartsExtension(fullpath);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
