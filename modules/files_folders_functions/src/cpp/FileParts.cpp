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
    if (res.compare(L".") == 0) {
        res = L"";
    }
    if (res.compare(L"/") == 0 || res.compare(L"\\") == 0) {
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
    if (path.compare(L"") == 0 && (filename.size() > 1 && filename[1] == L':')) {
        path = filename;
        filename = L"";
    }
    extension = FilePartsExtension(fullpath);
}
//=============================================================================
}
//=============================================================================
