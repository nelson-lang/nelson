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
#include "FindDynamicLibraryName.hpp"
#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
FindDynamicLibraryName(
    const std::wstring& directoryName, const std::wstring& initialLibraryName, bool bCaseSensitive)
{
    std::wstring res;
    boost::filesystem::directory_iterator end_iter;
    boost::filesystem::path dir = directoryName;
    if (!boost::filesystem::is_directory(dir)) {
        return res;
    }
    boost::filesystem::path fullfilename = directoryName;
    fullfilename /= initialLibraryName;
    bool bRes = false;
    try {
        bRes = boost::filesystem::exists(fullfilename)
            && !boost::filesystem::is_directory(fullfilename);
    } catch (const boost::filesystem::filesystem_error& e) {
        if (e.code() == boost::system::errc::permission_denied) {
            // ONLY FOR DEDUG
        }
        bRes = false;
    }
    if (bRes) {
        res = initialLibraryName;
        return res;
    }
    for (boost::filesystem::directory_iterator dir_iter(dir); dir_iter != end_iter; ++dir_iter) {
        boost::filesystem::path current = dir_iter->path();
        if (boost::filesystem::is_regular_file(current)) {
            if (bCaseSensitive) {
                if (initialLibraryName.compare(current.generic_wstring()) == 0) {
                    return current.generic_wstring();
                }
            } else {
                std::wstring currentfilename = current.filename().generic_wstring();
                if (boost::iequals(initialLibraryName, currentfilename)) {
                    return currentfilename;
                }
            }
        }
    }
    return res;
}
//=============================================================================
}
//=============================================================================
