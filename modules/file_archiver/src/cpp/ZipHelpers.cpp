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
#ifdef _MSC_VER
#include <sys/utime.h>
#include <windows.h>
#else
#include <utime.h>
#endif
#include <ctime>
#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/filesystem/convenience.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/regex.hpp>

#include "Zipper.hpp"
#include <mz_os.h>
#include <fstream>
#include "ZipHelpers.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
isExistingDirectory(const std::wstring& name)
{
    boost::filesystem::path p = name;
    bool res = false;
    try {
        res = boost::filesystem::is_directory(p);
    } catch (const boost::filesystem::filesystem_error&) {
        res = false;
    }
    return res;
}
//=============================================================================
bool
isExistingFile(const std::wstring& name)
{
    boost::filesystem::path p = name;
    bool res = false;
    try {
        res = boost::filesystem::is_regular_file(p);
    } catch (const boost::filesystem::filesystem_error&) {
        res = false;
    }
    return res;
}
//=============================================================================
std::wstring
normalizePath(const std::wstring& path)
{
    boost::filesystem::path p = path;
    p = p.generic_path().lexically_normal();
    if (boost::algorithm::starts_with(p.wstring(), L"./")) {
        p = p.wstring().substr(2);
    }
    return p.generic_wstring();
}
//=============================================================================
std::wstring
getRootPath(const std::wstring& rootpath)
{
    boost::filesystem::path p;
    if (rootpath == L".") {
        p = boost::filesystem::current_path();
    } else {
        p = rootpath;
    }
    return normalizePath(p.wstring());
}
//=============================================================================
} // namespace Nelson
//=============================================================================
