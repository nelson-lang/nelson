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
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/algorithm/string.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isDrive(const std::wstring& pathname)
{
#ifdef _MSC_VER
    return boost::algorithm::ends_with(pathname, L":/")
        || boost::algorithm::ends_with(pathname, L":\\")
        || (boost::algorithm::ends_with(pathname, L":")
               && (!boost::algorithm::contains(pathname, L".")
                      && !boost::algorithm::contains(pathname, L"/")
                      && !boost::algorithm::contains(pathname, L"\\")));
#else
    return false;
#endif
}
//=============================================================================
std::wstring
NormalizePath(const std::wstring& path)
{
    std::wstring uniformizedPath;
    if (path.empty()) {
        return uniformizedPath;
    }
    uniformizedPath = path;
    if (isDrive(uniformizedPath)) {
        boost::replace_all(uniformizedPath, L"\\", L"/");
        if (uniformizedPath.back() != L'/') {
            uniformizedPath = uniformizedPath + L"/";
        }
    } else {
        boost::filesystem::path absPath = boost::filesystem::absolute(path);
        boost::filesystem::path::iterator it = absPath.begin();
        boost::filesystem::path result = *it++;
        for (; exists(result / *it) && it != absPath.end(); ++it) {
            result /= *it;
        }
        result = boost::filesystem::canonical(result);
        for (; it != absPath.end(); ++it) {
            if (*it == "..") {
                result = result.parent_path();
            } else if (*it != ".") {
                result /= *it;
            }
        }
        uniformizedPath = result.generic_wstring();
    }
    return uniformizedPath;
}
//=============================================================================
}
//=============================================================================
