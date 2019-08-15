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
#include <boost/filesystem.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <string>
#include "GetUserDirectory.hpp"
//=============================================================================
using namespace boost::filesystem;
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring userDir;
//=============================================================================
ArrayOf
UserDir()
{
    return ArrayOf::characterArrayConstructor(GetUserDirectory());
}
//=============================================================================
std::wstring
GetUserDirectory()
{
    if (userDir == L"") {
#ifdef _MSC_VER
        std::wstring str;
        wchar_t* buf = nullptr;
        size_t sz = 0;
        if (_wdupenv_s(&buf, &sz, L"USERPROFILE") == 0) {
            if (sz > 0) {
                str = buf;
            }
            free(buf);
            buf = nullptr;
        }
        path pwd = path(str.c_str());
#else
        char* home = getenv("HOME");
        path pwd = path(home);
#endif
        userDir = pwd.generic_wstring();
        if (!boost::algorithm::ends_with(userDir, L"\\")
            && (!boost::algorithm::ends_with(userDir, L"/"))) {
            userDir.append(L"/");
        }

    }
    return userDir;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
