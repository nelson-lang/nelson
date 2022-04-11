//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
