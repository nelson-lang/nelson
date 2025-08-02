//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <string>
#include "FileSystemWrapper.hpp"
#include "GetUserDirectory.hpp"
#include "StringHelpers.hpp"
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
        FileSystemWrapper::Path pwd { str };
#else
        char* home = getenv("HOME");
        FileSystemWrapper::Path pwd { std::string(home) };
#endif
        userDir = pwd.getFinalPathname().generic_wstring();
        if (!StringHelpers::ends_with(userDir, L"\\")
            && (!StringHelpers::ends_with(userDir, L"/"))) {
            userDir.append(L"/");
        }
    }
    return userDir;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
