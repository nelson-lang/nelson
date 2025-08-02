//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <vector>
#include "SearchVariableEnvironment.hpp"
#include "GetVariableEnvironment.hpp"
#include "characters_encoding.hpp"
#include "FileSystemWrapper.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
wstringVector
splitEnvironmentPath(const std::wstring& envPaths)
{
    wstringVector result;
    size_t previous = 0;
#ifdef _MSC_VER
    const wchar_t delimiter = L';';
#else
    const wchar_t delimiter = L':';
#endif
    size_t index = envPaths.find(delimiter);
    while (index != std::string::npos) {
        std::wstring s = envPaths.substr(previous, index - previous);
        if (s != L"") {
            result.push_back(s);
        }
        previous = index + 1;
        index = envPaths.find(delimiter, previous);
    }
    std::wstring s = envPaths.substr(previous);
    if (s != L"") {
        result.push_back(s);
    }
    return result;
}
//=============================================================================
wstringVector
SearchVariableEnvironmentW(const std::wstring& fileToSearch, const std::wstring& envVarName)
{
    wstringVector res;
    std::wstring envValue = GetVariableEnvironment(envVarName, L"");
    wstringVector envValuevector = splitEnvironmentPath(envValue);
    for (auto& k : envValuevector) {
        FileSystemWrapper::Path fullpath(k);
        fullpath /= fileToSearch;
        if (fullpath.is_regular_file()) {
            res.push_back(fullpath.generic_wstring());
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
