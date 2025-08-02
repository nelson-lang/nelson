//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FullFile.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#if _MSC_VER
#define FILE_SEPW L"\\"
#else
#define FILE_SEPW L"/"
#endif
//=============================================================================
static std::wstring
FullFilename(const std::wstring& path)
{
    std::wstring result = path;
    bool haveSeparatorAtTheEnd
        = StringHelpers::ends_with(path, L"\\") || StringHelpers::ends_with(path, L"/");
#if _MSC_VER
    bool isUNC = StringHelpers::starts_with(path, L"\\\\");
    StringHelpers::replace_all(result, L"/", FILE_SEPW);
    StringHelpers::replace_all(result, L"\\.\\", L"");
    while (StringHelpers::contains(result, L"\\\\")) {
        StringHelpers::replace_all(result, L"\\\\", FILE_SEPW);
    }
    if (isUNC) {
        StringHelpers::replace_first(result, FILE_SEPW, L"\\\\");
    }
#else
    StringHelpers::replace_all(result, L"\\", FILE_SEPW);
    StringHelpers::replace_all(result, L"/./", L"");
    while (StringHelpers::contains(result, L"//")) {
        StringHelpers::replace_all(result, L"//", FILE_SEPW);
    }

#endif
    if (haveSeparatorAtTheEnd && !StringHelpers::ends_with(result, FILE_SEPW)) {
        result = result + FILE_SEPW;
    }
    return result;
}
//=============================================================================
std::wstring
FullFile(const wstringVector& parts)
{
    std::wstring result;
    indexType nbParts = parts.size();
    for (indexType k = 0; k < nbParts; ++k) {
        if (k == 0) {
            result = parts[k];
        } else {
            result = result + FILE_SEPW + parts[k];
        }
    }
    return FullFilename(result);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
