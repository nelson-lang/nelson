//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <filesystem>
#include <algorithm>
#include "FileParts.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static inline std::wstring
replaceAll(std::wstring str, const std::wstring& from, const std::wstring& to)
{
    size_t start_pos = 0;
    while ((start_pos = str.find(from, start_pos)) != std::wstring::npos) {
        str.replace(start_pos, from.length(), to);
        start_pos += to.length();
    }
    return str;
}
//=============================================================================
std::wstring
FilePartsPath(const std::wstring& fullpath)
{
    std::wstring res;
    std::filesystem::path pathToSplit = fullpath;
    if (pathToSplit.has_parent_path()) {
        res = pathToSplit.parent_path().generic_wstring();
    }
    res = replaceAll(res, L"//", L"/");
    return res;
}
//=============================================================================
std::wstring
FilePartsFilename(const std::wstring& fullpath)
{
    std::wstring res;
    std::filesystem::path pathToSplit = fullpath;
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
    std::wstring::size_type idxDot = fullpath.rfind(L'.');
    std::wstring::size_type idxSlash1 = fullpath.rfind(L'/');
    std::wstring::size_type idxSlash2 = fullpath.rfind(L'\\');
    std::wstring extension;
    if (idxSlash1 != std::wstring::npos || idxSlash2 != std::wstring::npos) {
        std::wstring::size_type idxSlash;
        if (idxSlash1 != std::wstring::npos && idxSlash2 != std::wstring::npos) {
            idxSlash = std::max(idxSlash1, idxSlash2);
        } else {
            if (idxSlash1 != std::wstring::npos) {
                idxSlash = idxSlash1;
            } else {
                idxSlash = idxSlash2;
            }
        }
        if (idxDot != std::wstring::npos && idxDot >= idxSlash + 1) {
            extension.append(fullpath.begin() + idxDot, fullpath.end());
        }
    } else {
        if (idxDot != std::wstring::npos) {
            extension.append(fullpath.begin() + idxDot, fullpath.end());
        }
    }
    return extension;
}
//=============================================================================
void
FileParts(const std::wstring& fullpath, std::wstring& path, std::wstring& filename,
    std::wstring& extension)
{
    extension = FilePartsExtension(fullpath);
    std::wstring fullpathWithoutExtention;
    fullpathWithoutExtention.append(fullpath.begin(), fullpath.end() - extension.length());
    filename = FilePartsFilename(fullpathWithoutExtention);
    path = FilePartsPath(fullpathWithoutExtention);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
