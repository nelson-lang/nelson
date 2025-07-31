//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <string>
#include "FileParts.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
FilePartsPath(const std::wstring& fullpath)
{
    std::wstring path = L"";
    std::wstring filename = L"";
    std::wstring extension = L"";
    FileParts(fullpath, path, filename, extension);
    return path;
}
//=============================================================================
std::wstring
FilePartsFilename(const std::wstring& fullpath)
{
    std::wstring path = L"";
    std::wstring filename = L"";
    std::wstring extension = L"";
    FileParts(fullpath, path, filename, extension);
    return filename;
}
//=============================================================================
std::wstring
FilePartsExtension(const std::wstring& fullpath)
{
    std::wstring path = L"";
    std::wstring filename = L"";
    std::wstring extension = L"";
    FileParts(fullpath, path, filename, extension);
    return extension;
}
//=============================================================================
static inline size_t
findLastFileSeparator(const std::wstring& fullpath)
{
    size_t indexSlash = fullpath.rfind(L'/');
    size_t indexBackslash = fullpath.rfind(L'\\');
    size_t indexFileSeparator = std::wstring::npos;

    if (indexSlash != std::wstring::npos && indexBackslash != std::wstring::npos) {
        indexFileSeparator = std::max(indexSlash, indexBackslash);
    } else {
        if (indexSlash != std::wstring::npos) {
            indexFileSeparator = indexSlash;

        } else {
            indexFileSeparator = indexBackslash;
        }
    }
    return indexFileSeparator;
}
//=============================================================================
void
FileParts(const std::wstring& fullpath, std::wstring& path, std::wstring& filename,
    std::wstring& extension)
{
    path = L"";
    filename = L"";
    extension = L"";

    size_t indexFileSeparator = findLastFileSeparator(fullpath);
    size_t indexDot = fullpath.rfind(L'.');

    std::wstring fullpathWithoutExtension;
    if (indexDot != std::wstring::npos) {
        if (indexFileSeparator != std::wstring::npos) {
            if (indexDot > indexFileSeparator) {
                fullpathWithoutExtension = L"";
                extension.append(fullpath.begin() + indexDot, fullpath.end());
                fullpathWithoutExtension.append(fullpath.begin(), fullpath.begin() + indexDot);
            } else {
                extension = L"";
                fullpathWithoutExtension = fullpath;
            }
        } else {
            path = L"";
            extension.append(fullpath.begin() + indexDot, fullpath.end());
            filename.append(fullpath.begin(), fullpath.begin() + indexDot);
            return;
        }
    } else {
        extension = L"";
        if (indexFileSeparator != std::wstring::npos) {
            path.append(fullpath.begin(), fullpath.begin() + indexFileSeparator);
            filename.append(fullpath.begin() + indexFileSeparator + 1, fullpath.end());
#ifdef _MSC_VER
            if (filename.empty() && path.length() > 1 && (path.back() == L':')) {
                path = fullpath;
                filename = L"";
            }
#endif
        } else {
            path = L"";
            filename = fullpath;
#ifdef _MSC_VER
            if (filename.length() > 1 && (filename.back() == L':')) {
                path = fullpath;
                filename = L"";
            }
#endif
        }
        return;
    }
    if (indexFileSeparator != std::wstring::npos) {
        if (indexFileSeparator == 0) {
            path.append(fullpathWithoutExtension.begin(), fullpathWithoutExtension.begin() + 1);
            filename.append(fullpathWithoutExtension.begin() + 1, fullpathWithoutExtension.end());
        } else {
            path.append(fullpathWithoutExtension.begin(),
                fullpathWithoutExtension.begin() + indexFileSeparator);
            filename.append(fullpathWithoutExtension.begin() + indexFileSeparator + 1,
                fullpathWithoutExtension.end());
        }
    } else {
        path = L"";
        filename = fullpathWithoutExtension;
    }
#ifdef _MSC_VER
    if (path.empty() && extension.empty() && (filename.length() > 1) && (filename.back() == L':')) {
        path = filename;
        filename = L"";
        extension = L"";
    }
#endif
}
//=============================================================================
} // namespace Nelson
//=============================================================================
