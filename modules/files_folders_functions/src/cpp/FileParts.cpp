//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FileParts.hpp"
#include <boost/filesystem.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
FilePartsPath(const std::wstring& fullpath)
{
    std::wstring res;
    boost::filesystem::path pathToSplit = fullpath;
    if (pathToSplit.has_parent_path()) {
        res = pathToSplit.parent_path().generic_wstring();
    }
    if (res.length() > 1 && res.back() == L':') {
        res = res + L"/";
    }
    return res;
}
//=============================================================================
std::wstring
FilePartsFilename(const std::wstring& fullpath)
{
    std::wstring res;
    boost::filesystem::path pathToSplit = fullpath;
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
    std::wstring res;
    boost::filesystem::path pathToSplit = fullpath;
    if (pathToSplit.has_extension()) {
        res = pathToSplit.extension().generic_wstring();
    }
    return res;
}
//=============================================================================
void
FileParts(const std::wstring& fullpath, std::wstring& path, std::wstring& filename,
    std::wstring& extension)
{
    path = FilePartsPath(fullpath);
    filename = FilePartsFilename(fullpath);
    if (path == L"" && (filename.size() > 1 && filename[1] == L':')) {
        path = filename;
        filename = L"";
    }
    extension = FilePartsExtension(fullpath);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
