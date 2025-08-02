//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4251)
#endif
//=============================================================================
#include <string>
#include "nlsFiles_folders_functions_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSFILES_FOLDERS_FUNCTIONS_IMPEXP FileInfo
{
    //=============================================================================
private:
    std::wstring folder;
    std::wstring name;
    std::wstring date;
    bool isdir;
    double bytes;
    double datenum;
    //=============================================================================
public:
    FileInfo(const std::wstring& filename);
    ~FileInfo();
    std::wstring
    getFolder();
    std::wstring
    getName();
    std::wstring
    getDate();
    bool
    isDir();
    double
    getBytes();
    double
    getDatenum();
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
