//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class FileFunction
{
private:
    std::wstring _fullfilename;
    std::wstring _name;
    bool _ismex;
    bool _withWatcher;
    bool _isOverload;

    std::wstring
    buildFullFilename(const std::wstring& directory, const std::wstring& objectName,
        const std::wstring& name, bool ismex, bool isPrivate);

public:
    FileFunction(const std::wstring& directory, const std::wstring& objectName,
        const std::wstring& name, bool ismex, bool withWatcher, bool isOverload, bool isPrivate);
    ~FileFunction();
    std::wstring
    getFilename();
    std::wstring
    getName();
    bool
    isMex();
    bool
    getWithWatcher();
    bool
    isOverload();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
