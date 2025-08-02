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
#include <string>
#include "nlsWebtools_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSWEBTOOLS_IMPEXP
void
RepositoryClone(const std::wstring& url, const std::wstring& user, const std::wstring& password,
    const std::wstring& branch, std::wstring& localPath, std::wstring& errorMessage);
//=============================================================================
NLSWEBTOOLS_IMPEXP
void
RepositoryExport(const std::wstring& url, const std::wstring& user, const std::wstring& password,
    const std::wstring& branch, std::wstring& localPath, std::wstring& errorMessage);
//=============================================================================
};
//=============================================================================
