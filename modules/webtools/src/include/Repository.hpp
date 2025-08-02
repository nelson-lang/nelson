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
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSWEBTOOLS_IMPEXP void
RepositoryClone(const std::wstring& url, const std::wstring& user, const std::wstring& password,
    const std::wstring& branchOrTag, std::wstring& localPath, std::wstring& errorMessage);
//=============================================================================
NLSWEBTOOLS_IMPEXP void
RepositoryExport(const std::wstring& url, const std::wstring& user, const std::wstring& password,
    const std::wstring& branchOrTag, std::wstring& localPath, std::wstring& errorMessage);
//=============================================================================
NLSWEBTOOLS_IMPEXP void
RepositoryCheckout(
    const std::wstring& localPath, const std::wstring& branchOrTag, std::wstring& errorMessage);
//=============================================================================
NLSWEBTOOLS_IMPEXP
void
RepositoryRemoveBranch(
    const std::wstring& localPath, const std::wstring& branchName, std::wstring& errorMessage);
//=============================================================================
NLSWEBTOOLS_IMPEXP void
RepositoryFetch(const std::wstring& localPath, const std::wstring& user,
    const std::wstring& password, std::wstring& errorMessage);
//=============================================================================
NLSWEBTOOLS_IMPEXP std::wstring
RepositoryGetCurrentBranchName(const std::wstring& localPath, std::wstring& errorMessage);
//=============================================================================
NLSWEBTOOLS_IMPEXP wstringVector
RepositoryBranchList(const std::wstring& localPath, std::wstring& errorMessage);
//=============================================================================
NLSWEBTOOLS_IMPEXP wstringVector
RepositoryTagList(const std::wstring& localPath, std::wstring& errorMessage);
//=============================================================================
NLSWEBTOOLS_IMPEXP
ArrayOf
RepositoryLog(const std::wstring& localPath, std::wstring& errorMessage);
//=============================================================================
};
//=============================================================================
