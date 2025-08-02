//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <git2.h>
#include "i18n.hpp"
#include "RepositoryRemoveBranch.hpp"
#include "RepositoryHelpers.hpp"
#include "RepositoryIsBranch.hpp"
#include "RepositoryIsTag.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
RepositoryRemoveBranch(
    const std::wstring& localPath, const std::wstring& branchName, std::wstring& errorMessage)
{
    if (RepositoryIsLocalBranch(localPath, branchName)) {
        git_libgit2_init();
        git_repository* repo = nullptr;
        std::string localPathUtf8 = wstring_to_utf8(localPath);
        int errorCode = git_repository_open(&repo, localPathUtf8.c_str());
        if (errorCode != 0) {
            errorMessage = gitErrorCodeToMessage(errorCode);
            git_libgit2_shutdown();
            return;
        }
        std::string branchNameUtf8 = wstring_to_utf8(branchName);
        git_reference* ref;
        errorCode = git_branch_lookup(&ref, repo, branchNameUtf8.c_str(), GIT_BRANCH_LOCAL);
        if (errorCode != 0) {
            errorMessage = gitErrorCodeToMessage(errorCode);
            git_libgit2_shutdown();
            return;
        }

        errorCode = git_branch_delete(ref);
        if (errorCode != 0) {
            errorMessage = gitErrorCodeToMessage(errorCode);
        }
        git_reference_free(ref);
        git_libgit2_shutdown();

    } else {
        errorMessage = _W("local branch name does not exist.");
    }
}
//=============================================================================
}
//=============================================================================
