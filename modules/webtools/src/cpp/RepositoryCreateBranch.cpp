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
#include "RepositoryCreateBranch.hpp"
#include "characters_encoding.hpp"
#include "RepositoryHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
RepositoryCreateBranch(
    const std::wstring& localPath, const std::wstring& branchName, std::wstring& errorMessage)
{
    std::string localPathUtf8 = wstring_to_utf8(localPath);
    std::string branchUtf8 = wstring_to_utf8(branchName);
    git_repository* repo;
    git_reference *head, *branch;
    git_commit* commit;
    git_oid commit_oid;
    int ret = 0;

    git_libgit2_init();

    int errorCode = git_repository_open_ext(&repo, localPathUtf8.c_str(), 0, nullptr);
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_libgit2_shutdown();
        return;
    }

    errorCode = git_repository_head(&head, repo);
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }

    const char* currentBranch = git_reference_shorthand(head);
    if (currentBranch == branchUtf8) {
        git_repository_free(repo);
        git_libgit2_shutdown();
    }

    errorCode = git_reference_name_to_id(&commit_oid, repo, "HEAD");
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }

    errorCode = git_commit_lookup(&commit, repo, &commit_oid);
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }

    errorCode = git_branch_create(&branch, repo, branchUtf8.c_str(), commit, 0);
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
    }

    git_repository_free(repo);
    git_libgit2_shutdown();
}
//=============================================================================

}
//=============================================================================
