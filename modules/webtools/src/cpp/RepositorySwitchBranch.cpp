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
#include "RepositorySwitchBranch.hpp"
#include "characters_encoding.hpp"
#include "RepositoryHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
RepositorySwitchBranch(
    const std::wstring& localPath, const std::wstring& branch, std::wstring& errorMessage)
{
    std::string localPathUtf8 = wstring_to_utf8(localPath);
    std::string branchUtf8 = wstring_to_utf8(branch);
    git_repository* repo;
    git_object* tree = nullptr;
    git_checkout_options opts;
    git_libgit2_init();
    int errorCode = git_repository_open_ext(&repo, localPathUtf8.c_str(), 0, nullptr);
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_libgit2_shutdown();
        return;
    }
    errorCode = git_checkout_init_options(&opts, GIT_CHECKOUT_OPTIONS_VERSION);
    opts.checkout_strategy = GIT_CHECKOUT_FORCE | GIT_CHECKOUT_REMOVE_UNTRACKED;
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }
    std::string src = "origin/" + branchUtf8;
    errorCode = git_revparse_single(&tree, repo, src.c_str());
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }
    errorCode = git_checkout_tree(repo, tree, &opts);
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }
    std::string ref = "refs/heads/" + branchUtf8;
    errorCode = git_repository_set_head(repo, ref.c_str());
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
    }
    git_repository_free(repo);
    git_libgit2_shutdown();
}
//=============================================================================
}
//=============================================================================
