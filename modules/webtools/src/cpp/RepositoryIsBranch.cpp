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
#include "StringHelpers.hpp"
#include "RepositoryIsBranch.hpp"
#include "characters_encoding.hpp"
#include "RepositoryHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
RepositoryIsBranch(
    const std::wstring& localPath, const std::wstring& branchName, git_branch_t branchType)
{
    bool found = false;
    std::string localPathUtf8 = wstring_to_utf8(localPath);
    git_repository* repo = nullptr;
    git_libgit2_init();

    int errorCode = git_repository_open(&repo, localPathUtf8.c_str());
    if (errorCode != 0) {
        return false;
    }
    git_branch_iterator* it;
    if (!git_branch_iterator_new(&it, repo, branchType)) {
        git_reference* ref;
        git_branch_t type;
        const char* name;
        while (!git_branch_next(&ref, &type, it)) {
            git_reference_free(ref);
            errorCode = git_branch_name(&name, ref);
            if (errorCode == 0) {
                if (StringHelpers::ends_with(utf8_to_wstring(name), branchName)) {
                    found = true;
                    break;
                }
            }
        }
        git_branch_iterator_free(it);
    }
    git_repository_free(repo);
    git_libgit2_shutdown();
    return found;
}
//=============================================================================
bool
RepositoryIsBranch(const std::wstring& localPath, const std::wstring& branch)
{
    return RepositoryIsBranch(localPath, branch, GIT_BRANCH_ALL);
}
//=============================================================================
bool
RepositoryIsLocalBranch(const std::wstring& localPath, const std::wstring& branch)
{
    return RepositoryIsBranch(localPath, branch, GIT_BRANCH_LOCAL);
}
//=============================================================================
bool
RepositoryIsRemoteBranch(const std::wstring& localPath, const std::wstring& branch)
{
    return RepositoryIsBranch(localPath, branch, GIT_BRANCH_REMOTE);
}
//=============================================================================
}
//=============================================================================
