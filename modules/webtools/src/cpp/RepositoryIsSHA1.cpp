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
#include "RepositoryIsTag.hpp"
#include "characters_encoding.hpp"
#include "RepositoryHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
RepositoryIsSHA1(const std::wstring& localPath, const std::wstring& sha1Str)
{
    bool found = false;
    git_object* treeish = nullptr;

    std::string localPathUtf8 = wstring_to_utf8(localPath);
    git_repository* repo = nullptr;
    git_libgit2_init();

    int errorCode = git_repository_open(&repo, localPathUtf8.c_str());
    if (errorCode != 0) {
        git_libgit2_shutdown();
        return found;
    }
    errorCode = git_revparse_single(&treeish, repo, wstring_to_utf8(sha1Str).c_str());
    if (errorCode != 0) {
        git_libgit2_shutdown();
        return found;
    }
    git_object_free(treeish);
    git_repository_free(repo);
    git_libgit2_shutdown();
    return true;
}
//=============================================================================
}
//=============================================================================
