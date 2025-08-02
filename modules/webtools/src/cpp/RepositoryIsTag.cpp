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
RepositoryIsTag(const std::wstring& localPath, const std::wstring& tagName)
{
    bool found = false;
    std::string localPathUtf8 = wstring_to_utf8(localPath);
    git_repository* repo = nullptr;
    git_libgit2_init();

    int errorCode = git_repository_open(&repo, localPathUtf8.c_str());
    if (errorCode != 0) {
        git_libgit2_shutdown();
        return found;
    }

    git_strarray tag_list;
    errorCode = git_tag_list(&tag_list, repo);
    if (errorCode != 0) {
        git_repository_free(repo);
        git_libgit2_shutdown();
        return found;
    }
    for (size_t i = 0; i < tag_list.count; i++) {
        const char* name = tag_list.strings[i];
        if (utf8_to_wstring(name) == tagName) {
            found = true;
            break;
        }
    }
    git_strarray_free(&tag_list);
    git_repository_free(repo);
    git_libgit2_shutdown();
    return found;
}
//=============================================================================
}
//=============================================================================
