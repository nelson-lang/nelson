//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include <git2.h>
#include "RepositoryTagList.hpp"
#include "characters_encoding.hpp"
#include "RepositoryHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
wstringVector
RepositoryTagList(const std::wstring& localPath, std::wstring& errorMessage)
{
    wstringVector tags;
    std::string localPathUtf8 = wstring_to_utf8(localPath);
    git_repository* repo = nullptr;
    git_libgit2_init();

    int errorCode = git_repository_open(&repo, localPathUtf8.c_str());
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_libgit2_shutdown();
        return tags;
    }

    git_strarray tag_list;
    errorCode = git_tag_list(&tag_list, repo);
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return tags;
    }
    for (size_t i = 0; i < tag_list.count; i++) {
        const char* name = tag_list.strings[i];
        tags.push_back(utf8_to_wstring(name));
    }
    git_strarray_free(&tag_list);
    git_repository_free(repo);
    git_libgit2_shutdown();
    return tags;
}
//=============================================================================
}
//=============================================================================
