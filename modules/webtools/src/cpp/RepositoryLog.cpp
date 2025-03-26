//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
#include <ctime>
#include "StringHelpers.hpp"
#include "RepositoryLog.hpp"
#include "characters_encoding.hpp"
#include "RepositoryHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
RepositoryLog(const std::wstring& localPath, std::wstring& errorMessage)
{
    ArrayOf logs;
    std::string localPathUtf8 = wstring_to_utf8(localPath);
    git_repository* repo = nullptr;
    git_libgit2_init();

    int errorCode = git_repository_open(&repo, localPathUtf8.c_str());
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_libgit2_shutdown();
        return logs;
    }
    git_revwalk* walk;
    errorCode = git_revwalk_new(&walk, repo);
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return logs;
    }
    git_revwalk_sorting(walk, GIT_SORT_TOPOLOGICAL | GIT_SORT_TIME);
    errorCode = git_revwalk_push_head(walk);
    if (errorCode != 0) {
        git_revwalk_free(walk);
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return logs;
    }
    git_oid oid;
    ArrayOfVector sha1s;
    ArrayOfVector msgs;
    ArrayOfVector times;
    ArrayOfVector authors;
    while (git_revwalk_next(&oid, walk) == 0) {
        git_commit* c;

        char oidstr[10] = { 0 };
        git_commit_lookup(&c, repo, &oid);
        git_oid_tostr(oidstr, 9, &oid);
        sha1s.push_back(ArrayOf::characterArrayConstructor(oidstr));
        std::string message = std::string(git_commit_message(c));
        if (StringHelpers::ends_with(message, "\n")) {
            message.pop_back();
        }
        msgs.push_back(ArrayOf::characterArrayConstructor(message));
        const git_signature* author = git_commit_author(c);
        authors.push_back(ArrayOf::characterArrayConstructor(
            std::string(author->name) + " <" + std::string(author->email) + ">"));
        time_t time = git_commit_time(c);
#if _MSC_VER
        std::string strtime = std::string(ctime(&time));
#else
        char buffer[128];
        std::string strtime = std::string(ctime_r(&time, buffer));
#endif
        if (StringHelpers::ends_with(strtime, "\n")) {
            strtime.pop_back();
        }
        times.push_back(ArrayOf::characterArrayConstructor(strtime));
        git_commit_free(c);
    }
    git_revwalk_free(walk);
    git_repository_free(repo);
    git_libgit2_shutdown();

    Dimensions dims(1, sha1s.size());
    stringVector fieldnames;
    fieldnames.reserve(4);
    fieldnames.push_back("sha1");
    fieldnames.push_back("message");
    fieldnames.push_back("author");
    fieldnames.push_back("date");
    auto* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.getElementCount(), fieldnames, false));
    logs = ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, fieldnames);
    logs.setFieldAsList("sha1", sha1s);
    logs.setFieldAsList("message", msgs);
    logs.setFieldAsList("author", authors);
    logs.setFieldAsList("date", times);
    return logs;
}
//=============================================================================
}
//=============================================================================
