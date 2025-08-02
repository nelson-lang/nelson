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
#include "characters_encoding.hpp"
#include "RepositoryFetch.hpp"
#include "RepositoryHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::string _username;
static std::string _password;
//=============================================================================
static int
credentialsCallback(git_cred** cred, const char* url, const char* username_from_url,
    unsigned int allowed_types, void* payload)
{
    return git_cred_userpass_plaintext_new(cred, _username.c_str(), _password.c_str());
}
//=============================================================================
void
RepositoryFetch(const std::wstring& localPath, const std::wstring& user,
    const std::wstring& password, std::wstring& errorMessage)
{
    git_libgit2_init();
    git_repository* repo = nullptr;
    std::string localPathUtf8 = wstring_to_utf8(localPath);
    _username = wstring_to_utf8(user);
    _password = wstring_to_utf8(password);

    int errorCode = git_repository_open(&repo, localPathUtf8.c_str());
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_libgit2_shutdown();
        return;
    }
    git_remote* origin = nullptr;
    errorCode = git_remote_lookup(&origin, repo, "origin");
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }
    git_fetch_options opts = GIT_FETCH_OPTIONS_INIT;
    opts.callbacks.credentials = credentialsCallback;

    errorCode = git_remote_fetch(origin, nullptr, &opts, nullptr);
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }
    errorCode = git_repository_state_cleanup(repo);
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
    }
    git_repository_free(repo);
    git_libgit2_shutdown();
}
//=============================================================================
};
//=============================================================================
