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
#include "RepositoryCredentials.hpp"
#include "RepositoryFetch.hpp"
#include "RepositoryHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
RepositoryFetch(const std::wstring& localPath, const RepositoryCredentialsOptions& credentials,
    std::wstring& errorMessage)
{
    git_libgit2_init();
    git_repository* repo = nullptr;
    std::string localPathUtf8 = wstring_to_utf8(localPath);
    RepositoryCredentialsPayload credentialsPayload(credentials);

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
    opts.callbacks.credentials = repositoryCredentialsCallback;
    opts.callbacks.payload = &credentialsPayload;
    const char* url = git_remote_url(origin);
    if (url != nullptr && RepositoryUrlRequiresSSH(url) && !RepositorySupportsSSH()) {
        errorMessage = RepositorySSHUnsupportedMessage();
        git_remote_free(origin);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }

    errorCode = git_remote_fetch(origin, nullptr, &opts, nullptr);
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_remote_free(origin);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }
    errorCode = git_repository_state_cleanup(repo);
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
    }
    git_remote_free(origin);
    git_repository_free(repo);
    git_libgit2_shutdown();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
