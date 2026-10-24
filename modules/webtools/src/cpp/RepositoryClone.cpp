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
#include "FileSystemWrapper.hpp"
#include "RemoveDirectory.hpp"
#include "RepositorySwitchBranch.hpp"
#include "characters_encoding.hpp"
#include "RepositoryClone.hpp"
#include "RepositoryCredentials.hpp"
#include "RepositoryHelpers.hpp"
#include "RepositoryCheckout.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
RepositoryClone(const std::wstring& url, const RepositoryCredentialsOptions& credentials,
    const std::wstring& branch, std::wstring& localPath, std::wstring& errorMessage)
{
    std::string localPathUtf8 = wstring_to_utf8(localPath);
    std::string urlutf8 = wstring_to_utf8(url);
    std::string branchUtf8 = wstring_to_utf8(branch);
    RepositoryCredentialsPayload credentialsPayload(credentials);
    if (RepositoryUrlRequiresSSH(urlutf8) && !RepositorySupportsSSH()) {
        errorMessage = RepositorySSHUnsupportedMessage();
        return;
    }
    git_libgit2_init();
    git_repository* repo = nullptr;
    git_clone_options clone_opts = GIT_CLONE_OPTIONS_INIT;
    git_checkout_options checkout_opts = GIT_CHECKOUT_OPTIONS_INIT;
    clone_opts.fetch_opts.callbacks.credentials = repositoryCredentialsCallback;
    clone_opts.fetch_opts.callbacks.payload = &credentialsPayload;
    if (!branchUtf8.empty()) {
        clone_opts.checkout_branch = branchUtf8.c_str();
    }
    checkout_opts.checkout_strategy = GIT_CHECKOUT_FORCE | GIT_CHECKOUT_REMOVE_UNTRACKED;
    clone_opts.checkout_opts = checkout_opts;
    int errorCode = git_clone(&repo, urlutf8.c_str(), localPathUtf8.c_str(), &clone_opts);
    errorMessage = gitErrorCodeToMessage(errorCode);
    if (repo != nullptr) {
        git_repository_free(repo);
    }
    git_libgit2_shutdown();
}
//=============================================================================
void
RepositoryExport(const std::wstring& url, const RepositoryCredentialsOptions& credentials,
    const std::wstring& branchOrTag, std::wstring& localPath, std::wstring& errorMessage)
{
    RepositoryClone(url, credentials, L"", localPath, errorMessage);

    if (errorMessage.empty()) {
        if (!branchOrTag.empty()) {
            RepositoryCheckout(localPath, branchOrTag, errorMessage);
        }
        FileSystemWrapper::Path p;
        if (errorMessage.empty()) {
            if (!StringHelpers::ends_with(localPath, L"\\")
                && (!StringHelpers::ends_with(localPath, L"/"))) {
                p = localPath + std::wstring(L"/.git");
            } else {
                p = localPath + std::wstring(L".git");
            }
        } else {
            p = localPath;
        }
        RemoveDirectory(p.generic_wstring(), true, errorMessage);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
