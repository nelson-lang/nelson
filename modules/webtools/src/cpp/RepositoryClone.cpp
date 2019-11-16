//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <git2.h>
#include <boost/filesystem.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include "RemoveDirectory.hpp"
#include "RepositorySwitchBranch.hpp"
#include "characters_encoding.hpp"
#include "RepositoryClone.hpp"
#include "RepositoryHelpers.hpp"
#include "RepositoryCheckout.hpp"
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
RepositoryClone(const std::wstring& url, const std::wstring& user, const std::wstring& password,
    const std::wstring& branch, std::wstring& localPath, std::wstring& errorMessage)
{
    std::string localPathUtf8 = wstring_to_utf8(localPath);
    std::string urlutf8 = wstring_to_utf8(url);
    std::string branchUtf8 = wstring_to_utf8(branch);
    _username = wstring_to_utf8(user);
    _password = wstring_to_utf8(password);
    git_libgit2_init();
    git_repository* repo = NULL;
    git_clone_options clone_opts = GIT_CLONE_OPTIONS_INIT;
    git_checkout_options checkout_opts = GIT_CHECKOUT_OPTIONS_INIT;
    clone_opts.fetch_opts.callbacks.credentials = credentialsCallback;
    if (!branchUtf8.empty()) {
        clone_opts.checkout_branch = branchUtf8.c_str();
    }
    checkout_opts.checkout_strategy = GIT_CHECKOUT_FORCE | GIT_CHECKOUT_REMOVE_UNTRACKED;
    clone_opts.checkout_opts = checkout_opts;
    int errorCode = git_clone(&repo, urlutf8.c_str(), localPathUtf8.c_str(), &clone_opts);
    errorMessage = gitErrorCodeToMessage(errorCode);
    git_repository_free(repo);
    git_libgit2_shutdown();
}
//=============================================================================
void
RepositoryExport(const std::wstring& url, const std::wstring& user, const std::wstring& password,
    const std::wstring& branchOrTag, std::wstring& localPath, std::wstring& errorMessage)
{
    RepositoryClone(url, user, password, L"", localPath, errorMessage);

    if (errorMessage.empty()) {
        if (!branchOrTag.empty()) {
            RepositoryCheckout(
                localPath, branchOrTag, errorMessage);
        }
        boost::filesystem::path p;
        if (errorMessage.empty()) {
            if (!boost::algorithm::ends_with(localPath, L"\\")
                && (!boost::algorithm::ends_with(localPath, L"/"))) {
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
}
//=============================================================================
