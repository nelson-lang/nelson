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
