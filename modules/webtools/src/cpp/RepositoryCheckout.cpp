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
#include "i18n.hpp"
#include "RepositoryCheckout.hpp"
#include "RepositoryHelpers.hpp"
#include "RepositoryIsBranch.hpp"
#include "RepositoryIsTag.hpp"
#include "RepositoryIsSHA1.hpp"
#include "RepositorySwitchBranch.hpp"
#include "RepositoryCreateBranch.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
RepositoryCheckout(const std::wstring& localPath, const std::wstring& branchName, bool detach,
    std::wstring& errorMessage)
{
    if (detach) {
        git_libgit2_init();
        git_repository* repo = NULL;
        std::string localPathUtf8 = wstring_to_utf8(localPath);
        std::string branchNameUtf8 = wstring_to_utf8(branchName);

        int errorCode = git_repository_open(&repo, localPathUtf8.c_str());
        if (errorCode != 0) {
            errorMessage = gitErrorCodeToMessage(errorCode);
            git_libgit2_shutdown();
            return;
        }

        git_checkout_options checkout_opts = GIT_CHECKOUT_OPTIONS_INIT;
        checkout_opts.checkout_strategy = GIT_CHECKOUT_FORCE;
        errorCode = git_checkout_head(repo, &checkout_opts);
        if (errorCode != 0) {
            errorMessage = gitErrorCodeToMessage(errorCode);
            git_repository_free(repo);
            git_libgit2_shutdown();
            return;
        }
        errorCode = git_checkout_index(repo, NULL, &checkout_opts);
        if (errorCode != 0) {
            errorMessage = gitErrorCodeToMessage(errorCode);
            git_repository_free(repo);
            git_libgit2_shutdown();
            return;
        }
        git_object* treeish = NULL;
        errorCode = git_revparse_single(&treeish, repo, branchNameUtf8.c_str());
        if (errorCode != 0) {
            std::string branchTempName = std::string("origin/") + branchNameUtf8;
            errorCode = git_revparse_single(&treeish, repo, branchTempName.c_str());
        }
        if (errorCode != 0) {
            errorMessage = gitErrorCodeToMessage(errorCode);
            git_repository_free(repo);
            git_libgit2_shutdown();
            return;
        }
        errorCode = git_checkout_tree(repo, treeish, &checkout_opts);
        if (errorCode != 0) {
            errorMessage = gitErrorCodeToMessage(errorCode);
            git_repository_free(repo);
            git_libgit2_shutdown();
            return;
        }
        if (detach) {
            errorCode = git_repository_set_head_detached(repo, git_object_id(treeish));
        }
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_object_free(treeish);
        git_repository_free(repo);
        git_libgit2_shutdown();
    } else {
        if (RepositoryIsRemoteBranch(localPath, branchName)) {
            if (!RepositoryIsLocalBranch(localPath, branchName)) {
                RepositoryCreateBranch(localPath, branchName, errorMessage);
                if (!errorMessage.empty()) {
                    return;
                }
            }
            RepositorySwitchBranch(localPath, branchName, errorMessage);
        } else {
            errorMessage = _W("remote branch does not exist.");
        }
    }
}
//=============================================================================
void
RepositoryCheckout(
    const std::wstring& localPath, const std::wstring& branchOrTag, std::wstring& errorMessage)
{
    bool isBranch = RepositoryIsBranch(localPath, branchOrTag);
    if (isBranch) {
        RepositoryCheckout(localPath, branchOrTag, false, errorMessage);
    } else {
        bool isTag = RepositoryIsTag(localPath, branchOrTag);
        if (isTag) {
            RepositoryCheckout(localPath, branchOrTag, true, errorMessage);
        } else {
            bool isSHA1 = RepositoryIsSHA1(localPath, branchOrTag);
            if (isSHA1) {
                RepositoryCheckout(localPath, branchOrTag, true, errorMessage);
            } else {
                errorMessage = _W("Valid tag or branch name expected.");
            }
        }
    }
}
//=============================================================================
}
//=============================================================================
