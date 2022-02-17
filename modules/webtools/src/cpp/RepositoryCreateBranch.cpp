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
#include "RepositoryCreateBranch.hpp"
#include "characters_encoding.hpp"
#include "RepositoryHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
RepositoryCreateBranch(
    const std::wstring& localPath, const std::wstring& branchName, std::wstring& errorMessage)
{
    std::string localPathUtf8 = wstring_to_utf8(localPath);
    std::string branchUtf8 = wstring_to_utf8(branchName);
    git_repository* repo;
    git_reference *head, *branch;
    git_commit* commit;
    git_oid commit_oid;
    int ret = 0;

    git_libgit2_init();

    int errorCode = git_repository_open_ext(&repo, localPathUtf8.c_str(), 0, nullptr);
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_libgit2_shutdown();
        return;
    }

    errorCode = git_repository_head(&head, repo);
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }

    const char* currentBranch = git_reference_shorthand(head);
    if (currentBranch == branchUtf8) {
        git_repository_free(repo);
        git_libgit2_shutdown();
    }

    errorCode = git_reference_name_to_id(&commit_oid, repo, "HEAD");
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }

    errorCode = git_commit_lookup(&commit, repo, &commit_oid);
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }

    errorCode = git_branch_create(&branch, repo, branchUtf8.c_str(), commit, 0);
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
    }

    git_repository_free(repo);
    git_libgit2_shutdown();
}
//=============================================================================

}
//=============================================================================
