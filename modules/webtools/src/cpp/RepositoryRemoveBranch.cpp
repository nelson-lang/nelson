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
#include "RepositoryRemoveBranch.hpp"
#include "RepositoryHelpers.hpp"
#include "RepositoryIsBranch.hpp"
#include "RepositoryIsTag.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
RepositoryRemoveBranch(
    const std::wstring& localPath, const std::wstring& branchName, std::wstring& errorMessage)
{
    if (RepositoryIsLocalBranch(localPath, branchName)) {
        git_libgit2_init();
        git_repository* repo = NULL;
        std::string localPathUtf8 = wstring_to_utf8(localPath);
        int errorCode = git_repository_open(&repo, localPathUtf8.c_str());
        if (errorCode != 0) {
            errorMessage = gitErrorCodeToMessage(errorCode);
            git_libgit2_shutdown();
            return;
        }
        std::string branchNameUtf8 = wstring_to_utf8(branchName);
        git_reference* ref;
        errorCode = git_branch_lookup(&ref, repo, branchNameUtf8.c_str(), GIT_BRANCH_LOCAL);
        if (errorCode != 0) {
            errorMessage = gitErrorCodeToMessage(errorCode);
            git_libgit2_shutdown();
            return;
        }

        errorCode = git_branch_delete(ref);
        if (errorCode != 0) {
            errorMessage = gitErrorCodeToMessage(errorCode);
        }
        git_reference_free(ref);
        git_libgit2_shutdown();

    } else {
        errorMessage = _W("local branch name does not exist.");
    }
}
//=============================================================================
}
//=============================================================================
