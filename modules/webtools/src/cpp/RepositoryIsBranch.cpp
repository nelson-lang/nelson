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
#include <boost/algorithm/string.hpp>
#include "RepositoryIsBranch.hpp"
#include "characters_encoding.hpp"
#include "RepositoryHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
RepositoryIsBranch(
    const std::wstring& localPath, const std::wstring& branchName, git_branch_t branchType)
{
    bool found = false;
    std::string localPathUtf8 = wstring_to_utf8(localPath);
    git_repository* repo = NULL;
    git_libgit2_init();

    int errorCode = git_repository_open(&repo, localPathUtf8.c_str());
    if (errorCode != 0) {
        return false;
    }
    git_branch_iterator* it;
    if (!git_branch_iterator_new(&it, repo, branchType)) {
        git_reference* ref;
        git_branch_t type;
        const char* name;
        while (!git_branch_next(&ref, &type, it)) {
            git_reference_free(ref);
            errorCode = git_branch_name(&name, ref);
            if (errorCode == 0) {
                if (boost::ends_with(utf8_to_wstring(name), branchName)) {
                    found = true;
                    break;
                }
            }
        }
        git_branch_iterator_free(it);
    }
    git_repository_free(repo);
    git_libgit2_shutdown();
    return found;
}
//=============================================================================
bool
RepositoryIsBranch(const std::wstring& localPath, const std::wstring& branch)
{
    return RepositoryIsBranch(localPath, branch, GIT_BRANCH_ALL);
}
//=============================================================================
bool
RepositoryIsLocalBranch(const std::wstring& localPath, const std::wstring& branch)
{
    return RepositoryIsBranch(localPath, branch, GIT_BRANCH_LOCAL);
}
//=============================================================================
bool
RepositoryIsRemoteBranch(const std::wstring& localPath, const std::wstring& branch)
{
    return RepositoryIsBranch(localPath, branch, GIT_BRANCH_REMOTE);
}
//=============================================================================
}
//=============================================================================
