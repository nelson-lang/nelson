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
#include "RepositoryIsTag.hpp"
#include "characters_encoding.hpp"
#include "RepositoryHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
RepositoryIsSHA1(const std::wstring& localPath, const std::wstring& sha1Str)
{
    bool found = false;
    git_object* treeish = NULL;

    std::string localPathUtf8 = wstring_to_utf8(localPath);
    git_repository* repo = NULL;
    git_libgit2_init();

    int errorCode = git_repository_open(&repo, localPathUtf8.c_str());
    if (errorCode != 0) {
        git_libgit2_shutdown();
        return found;
    }
    errorCode = git_revparse_single(&treeish, repo, wstring_to_utf8(sha1Str).c_str());
    if (errorCode != 0) {
        git_libgit2_shutdown();
        return found;
    }
    git_object_free(treeish);
    git_repository_free(repo);
    git_libgit2_shutdown();
    return true;
}
//=============================================================================
}
//=============================================================================
