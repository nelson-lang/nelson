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
RepositoryIsTag(const std::wstring& localPath, const std::wstring& tagName)
{
    bool found = false;
    std::string localPathUtf8 = wstring_to_utf8(localPath);
    git_repository* repo = nullptr;
    git_libgit2_init();

    int errorCode = git_repository_open(&repo, localPathUtf8.c_str());
    if (errorCode != 0) {
        git_libgit2_shutdown();
        return found;
    }

    git_strarray tag_list;
    errorCode = git_tag_list(&tag_list, repo);
    if (errorCode != 0) {
        git_repository_free(repo);
        git_libgit2_shutdown();
        return found;
    }
    for (size_t i = 0; i < tag_list.count; i++) {
        const char* name = tag_list.strings[i];
        if (utf8_to_wstring(name) == tagName) {
            found = true;
            break;
        }
    }
    git_strarray_free(&tag_list);
    git_repository_free(repo);
    git_libgit2_shutdown();
    return found;
}
//=============================================================================
}
//=============================================================================
