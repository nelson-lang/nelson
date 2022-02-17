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
#include "RepositoryBranchList.hpp"
#include "characters_encoding.hpp"
#include "RepositoryHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
RepositoryGetCurrentBranchName(const std::wstring& localPath, std::wstring& errorMessage)
{
    std::wstring branchName;
    std::string localPathUtf8 = wstring_to_utf8(localPath);
    git_repository* repo = nullptr;
    git_libgit2_init();

    int errorCode = git_repository_open(&repo, localPathUtf8.c_str());
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_libgit2_shutdown();
        return branchName;
    }
    git_reference* ref;
    errorCode = git_repository_head(&ref, repo);
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_libgit2_shutdown();
        return branchName;
    }
    const char* currentName = nullptr;
    errorCode = git_branch_name(&currentName, ref);
    std::wstring currentBranchName;
    if (errorCode == 0) {
        currentBranchName = utf8_to_wstring(currentName);
        git_reference_free(ref);
    } else {
        git_ref_t reftype = git_reference_type(ref);
        if (reftype == GIT_REF_SYMBOLIC) {
            currentName = git_reference_symbolic_target(ref);
            currentBranchName = utf8_to_wstring(currentName);
        } else if (reftype == GIT_REF_OID) {
            const git_oid* oid = git_reference_target(ref);
            currentName = git_oid_tostr_s(oid);
            currentBranchName
                = L"(" + _W("HEAD detached at ") + utf8_to_wstring(currentName) + L")";
        }
        git_reference_free(ref);
    }
    git_repository_free(repo);
    git_libgit2_shutdown();
    return currentBranchName;
}
//=============================================================================
wstringVector
RepositoryBranchList(const std::wstring& localPath, std::wstring& errorMessage)
{
    wstringVector branchs;
    std::string localPathUtf8 = wstring_to_utf8(localPath);
    git_repository* repo = nullptr;
    git_libgit2_init();

    int errorCode = git_repository_open(&repo, localPathUtf8.c_str());
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        return branchs;
    }
    git_reference* ref;
    errorCode = git_repository_head(&ref, repo);
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        return branchs;
    }
    const char* currentName = nullptr;
    errorCode = git_branch_name(&currentName, ref);
    std::wstring currentBranchName;
    if (errorCode == 0) {
        currentBranchName = utf8_to_wstring(currentName);
        git_reference_free(ref);
    } else {
        git_ref_t reftype = git_reference_type(ref);
        if (reftype == GIT_REF_SYMBOLIC) {
            currentName = git_reference_symbolic_target(ref);
            currentBranchName = utf8_to_wstring(currentName);
            branchs.push_back(currentBranchName);
        } else if (reftype == GIT_REF_OID) {
            const git_oid* oid = git_reference_target(ref);
            currentName = git_oid_tostr_s(oid);
            currentBranchName
                = L"(" + _W("HEAD detached at ") + utf8_to_wstring(currentName) + L")";
            branchs.push_back(currentBranchName);
        }
        git_reference_free(ref);
    }
    git_branch_iterator* it;
    if (!git_branch_iterator_new(&it, repo, GIT_BRANCH_ALL)) {
        git_reference* ref;
        git_branch_t type;
        const char* name = nullptr;
        while (!git_branch_next(&ref, &type, it)) {
            git_reference_free(ref);
            errorCode = git_branch_name(&name, ref);
            if (errorCode == 0) {
                branchs.push_back(utf8_to_wstring(name));
            } else {
                errorMessage = gitErrorCodeToMessage(errorCode);
                break;
            }
        }
        git_branch_iterator_free(it);
    }
    git_repository_free(repo);
    git_libgit2_shutdown();
    return branchs;
}
//=============================================================================
}
//=============================================================================
