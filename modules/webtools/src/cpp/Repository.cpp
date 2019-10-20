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
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include <git2.h>
#include <time.h>
#include <boost/algorithm/string.hpp>
#include "Repository.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring
errorCodeToMessage(int errorCode)
{
    std::wstring errorMessage;
    if (errorCode != 0) {
        const git_error* e = giterr_last();
        wchar_t buffer[4096];
        if (e) {
            std::wstring msg = utf8_to_wstring(e->message);
            swprintf(buffer, 4096, _W("repository error %d/%d: %ls").c_str(), errorCode, e->klass,
                msg.c_str());
        } else {
            swprintf(buffer, 4096, _W("repository error %d").c_str(), errorCode);
        }
        errorMessage = std::wstring(buffer);
    }
    return errorMessage;
}
//=============================================================================
static bool
RepositoryIsBranch(const std::wstring& localPath, const std::wstring& branchName)
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
    if (!git_branch_iterator_new(&it, repo, GIT_BRANCH_ALL)) {
        git_reference* ref;
        git_branch_t type;
        const char* name;
        while (!git_branch_next(&ref, &type, it)) {
            git_reference_free(ref);
            errorCode = git_branch_name(&name, ref);
            if (errorCode == 0) {
                if (utf8_to_wstring(name) == branchName) {
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
static bool
RepositoryIsTag(const std::wstring& localPath, const std::wstring& tagName)
{
    bool found = false;
    std::string localPathUtf8 = wstring_to_utf8(localPath);
    git_repository* repo = NULL;
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
static bool
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
void
RepositoryClone(const std::wstring& url, const std::wstring& localPath, std::wstring& errorMessage)
{
    std::string urlutf8 = wstring_to_utf8(url);
    std::string localPathUtf8 = wstring_to_utf8(localPath);

    git_libgit2_init();
    git_repository* repo = NULL;
    git_clone_options clone_opts = GIT_CLONE_OPTIONS_INIT;
    git_checkout_options checkout_opts = GIT_CHECKOUT_OPTIONS_INIT;
    checkout_opts.checkout_strategy = GIT_CHECKOUT_FORCE;
    clone_opts.checkout_opts = checkout_opts;
    int errorCode = git_clone(&repo, urlutf8.c_str(), localPathUtf8.c_str(), &clone_opts);
    errorMessage = errorCodeToMessage(errorCode);
    git_repository_free(repo);
    git_libgit2_shutdown();
}
//=============================================================================
static void
RepositoryCheckout(const std::wstring& localPath, const std::wstring& branchName, bool detach,
    std::wstring& errorMessage)
{
    git_libgit2_init();
    git_repository* repo = NULL;
    std::string localPathUtf8 = wstring_to_utf8(localPath);
    std::string branchNameUtf8 = wstring_to_utf8(branchName);

    int errorCode = git_repository_open(&repo, localPathUtf8.c_str());
    if (errorCode != 0) {
        errorMessage = errorCodeToMessage(errorCode);
        git_libgit2_shutdown();
        return;
    }

    git_checkout_options checkout_opts = GIT_CHECKOUT_OPTIONS_INIT;
    checkout_opts.checkout_strategy = GIT_CHECKOUT_FORCE;
    errorCode = git_checkout_head(repo, &checkout_opts);
    if (errorCode != 0) {
        errorMessage = errorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }
    errorCode = git_checkout_index(repo, NULL, &checkout_opts);
    if (errorCode != 0) {
        errorMessage = errorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }
    git_object* treeish = NULL;
    errorCode = git_revparse_single(&treeish, repo, branchNameUtf8.c_str());
    if (errorCode != 0) {
        errorMessage = errorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }
    errorCode = git_checkout_tree(repo, treeish, &checkout_opts);
    if (errorCode != 0) {
        errorMessage = errorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }
    if (detach) {
        errorCode = git_repository_set_head_detached(repo, git_object_id(treeish));
    } else {
        errorCode = git_repository_head_unborn(repo);
        if (errorCode != 0) {
            errorMessage = errorCodeToMessage(errorCode);
            git_object_free(treeish);
            git_repository_free(repo);
            git_libgit2_shutdown();
            return;
        }
        git_reference* ref;
        errorCode = git_repository_head(&ref, repo);
        if (errorCode != 0) {
            errorMessage = errorCodeToMessage(errorCode);
            git_object_free(treeish);
            git_repository_free(repo);
            git_libgit2_shutdown();
            return;
        }
        git_reference* branch;
        errorCode = git_branch_lookup(&branch, repo, branchNameUtf8.c_str(), GIT_BRANCH_LOCAL);
        if (errorCode != 0) {
            errorMessage = errorCodeToMessage(errorCode);
            git_object_free(treeish);
            git_repository_free(repo);
            git_libgit2_shutdown();
            return;
        }
        errorCode = git_repository_set_head(repo, git_reference_name(branch));
    }
    errorMessage = errorCodeToMessage(errorCode);
    git_object_free(treeish);
    git_repository_free(repo);
    git_libgit2_shutdown();
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
void
RepositoryPull(const std::wstring& localPath, std::wstring& errorMessage)
{
    git_libgit2_init();
    git_repository* repo = NULL;
    std::string localPathUtf8 = wstring_to_utf8(localPath);

    int errorCode = git_repository_open(&repo, localPathUtf8.c_str());
    if (errorCode != 0) {
        errorMessage = errorCodeToMessage(errorCode);
        git_libgit2_shutdown();
        return;
    }
    git_remote* origin = NULL;
    errorCode = git_remote_lookup(&origin, repo, "origin");
    if (errorCode != 0) {
        errorMessage = errorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }
    git_fetch_options opts = GIT_FETCH_OPTIONS_INIT;
    errorCode = git_remote_fetch(origin, nullptr, &opts, nullptr);
    if (errorCode != 0) {
        errorMessage = errorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }
    errorCode = git_repository_state_cleanup(repo);
    if (errorCode != 0) {
        errorMessage = errorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }

    std::pair<std::string, git_oid> oid_to_merge;
    errorCode = git_repository_fetchhead_foreach(
        repo,
        [](const char* name, const char* /*url*/, const git_oid* oid, unsigned int is_merge,
            void* payload) -> int {
            auto& oids_to_merge = *reinterpret_cast<std::pair<std::string, git_oid>*>(payload);
            if (is_merge != 0u) {
                oids_to_merge = { name, *oid };
            }
            return 0;
        },
        &oid_to_merge);
    if (errorCode != 0) {
        errorMessage = errorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }
    git_annotated_commit* originMaster;
    errorCode = git_annotated_commit_lookup(&originMaster, repo, &oid_to_merge.second);
    if (errorCode != 0) {
        errorMessage = errorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }

    auto annotatedCommits = const_cast<const git_annotated_commit**>(&originMaster);
    git_merge_analysis_t mergeAnalyse;
    git_merge_preference_t mergePreferences;
    errorCode = git_merge_analysis(&mergeAnalyse, &mergePreferences, repo, annotatedCommits, 1);
    if (errorCode != 0) {
        errorMessage = errorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }

    if ((mergeAnalyse & GIT_MERGE_ANALYSIS_UP_TO_DATE) != 0
        || (mergeAnalyse & GIT_MERGE_ANALYSIS_NONE) != 0) {
        errorMessage = errorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }

    if ((mergeAnalyse & GIT_MERGE_ANALYSIS_FASTFORWARD) != 0) {
        git_reference* master;
        errorCode = git_repository_head(&master, repo);
        if (errorCode != 0) {
            errorMessage = errorCodeToMessage(errorCode);
            git_repository_free(repo);
            git_libgit2_shutdown();
            return;
        }
        git_reference* createdRef;
        errorCode = git_reference_set_target(&createdRef, master, &oid_to_merge.second, "pull");
        if (errorCode != 0) {
            errorMessage = errorCodeToMessage(errorCode);
            git_repository_free(repo);
            git_libgit2_shutdown();
            return;
        }
        git_index* head;
        errorCode = git_repository_index(&head, repo);
        if (errorCode != 0) {
            errorMessage = errorCodeToMessage(errorCode);
            git_repository_free(repo);
            git_libgit2_shutdown();
            return;
        }
        git_oid oid {};
        errorCode = git_index_write_tree_to(&oid, head, repo);
        if (errorCode != 0) {
            errorMessage = errorCodeToMessage(errorCode);
            git_repository_free(repo);
            git_libgit2_shutdown();
            return;
        }
    } else if ((mergeAnalyse & GIT_MERGE_ANALYSIS_NORMAL) != 0) {
        git_merge_options mergeOpts = GIT_MERGE_OPTIONS_INIT;
        git_checkout_options checkoutOpts = GIT_CHECKOUT_OPTIONS_INIT;
        checkoutOpts.checkout_strategy = GIT_CHECKOUT_SAFE;
        errorCode = git_merge(repo, annotatedCommits, 1, &mergeOpts, &checkoutOpts);
        if (errorCode != 0) {
            errorMessage = errorCodeToMessage(errorCode);
            git_repository_free(repo);
            git_libgit2_shutdown();
            return;
        }
        git_index* head;
        errorCode = git_repository_index(&head, repo);
        if (errorCode != 0) {
            errorMessage = errorCodeToMessage(errorCode);
            git_repository_free(repo);
            git_libgit2_shutdown();
            return;
        }
        if (git_index_has_conflicts(head) != 0) {
            errorMessage = _W("Merge conflicts when pulling, please manually resolve them.");
            git_repository_free(repo);
            git_libgit2_shutdown();
            return;
        }
        git_signature* committerSignature;
        errorCode = git_signature_now(&committerSignature, "Nelson (automatic merge)", "");
        if (errorCode != 0) {
            errorMessage = errorCodeToMessage(errorCode);
            git_repository_free(repo);
            git_libgit2_shutdown();
            return;
        }
        git_commit* origin_master_commit;
        errorCode = git_commit_lookup(&origin_master_commit, repo, &oid_to_merge.second);
        if (errorCode != 0) {
            errorMessage = errorCodeToMessage(errorCode);
            git_repository_free(repo);
            git_libgit2_shutdown();
            return;
        }
        git_oid parent_headoid {};
        errorCode = git_reference_name_to_id(&parent_headoid, repo, "HEAD");
        if (errorCode != 0) {
            errorMessage = errorCodeToMessage(errorCode);
            git_repository_free(repo);
            git_libgit2_shutdown();
            return;
        }

        git_commit* head_parent;
        errorCode = git_commit_lookup(&head_parent, repo, &parent_headoid);
        if (errorCode != 0) {
            errorMessage = errorCodeToMessage(errorCode);
            git_repository_free(repo);
            git_libgit2_shutdown();
            return;
        }
        git_tree* tree;
        errorCode = git_commit_tree(&tree, head_parent);
        if (errorCode != 0) {
            errorMessage = errorCodeToMessage(errorCode);
            git_repository_free(repo);
            git_libgit2_shutdown();
            return;
        }

        const git_commit* parents[] = { head_parent, origin_master_commit };

        git_oid newCommit {};
        std::string commitMsg = std::string("Merge ") + git_oid_tostr_s(&oid_to_merge.second);
        errorCode = git_commit_create(&newCommit, repo, "HEAD", committerSignature,
            committerSignature, "UTF-8", commitMsg.c_str(), tree,
            sizeof(parents) / sizeof(git_commit*), static_cast<const git_commit**>(parents));
        if (errorCode != 0) {
            errorMessage = errorCodeToMessage(errorCode);
            git_repository_free(repo);
            git_libgit2_shutdown();
            return;
        }
    }

    git_annotated_commit_free(originMaster);
    git_repository_state_cleanup(repo);

    git_repository_free(repo);
    git_libgit2_shutdown();
}
//=============================================================================
wstringVector
RepositoryBranchList(const std::wstring& localPath, std::wstring& errorMessage)
{
    wstringVector branchs;
    std::string localPathUtf8 = wstring_to_utf8(localPath);
    git_repository* repo = NULL;
    git_libgit2_init();

    int errorCode = git_repository_open(&repo, localPathUtf8.c_str());
    if (errorCode != 0) {
        errorMessage = errorCodeToMessage(errorCode);
        return branchs;
    }
    git_reference* ref;
    errorCode = git_repository_head(&ref, repo);
    if (errorCode != 0) {
        errorMessage = errorCodeToMessage(errorCode);
        return branchs;
    }
    const char* currentName = NULL;
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
        const char* name = NULL;
        while (!git_branch_next(&ref, &type, it)) {
            git_reference_free(ref);
            errorCode = git_branch_name(&name, ref);
            if (errorCode == 0) {
                std::wstring wname = utf8_to_wstring(name);
                if (wname == currentBranchName && !currentBranchName.empty()) {
                    wname = std::wstring(L"* ") + wname;
                }
                branchs.push_back(wname);
            } else {
                errorMessage = errorCodeToMessage(errorCode);
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
wstringVector
RepositoryTagList(const std::wstring& localPath, std::wstring& errorMessage)
{
    wstringVector tags;
    std::string localPathUtf8 = wstring_to_utf8(localPath);
    git_repository* repo = NULL;
    git_libgit2_init();

    int errorCode = git_repository_open(&repo, localPathUtf8.c_str());
    if (errorCode != 0) {
        errorMessage = errorCodeToMessage(errorCode);
        git_libgit2_shutdown();
        return tags;
    }

    git_strarray tag_list;
    errorCode = git_tag_list(&tag_list, repo);
    if (errorCode != 0) {
        errorMessage = errorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return tags;
    }
    for (size_t i = 0; i < tag_list.count; i++) {
        const char* name = tag_list.strings[i];
        tags.push_back(utf8_to_wstring(name));
    }
    git_strarray_free(&tag_list);
    git_repository_free(repo);
    git_libgit2_shutdown();
    return tags;
}
//=============================================================================
ArrayOf
RepositoryLog(const std::wstring& localPath, std::wstring& errorMessage)
{
    ArrayOf logs;
    std::string localPathUtf8 = wstring_to_utf8(localPath);
    git_repository* repo = NULL;
    git_libgit2_init();

    int errorCode = git_repository_open(&repo, localPathUtf8.c_str());
    if (errorCode != 0) {
        errorMessage = errorCodeToMessage(errorCode);
        git_libgit2_shutdown();
        return logs;
    }
    git_revwalk* walk;
    errorCode = git_revwalk_new(&walk, repo);
    if (errorCode != 0) {
        errorMessage = errorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return logs;
    }
    git_revwalk_sorting(walk, GIT_SORT_TOPOLOGICAL | GIT_SORT_TIME);
    errorCode = git_revwalk_push_head(walk);
    if (errorCode != 0) {
        errorMessage = errorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return logs;
    }
    git_oid oid;
    ArrayOfVector sha1s;
    ArrayOfVector msgs;
    ArrayOfVector times;
    ArrayOfVector authors;

    while (git_revwalk_next(&oid, walk) == 0) {
        git_commit* c;

        char oidstr[10] = { 0 };
        git_commit_lookup(&c, repo, &oid);
        git_oid_tostr(oidstr, 9, &oid);
        sha1s.push_back(ArrayOf::characterArrayConstructor(oidstr));
        std::string message = std::string(git_commit_message(c));
        if (boost::algorithm::ends_with(message, "\n")) {
            message.pop_back();
        }
        msgs.push_back(ArrayOf::characterArrayConstructor(message));
        const git_signature* author = git_commit_author(c);
        authors.push_back(ArrayOf::characterArrayConstructor(
            std::string(author->name) + " <" + std::string(author->email) + ">"));
        time_t time = git_commit_time(c);
        std::string strtime = std::string(ctime(&time));
        if (boost::algorithm::ends_with(strtime, "\n")) {
            strtime.pop_back();
        }
        times.push_back(ArrayOf::characterArrayConstructor(strtime));
        git_commit_free(c);
    }
    git_repository_free(repo);
    git_libgit2_shutdown();

    Dimensions dims(1, sha1s.size());
    stringVector fieldnames;
    fieldnames.push_back("sha1");
    fieldnames.push_back("message");
    fieldnames.push_back("author");
    fieldnames.push_back("date");
    auto* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.getElementCount(), fieldnames, false));
    logs = ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, fieldnames);
    logs.setFieldAsList("sha1", sha1s);
    logs.setFieldAsList("message", msgs);
    logs.setFieldAsList("author", authors);
    logs.setFieldAsList("date", times);
    return logs;
}
//=============================================================================
}
//=============================================================================
