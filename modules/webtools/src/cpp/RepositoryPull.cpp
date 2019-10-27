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
#include "RepositoryPull.hpp"
#include "RepositoryHelpers.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
RepositoryPull(const std::wstring& localPath, std::wstring& errorMessage)
{
    git_libgit2_init();
    git_repository* repo = NULL;
    std::string localPathUtf8 = wstring_to_utf8(localPath);
    int errorCode = git_repository_open(&repo, localPathUtf8.c_str());

    git_reference* ref;
    errorCode = git_repository_head(&ref, repo);
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }
    const char* currentName = nullptr;
    errorCode = git_branch_name(&currentName, ref);
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }
    git_reference_free(ref);

    std::string localBranchName = currentName;

    git_remote* remote = nullptr;
    errorCode = git_remote_lookup(&remote, repo, "origin");
    if (errorCode < 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }

    git_fetch_options fetch_opts = GIT_FETCH_OPTIONS_INIT;
    fetch_opts.prune = GIT_FETCH_PRUNE;

    // const char* refs[] = { "refs/heads/branch_dev:refs/heads/branch_dev" };
    // git_strarray refspecs = { (char**)refs, 1 };
    // errorCode = git_remote_fetch(remote, &refspecs, &fetch_opts, nullptr);

    errorCode = git_remote_fetch(remote, nullptr, &fetch_opts, nullptr);
    if (errorCode < 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }

    std::string originBranchName = std::string("origin/") + localBranchName;

    git_reference* origin_remote = nullptr;
    errorCode
        = git_branch_lookup(&origin_remote, repo, originBranchName.c_str(), GIT_BRANCH_REMOTE);
    if (errorCode < 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }

    git_reference* local_master = nullptr;
    errorCode = git_branch_lookup(&local_master, repo, localBranchName.c_str(), GIT_BRANCH_LOCAL);
    if (errorCode < 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }

    errorCode = git_repository_set_head(repo, git_reference_name(local_master));
    if (errorCode < 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }

    const git_annotated_commit* their_head[10];
    errorCode = git_annotated_commit_from_ref(
        (git_annotated_commit**)&their_head[0], repo, origin_remote);

    git_merge_options merge_opt = GIT_MERGE_OPTIONS_INIT;
    git_checkout_options checkout_opt = GIT_CHECKOUT_OPTIONS_INIT;
    errorCode = git_merge(repo, their_head, 1, &merge_opt, &checkout_opt);
    if (errorCode < 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }

    git_index* index = nullptr;
    errorCode = git_repository_index(&index, repo);
    if (git_index_has_conflicts(index)) {
        git_index_conflict_iterator* conflict_iterator = nullptr;
        const git_index_entry* ancestor_out = nullptr;
        const git_index_entry* our_out = nullptr;
        const git_index_entry* their_out = nullptr;

        errorCode = git_index_conflict_iterator_new(&conflict_iterator, index);

        while (git_index_conflict_next(&ancestor_out, &our_out, &their_out, conflict_iterator)
            != GIT_ITEROVER) {
        }
        git_checkout_options opt = GIT_CHECKOUT_OPTIONS_INIT;
        opt.checkout_strategy |= GIT_CHECKOUT_USE_THEIRS;
        errorCode = git_checkout_index(repo, index, &opt);
        git_index_conflict_iterator_free(conflict_iterator);
    }

    git_commit* their_commit = nullptr;
    errorCode = git_commit_lookup(&their_commit, repo, git_reference_target(origin_remote));

    git_commit* our_commit = nullptr;
    errorCode = git_commit_lookup(&our_commit, repo, git_reference_target(local_master));

    // add and commit
    errorCode = git_index_update_all(index, nullptr, nullptr, nullptr);
    if (errorCode < 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }

    errorCode = git_index_write(index);
    if (errorCode < 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }

    git_oid new_tree_id;
    errorCode = git_index_write_tree(&new_tree_id, index);
    if (errorCode < 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }

    git_tree* new_tree = nullptr;
    errorCode = git_tree_lookup(&new_tree, repo, &new_tree_id);
    if (errorCode < 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }

    git_signature* signature = nullptr;
    errorCode = git_signature_now(&signature, "Nelson", "None");

    git_oid commit_id;
    errorCode = git_commit_create_v(&commit_id, repo, git_reference_name(local_master), signature,
        signature, "UTF-8", "Nelson pull commit", new_tree, 2, our_commit, their_commit);

    errorCode = git_repository_state_cleanup(repo);

    git_repository_free(repo);
    git_libgit2_shutdown();
}
//=============================================================================
}
//=============================================================================
