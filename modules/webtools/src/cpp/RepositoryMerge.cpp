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
#include "RepositoryMerge.hpp"
#include "RepositoryHelpers.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
RepositoryMerge(const std::wstring& localPath, std::wstring& errorMessage)
{
    git_libgit2_init();
    git_repository* repo = NULL;
    std::string localPathUtf8 = wstring_to_utf8(localPath);

    int errorCode = git_repository_open(&repo, localPathUtf8.c_str());
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_libgit2_shutdown();
        return;
    }

    std::pair<std::string, git_oid> oid_to_merge;
    errorCode = git_repository_fetchhead_foreach(repo,
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
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }

    git_annotated_commit* originMaster;
    errorCode = git_annotated_commit_lookup(&originMaster, repo, &oid_to_merge.second);
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }

    auto annotatedCommits = const_cast<const git_annotated_commit**>(&originMaster);
    git_merge_analysis_t mergeAnalyse;
    git_merge_preference_t mergePreferences;
    errorCode = git_merge_analysis(&mergeAnalyse, &mergePreferences, repo, annotatedCommits, 1);
    if (errorCode != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }

    if ((mergeAnalyse & GIT_MERGE_ANALYSIS_UP_TO_DATE) != 0
        || (mergeAnalyse & GIT_MERGE_ANALYSIS_NONE) != 0) {
        errorMessage = gitErrorCodeToMessage(errorCode);
        git_repository_free(repo);
        git_libgit2_shutdown();
        return;
    }

    if ((mergeAnalyse & GIT_MERGE_ANALYSIS_FASTFORWARD) != 0) {
        git_reference* master;
        errorCode = git_repository_head(&master, repo);
        if (errorCode != 0) {
            errorMessage = gitErrorCodeToMessage(errorCode);
            git_repository_free(repo);
            git_libgit2_shutdown();
            return;
        }
        git_reference* createdRef;
        errorCode = git_reference_set_target(&createdRef, master, &oid_to_merge.second, "pull");
        if (errorCode != 0) {
            errorMessage = gitErrorCodeToMessage(errorCode);
            git_repository_free(repo);
            git_libgit2_shutdown();
            return;
        }
        git_index* head;
        errorCode = git_repository_index(&head, repo);
        if (errorCode != 0) {
            errorMessage = gitErrorCodeToMessage(errorCode);
            git_repository_free(repo);
            git_libgit2_shutdown();
            return;
        }
        git_oid oid{};
        errorCode = git_index_write_tree_to(&oid, head, repo);
        if (errorCode != 0) {
            errorMessage = gitErrorCodeToMessage(errorCode);
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
            errorMessage = gitErrorCodeToMessage(errorCode);
            git_repository_free(repo);
            git_libgit2_shutdown();
            return;
        }
        git_index* head;
        errorCode = git_repository_index(&head, repo);
        if (errorCode != 0) {
            errorMessage = gitErrorCodeToMessage(errorCode);
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
            errorMessage = gitErrorCodeToMessage(errorCode);
            git_repository_free(repo);
            git_libgit2_shutdown();
            return;
        }
        git_commit* origin_master_commit;
        errorCode = git_commit_lookup(&origin_master_commit, repo, &oid_to_merge.second);
        if (errorCode != 0) {
            errorMessage = gitErrorCodeToMessage(errorCode);
            git_repository_free(repo);
            git_libgit2_shutdown();
            return;
        }
        git_oid parent_headoid{};
        errorCode = git_reference_name_to_id(&parent_headoid, repo, "HEAD");
        if (errorCode != 0) {
            errorMessage = gitErrorCodeToMessage(errorCode);
            git_repository_free(repo);
            git_libgit2_shutdown();
            return;
        }

        git_commit* head_parent;
        errorCode = git_commit_lookup(&head_parent, repo, &parent_headoid);
        if (errorCode != 0) {
            errorMessage = gitErrorCodeToMessage(errorCode);
            git_repository_free(repo);
            git_libgit2_shutdown();
            return;
        }
        git_tree* tree;
        errorCode = git_commit_tree(&tree, head_parent);
        if (errorCode != 0) {
            errorMessage = gitErrorCodeToMessage(errorCode);
            git_repository_free(repo);
            git_libgit2_shutdown();
            return;
        }

        const git_commit* parents[] = { head_parent, origin_master_commit };

        git_oid newCommit{};
        std::string commitMsg = std::string("Merge ") + git_oid_tostr_s(&oid_to_merge.second);
        errorCode = git_commit_create(&newCommit, repo, "HEAD", committerSignature,
            committerSignature, "UTF-8", commitMsg.c_str(), tree,
            sizeof(parents) / sizeof(git_commit*), static_cast<const git_commit**>(parents));
        if (errorCode != 0) {
            errorMessage = gitErrorCodeToMessage(errorCode);
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
}
//=============================================================================
