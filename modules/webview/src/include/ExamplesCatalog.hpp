//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "nlsWebview_exports.h"
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
struct ExamplesModule
{
    std::wstring name;
    std::wstring path;
    bool external = false;
};
//=============================================================================
// The catalog JSON is {"groups": [...], "diagnostics": [...]}. Every group is
// {"name", "title", "source": "nelson" | "external" | "nflow", "examples"} and
// every example carries "title", "description", "kind" ("script", "command"
// or "model"), "tags", "requirements" and either "file" or "command".
struct ExamplesCatalog
{
    std::string json;
    std::vector<std::wstring> allowedPaths;
    std::vector<std::wstring> allowedCommands;
};
//=============================================================================
// Builds the catalog of the examples/index.json manifests of the given modules
// and appends the groups produced by external providers. Each provider text
// is a JSON array of groups in the catalog schema (an NFlow model uses
// "kind": "model" and a "file" pointing at the .nflow document).
// requirementVocabularyPath points at etc/gallery_requirements.json, the single
// source of truth for the requirement tokens: an example with a token outside
// it is reported as a diagnostic. When the path is empty or unreadable the
// validation is skipped (the test helper still enforces the vocabulary in CI).
NLSWEBVIEW_IMPEXP ExamplesCatalog
BuildExamplesCatalog(const std::vector<ExamplesModule>& modules, const std::string& locale,
    const std::vector<std::string>& providerGroupsJson = {},
    const std::wstring& requirementVocabularyPath = {});
//=============================================================================
} // namespace Nelson
//=============================================================================
