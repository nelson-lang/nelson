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
#if _MSC_VER
#pragma warning(disable : 4251)
#endif
//=============================================================================
#include <string>
#include <unordered_map>
#include <vector>
#include <mutex>
#include <nlohmann/json.hpp>
#include "nlsNflow_engine_exports.h"
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
// Declarative description of a block type, resolved from the library manifests
// shipped by the nflow_blocks module. Drives programmatic model construction:
// default parameters, port counts and default geometry.
//=============================================================================
struct BlockCatalogEntry
{
    std::string type; // canonical block type, e.g. "gain"
    std::string libraryId; // e.g. "builtin.math"
    nlohmann::json defaultParams; // object copied into a new block's params
    int inputCount = 0;
    int outputCount = 0;
    int width = 80;
    int height = 80;
};
//=============================================================================
// Thread-safe catalog of block types. Loaded from the nflow_blocks library
// manifests (modules/nflow_blocks/libraries/<category>/library.json).
//=============================================================================
class NLSNFLOW_ENGINE_IMPEXP NFlowBlockCatalog
{
public:
    static NFlowBlockCatalog&
    instance();

    // Scan a directory of <subdir>/library.json manifests and register their
    // block types. Safe to call multiple times (later entries override).
    void
    loadManifestDirectory(const std::wstring& directory);

    // Idempotent; resolves the nflow_blocks libraries path once.
    void
    ensureLoaded();

    // Resolve a source specifier to a canonical block type. Accepted forms:
    //   "gain"                          (bare type)
    //   "nflow/math/gain"               (nflow/<category>/<type>)
    //   "nflow.builtin.math.gain"       (dotted editor alias)
    // Returns true and fills outType on success. Ambiguous bare types (same
    // name in two libraries) fail with ambiguous=true.
    bool
    resolveType(const std::string& source, std::string& outType, bool& ambiguous) const;

    bool
    hasType(const std::string& type) const;

    // Returns nullptr when the type is unknown.
    const BlockCatalogEntry*
    find(const std::string& type) const;

    std::vector<std::string>
    types() const;

    void
    clear(); // for tests

private:
    NFlowBlockCatalog() = default;
    NFlowBlockCatalog(const NFlowBlockCatalog&) = delete;
    NFlowBlockCatalog&
    operator=(const NFlowBlockCatalog&)
        = delete;

    void
    registerManifest(const nlohmann::json& manifest);

    mutable std::mutex m_mutex;
    std::unordered_map<std::string, BlockCatalogEntry> m_entries; // type -> entry
    bool m_builtinLoaded = false;
};
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
