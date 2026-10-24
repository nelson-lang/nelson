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
#include <vector>
#include <memory>
#include <unordered_map>
#include <mutex>
#include "NFlowModelDocument.hpp"
#include "nlsNflow_engine_exports.h"
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
// Thread-safe registry of loaded models, indexed by name and by numeric
// handle. Handles are doubles (reference-style); they are invalidated when the
// model is closed.
//=============================================================================
class NLSNFLOW_ENGINE_IMPEXP NFlowModelRegistry
{
public:
    static NFlowModelRegistry&
    instance();

    // Create and register an empty model. Empty requestedName -> auto name
    // (untitled, untitled1, ...). Returns the handle; fills outName. Fails if
    // requestedName is already loaded.
    bool
    createEmpty(const std::string& requestedName, double& outHandle, std::string& outName,
        std::string& err);

    // Register a model parsed elsewhere (e.g. from a file). Takes ownership.
    // If a model with the same name is already loaded, its content is refreshed
    // in place and the existing handle is returned (reloading an edited file
    // reflects the new content).
    bool
    add(std::unique_ptr<NFlowModelDocument> doc, double& outHandle, std::string& err);

    // Resolve either a handle (numeric) or a name to a document. Returns null
    // when not found.
    NFlowModelDocument*
    resolveByName(const std::string& name);
    NFlowModelDocument*
    resolveByHandle(double handle);

    bool
    handleOf(const std::string& name, double& outHandle) const;
    bool
    nameOfHandle(double handle, std::string& outName) const;

    // Block handles (gcbh): numeric handles for a specific block, allocated from
    // the same sequence as model handles so a numeric designator resolves
    // unambiguously to a model or a block. blockPath is a full 'model/block' path.
    // Invalidated when the block is deleted or its model is closed.
    // handleForBlock is idempotent.
    double
    handleForBlock(const std::string& blockPath);
    bool
    resolveBlockHandle(double handle, std::string& blockPath) const;
    void
    invalidateBlockHandle(const std::string& blockPath);

    bool
    isLoaded(const std::string& name) const;

    bool
    close(const std::string& name);

    std::vector<std::string>
    loadedNames() const;

    // Current editing context: the active system path (gcs) and the selected block
    // path (gcb). The GUI host updates them; new_system/load_system/open_system
    // also set the current system so a headless script has a defined value.
    // Closing the current model clears them. Empty string means "none".
    void
    setCurrentSystem(const std::string& systemPath);
    std::string
    currentSystem() const;
    void
    setCurrentBlock(const std::string& blockPath);
    std::string
    currentBlock() const;

    void
    clear(); // for tests

private:
    NFlowModelRegistry() = default;
    NFlowModelRegistry(const NFlowModelRegistry&) = delete;
    NFlowModelRegistry&
    operator=(const NFlowModelRegistry&)
        = delete;

    std::string
    autoName() const;

    mutable std::mutex m_mutex;
    std::unordered_map<std::string, std::unique_ptr<NFlowModelDocument>> m_byName;
    std::unordered_map<double, std::string> m_handleToName;
    std::unordered_map<std::string, double> m_nameToHandle;
    std::unordered_map<double, std::string> m_blockHandleToPath;
    std::unordered_map<std::string, double> m_pathToBlockHandle;
    double m_nextHandle = 1.0;
    std::string m_currentSystem;
    std::string m_currentBlock;
};
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
