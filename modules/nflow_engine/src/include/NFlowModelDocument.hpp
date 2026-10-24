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
#include <cstdint>
#include <functional>
#include <mutex>
#include <string>
#include <vector>
#include <nlohmann/json.hpp>
#include "nlsNflow_engine_exports.h"
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
// Who produced the latest change to a document. The live editor session uses it
// to break the commit/push echo loop between the scripted API and the GUI editor.
enum class ChangeOrigin
{
    Load,
    Script,
    Gui
};
//=============================================================================
// In-memory .nflow model. The nlohmann::json diagram is the single source of
// truth; all mutations are validated against the block catalog. Parameter
// values are exchanged as strings and converted to/from JSON internally.
//
// Thread-safety: the scripted builtins run on the interpreter thread while the
// GUI RPC bridge may commit edits from the WebUI callback thread. Every public
// mutator, replaceDiagram(), and the serializing readers (toJsonString(),
// revision(), dirty()) take m_mutex. The raw diagram() reference accessor is
// only for same-thread scripted callers; cross-thread readers use
// toJsonString().
//=============================================================================
class NLSNFLOW_ENGINE_IMPEXP NFlowModelDocument
{
public:
    explicit NFlowModelDocument(const std::string& name);

    static bool
    fromJsonString(const std::string& jsonText, const std::string& name, NFlowModelDocument& out,
        std::string& err);

    const std::string&
    name() const;
    void
    setName(const std::string& name);
    const std::string&
    filePath() const;
    void
    setFilePath(const std::string& path);
    bool
    dirty() const;
    void
    setDirty(bool value);

    // Monotonic change counter, bumped by every mutation (scripted or GUI). The
    // live document-editor session compares revisions to suppress echo commits.
    std::uint64_t
    revision() const;
    ChangeOrigin
    lastOrigin() const;

    // Called after every mutation. The GUI host uses it to push a "model changed"
    // event to the editor window, which then pulls the fresh diagram. One listener
    // only (one editor window per model); pass nullptr to clear.
    using ChangeListener = std::function<void(std::uint64_t revision, ChangeOrigin origin)>;
    void
    setChangeListener(ChangeListener listener);

    // Replace the whole diagram in one step (GUI commit path). Requires blocks and
    // connections to be present, keeps the document name, bumps the revision and
    // marks dirty. On failure returns false and leaves the current diagram intact.
    bool
    replaceDiagram(const nlohmann::json& next, ChangeOrigin origin, std::string& err);

    const nlohmann::json&
    diagram() const;

    // Resolve source via the catalog, create a block seeded with the catalog
    // default parameters. makeUnique appends a numeric suffix on collision;
    // otherwise a collision is an error. outName receives the final block id.
    bool
    addBlock(const std::string& source, const std::string& blockName, bool makeUnique,
        std::string& outName, std::string& err);
    bool
    deleteBlock(const std::string& blockName, std::string& err);
    bool
    renameBlock(const std::string& oldName, const std::string& newName, std::string& err);
    bool
    hasBlock(const std::string& blockName) const;
    std::vector<std::string>
    blockNames() const;

    bool
    setBlockParam(const std::string& blockName, const std::string& key, const std::string& value,
        std::string& err);
    bool
    getBlockParam(const std::string& blockName, const std::string& key, std::string& outValue,
        std::string& err) const;
    // Typed parameter access: a JSON value (number, array, matrix, bool, string)
    // lets vector/matrix parameters round-trip without string serialization.
    // Scalars set through setBlockParam keep the string convention.
    bool
    setBlockParamJson(const std::string& blockName, const std::string& key,
        const nlohmann::json& value, std::string& err);
    bool
    getBlockParamJson(const std::string& blockName, const std::string& key,
        nlohmann::json& outValue, std::string& err) const;
    // setBlockPosition takes [x y] and size (w,h); getBlockPosition reports
    // [x y x2 y2].
    bool
    setBlockPosition(
        const std::string& blockName, double x, double y, double w, double h, std::string& err);
    bool
    getBlockPosition(const std::string& blockName, double& x, double& y, double& x2, double& y2,
        std::string& err) const;
    // Model-level params: Name, StopTime, SampleTime, AutoRoute, Locked,
    // ScreenColor, Variables (Dirty is read-only).
    bool
    setModelParam(const std::string& key, const std::string& value, std::string& err);
    bool
    getModelParam(const std::string& key, std::string& outValue, std::string& err) const;

    // Ports are 1-based (external convention); stored 0-based internally.
    bool
    addLine(const std::string& fromBlock, int fromPort, const std::string& toBlock, int toPort,
        bool autoRoute, std::string& err);
    bool
    deleteLine(const std::string& fromBlock, int fromPort, const std::string& toBlock, int toPort,
        std::string& err);
    // Connect first output of src to the first unconnected input of dst.
    bool
    connectBlocks(const std::string& src, const std::string& dst, std::string& err);
    // Signal name of an existing connection. Endpoints use the 1-based port
    // convention of addLine; both must be in one container.
    bool
    setLineName(const std::string& fromBlock, int fromPort, const std::string& toBlock, int toPort,
        const std::string& name, std::string& err);
    bool
    getLineName(const std::string& fromBlock, int fromPort, const std::string& toBlock, int toPort,
        std::string& outName, std::string& err) const;

    // Arrange the blocks of one (sub)system into left-to-right layers by
    // connectivity. systemPath is relative to this model: "" = the root diagram,
    // "Sub" / "Sub/Inner" = a nested subsystem. Only that depth is arranged.
    bool
    layoutSystem(const std::string& systemPath, std::string& err);

    // Recompute the wire routes of one (sub)system and store the waypoints on each
    // connection. Port geometry is approximated from the catalog port counts.
    bool
    rerouteSystem(
        const std::string& systemPath, int canvasWidth, int canvasHeight, std::string& err);

    std::string
    toJsonString() const;

private:
    // Resolve a (possibly nested) "Sub/.../leaf" path to the diagram container
    // that directly holds the leaf block, and set leaf to the last segment. A bare
    // name resolves to the root diagram. The mutable form creates missing nested
    // subsystem diagrams on the way down; the const form returns nullptr instead.
    nlohmann::json*
    containerForPath(
        const std::string& path, std::string& leaf, std::string& err, bool createMissing = false);
    const nlohmann::json*
    containerForPath(const std::string& path, std::string& leaf, std::string& err) const;
    // Resolve a (sub)system path ("" = root, "Sub", "Sub/Inner") to its diagram
    // container. Unlike containerForPath, every segment is a subsystem.
    nlohmann::json*
    systemContainer(const std::string& systemPath, std::string& err);
    nlohmann::json*
    findBlock(const std::string& blockName);
    const nlohmann::json*
    findBlock(const std::string& blockName) const;
    // Bump revision, mark dirty and record the origin. Callers must already
    // hold m_mutex.
    void
    markMutated(ChangeOrigin origin);

    nlohmann::json m_diagram;
    std::string m_name;
    std::string m_filePath;
    bool m_dirty = false;
    std::uint64_t m_revision = 0;
    ChangeOrigin m_lastOrigin = ChangeOrigin::Load;
    ChangeListener m_changeListener;
    mutable std::recursive_mutex m_mutex;
};
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
