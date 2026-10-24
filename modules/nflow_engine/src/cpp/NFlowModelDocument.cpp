//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NFlowModelDocument.hpp"
#include "NFlowBlockCatalog.hpp"
#include "RouterEngine.hpp"
#include "FieldNames.hpp"
#include <algorithm>
#include <cmath>
#include <functional>
#include <unordered_map>
#include <unordered_set>
#include <vector>
//=============================================================================
using json = nlohmann::json;
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
// Root-level schema identifier of a .nflow document. Distinct from the
// block-parameter constant nflow::kFormat ("Format", display block): the file
// key is lowercase and identifies the schema generation a reader keys on.
static constexpr const char* kFileFormatKey = "format";
static constexpr const char* kCurrentFileFormat = "nflow-2";
//=============================================================================
static std::vector<std::string>
splitPath(const std::string& path);
static bool
checkDiagramFormat(const json& diagram, std::string& err);
static json*
findBlockInDiagram(json& diagram, const std::string& id);
static const json*
findBlockInDiagram(const json& diagram, const std::string& id);
static bool
inputOccupiedIn(const json& container, const std::string& leaf, int toIndex0);
static void
computeLayeredLayout(json& blocks, const json* connections);
//=============================================================================
static std::string
jsonScalarToString(const json& v)
{
    if (v.is_string()) {
        return v.get<std::string>();
    }
    if (v.is_boolean()) {
        return v.get<bool>() ? "on" : "off";
    }
    if (v.is_number_integer()) {
        return std::to_string(v.get<long long>());
    }
    if (v.is_number()) {
        return v.dump();
    }
    if (v.is_null()) {
        return "";
    }
    return v.dump();
}
//=============================================================================
NFlowModelDocument::NFlowModelDocument(const std::string& name) : m_name(name)
{
    m_diagram = json::object();
    m_diagram["version"] = "1.0.0";
    m_diagram[kFileFormatKey] = kCurrentFileFormat;
    m_diagram[nflow::kMetadata] = { { nflow::kTool, "Nelson nflow" } };
    m_diagram[nflow::kName] = name;
    m_diagram[nflow::kBlocks] = json::array();
    m_diagram[nflow::kConnections] = json::array();
    m_diagram[nflow::kVariables] = "";
    m_diagram[nflow::kRuntime] = 10;
    m_diagram[nflow::kSampleTime] = 0.01;
    m_diagram[nflow::kAutoRoute] = true;
    m_diagram[nflow::kLocked] = false;
}
//=============================================================================
bool
NFlowModelDocument::fromJsonString(
    const std::string& jsonText, const std::string& name, NFlowModelDocument& out, std::string& err)
{
    json parsed;
    try {
        parsed = json::parse(jsonText);
    } catch (const json::exception&) {
        err = "Error parsing .nflow model content.";
        return false;
    }
    if (!checkDiagramFormat(parsed, err)) {
        return false;
    }
    if (!parsed.contains(nflow::kBlocks)) {
        parsed[nflow::kBlocks] = json::array();
    }
    if (!parsed.contains(nflow::kConnections)) {
        parsed[nflow::kConnections] = json::array();
    }
    out.m_diagram = std::move(parsed);
    out.m_name = name;
    out.m_diagram[nflow::kName] = name;
    out.m_dirty = false;
    return true;
}
//=============================================================================
void
NFlowModelDocument::markMutated(ChangeOrigin origin)
{
    // Caller holds m_mutex.
    ++m_revision;
    m_dirty = true;
    m_lastOrigin = origin;
    if (m_changeListener) {
        // The listener only sends a small "changed" notification (the editor
        // pulls the diagram afterwards through toJsonString, which re-locks the
        // recursive mutex on this same thread). Firing under the lock is safe.
        m_changeListener(m_revision, origin);
    }
}
//=============================================================================
void
NFlowModelDocument::setChangeListener(ChangeListener listener)
{
    std::lock_guard<std::recursive_mutex> lk(m_mutex);
    m_changeListener = std::move(listener);
}
//=============================================================================
const std::string&
NFlowModelDocument::name() const
{
    return m_name;
}
//=============================================================================
void
NFlowModelDocument::setName(const std::string& name)
{
    std::lock_guard<std::recursive_mutex> lk(m_mutex);
    m_name = name;
    m_diagram[nflow::kName] = name;
    markMutated(ChangeOrigin::Script);
}
//=============================================================================
const std::string&
NFlowModelDocument::filePath() const
{
    return m_filePath;
}
//=============================================================================
void
NFlowModelDocument::setFilePath(const std::string& path)
{
    std::lock_guard<std::recursive_mutex> lk(m_mutex);
    m_filePath = path;
}
//=============================================================================
bool
NFlowModelDocument::dirty() const
{
    std::lock_guard<std::recursive_mutex> lk(m_mutex);
    return m_dirty;
}
//=============================================================================
void
NFlowModelDocument::setDirty(bool value)
{
    std::lock_guard<std::recursive_mutex> lk(m_mutex);
    m_dirty = value;
}
//=============================================================================
std::uint64_t
NFlowModelDocument::revision() const
{
    std::lock_guard<std::recursive_mutex> lk(m_mutex);
    return m_revision;
}
//=============================================================================
ChangeOrigin
NFlowModelDocument::lastOrigin() const
{
    std::lock_guard<std::recursive_mutex> lk(m_mutex);
    return m_lastOrigin;
}
//=============================================================================
bool
NFlowModelDocument::replaceDiagram(const json& next, ChangeOrigin origin, std::string& err)
{
    std::lock_guard<std::recursive_mutex> lk(m_mutex);
    if (!next.is_object()) {
        err = "A .nflow diagram must be a JSON object.";
        return false;
    }
    if (!checkDiagramFormat(next, err)) {
        return false;
    }
    json updated = next;
    if (!updated.contains(nflow::kBlocks) || !updated[nflow::kBlocks].is_array()) {
        updated[nflow::kBlocks] = json::array();
    }
    if (!updated.contains(nflow::kConnections) || !updated[nflow::kConnections].is_array()) {
        updated[nflow::kConnections] = json::array();
    }
    // Identity is owned by the document, not by the incoming payload: keep the
    // model name stable across a GUI commit.
    updated[nflow::kName] = m_name;
    m_diagram = std::move(updated);
    markMutated(origin);
    return true;
}
//=============================================================================
const json&
NFlowModelDocument::diagram() const
{
    // Same-thread scripted callers only; cross-thread readers use toJsonString().
    return m_diagram;
}
//=============================================================================
json*
NFlowModelDocument::containerForPath(
    const std::string& path, std::string& leaf, std::string& err, bool createMissing)
{
    std::vector<std::string> parts = splitPath(path);
    if (parts.empty()) {
        err = "Empty block path.";
        return nullptr;
    }
    leaf = parts.back();
    json* container = &m_diagram;
    for (size_t i = 0; i + 1 < parts.size(); ++i) {
        json* sub = findBlockInDiagram(*container, parts[i]);
        if (!sub) {
            err = "No block named '" + parts[i] + "'.";
            return nullptr;
        }
        if (!sub->contains(nflow::kParams) || !(*sub)[nflow::kParams].is_object()) {
            if (!createMissing) {
                err = "Block '" + parts[i] + "' is not a subsystem.";
                return nullptr;
            }
            (*sub)[nflow::kParams] = json::object();
        }
        json& params = (*sub)[nflow::kParams];
        if (!params.contains("subsystem") || !params["subsystem"].is_object()) {
            if (!createMissing) {
                err = "Block '" + parts[i] + "' is not a subsystem.";
                return nullptr;
            }
            params["subsystem"] = json::object();
        }
        json& subDiagram = params["subsystem"];
        if (!subDiagram.contains(nflow::kBlocks) || !subDiagram[nflow::kBlocks].is_array()) {
            subDiagram[nflow::kBlocks] = json::array();
        }
        if (!subDiagram.contains(nflow::kConnections)
            || !subDiagram[nflow::kConnections].is_array()) {
            subDiagram[nflow::kConnections] = json::array();
        }
        container = &subDiagram;
    }
    return container;
}
//=============================================================================
const json*
NFlowModelDocument::containerForPath(
    const std::string& path, std::string& leaf, std::string& err) const
{
    std::vector<std::string> parts = splitPath(path);
    if (parts.empty()) {
        err = "Empty block path.";
        return nullptr;
    }
    leaf = parts.back();
    const json* container = &m_diagram;
    for (size_t i = 0; i + 1 < parts.size(); ++i) {
        const json* sub = findBlockInDiagram(*container, parts[i]);
        if (!sub) {
            err = "No block named '" + parts[i] + "'.";
            return nullptr;
        }
        if (!sub->contains(nflow::kParams) || !(*sub)[nflow::kParams].is_object()
            || !(*sub)[nflow::kParams].contains("subsystem")
            || !(*sub)[nflow::kParams]["subsystem"].is_object()) {
            err = "Block '" + parts[i] + "' is not a subsystem.";
            return nullptr;
        }
        container = &(*sub)[nflow::kParams]["subsystem"];
    }
    return container;
}
//=============================================================================
json*
NFlowModelDocument::findBlock(const std::string& blockName)
{
    std::string leaf, err;
    json* container = containerForPath(blockName, leaf, err, false);
    if (!container) {
        return nullptr;
    }
    return findBlockInDiagram(*container, leaf);
}
//=============================================================================
const json*
NFlowModelDocument::findBlock(const std::string& blockName) const
{
    std::string leaf, err;
    const json* container = containerForPath(blockName, leaf, err);
    if (!container) {
        return nullptr;
    }
    return findBlockInDiagram(*container, leaf);
}
//=============================================================================
bool
NFlowModelDocument::hasBlock(const std::string& blockName) const
{
    std::lock_guard<std::recursive_mutex> lk(m_mutex);
    return findBlock(blockName) != nullptr;
}
//=============================================================================
std::vector<std::string>
NFlowModelDocument::blockNames() const
{
    std::lock_guard<std::recursive_mutex> lk(m_mutex);
    std::vector<std::string> names;
    for (const auto& b : m_diagram[nflow::kBlocks]) {
        names.push_back(b.value(nflow::kId, std::string()));
    }
    return names;
}
//=============================================================================
bool
NFlowModelDocument::addBlock(const std::string& source, const std::string& blockName,
    bool makeUnique, std::string& outName, std::string& err)
{
    std::lock_guard<std::recursive_mutex> lk(m_mutex);
    NFlowBlockCatalog::instance().ensureLoaded();
    std::string type;
    bool ambiguous = false;
    if (!NFlowBlockCatalog::instance().resolveType(source, type, ambiguous)) {
        err = ambiguous ? ("Ambiguous block source: " + source)
                        : ("Unknown block source: " + source);
        return false;
    }
    // The destination may be a nested "Sub/.../leaf" path: resolve the
    // container that will hold the leaf, creating intermediate subsystem
    // diagrams as needed.
    std::string leaf;
    json* container = containerForPath(blockName, leaf, err, /*createMissing=*/true);
    if (!container) {
        return false;
    }
    std::string finalLeaf = leaf;
    if (findBlockInDiagram(*container, finalLeaf) != nullptr) {
        if (!makeUnique) {
            err = "A block named '" + finalLeaf + "' already exists.";
            return false;
        }
        for (int i = 1; i < 100000; ++i) {
            std::string candidate = leaf + std::to_string(i);
            if (findBlockInDiagram(*container, candidate) == nullptr) {
                finalLeaf = candidate;
                break;
            }
        }
    }
    const BlockCatalogEntry* entry = NFlowBlockCatalog::instance().find(type);
    json block = json::object();
    block[nflow::kId] = finalLeaf;
    block[nflow::kType] = type;
    block[nflow::kX] = 100;
    block[nflow::kY] = 100;
    block[nflow::kRotation] = 0;
    block[nflow::kParams] = entry ? entry->defaultParams : json::object();
    (*container)[nflow::kBlocks].push_back(std::move(block));
    // Report the leaf under its parent path so the caller can address it.
    auto slash = blockName.rfind('/');
    outName
        = (slash == std::string::npos) ? finalLeaf : (blockName.substr(0, slash + 1) + finalLeaf);
    markMutated(ChangeOrigin::Script);
    return true;
}
//=============================================================================
bool
NFlowModelDocument::deleteBlock(const std::string& blockName, std::string& err)
{
    std::lock_guard<std::recursive_mutex> lk(m_mutex);
    std::string leaf;
    json* container = containerForPath(blockName, leaf, err);
    if (!container) {
        return false;
    }
    if (findBlockInDiagram(*container, leaf) == nullptr) {
        err = "No block named '" + leaf + "'.";
        return false;
    }
    auto& blocks = (*container)[nflow::kBlocks];
    blocks.erase(std::remove_if(blocks.begin(), blocks.end(),
                     [&](const json& b) { return b.value(nflow::kId, std::string()) == leaf; }),
        blocks.end());
    auto& conns = (*container)[nflow::kConnections];
    conns.erase(std::remove_if(conns.begin(), conns.end(),
                    [&](const json& c) {
                        return c.value(nflow::kFrom, std::string()) == leaf
                            || c.value(nflow::kTo, std::string()) == leaf;
                    }),
        conns.end());
    markMutated(ChangeOrigin::Script);
    return true;
}
//=============================================================================
bool
NFlowModelDocument::renameBlock(
    const std::string& oldName, const std::string& newName, std::string& err)
{
    std::lock_guard<std::recursive_mutex> lk(m_mutex);
    if (newName.find('/') != std::string::npos) {
        err = "Block name must not contain '/'.";
        return false;
    }
    std::string leaf;
    json* container = containerForPath(oldName, leaf, err);
    if (!container) {
        return false;
    }
    json* b = findBlockInDiagram(*container, leaf);
    if (!b) {
        err = "No block named '" + leaf + "'.";
        return false;
    }
    if (leaf == newName) {
        return true;
    }
    if (findBlockInDiagram(*container, newName) != nullptr) {
        err = "A block named '" + newName + "' already exists.";
        return false;
    }
    (*b)[nflow::kId] = newName;
    for (auto& c : (*container)[nflow::kConnections]) {
        if (c.value(nflow::kFrom, std::string()) == leaf) {
            c[nflow::kFrom] = newName;
        }
        if (c.value(nflow::kTo, std::string()) == leaf) {
            c[nflow::kTo] = newName;
        }
    }
    markMutated(ChangeOrigin::Script);
    return true;
}
//=============================================================================
bool
NFlowModelDocument::setBlockParam(const std::string& blockName, const std::string& key,
    const std::string& value, std::string& err)
{
    std::lock_guard<std::recursive_mutex> lk(m_mutex);
    json* b = findBlock(blockName);
    if (!b) {
        err = "No block named '" + blockName + "'.";
        return false;
    }
    if (key == "Position") {
        err = "Position is set through set_param with a numeric vector, not a string.";
        return false;
    }
    if (key == "Name") {
        return renameBlock(blockName, value, err);
    }
    if (key == "Orientation") {
        (*b)[nflow::kRotation] = value;
        markMutated(ChangeOrigin::Script);
        return true;
    }
    if (key == "BlockType" || key == "Parent") {
        err = "Parameter '" + key + "' is read-only.";
        return false;
    }
    if (!b->contains(nflow::kParams) || !(*b)[nflow::kParams].is_object()) {
        (*b)[nflow::kParams] = json::object();
    }
    (*b)[nflow::kParams][key] = value;
    markMutated(ChangeOrigin::Script);
    return true;
}
//=============================================================================
bool
NFlowModelDocument::getBlockParam(const std::string& blockName, const std::string& key,
    std::string& outValue, std::string& err) const
{
    std::lock_guard<std::recursive_mutex> lk(m_mutex);
    const json* b = findBlock(blockName);
    if (!b) {
        err = "No block named '" + blockName + "'.";
        return false;
    }
    if (key == "Name") {
        outValue = blockName;
        return true;
    }
    if (key == "BlockType") {
        outValue = b->value(nflow::kType, std::string());
        return true;
    }
    if (key == "Orientation") {
        outValue = jsonScalarToString(b->value(nflow::kRotation, json(0)));
        return true;
    }
    if (b->contains(nflow::kParams) && (*b)[nflow::kParams].contains(key)) {
        outValue = jsonScalarToString((*b)[nflow::kParams][key]);
        return true;
    }
    err = "Block '" + blockName + "' has no parameter '" + key + "'.";
    return false;
}
//=============================================================================
bool
NFlowModelDocument::setBlockParamJson(
    const std::string& blockName, const std::string& key, const json& value, std::string& err)
{
    std::lock_guard<std::recursive_mutex> lk(m_mutex);
    json* b = findBlock(blockName);
    if (!b) {
        err = "No block named '" + blockName + "'.";
        return false;
    }
    if (key == "BlockType" || key == "Parent") {
        err = "Parameter '" + key + "' is read-only.";
        return false;
    }
    if (!b->contains(nflow::kParams) || !(*b)[nflow::kParams].is_object()) {
        (*b)[nflow::kParams] = json::object();
    }
    (*b)[nflow::kParams][key] = value;
    markMutated(ChangeOrigin::Script);
    return true;
}
//=============================================================================
bool
NFlowModelDocument::getBlockParamJson(
    const std::string& blockName, const std::string& key, json& outValue, std::string& err) const
{
    std::lock_guard<std::recursive_mutex> lk(m_mutex);
    const json* b = findBlock(blockName);
    if (!b) {
        err = "No block named '" + blockName + "'.";
        return false;
    }
    if (key == "Name") {
        outValue = blockName;
        return true;
    }
    if (key == "BlockType") {
        outValue = b->value(nflow::kType, std::string());
        return true;
    }
    if (key == "Orientation") {
        outValue = b->value(nflow::kRotation, json(0));
        return true;
    }
    if (b->contains(nflow::kParams) && (*b)[nflow::kParams].contains(key)) {
        outValue = (*b)[nflow::kParams][key];
        return true;
    }
    err = "Block '" + blockName + "' has no parameter '" + key + "'.";
    return false;
}
//=============================================================================
bool
NFlowModelDocument::setBlockPosition(
    const std::string& blockName, double x, double y, double w, double h, std::string& err)
{
    std::lock_guard<std::recursive_mutex> lk(m_mutex);
    json* b = findBlock(blockName);
    if (!b) {
        err = "No block named '" + blockName + "'.";
        return false;
    }
    (*b)[nflow::kX] = x;
    (*b)[nflow::kY] = y;
    if (!b->contains(nflow::kParams) || !(*b)[nflow::kParams].is_object()) {
        (*b)[nflow::kParams] = json::object();
    }
    (*b)[nflow::kParams]["width"] = w;
    (*b)[nflow::kParams]["height"] = h;
    markMutated(ChangeOrigin::Script);
    return true;
}
//=============================================================================
bool
NFlowModelDocument::getBlockPosition(const std::string& blockName, double& x, double& y, double& x2,
    double& y2, std::string& err) const
{
    std::lock_guard<std::recursive_mutex> lk(m_mutex);
    const json* b = findBlock(blockName);
    if (!b) {
        err = "No block named '" + blockName + "'.";
        return false;
    }
    x = b->value(nflow::kX, 0.0);
    y = b->value(nflow::kY, 0.0);
    double w = 80.0, h = 80.0;
    if (b->contains(nflow::kParams) && (*b)[nflow::kParams].is_object()) {
        w = (*b)[nflow::kParams].value("width", w);
        h = (*b)[nflow::kParams].value("height", h);
    }
    x2 = x + w;
    y2 = y + h;
    return true;
}
//=============================================================================
static const char*
modelKeyToJson(const std::string& key)
{
    if (key == "Name") {
        return nflow::kName;
    }
    if (key == "StopTime") {
        return nflow::kRuntime;
    }
    if (key == "SampleTime") {
        return nflow::kSampleTime;
    }
    if (key == "AutoRoute") {
        return nflow::kAutoRoute;
    }
    if (key == "Locked") {
        return nflow::kLocked;
    }
    if (key == "Solver") {
        return "solver";
    }
    if (key == "RelTol") {
        return "relTol";
    }
    if (key == "AbsTol") {
        return "absTol";
    }
    if (key == "MaxStep") {
        return "maxStep";
    }
    if (key == "ScreenColor") {
        return "canvasBackground";
    }
    if (key == "Variables") {
        return nflow::kVariables;
    }
    return nullptr;
}
//=============================================================================
bool
NFlowModelDocument::setModelParam(
    const std::string& key, const std::string& value, std::string& err)
{
    std::lock_guard<std::recursive_mutex> lk(m_mutex);
    if (key == "Dirty") {
        err = "Model parameter 'Dirty' is read-only.";
        return false;
    }
    const char* jsonKey = modelKeyToJson(key);
    if (!jsonKey) {
        err = "Unknown model parameter '" + key + "'.";
        return false;
    }
    if (key == "Name") {
        setName(value);
        return true;
    }
    if (key == "AutoRoute" || key == "Locked") {
        m_diagram[jsonKey] = (value == "on" || value == "true" || value == "1");
    } else if (key == "RelTol" || key == "AbsTol" || key == "MaxStep") {
        // Numeric solver tolerances: store as numbers so the engine reads them
        // (SimEngine expects diagram["relTol"] etc. to be JSON numbers).
        try {
            size_t consumed = 0;
            double d = std::stod(value, &consumed);
            if (consumed != value.size()) {
                err = "Model parameter '" + key + "' must be a number.";
                return false;
            }
            m_diagram[jsonKey] = d;
        } catch (...) {
            err = "Model parameter '" + key + "' must be a number.";
            return false;
        }
    } else {
        m_diagram[jsonKey] = value;
    }
    markMutated(ChangeOrigin::Script);
    return true;
}
//=============================================================================
bool
NFlowModelDocument::getModelParam(
    const std::string& key, std::string& outValue, std::string& err) const
{
    std::lock_guard<std::recursive_mutex> lk(m_mutex);
    if (key == "Dirty") {
        outValue = m_dirty ? "on" : "off";
        return true;
    }
    if (key == "Name") {
        outValue = m_name;
        return true;
    }
    const char* jsonKey = modelKeyToJson(key);
    if (!jsonKey) {
        err = "Unknown model parameter '" + key + "'.";
        return false;
    }
    if (m_diagram.contains(jsonKey)) {
        outValue = jsonScalarToString(m_diagram[jsonKey]);
        return true;
    }
    outValue = "";
    return true;
}
//=============================================================================
bool
NFlowModelDocument::addLine(const std::string& fromBlock, int fromPort, const std::string& toBlock,
    int toPort, bool autoRoute, std::string& err)
{
    std::lock_guard<std::recursive_mutex> lk(m_mutex);
    // Both endpoints must live in the same container: resolve each
    // path and require the containers to match; the connection is stored with
    // leaf names, relative to that container.
    std::string fromLeaf, toLeaf;
    json* fromContainer = containerForPath(fromBlock, fromLeaf, err);
    if (!fromContainer) {
        return false;
    }
    std::string toErr;
    json* toContainer = containerForPath(toBlock, toLeaf, toErr);
    if (!toContainer) {
        err = toErr;
        return false;
    }
    if (fromContainer != toContainer) {
        err = "A connection must be between two blocks of the same (sub)system.";
        return false;
    }
    const json* src = findBlockInDiagram(*fromContainer, fromLeaf);
    const json* dst = findBlockInDiagram(*toContainer, toLeaf);
    if (!src) {
        err = "No block named '" + fromLeaf + "'.";
        return false;
    }
    if (!dst) {
        err = "No block named '" + toLeaf + "'.";
        return false;
    }
    if (fromPort < 1 || toPort < 1) {
        err = "Port numbers are 1-based and must be positive.";
        return false;
    }
    const BlockCatalogEntry* srcEntry
        = NFlowBlockCatalog::instance().find(src->value(nflow::kType, std::string()));
    const BlockCatalogEntry* dstEntry
        = NFlowBlockCatalog::instance().find(dst->value(nflow::kType, std::string()));
    if (srcEntry && fromPort > srcEntry->outputCount) {
        err = "Output port " + std::to_string(fromPort) + " does not exist on '" + fromLeaf + "'.";
        return false;
    }
    if (dstEntry && toPort > dstEntry->inputCount) {
        err = "Input port " + std::to_string(toPort) + " does not exist on '" + toLeaf + "'.";
        return false;
    }
    int fromIndex0 = fromPort - 1;
    int toIndex0 = toPort - 1;
    if (inputOccupiedIn(*toContainer, toLeaf, toIndex0)) {
        err = "Input port " + std::to_string(toPort) + " of '" + toLeaf + "' is already connected.";
        return false;
    }
    for (const auto& c : (*fromContainer)[nflow::kConnections]) {
        if (c.value(nflow::kFrom, std::string()) == fromLeaf
            && c.value(nflow::kFromIndex, -1) == fromIndex0
            && c.value(nflow::kTo, std::string()) == toLeaf
            && c.value(nflow::kToIndex, -1) == toIndex0) {
            err = "This connection already exists.";
            return false;
        }
    }
    json conn = json::object();
    conn[nflow::kFrom] = fromLeaf;
    conn[nflow::kTo] = toLeaf;
    conn[nflow::kFromIndex] = fromIndex0;
    conn[nflow::kToIndex] = toIndex0;
    (void)autoRoute;
    (*fromContainer)[nflow::kConnections].push_back(std::move(conn));
    markMutated(ChangeOrigin::Script);
    return true;
}
//=============================================================================
bool
NFlowModelDocument::deleteLine(const std::string& fromBlock, int fromPort,
    const std::string& toBlock, int toPort, std::string& err)
{
    std::lock_guard<std::recursive_mutex> lk(m_mutex);
    std::string fromLeaf, toLeaf;
    json* fromContainer = containerForPath(fromBlock, fromLeaf, err);
    if (!fromContainer) {
        return false;
    }
    std::string toErr;
    json* toContainer = containerForPath(toBlock, toLeaf, toErr);
    if (!toContainer || fromContainer != toContainer) {
        err = "A connection must be between two blocks of the same (sub)system.";
        return false;
    }
    int fromIndex0 = fromPort - 1;
    int toIndex0 = toPort - 1;
    auto& conns = (*fromContainer)[nflow::kConnections];
    auto before = conns.size();
    conns.erase(std::remove_if(conns.begin(), conns.end(),
                    [&](const json& c) {
                        return c.value(nflow::kFrom, std::string()) == fromLeaf
                            && c.value(nflow::kFromIndex, -1) == fromIndex0
                            && c.value(nflow::kTo, std::string()) == toLeaf
                            && c.value(nflow::kToIndex, -1) == toIndex0;
                    }),
        conns.end());
    if (conns.size() == before) {
        err = "No such connection to delete.";
        return false;
    }
    markMutated(ChangeOrigin::Script);
    return true;
}
//=============================================================================
bool
NFlowModelDocument::connectBlocks(const std::string& src, const std::string& dst, std::string& err)
{
    std::lock_guard<std::recursive_mutex> lk(m_mutex);
    std::string srcLeaf, dstLeaf;
    json* srcContainer = containerForPath(src, srcLeaf, err);
    if (!srcContainer) {
        return false;
    }
    std::string dstErr;
    json* dstContainer = containerForPath(dst, dstLeaf, dstErr);
    if (!dstContainer || srcContainer != dstContainer) {
        err = "connect_blocks: source and destination must be in the same (sub)system.";
        return false;
    }
    const json* dstBlock = findBlockInDiagram(*dstContainer, dstLeaf);
    if (findBlockInDiagram(*srcContainer, srcLeaf) == nullptr || dstBlock == nullptr) {
        err = "connect_blocks: unknown source or destination block.";
        return false;
    }
    const BlockCatalogEntry* dstEntry
        = NFlowBlockCatalog::instance().find(dstBlock->value(nflow::kType, std::string()));
    int inputCount = dstEntry ? dstEntry->inputCount : 1;
    for (int i = 0; i < inputCount; ++i) {
        if (!inputOccupiedIn(*dstContainer, dstLeaf, i)) {
            return addLine(src, 1, dst, i + 1, true, err);
        }
    }
    err = "You must connect to a block with at least one unconnected input port.";
    return false;
}
//=============================================================================
bool
NFlowModelDocument::setLineName(const std::string& fromBlock, int fromPort,
    const std::string& toBlock, int toPort, const std::string& name, std::string& err)
{
    std::lock_guard<std::recursive_mutex> lk(m_mutex);
    std::string fromLeaf, toLeaf;
    json* fromContainer = containerForPath(fromBlock, fromLeaf, err);
    if (!fromContainer) {
        return false;
    }
    std::string toErr;
    json* toContainer = containerForPath(toBlock, toLeaf, toErr);
    if (!toContainer || fromContainer != toContainer) {
        err = "A connection must be between two blocks of the same (sub)system.";
        return false;
    }
    const int fi = fromPort - 1;
    const int ti = toPort - 1;
    for (auto& c : (*fromContainer)[nflow::kConnections]) {
        if (c.value(nflow::kFrom, std::string()) == fromLeaf && c.value(nflow::kFromIndex, -1) == fi
            && c.value(nflow::kTo, std::string()) == toLeaf && c.value(nflow::kToIndex, -1) == ti) {
            c["name"] = name;
            markMutated(ChangeOrigin::Script);
            return true;
        }
    }
    err = "No such connection.";
    return false;
}
//=============================================================================
bool
NFlowModelDocument::getLineName(const std::string& fromBlock, int fromPort,
    const std::string& toBlock, int toPort, std::string& outName, std::string& err) const
{
    std::lock_guard<std::recursive_mutex> lk(m_mutex);
    std::string fromLeaf, toLeaf;
    const json* fromContainer = containerForPath(fromBlock, fromLeaf, err);
    if (!fromContainer) {
        return false;
    }
    std::string toErr;
    const json* toContainer = containerForPath(toBlock, toLeaf, toErr);
    if (!toContainer || fromContainer != toContainer) {
        err = "A connection must be between two blocks of the same (sub)system.";
        return false;
    }
    const int fi = fromPort - 1;
    const int ti = toPort - 1;
    for (const auto& c : (*fromContainer)[nflow::kConnections]) {
        if (c.value(nflow::kFrom, std::string()) == fromLeaf && c.value(nflow::kFromIndex, -1) == fi
            && c.value(nflow::kTo, std::string()) == toLeaf && c.value(nflow::kToIndex, -1) == ti) {
            outName = c.value("name", std::string());
            return true;
        }
    }
    err = "No such connection.";
    return false;
}
//=============================================================================
json*
NFlowModelDocument::systemContainer(const std::string& systemPath, std::string& err)
{
    json* container = &m_diagram;
    if (systemPath.empty()) {
        return container;
    }
    for (const auto& seg : splitPath(systemPath)) {
        if (seg.empty()) {
            continue;
        }
        json* sub = findBlockInDiagram(*container, seg);
        if (!sub) {
            err = "No block named '" + seg + "'.";
            return nullptr;
        }
        if (!sub->contains(nflow::kParams) || !(*sub)[nflow::kParams].is_object()
            || !(*sub)[nflow::kParams].contains("subsystem")
            || !(*sub)[nflow::kParams]["subsystem"].is_object()) {
            err = "Block '" + seg + "' is not a subsystem.";
            return nullptr;
        }
        container = &(*sub)[nflow::kParams]["subsystem"];
    }
    return container;
}
//=============================================================================
bool
NFlowModelDocument::layoutSystem(const std::string& systemPath, std::string& err)
{
    std::lock_guard<std::recursive_mutex> lk(m_mutex);
    json* container = systemContainer(systemPath, err);
    if (!container) {
        return false;
    }
    if (!container->contains(nflow::kBlocks) || !(*container)[nflow::kBlocks].is_array()) {
        return true;
    }
    json& blocks = (*container)[nflow::kBlocks];
    if (blocks.empty()) {
        return true;
    }
    const json* connections
        = (container->contains(nflow::kConnections) && (*container)[nflow::kConnections].is_array())
        ? &(*container)[nflow::kConnections]
        : nullptr;
    // Layered ("Sugiyama") layout: sources left, layers by connectivity, wire crossings
    // minimised, feedback edges reversed, pure sinks grouped on the right.
    computeLayeredLayout(blocks, connections);
    markMutated(ChangeOrigin::Script);
    return true;
}
//=============================================================================
bool
NFlowModelDocument::rerouteSystem(
    const std::string& systemPath, int canvasWidth, int canvasHeight, std::string& err)
{
    std::lock_guard<std::recursive_mutex> lk(m_mutex);
    json* container = systemContainer(systemPath, err);
    if (!container) {
        return false;
    }
    if (!container->contains(nflow::kBlocks) || !(*container)[nflow::kBlocks].is_array()) {
        return true;
    }
    if (!container->contains(nflow::kConnections)
        || !(*container)[nflow::kConnections].is_array()) {
        return true;
    }
    NFlowBlockCatalog::instance().ensureLoaded();
    // Build the router snapshot: each block with its geometry and approximate
    // port positions (inputs on the left edge, outputs on the right edge).
    json snapshot;
    snapshot["width"] = canvasWidth;
    snapshot["height"] = canvasHeight;
    json snapBlocks = json::array();
    for (const auto& b : (*container)[nflow::kBlocks]) {
        const std::string type = b.value(nflow::kType, std::string());
        const BlockCatalogEntry* entry = NFlowBlockCatalog::instance().find(type);
        int w = entry ? entry->width : 80;
        int h = entry ? entry->height : 80;
        if (b.contains(nflow::kParams) && b[nflow::kParams].is_object()) {
            w = b[nflow::kParams].value("width", w);
            h = b[nflow::kParams].value("height", h);
        }
        const int nin = entry ? entry->inputCount : 0;
        const int nout = entry ? entry->outputCount : 0;
        json ports = json::array();
        for (int i = 0; i < nin; ++i) {
            ports.push_back(
                { { "x", 0 }, { "y", h * (i + 1) / (nin + 1) }, { "type", "in" }, { "index", i } });
        }
        for (int i = 0; i < nout; ++i) {
            ports.push_back({ { "x", w }, { "y", h * (i + 1) / (nout + 1) }, { "type", "out" },
                { "index", i } });
        }
        json sb;
        sb["id"] = b.value(nflow::kId, std::string());
        sb["x"] = b.value(nflow::kX, 0.0);
        sb["y"] = b.value(nflow::kY, 0.0);
        sb["width"] = w;
        sb["height"] = h;
        sb["rotation"] = b.value(nflow::kRotation, 0);
        sb["ports"] = ports;
        snapBlocks.push_back(std::move(sb));
    }
    snapshot["blocks"] = std::move(snapBlocks);
    snapshot["connections"] = (*container)[nflow::kConnections];

    json result;
    try {
        result = json::parse(routeWires(snapshot.dump()));
    } catch (const std::exception& e) {
        err = std::string("router error: ") + e.what();
        return false;
    }
    if (!result.value("ok", false)) {
        err = result.value("error", std::string("wire routing failed"));
        return false;
    }
    const json& routes = result["routes"];
    json& conns = (*container)[nflow::kConnections];
    for (size_t i = 0; i < conns.size() && i < routes.size(); ++i) {
        if (routes[i].is_array()) {
            conns[i]["waypoints"] = routes[i];
        }
    }
    markMutated(ChangeOrigin::Script);
    return true;
}
//=============================================================================
std::string
NFlowModelDocument::toJsonString() const
{
    std::lock_guard<std::recursive_mutex> lk(m_mutex);
    return m_diagram.dump(2) + "\n";
}
//=============================================================================
// Format gate and migration dispatch point. A missing "format" means a file
// predating the identifier and is read as the current format. Migration of older
// supported generations is dispatched here, keyed on the "format" value, so models
// published since the 1.0.0-beta.1 contract stay readable.
static bool
checkDiagramFormat(const json& diagram, std::string& err)
{
    std::string fmt;
    if (diagram.contains(kFileFormatKey) && diagram[kFileFormatKey].is_string()) {
        fmt = diagram[kFileFormatKey].get<std::string>();
    }
    if (fmt.empty() || fmt == kCurrentFileFormat) {
        return true;
    }
    if (fmt == "nflow-1" || fmt == "nblock-1") {
        err = "Unsupported legacy format '" + fmt + "'; only nflow-2 is supported.";
        return false;
    }
    if (fmt == "nblock-2") {
        err = "'nblock-2' describes a reusable block (.nblock), not a model; use it from the "
              "block library instead.";
        return false;
    }
    err = "Unsupported model format '" + fmt
        + "'. This file was probably written by a newer version of nflow; this version reads "
          "formats up to 'nflow-2'.";
    return false;
}
//=============================================================================
static std::vector<std::string>
splitPath(const std::string& path)
{
    std::vector<std::string> parts;
    size_t start = 0;
    while (start <= path.size()) {
        size_t pos = path.find('/', start);
        if (pos == std::string::npos) {
            parts.push_back(path.substr(start));
            break;
        }
        parts.push_back(path.substr(start, pos - start));
        start = pos + 1;
    }
    return parts;
}
//=============================================================================
static json*
findBlockInDiagram(json& diagram, const std::string& id)
{
    if (!diagram.contains(nflow::kBlocks) || !diagram[nflow::kBlocks].is_array()) {
        return nullptr;
    }
    for (auto& b : diagram[nflow::kBlocks]) {
        if (b.value(nflow::kId, std::string()) == id) {
            return &b;
        }
    }
    return nullptr;
}
//=============================================================================
static const json*
findBlockInDiagram(const json& diagram, const std::string& id)
{
    if (!diagram.contains(nflow::kBlocks) || !diagram[nflow::kBlocks].is_array()) {
        return nullptr;
    }
    for (const auto& b : diagram[nflow::kBlocks]) {
        if (b.value(nflow::kId, std::string()) == id) {
            return &b;
        }
    }
    return nullptr;
}
//=============================================================================
static bool
inputOccupiedIn(const json& container, const std::string& leaf, int toIndex0)
{
    if (!container.contains(nflow::kConnections) || !container[nflow::kConnections].is_array()) {
        return false;
    }
    for (const auto& c : container[nflow::kConnections]) {
        if (c.value(nflow::kTo, std::string()) == leaf
            && c.value(nflow::kToIndex, -1) == toIndex0) {
            return true;
        }
    }
    return false;
}
//=============================================================================
// Layered ("Sugiyama") auto-layout. Pure geometry: only block x/y change.
//=============================================================================
struct SgVertex
{
    int node = -1; // index into the component's node list, or -1 for a dummy
    double width = 0.0;
    double height = 0.0;
    int layer = 0;
    int order = 0;
    double center = 0.0;
    std::vector<int> up; // predecessor vertex indices (previous layer)
    std::vector<int> down; // successor vertex indices (next layer)
};
//=============================================================================
struct SgLayoutConfig
{
    double layerGap = 80.0;
    double nodeGap = 40.0;
    double marginX = 60.0;
    double marginY = 60.0;
    double componentGap = 80.0;
    double grid = 10.0;
    int maxSweeps = 8;
    int coordPasses = 6;
};
//=============================================================================
static double
sgMedian(const std::vector<double>& sorted)
{
    const size_t m = sorted.size() / 2;
    if (sorted.empty()) {
        return 0.0;
    }
    return (sorted.size() % 2 == 1) ? sorted[m] : (sorted[m - 1] + sorted[m]) / 2.0;
}
//=============================================================================
// Weighted median of neighbour orders (Sugiyama ordering heuristic); -1 marks a
// vertex with no neighbour on the reference side (kept in place).
static double
sgOrderMedian(std::vector<double> values)
{
    if (values.empty()) {
        return -1.0;
    }
    std::sort(values.begin(), values.end());
    const size_t n = values.size();
    const size_t m = n / 2;
    if (n % 2 == 1) {
        return values[m];
    }
    if (n == 2) {
        return (values[0] + values[1]) / 2.0;
    }
    const double left = values[m - 1] - values[0];
    const double right = values[n - 1] - values[m];
    if (left + right == 0.0) {
        return (values[m - 1] + values[m]) / 2.0;
    }
    return (values[m - 1] * right + values[m] * left) / (left + right);
}
//=============================================================================
static long
sgCountInversions(std::vector<int>& arr)
{
    if (arr.size() < 2) {
        return 0;
    }
    std::vector<int> tmp(arr.size());
    std::function<long(size_t, size_t)> sortRange = [&](size_t lo, size_t hi) -> long {
        if (hi - lo < 2) {
            return 0;
        }
        const size_t mid = (lo + hi) / 2;
        long count = sortRange(lo, mid) + sortRange(mid, hi);
        size_t i = lo, j = mid, k = lo;
        while (i < mid && j < hi) {
            if (arr[i] <= arr[j]) {
                tmp[k++] = arr[i++];
            } else {
                tmp[k++] = arr[j++];
                count += static_cast<long>(mid - i);
            }
        }
        while (i < mid) {
            tmp[k++] = arr[i++];
        }
        while (j < hi) {
            tmp[k++] = arr[j++];
        }
        for (size_t t = lo; t < hi; ++t) {
            arr[t] = tmp[t];
        }
        return count;
    };
    return sortRange(0, arr.size());
}
//=============================================================================
static long
sgCrossingsBetween(
    const std::vector<std::vector<int>>& layers, const std::vector<SgVertex>& verts, int lu, int ll)
{
    if (lu < 0 || ll >= static_cast<int>(layers.size())) {
        return 0;
    }
    std::unordered_map<int, int> upperOrder;
    for (size_t k = 0; k < layers[lu].size(); ++k) {
        upperOrder[layers[lu][k]] = static_cast<int>(k);
    }
    std::vector<int> seq;
    for (int lower : layers[ll]) {
        std::vector<int> ups;
        for (int ui : verts[lower].up) {
            auto it = upperOrder.find(ui);
            if (it != upperOrder.end()) {
                ups.push_back(it->second);
            }
        }
        std::sort(ups.begin(), ups.end());
        for (int u : ups) {
            seq.push_back(u);
        }
    }
    return sgCountInversions(seq);
}
//=============================================================================
static long
sgTotalCrossings(const std::vector<std::vector<int>>& layers, const std::vector<SgVertex>& verts)
{
    long total = 0;
    for (int l = 0; l + 1 < static_cast<int>(layers.size()); ++l) {
        total += sgCrossingsBetween(layers, verts, l, l + 1);
    }
    return total;
}
//=============================================================================
static void
sgSyncOrder(std::vector<std::vector<int>>& layers, std::vector<SgVertex>& verts)
{
    for (auto& lay : layers) {
        for (size_t k = 0; k < lay.size(); ++k) {
            verts[lay[k]].order = static_cast<int>(k);
        }
    }
}
//=============================================================================
static void
sgMedianSort(
    std::vector<std::vector<int>>& layers, const std::vector<SgVertex>& verts, int l, bool useUp)
{
    std::vector<int>& lay = layers[l];
    std::unordered_map<int, double> medians;
    for (int vi : lay) {
        const std::vector<int>& neigh = useUp ? verts[vi].up : verts[vi].down;
        std::vector<double> orders;
        orders.reserve(neigh.size());
        for (int ni : neigh) {
            orders.push_back(static_cast<double>(verts[ni].order));
        }
        medians[vi] = sgOrderMedian(orders);
    }
    std::vector<std::pair<int, int>> fixed; // (position, vertex)
    std::vector<int> movable;
    for (size_t k = 0; k < lay.size(); ++k) {
        if (medians[lay[k]] < 0.0) {
            fixed.emplace_back(static_cast<int>(k), lay[k]);
        } else {
            movable.push_back(lay[k]);
        }
    }
    std::stable_sort(
        movable.begin(), movable.end(), [&](int a, int b) { return medians[a] < medians[b]; });
    std::vector<int> result(lay.size(), -1);
    for (const auto& f : fixed) {
        result[f.first] = f.second;
    }
    size_t mi = 0;
    for (size_t k = 0; k < result.size(); ++k) {
        if (result[k] == -1) {
            result[k] = movable[mi++];
        }
    }
    lay = result;
}
//=============================================================================
static void
sgTranspose(std::vector<std::vector<int>>& layers, std::vector<SgVertex>& verts)
{
    bool improved = true;
    int guard = 0;
    while (improved && guard < 4) {
        improved = false;
        ++guard;
        for (int l = 0; l < static_cast<int>(layers.size()); ++l) {
            std::vector<int>& lay = layers[l];
            for (size_t i = 0; i + 1 < lay.size(); ++i) {
                const long before = sgCrossingsBetween(layers, verts, l - 1, l)
                    + sgCrossingsBetween(layers, verts, l, l + 1);
                std::swap(lay[i], lay[i + 1]);
                sgSyncOrder(layers, verts);
                const long after = sgCrossingsBetween(layers, verts, l - 1, l)
                    + sgCrossingsBetween(layers, verts, l, l + 1);
                if (after < before) {
                    improved = true;
                } else {
                    std::swap(lay[i], lay[i + 1]);
                    sgSyncOrder(layers, verts);
                }
            }
        }
    }
}
//=============================================================================
static void
sgOrderLayers(std::vector<std::vector<int>>& layers, std::vector<SgVertex>& verts, int maxSweeps)
{
    sgSyncOrder(layers, verts);
    std::vector<std::vector<int>> best = layers;
    long bestCrossings = sgTotalCrossings(layers, verts);
    for (int sweep = 0; sweep < maxSweeps; ++sweep) {
        const bool downward = (sweep % 2 == 0);
        if (downward) {
            for (int l = 1; l < static_cast<int>(layers.size()); ++l) {
                sgMedianSort(layers, verts, l, true);
            }
        } else {
            for (int l = static_cast<int>(layers.size()) - 2; l >= 0; --l) {
                sgMedianSort(layers, verts, l, false);
            }
        }
        sgSyncOrder(layers, verts);
        sgTranspose(layers, verts);
        sgSyncOrder(layers, verts);
        const long crossings = sgTotalCrossings(layers, verts);
        if (crossings < bestCrossings) {
            bestCrossings = crossings;
            best = layers;
            if (crossings == 0) {
                break;
            }
        }
    }
    layers = best;
    sgSyncOrder(layers, verts);
}
//=============================================================================
// Priority method: move each vertex toward the median of its reference-side
// neighbours, honouring dummy priority and the minimum vertical separation.
static void
sgAlignLayer(const std::vector<int>& lay, std::vector<SgVertex>& verts, bool useUp, double gap)
{
    const int n = static_cast<int>(lay.size());
    if (n == 0) {
        return;
    }
    std::vector<double> target(n, std::nan(""));
    for (int k = 0; k < n; ++k) {
        const std::vector<int>& neigh = useUp ? verts[lay[k]].up : verts[lay[k]].down;
        if (!neigh.empty()) {
            std::vector<double> centres;
            centres.reserve(neigh.size());
            for (int ni : neigh) {
                centres.push_back(verts[ni].center);
            }
            std::sort(centres.begin(), centres.end());
            target[k] = sgMedian(centres);
        }
    }
    std::vector<double> minSep(n, 0.0);
    for (int k = 1; k < n; ++k) {
        minSep[k] = verts[lay[k - 1]].height / 2.0 + verts[lay[k]].height / 2.0 + gap;
    }
    std::vector<double> prio(n, 0.0);
    for (int k = 0; k < n; ++k) {
        const SgVertex& v = verts[lay[k]];
        prio[k] = (v.node < 0) ? 1e12 : static_cast<double>(v.up.size() + v.down.size());
    }
    std::vector<int> byPrio(n);
    for (int k = 0; k < n; ++k) {
        byPrio[k] = k;
    }
    std::stable_sort(byPrio.begin(), byPrio.end(), [&](int a, int b) { return prio[a] > prio[b]; });
    std::vector<double> center(n);
    for (int k = 0; k < n; ++k) {
        center[k] = verts[lay[k]].center;
    }
    for (int k : byPrio) {
        if (std::isnan(target[k])) {
            continue;
        }
        if (target[k] > center[k]) {
            double need = 0.0, cap = 1e300;
            for (int j = k + 1; j < n; ++j) {
                need += minSep[j];
                if (prio[j] >= prio[k]) {
                    cap = center[j] - need;
                    break;
                }
            }
            const double nc = std::min(target[k], cap);
            if (nc > center[k]) {
                center[k] = nc;
                for (int j = k + 1; j < n; ++j) {
                    const double minC = center[j - 1] + minSep[j];
                    if (center[j] < minC) {
                        center[j] = minC;
                    } else {
                        break;
                    }
                }
            }
        } else if (target[k] < center[k]) {
            double need = 0.0, cap = -1e300;
            for (int j = k - 1; j >= 0; --j) {
                need += minSep[j + 1];
                if (prio[j] >= prio[k]) {
                    cap = center[j] + need;
                    break;
                }
            }
            const double nc = std::max(target[k], cap);
            if (nc < center[k]) {
                center[k] = nc;
                for (int j = k - 1; j >= 0; --j) {
                    const double maxC = center[j + 1] - minSep[j + 1];
                    if (center[j] > maxC) {
                        center[j] = maxC;
                    } else {
                        break;
                    }
                }
            }
        }
    }
    for (int k = 0; k < n; ++k) {
        verts[lay[k]].center = center[k];
    }
}
//=============================================================================
static void
sgAssignCoords(
    std::vector<std::vector<int>>& layers, std::vector<SgVertex>& verts, const SgLayoutConfig& cfg)
{
    for (auto& lay : layers) {
        double y = 0.0;
        for (int vi : lay) {
            verts[vi].center = y + verts[vi].height / 2.0;
            y += verts[vi].height + cfg.nodeGap;
        }
    }
    for (int pass = 0; pass < cfg.coordPasses; ++pass) {
        const bool downward = (pass % 2 == 0);
        if (downward) {
            for (int l = 0; l < static_cast<int>(layers.size()); ++l) {
                sgAlignLayer(layers[l], verts, true, cfg.nodeGap);
            }
        } else {
            for (int l = static_cast<int>(layers.size()) - 1; l >= 0; --l) {
                sgAlignLayer(layers[l], verts, false, cfg.nodeGap);
            }
        }
    }
}
//=============================================================================
// Lay out one connected component. `nodeW/nodeH` are per-node sizes; `edges`
// are directed (from,to) index pairs into the component's node list. Returns a
// position (top-left) per node index, normalised so the drawing starts at 0,0,
// with the component's overall width/height in outW/outH.
static std::vector<std::pair<double, double>>
sgLayoutComponent(int localN, const std::vector<double>& nodeW, const std::vector<double>& nodeH,
    const std::vector<std::pair<int, int>>& edges, const SgLayoutConfig& cfg, double& outW,
    double& outH)
{
    std::vector<std::vector<int>> out(localN), inc(localN);
    for (const auto& e : edges) {
        out[e.first].push_back(e.second);
        inc[e.second].push_back(e.first);
    }
    // Stage 1: break cycles with a DFS seeded from low in-degree nodes (true
    // sources), so a control loop is broken at its feedback edge.
    std::vector<int> startOrder(localN);
    for (int i = 0; i < localN; ++i) {
        startOrder[i] = i;
    }
    std::stable_sort(startOrder.begin(), startOrder.end(),
        [&](int a, int b) { return inc[a].size() < inc[b].size(); });
    std::vector<std::unordered_set<int>> reversed(localN);
    std::vector<int> state(localN, 0); // 0=unvisited, 1=on stack, 2=done
    for (int start : startOrder) {
        if (state[start] != 0) {
            continue;
        }
        std::vector<std::pair<int, size_t>> stack;
        stack.emplace_back(start, 0);
        state[start] = 1;
        while (!stack.empty()) {
            auto& top = stack.back();
            if (top.second < out[top.first].size()) {
                const int v = out[top.first][top.second++];
                if (state[v] == 1) {
                    reversed[top.first].insert(v);
                } else if (state[v] == 0) {
                    state[v] = 1;
                    stack.emplace_back(v, 0);
                }
            } else {
                state[top.first] = 2;
                stack.pop_back();
            }
        }
    }
    std::vector<std::vector<int>> forward(localN), fpred(localN);
    for (int u = 0; u < localN; ++u) {
        for (int v : out[u]) {
            if (reversed[u].count(v)) {
                forward[v].push_back(u);
            } else {
                forward[u].push_back(v);
            }
        }
    }
    for (int u = 0; u < localN; ++u) {
        for (int v : forward[u]) {
            fpred[v].push_back(u);
        }
    }
    // Stage 2: longest-path layering (topological relaxation).
    std::vector<int> layer(localN, 0);
    std::vector<int> indeg(localN);
    for (int i = 0; i < localN; ++i) {
        indeg[i] = static_cast<int>(fpred[i].size());
    }
    std::vector<int> queue;
    for (int i = 0; i < localN; ++i) {
        if (indeg[i] == 0) {
            queue.push_back(i);
        }
    }
    std::sort(queue.begin(), queue.end());
    std::vector<int> topo;
    for (size_t qi = 0; qi < queue.size(); ++qi) {
        const int u = queue[qi];
        topo.push_back(u);
        for (int v : forward[u]) {
            if (--indeg[v] == 0) {
                queue.push_back(v);
            }
        }
    }
    for (int u : topo) {
        for (int v : forward[u]) {
            if (layer[u] + 1 > layer[v]) {
                layer[v] = layer[u] + 1;
            }
        }
    }
    // Pure sinks grouped in the last column.
    int maxLayer = 0;
    for (int i = 0; i < localN; ++i) {
        maxLayer = std::max(maxLayer, layer[i]);
    }
    for (int i = 0; i < localN; ++i) {
        if (forward[i].empty() && layer[i] < maxLayer) {
            layer[i] = maxLayer;
        }
    }
    int layerCount = 0;
    for (int i = 0; i < localN; ++i) {
        layerCount = std::max(layerCount, layer[i] + 1);
    }
    // Build vertices + dummy chains for edges spanning several layers.
    std::vector<SgVertex> verts(localN);
    for (int i = 0; i < localN; ++i) {
        verts[i].node = i;
        verts[i].width = nodeW[i];
        verts[i].height = nodeH[i];
        verts[i].layer = layer[i];
    }
    auto link = [&](int a, int b) {
        verts[a].down.push_back(b);
        verts[b].up.push_back(a);
    };
    for (int u = 0; u < localN; ++u) {
        for (int v : forward[u]) {
            const int lu = layer[u], lv = layer[v];
            if (lv - lu <= 1) {
                link(u, v);
                continue;
            }
            int prev = u;
            for (int l = lu + 1; l < lv; ++l) {
                SgVertex d;
                d.node = -1;
                d.layer = l;
                const int di = static_cast<int>(verts.size());
                verts.push_back(d);
                link(prev, di);
                prev = di;
            }
            link(prev, v);
        }
    }
    std::vector<std::vector<int>> layers(layerCount);
    for (int i = 0; i < static_cast<int>(verts.size()); ++i) {
        layers[verts[i].layer].push_back(i);
    }
    sgSyncOrder(layers, verts);
    sgOrderLayers(layers, verts, cfg.maxSweeps);
    sgAssignCoords(layers, verts, cfg);
    std::vector<double> colX(layers.size(), 0.0);
    double x = 0.0;
    for (size_t l = 0; l < layers.size(); ++l) {
        colX[l] = x;
        double maxW = 0.0;
        for (int vi : layers[l]) {
            maxW = std::max(maxW, verts[vi].width);
        }
        x += maxW + cfg.layerGap;
    }
    std::vector<std::pair<double, double>> pos(localN, { 0.0, 0.0 });
    double minX = 1e300, minY = 1e300, maxX = -1e300, maxY = -1e300;
    for (const SgVertex& v : verts) {
        if (v.node < 0) {
            continue;
        }
        const double px = colX[v.layer];
        const double py = v.center - v.height / 2.0;
        pos[v.node] = { px, py };
        minX = std::min(minX, px);
        minY = std::min(minY, py);
        maxX = std::max(maxX, px + v.width);
        maxY = std::max(maxY, py + v.height);
    }
    for (auto& p : pos) {
        p.first -= minX;
        p.second -= minY;
    }
    outW = maxX - minX;
    outH = maxY - minY;
    return pos;
}
//=============================================================================
static void
computeLayeredLayout(json& blocks, const json* connections)
{
    const int n = static_cast<int>(blocks.size());
    if (n == 0) {
        return;
    }
    NFlowBlockCatalog::instance().ensureLoaded();
    SgLayoutConfig cfg;

    std::unordered_map<std::string, int> indexOf;
    std::vector<double> nodeW(n), nodeH(n);
    for (int i = 0; i < n; ++i) {
        const json& b = blocks[i];
        indexOf[b.value(nflow::kId, std::string())] = i;
        const BlockCatalogEntry* e
            = NFlowBlockCatalog::instance().find(b.value(nflow::kType, std::string()));
        int w = e ? e->width : 80;
        int h = e ? e->height : 80;
        if (b.contains(nflow::kParams) && b[nflow::kParams].is_object()) {
            w = b[nflow::kParams].value("width", w);
            h = b[nflow::kParams].value("height", h);
        }
        nodeW[i] = static_cast<double>(w);
        nodeH[i] = static_cast<double>(h);
    }
    // Clean, de-duplicated directed edges (both endpoints exist, no self-loops).
    std::vector<std::pair<int, int>> edges;
    std::unordered_set<long long> seenEdge;
    std::vector<std::vector<int>> adj(n); // undirected, for components
    if (connections) {
        for (const auto& c : *connections) {
            auto f = indexOf.find(c.value(nflow::kFrom, std::string()));
            auto t = indexOf.find(c.value(nflow::kTo, std::string()));
            if (f == indexOf.end() || t == indexOf.end() || f->second == t->second) {
                continue;
            }
            const long long key = static_cast<long long>(f->second) * n + t->second;
            if (seenEdge.count(key)) {
                continue;
            }
            seenEdge.insert(key);
            edges.emplace_back(f->second, t->second);
            adj[f->second].push_back(t->second);
            adj[t->second].push_back(f->second);
        }
    }
    std::vector<int> comp(n, -1);
    std::vector<std::vector<int>> components;
    for (int i = 0; i < n; ++i) {
        if (comp[i] != -1) {
            continue;
        }
        std::vector<int> stack { i };
        comp[i] = static_cast<int>(components.size());
        std::vector<int> members;
        while (!stack.empty()) {
            const int v = stack.back();
            stack.pop_back();
            members.push_back(v);
            for (int w : adj[v]) {
                if (comp[w] == -1) {
                    comp[w] = comp[i];
                    stack.push_back(w);
                }
            }
        }
        std::sort(members.begin(), members.end());
        components.push_back(members);
    }
    struct Laid
    {
        std::vector<int> nodes; // global indices
        std::vector<std::pair<double, double>> pos; // local top-left, per node
        double width = 0.0;
        double height = 0.0;
    };
    std::vector<Laid> laid;
    std::vector<int> singletons;
    for (const auto& members : components) {
        if (members.size() == 1) {
            singletons.push_back(members[0]);
            continue;
        }
        std::unordered_map<int, int> g2l;
        for (size_t k = 0; k < members.size(); ++k) {
            g2l[members[k]] = static_cast<int>(k);
        }
        std::vector<double> cw(members.size()), ch(members.size());
        for (size_t k = 0; k < members.size(); ++k) {
            cw[k] = nodeW[members[k]];
            ch[k] = nodeH[members[k]];
        }
        std::vector<std::pair<int, int>> cedges;
        for (const auto& e : edges) {
            auto a = g2l.find(e.first), b = g2l.find(e.second);
            if (a != g2l.end() && b != g2l.end()) {
                cedges.emplace_back(a->second, b->second);
            }
        }
        Laid item;
        item.nodes = members;
        item.pos = sgLayoutComponent(
            static_cast<int>(members.size()), cw, ch, cedges, cfg, item.width, item.height);
        laid.push_back(std::move(item));
    }
    // Larger components first; ties keep their stable order.
    std::stable_sort(laid.begin(), laid.end(),
        [](const Laid& a, const Laid& b) { return a.nodes.size() > b.nodes.size(); });

    std::vector<std::pair<double, double>> finalPos(n, { 0.0, 0.0 });
    double cursorY = cfg.marginY;
    for (const Laid& block : laid) {
        for (size_t k = 0; k < block.nodes.size(); ++k) {
            finalPos[block.nodes[k]] = { block.pos[k].first, block.pos[k].second + cursorY };
        }
        cursorY += block.height + cfg.componentGap;
    }
    // Isolated blocks: pack into a grid below the connected drawing.
    if (!singletons.empty()) {
        std::sort(singletons.begin(), singletons.end(), [&](int a, int b) {
            return blocks[a].value(nflow::kId, std::string())
                < blocks[b].value(nflow::kId, std::string());
        });
        int cols = 1;
        while (cols * cols < static_cast<int>(singletons.size())) {
            ++cols;
        }
        double cellW = 0.0;
        for (int s : singletons) {
            cellW = std::max(cellW, nodeW[s]);
        }
        cellW += cfg.layerGap;
        int col = 0;
        double rowHeight = 0.0;
        double y = cursorY;
        for (int s : singletons) {
            finalPos[s] = { cfg.marginX + col * cellW, y };
            rowHeight = std::max(rowHeight, nodeH[s]);
            if (++col >= cols) {
                col = 0;
                y += rowHeight + cfg.nodeGap;
                rowHeight = 0.0;
            }
        }
    }
    double minX = 1e300, minY = 1e300;
    for (const auto& p : finalPos) {
        minX = std::min(minX, p.first);
        minY = std::min(minY, p.second);
    }
    const double g = cfg.grid > 0.0 ? cfg.grid : 10.0;
    for (int i = 0; i < n; ++i) {
        const double x = std::round((finalPos[i].first - minX + cfg.marginX) / g) * g;
        // Snap the block's vertical CENTRE to the grid (not its top edge), so a
        // block of odd height keeps its centred port grid-aligned and a wire
        // between two centred-port blocks renders straight instead of jogged.
        const double centreY = finalPos[i].second - minY + cfg.marginY + nodeH[i] / 2.0;
        const double y = std::round(centreY / g) * g - nodeH[i] / 2.0;
        blocks[i][nflow::kX] = x;
        blocks[i][nflow::kY] = y;
    }
}
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
