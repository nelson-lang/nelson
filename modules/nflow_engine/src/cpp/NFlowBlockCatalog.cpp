//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NFlowBlockCatalog.hpp"
#include "FieldNames.hpp"
#include <filesystem>
#include <fstream>
//=============================================================================
namespace fs = std::filesystem;
using json = nlohmann::json;
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
NFlowBlockCatalog&
NFlowBlockCatalog::instance()
{
    static NFlowBlockCatalog s_instance;
    return s_instance;
}
//=============================================================================
void
NFlowBlockCatalog::registerManifest(const json& manifest)
{
    if (!manifest.contains(nflow::kBlocks) || !manifest[nflow::kBlocks].is_array()) {
        return;
    }
    std::string libId = manifest.value(nflow::kId, std::string());
    for (const auto& b : manifest[nflow::kBlocks]) {
        if (!b.contains(nflow::kType) || !b[nflow::kType].is_string()) {
            continue;
        }
        BlockCatalogEntry entry;
        entry.type = b[nflow::kType].get<std::string>();
        entry.libraryId = libId;
        if (b.contains("defaultParams") && b["defaultParams"].is_object()) {
            entry.defaultParams = b["defaultParams"];
        } else {
            entry.defaultParams = json::object();
        }
        if (b.contains(nflow::kInputs) && b[nflow::kInputs].is_array()) {
            entry.inputCount = static_cast<int>(b[nflow::kInputs].size());
        }
        if (b.contains(nflow::kOutputs) && b[nflow::kOutputs].is_array()) {
            entry.outputCount = static_cast<int>(b[nflow::kOutputs].size());
        }
        entry.width = b.value(nflow::kWidth, 80);
        entry.height = b.value(nflow::kHeight, 80);
        std::lock_guard<std::mutex> lock(m_mutex);
        m_entries[entry.type] = std::move(entry);
    }
}
//=============================================================================
void
NFlowBlockCatalog::ensureLoaded()
{
    // The builtin layer does the one-time load from the module libraries path.
    // This hook stays a no-op so the core does not depend on the module-path code.
}
//=============================================================================
void
NFlowBlockCatalog::loadManifestDirectory(const std::wstring& directory)
{
    fs::path dir(directory);
    std::error_code ec;
    if (!fs::exists(dir, ec) || !fs::is_directory(dir, ec)) {
        return;
    }
    for (const auto& sub : fs::directory_iterator(dir, ec)) {
        if (!sub.is_directory()) {
            continue;
        }
        fs::path manifestPath = sub.path() / "library.json";
        if (!fs::exists(manifestPath, ec)) {
            continue;
        }
        std::ifstream ifs(manifestPath);
        if (!ifs.is_open()) {
            continue;
        }
        try {
            json manifest = json::parse(ifs);
            registerManifest(manifest);
        } catch (const json::exception&) {
        }
    }
}
//=============================================================================
bool
NFlowBlockCatalog::resolveType(
    const std::string& source, std::string& outType, bool& ambiguous) const
{
    ambiguous = false;
    std::lock_guard<std::mutex> lock(m_mutex);

    // Dotted editor alias: nflow.<libraryId>.<type>  e.g. nflow.builtin.math.gain
    if (source.rfind("nflow.", 0) == 0) {
        auto pos = source.rfind('.');
        if (pos != std::string::npos && pos + 1 < source.size()) {
            std::string t = source.substr(pos + 1);
            if (m_entries.count(t)) {
                outType = t;
                return true;
            }
        }
    }
    if (source.rfind("nflow/", 0) == 0) {
        auto pos = source.rfind('/');
        if (pos != std::string::npos && pos + 1 < source.size()) {
            std::string t = source.substr(pos + 1);
            auto it = m_entries.find(t);
            if (it != m_entries.end()) {
                outType = t;
                return true;
            }
        }
        return false;
    }
    auto it = m_entries.find(source);
    if (it != m_entries.end()) {
        outType = source;
        return true;
    }
    return false;
}
//=============================================================================
bool
NFlowBlockCatalog::hasType(const std::string& type) const
{
    std::lock_guard<std::mutex> lock(m_mutex);
    return m_entries.count(type) != 0;
}
//=============================================================================
const BlockCatalogEntry*
NFlowBlockCatalog::find(const std::string& type) const
{
    std::lock_guard<std::mutex> lock(m_mutex);
    auto it = m_entries.find(type);
    return (it == m_entries.end()) ? nullptr : &it->second;
}
//=============================================================================
std::vector<std::string>
NFlowBlockCatalog::types() const
{
    std::lock_guard<std::mutex> lock(m_mutex);
    std::vector<std::string> out;
    out.reserve(m_entries.size());
    for (const auto& kv : m_entries) {
        out.push_back(kv.first);
    }
    return out;
}
//=============================================================================
void
NFlowBlockCatalog::clear()
{
    std::lock_guard<std::mutex> lock(m_mutex);
    m_entries.clear();
    m_builtinLoaded = false;
}
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
