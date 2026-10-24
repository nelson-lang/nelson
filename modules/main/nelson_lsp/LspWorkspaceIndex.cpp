//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "LspWorkspaceIndex.hpp"
#include "CodeAnalyzer.hpp"
#include "LspServerContext.hpp"
#include "LspUtilities.hpp"
#include <algorithm>
#include <filesystem>
#include <fstream>
#include <set>
#include <sstream>
//=============================================================================
namespace Nelson::Lsp {
//=============================================================================
bool
readTextFile(const std::filesystem::path& path, std::string& text)
{
    std::ifstream stream(path, std::ios::binary);
    if (!stream.good()) {
        return false;
    }
    std::ostringstream buffer;
    buffer << stream.rdbuf();
    text = buffer.str();
    return true;
}
//=============================================================================
uint64_t
fnv1a(const std::string& text)
{
    uint64_t hash = 14695981039346656037ULL;
    for (unsigned char c : text) {
        hash ^= static_cast<uint64_t>(c);
        hash *= 1099511628211ULL;
    }
    return hash;
}
//=============================================================================
int64_t
fileMtime(const std::filesystem::path& path)
{
    try {
        return std::filesystem::last_write_time(path).time_since_epoch().count();
    } catch (...) {
    }
    return 0;
}
//=============================================================================
uint64_t
workspaceConfigHash(const std::filesystem::path& root)
{
    std::string configText;
    std::filesystem::path config = workspaceConfigForRoot(root);
    if (!config.empty()) {
        readTextFile(config, configText);
    }
    return fnv1a(configText + "\n" + lspLintSettings.dump());
}
//=============================================================================
bool
sameIndexedFile(const LspIndexedFile& indexed, const std::filesystem::path& path, uintmax_t size,
    int64_t mtime, uint64_t hash)
{
    return normalizedPathKey(indexed.path) == normalizedPathKey(path) && indexed.size == size
        && indexed.mtime == mtime && indexed.hash == hash;
}
//=============================================================================
void
appendOpenDocumentsToIndex(LspWorkspaceIndex& index)
{
    for (const auto& item : documents) {
        std::filesystem::path path = pathFromUri(item.first);
        if (path.empty() || !pathIsInsideOrEqual(path, index.rootPath)) {
            continue;
        }
        LspIndexedFile indexed;
        indexed.uri = item.first;
        indexed.path = path;
        indexed.size = item.second.text.size();
        indexed.hash = fnv1a(item.second.text);
        indexed.model = item.second.model.text.empty()
            ? buildLspDocumentModel(item.first, item.second.text)
            : item.second.model;
        index.files[normalizedPathKey(path)] = std::move(indexed);
    }
}
//=============================================================================
LspWorkspaceIndex&
workspaceIndexForRoot(const std::string& rootUri)
{
    LspWorkspaceIndex& index = workspaceIndexes[rootUri];
    std::filesystem::path root = pathFromUri(rootUri);
    const uint64_t configHash = workspaceConfigHash(root);
    if (index.ready && index.configHash == configHash) {
        appendOpenDocumentsToIndex(index);
        return index;
    }
    LspWorkspaceIndex previous = std::move(index);
    index = LspWorkspaceIndex {};
    index.rootUri = rootUri;
    index.rootPath = root;
    index.configHash = configHash;
    if (root.empty()) {
        index.ready = true;
        return index;
    }
    Nelson::CodeAnalyzerOptions options = analyzerOptionsForPath(root);
    options.includeSubfolders = true;
    for (const std::wstring& filename :
        Nelson::codeAnalyzerIncludedFiles({ root.wstring() }, options)) {
        std::filesystem::path path(filename);
        if (path.extension().generic_string() != ".m") {
            continue;
        }
        std::string text;
        if (!readTextFile(path, text)) {
            continue;
        }
        const uintmax_t size = std::filesystem::file_size(path);
        const int64_t mtime = fileMtime(path);
        const uint64_t hash = fnv1a(text);
        const std::string key = normalizedPathKey(path);
        auto previousFile = previous.files.find(key);
        if (previousFile != previous.files.end()
            && sameIndexedFile(previousFile->second, path, size, mtime, hash)) {
            index.files[key] = std::move(previousFile->second);
            continue;
        }
        LspIndexedFile indexed;
        indexed.uri = pathToUri(path);
        indexed.path = path;
        indexed.size = size;
        indexed.mtime = mtime;
        indexed.hash = hash;
        indexed.model = buildLspDocumentModel(indexed.uri, text);
        index.files[key] = std::move(indexed);
    }
    appendOpenDocumentsToIndex(index);
    index.ready = true;
    return index;
}
//=============================================================================
void
clearWorkspaceIndexes()
{
    workspaceIndexes.clear();
}
//=============================================================================
void
invalidateWorkspaceIndexForUri(const std::string& uri)
{
    std::filesystem::path path = pathFromUri(uri);
    if (path.empty()) {
        return;
    }
    for (auto& item : workspaceIndexes) {
        if (pathIsInsideOrEqual(path, item.second.rootPath)) {
            item.second.ready = false;
        }
    }
}
//=============================================================================
void
appendWorkspaceCompletionItems(json& items, const std::string& uri, const std::string& prefix)
{
    std::set<std::string> labels;
    for (const auto& item : items) {
        labels.insert(item.value("label", ""));
    }
    std::filesystem::path root = workspaceRootForPath(pathFromUri(uri));
    for (const std::string& rootUri : workspaceRootUris) {
        LspWorkspaceIndex& index = workspaceIndexForRoot(rootUri);
        if (!root.empty() && normalizedPathKey(index.rootPath) != normalizedPathKey(root)) {
            continue;
        }
        for (const auto& indexed : index.files) {
            for (const auto& symbol : indexed.second.model.symbols) {
                if (!prefix.empty() && symbol.name.rfind(prefix, 0) != 0) {
                    continue;
                }
                if (!labels.insert(symbol.name).second) {
                    continue;
                }
                json item;
                item["label"] = symbol.name;
                item["kind"] = symbol.kind;
                item["detail"] = indexed.second.uri;
                if (!symbol.parameters.empty()) {
                    std::ostringstream insertText;
                    insertText << symbol.name << "(";
                    for (size_t k = 0; k < symbol.parameters.size(); ++k) {
                        if (k > 0) {
                            insertText << ", ";
                        }
                        insertText << "${" << k + 1 << ":" << symbol.parameters[k] << "}";
                    }
                    insertText << ")";
                    item["insertText"] = insertText.str();
                    item["insertTextFormat"] = 2;
                }
                items.push_back(item);
            }
            std::string fileStem = indexed.second.path.stem().generic_string();
            if (!prefix.empty() && fileStem.rfind(prefix, 0) != 0) {
                continue;
            }
            if (labels.insert(fileStem).second) {
                json item;
                item["label"] = fileStem;
                item["kind"] = 17;
                item["detail"] = indexed.second.path.generic_string();
                items.push_back(item);
            }
        }
    }
}
//=============================================================================
json
workspaceSignatureHelpFor(const std::string& uri, const std::string& name)
{
    std::filesystem::path root = workspaceRootForPath(pathFromUri(uri));
    for (const std::string& rootUri : workspaceRootUris) {
        LspWorkspaceIndex& index = workspaceIndexForRoot(rootUri);
        if (!root.empty() && normalizedPathKey(index.rootPath) != normalizedPathKey(root)) {
            continue;
        }
        for (const auto& indexed : index.files) {
            for (const auto& symbol : indexed.second.model.symbols) {
                if (symbol.name != name || symbol.parameters.empty()) {
                    continue;
                }
                json result;
                result["signatures"] = json::array();
                result["activeSignature"] = 0;
                result["activeParameter"] = 0;
                json signature;
                std::ostringstream label;
                label << symbol.name << "(";
                for (size_t k = 0; k < symbol.parameters.size(); ++k) {
                    if (k > 0) {
                        label << ", ";
                    }
                    label << symbol.parameters[k];
                }
                label << ")";
                signature["label"] = label.str();
                signature["documentation"] = indexed.second.uri;
                signature["parameters"] = json::array();
                for (const auto& parameter : symbol.parameters) {
                    signature["parameters"].push_back({ { "label", parameter } });
                }
                result["signatures"].push_back(signature);
                return result;
            }
        }
    }
    return nullptr;
}
//=============================================================================
json
workspaceDefinitionFor(const std::string& uri, const std::string& name)
{
    if (name.empty()) {
        return nullptr;
    }
    std::filesystem::path root = workspaceRootForPath(pathFromUri(uri));
    for (const std::string& rootUri : workspaceRootUris) {
        LspWorkspaceIndex& index = workspaceIndexForRoot(rootUri);
        if (!root.empty() && normalizedPathKey(index.rootPath) != normalizedPathKey(root)) {
            continue;
        }
        for (const auto& indexed : index.files) {
            for (const auto& symbol : indexed.second.model.symbols) {
                if (symbol.name == name) {
                    return lspLocation(indexed.second.uri, symbol);
                }
            }
        }
    }
    return nullptr;
}
//=============================================================================
std::string
locationKey(const json& location)
{
    const json range = location.value("range", json::object());
    return location.value("uri", "") + ":" + range.dump();
}
//=============================================================================
bool
workspaceHasPublicSymbol(const std::string& uri, const std::string& name)
{
    return !workspaceDefinitionFor(uri, name).is_null();
}
//=============================================================================
json
workspaceReferencesFor(const std::string& uri, const std::string& name, const json& localReferences)
{
    json result = json::array();
    std::set<std::string> emitted;
    for (const auto& location : localReferences) {
        emitted.insert(locationKey(location));
        result.push_back(location);
    }
    if (!workspaceHasPublicSymbol(uri, name)) {
        return result;
    }
    std::filesystem::path root = workspaceRootForPath(pathFromUri(uri));
    for (const std::string& rootUri : workspaceRootUris) {
        LspWorkspaceIndex& index = workspaceIndexForRoot(rootUri);
        if (!root.empty() && normalizedPathKey(index.rootPath) != normalizedPathKey(root)) {
            continue;
        }
        for (const auto& indexed : index.files) {
            for (const auto& location : lspReferences(indexed.second.model, name)) {
                const std::string key = locationKey(location);
                if (emitted.insert(key).second) {
                    result.push_back(location);
                }
            }
        }
    }
    return result;
}
//=============================================================================
} // namespace Nelson::Lsp
//=============================================================================
