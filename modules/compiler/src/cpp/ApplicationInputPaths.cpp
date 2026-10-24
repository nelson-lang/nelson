//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#include "ApplicationInputs.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "characters_encoding.hpp"
#include "nlohmann/json.hpp"
#include <set>
//=============================================================================
namespace Nelson::Application {
namespace fs = std::filesystem;
using Json = nlohmann::json;
//=============================================================================
namespace {
    bool
    hasNestedFunctionPaths(const Json& document)
    {
        for (const auto& file : document["files"]) {
            if (file["module"] == "" && file["representation"] != "resource"
                && functionSearchRoot(fs::u8path(file["source"].get<std::string>()).parent_path())
                    != fs::u8path(file["root"].get<std::string>())) {
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    Json
    pathRecord(const Json& document, const fs::path& path)
    {
        for (const auto& item : document["searchRoots"]) {
            const auto root = fs::u8path(item.get<std::string>());
            if (within(path, root)) {
                const auto relative = path.lexically_relative(root).generic_u8string();
                return { { "root", item }, { "relative", relative == "." ? "" : relative } };
            }
        }
        return {};
    }
    //=============================================================================
    void
    finishLookupPaths(Json& document, const fs::path& applicationRoot, const InputSelection& inputs)
    {
        // Retain implicit nested lookup paths without scanning all paths for flat applications.
        if (inputs.files.empty() && inputs.directories.empty()
            && !hasNestedFunctionPaths(document)) {
            return;
        }
        document["directories"] = Json::array();
        std::set<std::string> directories;
        for (const auto& directory : inputs.directories) {
            document["directories"].push_back(pathRecord(document, directory));
            directories.insert(fileKey(directory));
        }
        document["pathEntries"] = Json::array();
        std::set<std::string> retained;
        auto append = [&](const fs::path& requested) {
            const auto path = fs::weakly_canonical(requested);
            const auto record = pathRecord(document, path);
            if (record.empty() || retained.count(fileKey(path)) != 0) {
                return;
            }
            bool present = record["relative"] == "" || directories.count(fileKey(path)) != 0;
            for (const auto& item : document["files"]) {
                if (present) {
                    break;
                }
                present = present
                    || (item["module"] == ""
                        && within(fs::u8path(item["source"].get<std::string>()), path));
            }
            if (present) {
                document["pathEntries"].push_back(record);
                retained.insert(fileKey(path));
            }
        };
        append(applicationRoot);
        auto* manager = PathFunctionIndexerManager::getInstance();
        const auto userPath = manager->getUserPath();
        if (!userPath.empty()) {
            append(fs::u8path(wstring_to_utf8(userPath)));
        }
        for (const auto& path : manager->getPathNameVector()) {
            append(fs::u8path(wstring_to_utf8(path)));
        }
        for (const auto& path : inputs.lookupPaths) {
            append(path);
        }
        for (const auto& root : document["searchRoots"]) {
            append(fs::u8path(root.get<std::string>()));
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
fs::path
inputRoot(const fs::path& path, const fs::path& applicationRoot, const InputSelection& inputs)
{
    fs::path selected = within(path, applicationRoot) ? applicationRoot : fs::path();
    for (const auto& root : inputs.roots) {
        if (within(path, root) && (selected.empty() || within(selected, root))) {
            selected = root;
        }
    }
    return selected.empty() ? functionSearchRoot(path.parent_path()) : selected;
}
//=============================================================================
void
finishInputPaths(Json& document, const fs::path& applicationRoot, const InputSelection& inputs)
{
    document["searchRoots"] = Json::array();
    std::set<std::string> roots;
    for (const auto& item : document["files"]) {
        if (item["module"] == "") {
            roots.insert(item["root"].get<std::string>());
        }
    }
    for (const auto& directory : inputs.directories) {
        roots.insert(inputRoot(directory, applicationRoot, inputs).generic_u8string());
    }
    auto retainRoot = [&](const fs::path& path) {
        const auto key = fs::weakly_canonical(path).generic_u8string();
        if (roots.erase(key) != 0) {
            document["searchRoots"].push_back(key);
        }
    };
    retainRoot(inputRoot(applicationRoot, applicationRoot, inputs));
    auto* manager = PathFunctionIndexerManager::getInstance();
    const auto userPath = manager->getUserPath();
    if (!userPath.empty()) {
        retainRoot(fs::u8path(wstring_to_utf8(userPath)));
    }
    const auto paths = manager->getPathNameVector();
    for (auto it = paths.rbegin(); it != paths.rend(); ++it) {
        retainRoot(fs::u8path(wstring_to_utf8(*it)));
    }
    for (const auto& root : roots) {
        document["searchRoots"].push_back(root);
    }
    finishLookupPaths(document, applicationRoot, inputs);
}
//=============================================================================
} // namespace Nelson::Application
//=============================================================================
