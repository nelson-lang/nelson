//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
#include "NFlowExamples.hpp"
#include "ModulePath.hpp"
#include "characters_encoding.hpp"
#include <nlohmann/json.hpp>
#include <algorithm>
#include <cctype>
#include <filesystem>
#include <fstream>
#include <map>
#include <sstream>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson::NFlowExamples {
//=============================================================================
namespace fs = std::filesystem;
//=============================================================================
static std::string
listExamplesImpl(const std::vector<std::string>& librarySearchPaths,
    const std::vector<std::string>& libraryManifests);
static std::string
pathToUtf8(const fs::path& p);
static fs::path
moduleExamplesRoot(const std::wstring& moduleShortName);
static bool
readTextFile(const fs::path& path, std::string& out);
static std::string
humanizeStem(const std::string& stem);
static std::string
categoryLabel(const std::string& category);
static std::string
extractScriptDescription(const std::string& content);
static long long
lastWriteTicks(const fs::path& path);
static void
collectModelsAndScripts(const fs::path& dir, const std::string& category, const std::string& group,
    const std::string& source, bool categoryFromSubdir, nlohmann::json& out);
static void
appendExample(const fs::path& file, const std::string& category, const std::string& group,
    const std::string& source, nlohmann::json& out);
//=============================================================================
std::string
listExamplesJson(const std::vector<std::string>& librarySearchPaths,
    const std::vector<std::string>& libraryManifests)
{
    try {
        return listExamplesImpl(librarySearchPaths, libraryManifests);
    } catch (...) {
        return std::string("{\"examples\":[]}");
    }
}
//=============================================================================
static std::string
listExamplesImpl(const std::vector<std::string>& librarySearchPaths,
    const std::vector<std::string>& libraryManifests)
{
    nlohmann::json examples = nlohmann::json::array();

    // nflow_blocks/examples : one category per immediate sub-directory.
    const fs::path blocksRoot = moduleExamplesRoot(L"nflow_blocks");
    if (fs::is_directory(blocksRoot)) {
        collectModelsAndScripts(
            blocksRoot, std::string(), std::string(), "builtin", true, examples);
    }
    // nflow_fmi/examples : a single "fmi" category (files live flat + sub-dirs).
    const fs::path fmiRoot = moduleExamplesRoot(L"nflow_fmi");
    if (fs::is_directory(fmiRoot)) {
        collectModelsAndScripts(fmiRoot, "fmi", categoryLabel("fmi"), "builtin", false, examples);
    }

    // Map every discovered library directory to its human-readable title so an
    // "examples" folder sitting next to a library.json inherits the library name.
    std::map<std::string, std::string> dirToTitle;
    for (const auto& manifestStr : libraryManifests) {
        try {
            const nlohmann::json manifest = nlohmann::json::parse(manifestStr);
            if (manifest.is_object() && manifest.contains("_dir")) {
                const std::string dir = manifest.value("_dir", "");
                std::string title = manifest.value("title", "");
                if (title.empty()) {
                    title = manifest.value("id", "");
                }
                if (!dir.empty() && !title.empty()) {
                    dirToTitle[fs::path(dir).lexically_normal().string()] = title;
                }
            }
        } catch (...) {
        }
    }

    for (const std::string& sp : librarySearchPaths) {
        std::error_code ec;
        const fs::path root(sp);
        if (!fs::is_directory(root, ec)) {
            continue;
        }
        const fs::path directExamples = root / "examples";
        if (fs::is_directory(directExamples, ec)) {
            const std::string group = root.filename().empty()
                ? std::string("External examples")
                : humanizeStem(root.filename().string());
            const std::string category = "ext:" + pathToUtf8(directExamples);
            collectModelsAndScripts(directExamples, category, group, "external", false, examples);
        }
        for (fs::directory_iterator it(root, fs::directory_options::skip_permission_denied, ec),
            end;
            !ec && it != end; it.increment(ec)) {
            std::error_code ecEntry;
            if (!it->is_directory(ecEntry)) {
                continue;
            }
            const fs::path libDir = it->path();
            const fs::path libExamples = libDir / "examples";
            if (!fs::is_directory(libExamples, ecEntry)) {
                continue;
            }
            const std::string norm = libDir.lexically_normal().string();
            std::string group;
            const auto found = dirToTitle.find(norm);
            if (found != dirToTitle.end()) {
                group = found->second;
            } else {
                group = humanizeStem(libDir.filename().string());
            }
            const std::string category = "ext:" + pathToUtf8(libExamples);
            collectModelsAndScripts(libExamples, category, group, "external", false, examples);
        }
    }

    nlohmann::json res;
    res["examples"] = examples;
    return res.dump();
}
//=============================================================================
// Collects .nflow models and .m scripts under a root. With categoryFromSubdir,
// files are grouped by immediate sub-directory (built-in nflow_blocks layout);
// otherwise every file shares the given category/group.
static void
collectModelsAndScripts(const fs::path& dir, const std::string& category, const std::string& group,
    const std::string& source, bool categoryFromSubdir, nlohmann::json& out)
{
    std::error_code ec;
    for (fs::recursive_directory_iterator
             it(dir, fs::directory_options::skip_permission_denied, ec),
        end;
        !ec && it != end; it.increment(ec)) {
        std::error_code ecEntry;
        if (it->is_directory(ecEntry)) {
            continue;
        }
        const fs::path file = it->path();
        std::string ext = file.extension().string();
        std::transform(ext.begin(), ext.end(), ext.begin(),
            [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
        if (ext != ".nflow" && ext != ".m") {
            continue;
        }
        std::string cat = category;
        std::string grp = group;
        if (categoryFromSubdir) {
            const fs::path rel = fs::relative(file, dir, ecEntry);
            // Only treat the first path component as a category when the file
            // actually lives in a sub-directory (rel has >1 component); a file
            // sitting directly in the root falls back to "general".
            if (!ecEntry && std::distance(rel.begin(), rel.end()) > 1) {
                cat = rel.begin()->string();
            } else {
                cat = "general";
            }
            grp = categoryLabel(cat);
        }
        appendExample(file, cat, grp, source, out);
    }
}
//=============================================================================
static void
appendExample(const fs::path& file, const std::string& category, const std::string& group,
    const std::string& source, nlohmann::json& out)
{
    std::string ext = file.extension().string();
    std::transform(ext.begin(), ext.end(), ext.begin(),
        [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    const bool isModel = (ext == ".nflow");

    nlohmann::json entry;
    entry["path"] = pathToUtf8(file);
    entry["category"] = category;
    entry["group"] = group;
    entry["source"] = source;
    entry["kind"] = isModel ? "model" : "script";
    entry["mtime"] = lastWriteTicks(file);

    const std::string stem = file.stem().string();
    std::string name = humanizeStem(stem);
    std::string description;
    std::string author;
    nlohmann::json tags = nlohmann::json::array();

    std::string content;
    const bool haveContent = readTextFile(file, content);

    if (isModel) {
        if (haveContent) {
            try {
                const nlohmann::json model = nlohmann::json::parse(content);
                if (model.is_object()) {
                    if (model.contains("name") && model["name"].is_string()
                        && !model["name"].get<std::string>().empty()) {
                        name = model["name"].get<std::string>();
                    }
                    if (model.contains("comments") && model["comments"].is_string()) {
                        const std::string c = model["comments"].get<std::string>();
                        if (c != "Generated by nflow") {
                            description = c;
                        }
                    }
                    if (model.contains("metadata") && model["metadata"].is_object()) {
                        author = model["metadata"].value("author", "");
                    }
                    if (model.contains("runtime")) {
                        entry["runtime"] = model["runtime"];
                    }
                    if (model.contains("sampleTime")) {
                        entry["sampleTime"] = model["sampleTime"];
                    }
                    int blockCount = 0;
                    bool acausal = false;
                    if (model.contains("blocks") && model["blocks"].is_array()) {
                        blockCount = static_cast<int>(model["blocks"].size());
                    }
                    if (model.contains("physicalWires")
                        && ((model["physicalWires"].is_array() && !model["physicalWires"].empty())
                            || (model["physicalWires"].is_object()
                                && !model["physicalWires"].empty()))) {
                        acausal = true;
                    }
                    entry["blockCount"] = blockCount;
                    if (acausal) {
                        tags.push_back("acausal");
                    }
                }
            } catch (...) {
            }
        }
        if (category == "fmi" || name.find("FMU") != std::string::npos
            || stem.find("fmu") != std::string::npos) {
            tags.push_back("fmu");
        }
    } else {
        if (haveContent) {
            description = extractScriptDescription(content);
        }
    }

    if (source == "external") {
        tags.push_back("external");
    }

    entry["name"] = name;
    entry["description"] = description;
    entry["author"] = author;
    entry["tags"] = tags;
    out.push_back(entry);
}
//=============================================================================
static fs::path
moduleExamplesRoot(const std::wstring& moduleShortName)
{
    try {
        const std::wstring root = ModulePath(moduleShortName, GET_ROOT_PATH);
        return fs::path(root) / L"examples";
    } catch (...) {
        return fs::path();
    }
}
//=============================================================================
static bool
readTextFile(const fs::path& path, std::string& out)
{
    std::ifstream file(path, std::ios::binary);
    if (!file.is_open()) {
        return false;
    }
    std::ostringstream ss;
    ss << file.rdbuf();
    out = ss.str();
    return true;
}
//=============================================================================
static std::string
pathToUtf8(const fs::path& p)
{
#ifdef _MSC_VER
    return wstring_to_utf8(p.wstring());
#else
    return p.string();
#endif
}
//=============================================================================
// Turns "ODE45_Solver_Demo" into "ODE45 Solver Demo": separators become spaces,
// tokens that are already all-lowercase get their first letter capitalized, and
// tokens carrying uppercase (acronyms) are preserved verbatim.
static std::string
humanizeStem(const std::string& stem)
{
    std::string spaced;
    spaced.reserve(stem.size());
    for (char c : stem) {
        spaced.push_back((c == '_' || c == '-') ? ' ' : c);
    }
    std::istringstream iss(spaced);
    std::string token;
    std::string result;
    while (iss >> token) {
        bool hasUpper = false;
        for (char c : token) {
            if (std::isupper(static_cast<unsigned char>(c))) {
                hasUpper = true;
                break;
            }
        }
        if (!hasUpper && !token.empty()) {
            token[0] = static_cast<char>(std::toupper(static_cast<unsigned char>(token[0])));
        }
        if (!result.empty()) {
            result.push_back(' ');
        }
        result += token;
    }
    return result.empty() ? stem : result;
}
//=============================================================================
static std::string
categoryLabel(const std::string& category)
{
    static const std::map<std::string, std::string> labels = {
        { "acausal", "Acausal" },
        { "causal", "Causal" },
        { "dashboard", "Dashboard" },
        { "solvers", "Solvers" },
        { "subsystems", "Subsystems" },
        { "workspace", "Workspace" },
        { "fmi", "FMI / co-simulation" },
    };
    const auto it = labels.find(category);
    if (it != labels.end()) {
        return it->second;
    }
    return humanizeStem(category);
}
//=============================================================================
// Pulls the first meaningful sentence out of a Nelson script header, skipping
// the license block, separator rules, and copyright/SPDX boilerplate.
static std::string
extractScriptDescription(const std::string& content)
{
    std::istringstream iss(content);
    std::string line;
    bool pastLicence = false;
    while (std::getline(iss, line)) {
        while (
            !line.empty() && (line.back() == '\r' || line.back() == ' ' || line.back() == '\t')) {
            line.pop_back();
        }
        std::string trimmed = line;
        size_t start = trimmed.find_first_not_of(" \t");
        if (start == std::string::npos) {
            continue;
        }
        trimmed = trimmed.substr(start);
        if (trimmed.empty() || trimmed[0] != '%') {
            if (!pastLicence) {
                continue;
            }
            break;
        }
        std::string text = trimmed.substr(1);
        size_t ts = text.find_first_not_of(" \t");
        text = (ts == std::string::npos) ? std::string() : text.substr(ts);

        if (text.find("LICENCE_BLOCK_END") != std::string::npos) {
            pastLicence = true;
            continue;
        }
        if (!pastLicence) {
            continue;
        }
        if (text.find_first_not_of("=-") == std::string::npos) {
            continue;
        }
        if (text.empty()) {
            continue;
        }
        bool hasAlpha = false;
        for (char c : text) {
            if (std::isalpha(static_cast<unsigned char>(c))) {
                hasAlpha = true;
                break;
            }
        }
        if (hasAlpha) {
            return text;
        }
    }
    return std::string();
}
//=============================================================================
static long long
lastWriteTicks(const fs::path& path)
{
    std::error_code ec;
    const auto t = fs::last_write_time(path, ec);
    if (ec) {
        return 0;
    }
    return static_cast<long long>(t.time_since_epoch().count());
}
//=============================================================================
} // namespace Nelson::NFlowExamples
//=============================================================================
