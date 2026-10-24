//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NFlowExamplesProvider.hpp"
#include "BlockRegistry.hpp"
#include "NFlowExamples.hpp"
#include <nlohmann/json.hpp>
#include <algorithm>
#include <map>
#include <vector>
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
using json = nlohmann::json;
//=============================================================================
static size_t
categoryRank(const std::string& source, const std::string& category);
static json
groupExample(const json& item);
//=============================================================================
std::string
nflowExampleGroupsJson()
{
    auto& registry = Nelson::NFlow::BlockRegistry::instance();
    json list = json::parse(Nelson::NFlowExamples::listExamplesJson(
        registry.searchPaths(), registry.discoveredManifests()));
    // Group by (source, category); the map keeps categories in discovery order
    // and the final sort places built-in categories first in their usual order.
    std::vector<json> groups;
    std::map<std::string, size_t> indexByKey;
    for (const auto& item : list.value("examples", json::array())) {
        const std::string source
            = item.value("source", "builtin") == "external" ? "external" : "nflow";
        const std::string category = item.value("category", "general");
        const std::string key = source + ":" + category;
        auto found = indexByKey.find(key);
        if (found == indexByKey.end()) {
            found = indexByKey.emplace(key, groups.size()).first;
            groups.push_back(
                { { "name", key }, { "title", item.value("group", category) }, { "source", source },
                    { "examples", json::array() }, { "rank", categoryRank(source, category) } });
        }
        groups[found->second]["examples"].push_back(groupExample(item));
    }
    std::stable_sort(groups.begin(), groups.end(), [](const json& a, const json& b) {
        if (a["rank"] != b["rank"]) {
            return a["rank"].get<size_t>() < b["rank"].get<size_t>();
        }
        return a["title"].get<std::string>() < b["title"].get<std::string>();
    });
    json result = json::array();
    for (json& group : groups) {
        json& examples = group["examples"];
        std::stable_sort(examples.begin(), examples.end(), [](const json& a, const json& b) {
            return a["title"].get<std::string>() < b["title"].get<std::string>();
        });
        group.erase("rank");
        result.push_back(std::move(group));
    }
    return result.dump();
}
//=============================================================================
static size_t
categoryRank(const std::string& source, const std::string& category)
{
    static const std::vector<std::string> order
        = { "causal", "acausal", "dashboard", "subsystems", "solvers", "workspace", "fmi" };
    if (source != "nflow") {
        return order.size() + 1;
    }
    const auto found = std::find(order.begin(), order.end(), category);
    return found == order.end() ? order.size() : static_cast<size_t>(found - order.begin());
}
//=============================================================================
static json
groupExample(const json& item)
{
    json entry
        = { { "title", item.value("name", "") }, { "description", item.value("description", "") },
              { "kind", item.value("kind", "model") }, { "file", item.value("path", "") },
              { "tags", item.value("tags", json::array()) }, { "requirements", json::array() } };
    if (item.contains("blockCount")) {
        entry["blockCount"] = item.at("blockCount");
    }
    return entry;
}
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
extern "C" int
nlsNFlowExampleGroupsJson(
    const char* locale, void (*sink)(void* context, const char* groupsJson), void* context)
{
    (void)locale;
    if (sink == nullptr) {
        return 0;
    }
    try {
        const std::string groups = Nelson::NFlowGateway::nflowExampleGroupsJson();
        sink(context, groups.c_str());
        return 1;
    } catch (...) {
        return 0;
    }
}
//=============================================================================
