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
#include <nlohmann/json.hpp>
#include <algorithm>
#include <map>
#include <set>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
inline void
inferBusSignalNames(nlohmann::json& diagram)
{
    using json = nlohmann::json;
    if (!diagram.is_object() || !diagram.contains("blocks") || !diagram["blocks"].is_array()) {
        return;
    }
    struct InputName
    {
        std::string source;
        int sourcePort;
        std::string label;
    };
    std::map<std::pair<std::string, int>, std::string> sourceNames;
    std::map<std::string, std::map<int, InputName>> incoming;
    if (diagram.contains("connections") && diagram["connections"].is_array()) {
        for (const auto& conn : diagram["connections"]) {
            if (!conn.is_object()) {
                continue;
            }
            const std::string source = conn.value("from", std::string());
            const std::string target = conn.value("to", std::string());
            const int sourcePort = conn.value("fromIndex", 0);
            const int targetPort = conn.value("toIndex", 0);
            const std::string label = conn.value("label", std::string());
            if (source.empty() || target.empty() || targetPort < 0) {
                continue;
            }
            if (!label.empty()) {
                sourceNames.emplace(std::make_pair(source, sourcePort), label);
            }
            incoming[target][targetPort] = { source, sourcePort, label };
        }
    }
    for (auto& block : diagram["blocks"]) {
        if (!block.is_object()) {
            continue;
        }
        if (!block.contains("params") || !block["params"].is_object()) {
            block["params"] = json::object();
        }
        auto& params = block["params"];
        if (params.contains("subsystem") && params["subsystem"].is_object()) {
            inferBusSignalNames(params["subsystem"]);
        }
        if (block.value("type", std::string()) != "busCreator") {
            continue;
        }
        if (params.contains("MemberNames") && params["MemberNames"].is_array()
            && !params["MemberNames"].empty()) {
            continue;
        }
        const auto found = incoming.find(block.value("id", std::string()));
        if (found == incoming.end()) {
            continue;
        }
        int count = std::max(1, block.value("inputs", 0));
        count = std::max(count, found->second.rbegin()->first + 1);
        std::vector<std::string> names;
        bool hasSignalName = false;
        for (int index = 0; index < count; ++index) {
            std::string name;
            auto entry = found->second.find(index);
            if (entry != found->second.end()) {
                name = entry->second.label;
                if (name.empty()) {
                    const auto source
                        = sourceNames.find({ entry->second.source, entry->second.sourcePort });
                    if (source != sourceNames.end()) {
                        name = source->second;
                    }
                }
            }
            if (!name.empty()) {
                hasSignalName = true;
            } else {
                name = "signal" + std::to_string(index + 1);
            }
            names.push_back(name);
        }
        if (!hasSignalName) {
            continue;
        }
        const std::set<std::string> unique(names.begin(), names.end());
        if (unique.size() != names.size()) {
            for (size_t index = 0; index < names.size(); ++index) {
                names[index] += std::to_string(index + 1);
            }
        }
        params["MemberNames"] = names;
    }
}
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
