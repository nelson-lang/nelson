//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "BlockRegistry.hpp"
#include "NFlowCodegenHelpers.hpp"
#include <unordered_map>
//=============================================================================
namespace nflow {
//=============================================================================
struct DashboardTunable
{
    std::string bindingId;
    std::string field;
    std::string targetId;
    std::string parameter;
    std::string variable;
    double initialValue = 0.0;
};
//=============================================================================
struct DashboardCodegenPlan
{
    json diagram;
    std::vector<DashboardTunable> tunables;
    std::vector<std::string> errors;

    const DashboardTunable*
    parameterFor(const std::string& blockId, const std::string& parameter) const
    {
        for (const auto& tunable : tunables) {
            if (tunable.targetId == blockId && tunable.parameter == parameter) {
                return &tunable;
            }
        }
        return nullptr;
    }

    const DashboardTunable*
    variableFor(const json& params, const std::string& parameter) const
    {
        if (!params.is_object() || !params.contains(parameter) || !params[parameter].is_string()) {
            return nullptr;
        }
        const std::string name = params[parameter].get<std::string>();
        for (const auto& tunable : tunables) {
            if (tunable.variable == name) {
                return &tunable;
            }
        }
        return nullptr;
    }
};
//=============================================================================
inline bool
isDashboardBlock(const json& block)
{
    return jstr(block, nflow::kType).rfind("dashboard", 0) == 0;
}
//=============================================================================
inline DashboardCodegenPlan
buildDashboardCodegenPlan(const json& source, const std::string& language)
{
    DashboardCodegenPlan plan;
    plan.diagram = source;
    json sourceBlocks = source.contains(nflow::kBlocks) ? source[nflow::kBlocks] : json::array();
    const json variables
        = source.contains(nflow::kVariables) ? source[nflow::kVariables] : json::object();
    std::unordered_map<std::string, json> byId;
    for (const auto& block : sourceBlocks) {
        if (!isDashboardBlock(block)) {
            byId[jstr(block, nflow::kId)] = block;
        }
    }

    for (const auto& block : sourceBlocks) {
        const std::string type = jstr(block, nflow::kType);
        if (!isDashboardBlock(block) || type == "dashboardCallbackButton") {
            continue;
        }
        const json params = jval(block, nflow::kParams);
        const json binding = params.is_object() ? jval(params, "Binding") : json();
        const std::string kind = binding.is_object() ? jstr(binding, "kind") : std::string();
        if (kind != "parameter" && kind != "variable") {
            continue;
        }
        DashboardTunable tunable;
        tunable.bindingId = jstr(block, nflow::kId);
        tunable.field = "dashboard_" + sanitizeId(tunable.bindingId);
        if (!jstr(binding, "element").empty()) {
            plan.errors.push_back("dashboard_binding(" + tunable.bindingId
                + "): composite elements are not scalar tunables");
            continue;
        }
        if (kind == "variable") {
            tunable.variable = jstr(binding, "varName");
            if (tunable.variable.empty() || !variables.is_object()
                || !variables.contains(tunable.variable)
                || !variables[tunable.variable].is_number()) {
                plan.errors.push_back("dashboard_binding(" + tunable.bindingId
                    + "): variable is missing or not numeric");
                continue;
            }
            tunable.initialValue = variables[tunable.variable].get<double>();
            plan.tunables.push_back(std::move(tunable));
            continue;
        }

        const json path = jval(binding, "blockPath");
        if (!path.is_array() || path.size() != 1 || !path[0].is_string()) {
            plan.errors.push_back("dashboard_binding(" + tunable.bindingId
                + "): generated nested targets are not supported");
            continue;
        }
        tunable.targetId = path[0].get<std::string>();
        tunable.parameter = jstr(binding, "paramName");
        auto target = byId.find(tunable.targetId);
        const json targetParams
            = target == byId.end() ? json() : jval(target->second, nflow::kParams);
        if (target == byId.end() || tunable.parameter.empty() || !targetParams.is_object()
            || !targetParams.contains(tunable.parameter)
            || !targetParams[tunable.parameter].is_number()) {
            plan.errors.push_back("dashboard_binding(" + tunable.bindingId
                + "): target parameter is missing or not scalar numeric");
            continue;
        }
        Nelson::NFlow::BlockCodegenTemplate codegen;
        const std::string targetType = jstr(target->second, nflow::kType);
        const std::string token = "{param:" + tunable.parameter;
        const bool hasCodegen
            = Nelson::NFlow::BlockRegistry::instance().getCodegen(targetType, language, codegen);
        bool stateUsesParameter = false;
        for (const auto& state : codegen.state) {
            stateUsesParameter = stateUsesParameter || state.init.find(token) != std::string::npos;
        }
        if (!hasCodegen || codegen.notImplemented || codegen.step.find(token) == std::string::npos
            || codegen.emitStep || stateUsesParameter) {
            plan.errors.push_back("dashboard_binding(" + tunable.bindingId
                + "): target parameter is structural or unsupported");
            continue;
        }
        tunable.initialValue = targetParams[tunable.parameter].get<double>();
        plan.tunables.push_back(std::move(tunable));
    }

    for (const auto& tunable : plan.tunables) {
        if (tunable.variable.empty()) {
            continue;
        }
        for (const auto& target : sourceBlocks) {
            if (isDashboardBlock(target)) {
                continue;
            }
            const json params = jval(target, nflow::kParams);
            if (!params.is_object()) {
                continue;
            }
            for (const auto& item : params.items()) {
                if (!item.value().is_string()
                    || item.value().get<std::string>() != tunable.variable) {
                    continue;
                }
                Nelson::NFlow::BlockCodegenTemplate codegen;
                const std::string token = "{param:" + item.key();
                if (!Nelson::NFlow::BlockRegistry::instance().getCodegen(
                        jstr(target, nflow::kType), language, codegen)
                    || codegen.notImplemented || codegen.emitStep
                    || codegen.step.find(token) == std::string::npos) {
                    plan.errors.push_back("dashboard_binding(" + tunable.bindingId
                        + "): variable is used by a structural or unsupported parameter");
                }
            }
        }
    }

    json kept = json::array();
    for (const auto& block : sourceBlocks) {
        if (!isDashboardBlock(block)) {
            kept.push_back(block);
        }
    }
    plan.diagram[nflow::kBlocks] = std::move(kept);
    if (plan.diagram.contains(nflow::kConnections)
        && plan.diagram[nflow::kConnections].is_array()) {
        json connections = json::array();
        for (const auto& connection : plan.diagram[nflow::kConnections]) {
            if (byId.count(jstr(connection, nflow::kFrom)) > 0
                && byId.count(jstr(connection, nflow::kTo)) > 0) {
                connections.push_back(connection);
            }
        }
        plan.diagram[nflow::kConnections] = std::move(connections);
    }
    return plan;
}
//=============================================================================
} // namespace nflow
//=============================================================================
