//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "DashboardBindingManager.hpp"
#include <algorithm>
#include <cctype>
#include <stdexcept>
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
static bool
isDashboardObserver(const std::string& type)
{
    return type == "dashboardScope" || type == "dashboardDisplay" || type == "dashboardGauge"
        || type == "dashboardHalfGauge" || type == "dashboardQuarterGauge"
        || type == "dashboardLinearGauge" || type == "dashboardLamp"
        || type == "dashboardMultiStateImage";
}
//=============================================================================
static bool
isDashboardControl(const std::string& type)
{
    return type.rfind("dashboard", 0) == 0 && !isDashboardObserver(type)
        && type != "dashboardCallbackButton";
}
//=============================================================================
static std::vector<std::string>
bindingPath(const json& binding)
{
    std::vector<std::string> path;
    if (!binding.is_object() || !binding.contains("blockPath")) {
        return path;
    }
    const json& raw = binding["blockPath"];
    if (raw.is_string() && !raw.get<std::string>().empty()) {
        path.push_back(raw.get<std::string>());
    } else if (raw.is_array()) {
        for (const json& item : raw) {
            if (item.is_string() && !item.get<std::string>().empty()) {
                path.push_back(item.get<std::string>());
            }
        }
    }
    return path;
}
//=============================================================================
static Block*
findBlockByPath(std::vector<Block>& blocks, std::vector<BlockState>& states,
    const std::vector<std::string>& path, size_t index = 0)
{
    if (index >= path.size()) {
        return nullptr;
    }
    for (Block& block : blocks) {
        if (block.id != path[index]) {
            continue;
        }
        if (index + 1 == path.size()) {
            return &block;
        }
        if (block.nid < 0 || block.nid >= static_cast<BlockId>(states.size())
            || !states[block.nid].subsystem) {
            return nullptr;
        }
        auto& subsystem = *states[block.nid].subsystem;
        return findBlockByPath(subsystem.blocks, subsystem.blockStateVec, path, index + 1);
    }
    return nullptr;
}
//=============================================================================
struct RuntimeBlockRef
{
    Block* block = nullptr;
    BlockState* state = nullptr;
};
//=============================================================================
static RuntimeBlockRef
findBlockById(std::vector<Block>& blocks, std::vector<BlockState>& states, const std::string& id)
{
    for (Block& block : blocks) {
        if (block.id == id) {
            BlockState* state = block.nid >= 0 && block.nid < static_cast<BlockId>(states.size())
                ? &states[block.nid]
                : nullptr;
            return { &block, state };
        }
        if (block.nid >= 0 && block.nid < static_cast<BlockId>(states.size())
            && states[block.nid].subsystem) {
            auto& subsystem = *states[block.nid].subsystem;
            RuntimeBlockRef nested = findBlockById(subsystem.blocks, subsystem.blockStateVec, id);
            if (nested.block) {
                return nested;
            }
        }
    }
    return {};
}
//=============================================================================
static bool
parseElement(const std::string& element, std::vector<size_t>& indices)
{
    indices.clear();
    if (element.empty()) {
        return true;
    }
    std::string text;
    for (char c : element) {
        if (c != '(' && c != ')' && !std::isspace(static_cast<unsigned char>(c))) {
            text.push_back(c);
        }
    }
    size_t begin = 0;
    while (begin <= text.size()) {
        const size_t end = text.find(',', begin);
        const std::string item = text.substr(begin, end - begin);
        if (item.empty() || item.find_first_not_of("0123456789") != std::string::npos) {
            return false;
        }
        unsigned long oneBased = 0;
        try {
            oneBased = std::stoul(item);
        } catch (const std::out_of_range&) {
            // An over-long numeric string from a saved model would throw uncaught;
            // reject the index instead.
            return false;
        } catch (const std::invalid_argument&) {
            return false;
        }
        if (oneBased == 0) {
            return false;
        }
        indices.push_back(static_cast<size_t>(oneBased - 1));
        if (end == std::string::npos) {
            break;
        }
        begin = end + 1;
    }
    return !indices.empty();
}
//=============================================================================
static json*
jsonElement(json& value, const std::string& element)
{
    std::vector<size_t> indices;
    if (!parseElement(element, indices)) {
        return nullptr;
    }
    json* current = &value;
    for (size_t index : indices) {
        if (!current->is_array() || index >= current->size()) {
            return nullptr;
        }
        current = &(*current)[index];
    }
    return current;
}
//=============================================================================
static std::string
variableKey(const std::string& name, const std::string& element)
{
    if (element.empty()) {
        return name;
    }
    std::vector<size_t> indices;
    if (!parseElement(element, indices)) {
        return std::string();
    }
    std::string key = name + "(";
    for (size_t i = 0; i < indices.size(); ++i) {
        if (i > 0) {
            key += ",";
        }
        key += std::to_string(indices[i] + 1);
    }
    return key + ")";
}
//=============================================================================
void
flattenDashboardVariable(
    const std::string& name, const json& value, ValMap& variables, std::vector<size_t> path)
{
    if (value.is_number()) {
        std::string key = name;
        if (!path.empty()) {
            key += "(";
            for (size_t i = 0; i < path.size(); ++i) {
                if (i > 0) {
                    key += ",";
                }
                key += std::to_string(path[i] + 1);
            }
            key += ")";
        }
        variables[key] = value.get<double>();
        return;
    }
    if (!value.is_array()) {
        return;
    }
    for (size_t i = 0; i < value.size(); ++i) {
        std::vector<size_t> child = path;
        child.push_back(i);
        flattenDashboardVariable(name, value[i], variables, std::move(child));
    }
}
//=============================================================================
static DashboardBindingDiagnostic
diagnostic(const Block& block, const std::string& message)
{
    return { block.id, "dashboard_binding(" + block.id + "): " + message };
}
//=============================================================================
static bool
validateOne(Block& block, BlockState* dashboardState, std::vector<Block>& blocks,
    std::vector<BlockState>& states, const ValMap& variables, DashboardBindingDiagnostic& error)
{
    if (!asJson(block.params).is_object() || !asJson(block.params).contains("Binding")
        || asJson(block.params)["Binding"].is_null()) {
        return true;
    }
    const json& binding = asJson(block.params)["Binding"];
    const std::string kind = binding.value("kind", std::string());
    if (kind.empty()) {
        return true;
    }
    if (isDashboardObserver(block.type)) {
        if (kind != "signal") {
            error = diagnostic(block, "observer requires a signal target");
            return false;
        }
        const std::vector<std::string> path = bindingPath(binding);
        Block* target = findBlockByPath(blocks, states, path);
        const int port = binding.value("outputPortIndex", 1) - 1;
        if (!target) {
            error = diagnostic(block, "target block was not found");
            return false;
        }
        if (port < 0 || port >= target->outputs) {
            error = diagnostic(block, "output port is out of range");
            return false;
        }
        const int width = target->outWidth(port);
        if (block.type != "dashboardScope" && block.type != "dashboardDisplay" && width != 1) {
            error = diagnostic(block, "target signal must be scalar");
            return false;
        }
        return true;
    }
    if (!isDashboardControl(block.type)) {
        return true;
    }
    if (kind == "variable") {
        const std::string name = binding.value("varName", std::string());
        const std::string key = variableKey(name, binding.value("element", std::string()));
        const auto found = variables.find(key);
        if (name.empty() || key.empty() || found == variables.end()) {
            error = diagnostic(block, "variable is missing, non-scalar, or non-numeric");
            return false;
        }
        if (dashboardState) {
            dashboardState->scalar = found->second;
            dashboardState->dashboardTouched = true;
        }
        return true;
    }
    if (kind != "parameter") {
        error = diagnostic(block, "control requires a parameter or variable target");
        return false;
    }
    const std::vector<std::string> path = bindingPath(binding);
    Block* target = findBlockByPath(blocks, states, path);
    const std::string parameter = binding.value("paramName", std::string());
    if (!target || parameter.empty() || !asJson(target->params).contains(parameter)) {
        error = diagnostic(block, "target parameter was not found");
        return false;
    }
    json* value = jsonElement(asJson(target->params)[parameter], binding.value("element", std::string()));
    if (!value || !value->is_number()) {
        error = diagnostic(block, "target parameter must be scalar and numeric");
        return false;
    }
    if (dashboardState) {
        dashboardState->scalar = value->get<double>();
        dashboardState->dashboardTouched = true;
    }
    return true;
}
//=============================================================================
static void
validateDashboardLevel(std::vector<Block>& levelBlocks, std::vector<BlockState>& levelStates,
    std::vector<Block>& rootBlocks, std::vector<BlockState>& rootStates, const ValMap& variables,
    std::vector<DashboardBindingDiagnostic>& errors)
{
    for (Block& block : levelBlocks) {
        BlockState* state = block.nid >= 0 && block.nid < static_cast<BlockId>(levelStates.size())
            ? &levelStates[block.nid]
            : nullptr;
        if (block.type.rfind("dashboard", 0) == 0) {
            DashboardBindingDiagnostic error;
            if (!validateOne(block, state, rootBlocks, rootStates, variables, error)) {
                errors.push_back(std::move(error));
            }
        }
        if (state && state->subsystem) {
            auto& subsystem = *state->subsystem;
            validateDashboardLevel(subsystem.blocks, subsystem.blockStateVec, rootBlocks,
                rootStates, variables, errors);
        }
    }
}
//=============================================================================
std::vector<DashboardBindingDiagnostic>
validateDashboardBindings(
    std::vector<Block>& blocks, std::vector<BlockState>& states, const ValMap& variables)
{
    std::vector<DashboardBindingDiagnostic> errors;
    validateDashboardLevel(blocks, states, blocks, states, variables, errors);
    return errors;
}
//=============================================================================
bool
applyDashboardBindingValue(std::vector<Block>& blocks, std::vector<BlockState>& states,
    ValMap& variables, const json& event, DashboardBindingDiagnostic& error)
{
    const std::string id = event.value("id", std::string());
    if (id.empty() || !event.contains("value") || !event["value"].is_number()) {
        error = { id, "dashboard event requires an id and numeric value" };
        return false;
    }
    RuntimeBlockRef control = findBlockById(blocks, states, id);
    if (!control.block || !isDashboardControl(control.block->type)
        || !asJson(control.block->params).contains("Binding")) {
        error = { id, "dashboard_binding(" + id + "): control was not found" };
        return false;
    }
    const json& binding = asJson(control.block->params)["Binding"];
    const std::string kind = binding.value("kind", std::string());
    const double value = event["value"].get<double>();
    if (kind == "variable") {
        const std::string name = binding.value("varName", std::string());
        const std::string key = variableKey(name, binding.value("element", std::string()));
        auto found = variables.find(key);
        if (key.empty() || found == variables.end()) {
            error = diagnostic(*control.block, "variable target was deleted or is not scalar");
            return false;
        }
        found->second = value;
    } else if (kind == "parameter") {
        Block* target = findBlockByPath(blocks, states, bindingPath(binding));
        const std::string parameter = binding.value("paramName", std::string());
        if (!target || parameter.empty() || !asJson(target->params).contains(parameter)) {
            error = diagnostic(*control.block, "parameter target was deleted");
            return false;
        }
        json* targetValue
            = jsonElement(asJson(target->params)[parameter], binding.value("element", std::string()));
        if (!targetValue || !targetValue->is_number()) {
            error = diagnostic(*control.block, "parameter target is not scalar numeric");
            return false;
        }
        *targetValue = value;
    } else {
        error = diagnostic(*control.block, "control target type is invalid");
        return false;
    }
    if (control.state) {
        control.state->scalar = value;
        control.state->scalar2 = 1.0;
        control.state->dashboardTouched = true;
        control.state->dashboardError.clear();
    }
    return true;
}
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
