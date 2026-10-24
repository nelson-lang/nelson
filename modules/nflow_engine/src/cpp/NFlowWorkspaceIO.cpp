//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NFlowWorkspaceIO.hpp"
#include <nlohmann/json.hpp>
#include "NelsonWorkspaceBridge.hpp"
#include "FieldNames.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    namespace {
        //=============================================================================
        using json = nlohmann::json;
        //=============================================================================
        // Walks every "blocks" array, including nested subsystem diagrams.
        bool
        resolveBlocksIn(json& node, std::string& failedBlockId, std::string& message)
        {
            if (!node.is_object() || !node.contains(nflow::kBlocks)
                || !node[nflow::kBlocks].is_array()) {
                return true;
            }
            for (auto& b : node[nflow::kBlocks]) {
                if (!b.is_object()) {
                    continue;
                }
                const std::string type = b.contains(nflow::kType) && b[nflow::kType].is_string()
                    ? b[nflow::kType].get<std::string>()
                    : std::string {};
                json& params = b[nflow::kParams];
                if (type == "fromWorkspace") {
                    std::string variableName = "simin";
                    if (params.is_object() && params.contains(nflow::kVariableName)
                        && params[nflow::kVariableName].is_string()) {
                        variableName = params[nflow::kVariableName].get<std::string>();
                    }
                    WorkspaceSignal signal = readWorkspaceSignal(variableName);
                    if (!signal.ok) {
                        failedBlockId = b.contains(nflow::kId) && b[nflow::kId].is_string()
                            ? b[nflow::kId].get<std::string>()
                            : std::string {};
                        message = signal.errorMessage;
                        return false;
                    }
                    params[nflow::kTimes] = signal.times;
                    params[nflow::kValues] = signal.values;
                    params[nflow::kWidth] = signal.width;
                    if (signal.isComplex) {
                        params["valuesImag"] = signal.valuesImag;
                        params["isComplex"] = true;
                    }
                }
                // {"params":{"subsystem":{"blocks":[...]}}}
                if (params.is_object() && params.contains(nflow::kSubsystem)
                    && params[nflow::kSubsystem].is_object()) {
                    if (!resolveBlocksIn(params[nflow::kSubsystem], failedBlockId, message)) {
                        return false;
                    }
                }
            }
            return true;
        }
        //=============================================================================
    } // namespace
    //=============================================================================
    bool
    resolveWorkspaceInputs(std::string& diagramJson, std::string& errorJson)
    {
        json diagram;
        try {
            diagram = json::parse(diagramJson);
        } catch (const json::exception&) {
            return true;
        }
        std::string failedBlockId;
        std::string message;
        if (!resolveBlocksIn(diagram, failedBlockId, message)) {
            json err;
            err["status"] = "error";
            err["reason"] = "fromWorkspace: " + message;
            err["errorBlocks"] = json::array({ failedBlockId });
            errorJson = err.dump();
            return false;
        }
        diagramJson = diagram.dump();
        return true;
    }
    //=============================================================================
    std::vector<std::string>
    flushWorkspaceOutputs(const std::string& resultJson)
    {
        std::vector<std::string> errors;
        json result;
        try {
            result = json::parse(resultJson);
        } catch (const json::exception&) {
            return errors;
        }
        if (!result.is_object() || !result.contains("scopes") || !result["scopes"].is_array()) {
            return errors;
        }
        for (const auto& entry : result["scopes"]) {
            if (!entry.is_object() || !entry.contains(nflow::kType)
                || entry[nflow::kType] != "toWorkspace") {
                continue;
            }
            WorkspacePayload payload;
            payload.variableName = entry.value(nflow::kName, std::string("simout"));
            payload.saveFormat = entry.value("saveFormat", std::string("Structure With Time"));
            payload.blockName = entry.value("blockName", std::string {});
            payload.width = std::max(1, entry.value(nflow::kWidth, 1));
            if (entry.contains(nflow::kTimes) && entry[nflow::kTimes].is_array()) {
                for (const auto& v : entry[nflow::kTimes]) {
                    payload.times.push_back(v.is_number() ? v.get<double>() : 0.0);
                }
            }
            if (entry.contains(nflow::kValues) && entry[nflow::kValues].is_array()) {
                for (const auto& v : entry[nflow::kValues]) {
                    payload.values.push_back(v.is_number() ? v.get<double>() : 0.0);
                }
            }
            if (entry.contains("valuesImag") && entry["valuesImag"].is_array()) {
                for (const auto& v : entry["valuesImag"]) {
                    payload.valuesImag.push_back(v.is_number() ? v.get<double>() : 0.0);
                }
                payload.isComplex = !payload.valuesImag.empty();
            }
            if (entry.contains("valuesI64") && entry["valuesI64"].is_array()) {
                for (const auto& v : entry["valuesI64"]) {
                    if (v.is_number_unsigned()) {
                        payload.valuesI64.push_back((long long)v.get<unsigned long long>());
                    } else if (v.is_number_integer()) {
                        payload.valuesI64.push_back(v.get<long long>());
                    } else {
                        payload.valuesI64.push_back(v.is_number() ? (long long)v.get<double>() : 0);
                    }
                }
                payload.exactType = entry.value("exactType", std::string {});
            }
            std::string errorMessage;
            if (!writeWorkspaceVariable(payload, errorMessage)) {
                errors.push_back("toWorkspace '" + payload.variableName + "': " + errorMessage);
            }
        }
        return errors;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
