//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#include "WebGuiRpc.hpp"
#include "WebGuiServer.hpp"
#include "Evaluator.hpp"
#include "Context.hpp"
#include "FunctionDef.hpp"
#include "NelsonException.hpp"
#include "characters_encoding.hpp"
#include <stdexcept>
//=============================================================================
namespace Nelson {
nlohmann::json
WebGuiRpc::figureAction(const nlohmann::json& params)
{
    const long long handle = params.at("handle").get<long long>();
    const auto& request = params.at("request");
    const std::string encoded = request.dump();
    const bool compactView = request.value("compactView", false);
    nlohmann::json result;
    runOnInterpreterThread([&] {
        if (m_evaluator == nullptr) {
            throw std::runtime_error("Interpreter is unavailable.");
        }
        FunctionDefPtr function = nullptr;
        if (!m_evaluator->lookupFunction("__web_figure_action__", function) || !function) {
            throw std::runtime_error("Figure actions are unavailable.");
        }
        try {
            ArrayOfVector inputs;
            inputs << ArrayOf::doubleScalar(static_cast<double>(handle));
            inputs << ArrayOf::characterArray(utf8_to_wstring(encoded));
            // Interactive pan/zoom gestures only need the metadata here. The
            // shared WebGL preview below renders the retained mesh matrices;
            // rendering a complete display list in this call would defeat the
            // point of coalescing pointer events.
            if (compactView) {
                inputs << ArrayOf::logicalScalar(false);
            }
            auto outputs = function->evaluateFunction(m_evaluator, inputs, 1);
            if (outputs.empty()) {
                throw std::runtime_error("Missing figure action response.");
            }
            result = nlohmann::json::parse(wstring_to_utf8(outputs[0].asWideString()));
        } catch (const NelsonException& error) {
            throw std::runtime_error(wstring_to_utf8(error.what()));
        }
    });
    if (result.contains("displayList")) {
        result["displayList"]["handle"] = handle;
        if (!compactView) {
            m_server->pushJson("figure.updated", { { "handle", handle } });
        }
    }
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
