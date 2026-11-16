//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "add_lineBuiltin.hpp"
#include "NFlowBuiltinHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <cctype>
//=============================================================================
using namespace Nelson;
using namespace Nelson::NFlow;
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
static bool
parseAutoRouteOptions(const ArrayOfVector& argIn, size_t start);
static void
parseEndpoint(const std::string& ref, std::string& block, std::string& token, bool& isPhysical);
//=============================================================================
// add_line(model, 'A/outPort', 'B/inPort' [, 'autorouting', 'on'|'off'])
// Wires two blocks. A numbered endpoint ('A/1') is a 1-based SIGNAL port; a named
// endpoint ('A/p', 'A/flange') is a physical PIN of an acausal block, routed to an
// acausal node instead. Both endpoints must be of the same kind.
ArrayOfVector
add_lineBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, -1);
    nargoutcheck(nLhs, 0, 1);
    ensureCatalogLoaded();
    std::string modelName;
    NFlowModelDocument& doc = requireModel(argIn[0], modelName);
    std::string fromBlock, toBlock, fromToken, toToken;
    bool fromPhysical = false, toPhysical = false;
    parseEndpoint(argIn[1].asUtf8String(), fromBlock, fromToken, fromPhysical);
    parseEndpoint(argIn[2].asUtf8String(), toBlock, toToken, toPhysical);
    if (fromPhysical != toPhysical) {
        Error("Nelson:nflow_engine:mixedConnection",
            "add_line: a signal port cannot be connected to a physical pin. Use numbered "
            "ports for signals and named pins for physical connections.");
    }
    std::string err;
    ArrayOfVector retval;
    if (fromPhysical) {
        // Named pins -> one acausal node. Repeated calls that share a pin merge into
        // the same node, so a node joining three or more pins is built incrementally.
        if (!doc.addPhysicalWire(fromBlock, fromToken, toBlock, toToken, err)) {
            Error("Nelson:nflow_engine:addLineFailed", err);
        }
        retval << ArrayOf::characterArray(
            modelName + "/" + fromBlock + "." + fromToken + "-" + toBlock + "." + toToken);
        return retval;
    }
    int fromPort = std::stoi(fromToken);
    int toPort = std::stoi(toToken);
    bool autoRoute = parseAutoRouteOptions(argIn, 3);
    if (!doc.addLine(fromBlock, fromPort, toBlock, toPort, autoRoute, err)) {
        Error("Nelson:nflow_engine:addLineFailed", err);
    }
    retval << ArrayOf::characterArray(modelName + "/" + fromBlock + "/" + std::to_string(fromPort)
        + "->" + toBlock + "/" + std::to_string(toPort));
    return retval;
}
//=============================================================================
static void
parseEndpoint(const std::string& ref, std::string& block, std::string& token, bool& isPhysical)
{
    auto pos = ref.rfind('/');
    if (pos == std::string::npos || pos == 0 || pos + 1 >= ref.size()) {
        Error("Nelson:nflow_engine:invalidPortReference",
            "Port reference must be of the form 'Block/port' or 'Block/pin': %s", ref);
    }
    block = ref.substr(0, pos);
    token = ref.substr(pos + 1);
    // A purely numeric token is a signal port; any other name is a physical pin.
    isPhysical = false;
    for (char c : token) {
        if (!std::isdigit(static_cast<unsigned char>(c))) {
            isPhysical = true;
            break;
        }
    }
}
//=============================================================================
static bool
parseAutoRouteOptions(const ArrayOfVector& argIn, size_t start)
{
    bool autoRoute = true;
    for (size_t i = start; i + 1 < argIn.size(); i += 2) {
        if (toLowerCopy(argIn[i].asUtf8String()) == "autorouting") {
            autoRoute = toLowerCopy(argIn[i + 1].asUtf8String()) != "off";
        }
    }
    return autoRoute;
}
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
