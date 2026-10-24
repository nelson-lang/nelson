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
//=============================================================================
using namespace Nelson;
using namespace Nelson::NFlow;
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
static bool
parseAutoRouteOptions(const ArrayOfVector& argIn, size_t start);
//=============================================================================
ArrayOfVector
add_lineBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, -1);
    nargoutcheck(nLhs, 0, 1);
    ensureCatalogLoaded();
    std::string modelName;
    NFlowModelDocument& doc = requireModel(argIn[0], modelName);
    std::string fromBlock, toBlock;
    int fromPort = 0, toPort = 0;
    parseBlockPort(argIn[1].asUtf8String(), fromBlock, fromPort);
    parseBlockPort(argIn[2].asUtf8String(), toBlock, toPort);
    bool autoRoute = parseAutoRouteOptions(argIn, 3);
    std::string err;
    if (!doc.addLine(fromBlock, fromPort, toBlock, toPort, autoRoute, err)) {
        Error("Nelson:nflow_engine:addLineFailed", err);
    }
    ArrayOfVector retval;
    retval << ArrayOf::characterArray(modelName + "/" + fromBlock + "/" + std::to_string(fromPort)
        + "->" + toBlock + "/" + std::to_string(toPort));
    return retval;
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
