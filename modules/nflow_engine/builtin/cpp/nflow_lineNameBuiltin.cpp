//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nflow_lineNameBuiltin.hpp"
#include "NFlowBuiltinHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <string>
//=============================================================================
using namespace Nelson;
using namespace Nelson::NFlow;
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
// nflow_lineName(model, 'From/port', 'To/port')            -> get the signal name
// nflow_lineName(model, 'From/port', 'To/port', name)      -> set the signal name
//   Reads or sets the signal name of a connection. Endpoints use the
//   1-based 'Block/port' convention; blocks may be nested (model/Sub/Block).
//=============================================================================
ArrayOfVector
nflow_lineNameBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, 4);
    nargoutcheck(nLhs, 0, 1);
    std::string modelName;
    NFlowModelDocument& doc = requireModel(argIn[0], modelName);
    std::string fromBlock, toBlock;
    int fromPort = 0, toPort = 0;
    parseBlockPort(argIn[1].asUtf8String(), fromBlock, fromPort);
    parseBlockPort(argIn[2].asUtf8String(), toBlock, toPort);

    std::string err;
    if (argIn.size() == 4) {
        const std::string name = argIn[3].asUtf8String();
        if (!doc.setLineName(fromBlock, fromPort, toBlock, toPort, name, err)) {
            Error("Nelson:nflow_engine:lineNameFailed", err);
        }
        return ArrayOfVector();
    }
    std::string outName;
    if (!doc.getLineName(fromBlock, fromPort, toBlock, toPort, outName, err)) {
        Error("Nelson:nflow_engine:lineNameFailed", err);
    }
    ArrayOfVector retval;
    retval << ArrayOf::characterArray(outName);
    return retval;
}
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
