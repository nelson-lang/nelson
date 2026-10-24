//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "delete_lineBuiltin.hpp"
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
ArrayOfVector
delete_lineBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, 3);
    nargoutcheck(nLhs, 0, 0);
    std::string modelName;
    NFlowModelDocument& doc = requireModel(argIn[0], modelName);
    std::string fromBlock, toBlock;
    int fromPort = 0, toPort = 0;
    parseBlockPort(argIn[1].asUtf8String(), fromBlock, fromPort);
    parseBlockPort(argIn[2].asUtf8String(), toBlock, toPort);
    std::string err;
    if (!doc.deleteLine(fromBlock, fromPort, toBlock, toPort, err)) {
        Error("Nelson:nflow_engine:deleteLineFailed", err);
    }
    return ArrayOfVector();
}
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
