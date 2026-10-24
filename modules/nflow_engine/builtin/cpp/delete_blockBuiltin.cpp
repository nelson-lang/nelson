//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "delete_blockBuiltin.hpp"
#include "NFlowBuiltinHelpers.hpp"
#include "NFlowModelRegistry.hpp"
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
delete_blockBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    std::string modelName, blockName;
    splitModelBlock(argIn[0].asUtf8String(), modelName, blockName);
    std::string mn;
    NFlowModelDocument& doc = requireModel(ArrayOf::characterArray(modelName), mn);
    std::string err;
    if (!doc.deleteBlock(blockName, err)) {
        Error("Nelson:nflow_engine:deleteBlockFailed", err);
    }
    NFlowModelRegistry::instance().invalidateBlockHandle(modelName + "/" + blockName);
    return ArrayOfVector();
}
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
