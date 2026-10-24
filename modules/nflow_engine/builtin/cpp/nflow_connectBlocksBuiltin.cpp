//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nflow_connectBlocksBuiltin.hpp"
#include "NFlowBuiltinHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::NFlow;
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
// NFlow.connectBlocks(src, dst [, 'RoutingStyle', style]) internal builtin.
ArrayOfVector
nflow_connectBlocksBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, -1);
    nargoutcheck(nLhs, 0, 1);
    ensureCatalogLoaded();
    std::string srcModel, srcBlock, dstModel, dstBlock;
    splitModelBlock(argIn[0].asUtf8String(), srcModel, srcBlock);
    splitModelBlock(argIn[1].asUtf8String(), dstModel, dstBlock);
    auto stripPort = [](std::string& b) {
        auto pos = b.find('/');
        if (pos != std::string::npos) {
            b = b.substr(0, pos);
        }
    };
    stripPort(srcBlock);
    stripPort(dstBlock);
    if (srcModel != dstModel) {
        Error("Nelson:nflow_engine:invalidArgument:connectBlocksSourceDestinationModel",
            "connect_blocks: source and destination must be in the same model.");
    }
    std::string mn;
    NFlowModelDocument& doc = requireModel(ArrayOf::characterArray(srcModel), mn);
    std::string err;
    if (!doc.connectBlocks(srcBlock, dstBlock, err)) {
        Error("Nelson:nflow_engine:connectBlocksFailed", err);
    }
    ArrayOfVector retval;
    retval << ArrayOf::characterArray(srcModel + "/" + srcBlock + "->" + dstBlock);
    return retval;
}
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
