//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "bdcloseBuiltin.hpp"
#include "NFlowBuiltinHelpers.hpp"
#include "NFlowModelRegistry.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::NFlow;
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
ArrayOfVector
bdcloseBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 0);
    auto& reg = NFlowModelRegistry::instance();
    if (argIn.empty()) {
        return ArrayOfVector();
    }
    if (argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray()) {
        std::string arg = argIn[0].asUtf8String();
        if (arg == "all") {
            for (const auto& name : reg.loadedNames()) {
                reg.close(name);
            }
            return ArrayOfVector();
        }
    }
    std::string modelName;
    requireModel(argIn[0], modelName);
    reg.close(modelName);
    return ArrayOfVector();
}
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
