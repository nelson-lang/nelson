//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "close_systemBuiltin.hpp"
#include "save_systemBuiltin.hpp"
#include "NFlowBuiltinHelpers.hpp"
#include "NFlowModelRegistry.hpp"
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
ArrayOfVector
close_systemBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 0);
    std::string modelName;
    NFlowModelDocument& doc = requireModel(argIn[0], modelName);
    bool hasFlag = argIn.size() >= 2;
    bool save = false;
    if (hasFlag) {
        save = argIn[1].isLogical() ? argIn[1].asLogicalScalar()
                                    : (argIn[1].asDoubleScalar() != 0.0);
    }
    if (doc.dirty() && !hasFlag) {
        Error("Nelson:nflow_engine:modelDirty",
            "Unable to close the model '%s' because it has been changed. To save changes, use "
            "close_system(model, 1). To discard changes, use close_system(model, 0).",
            modelName);
    }
    if (save) {
        ArrayOfVector saveArgs;
        saveArgs << argIn[0];
        save_systemBuiltin(0, saveArgs);
    }
    NFlowModelRegistry::instance().close(modelName);
    return ArrayOfVector();
}
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
