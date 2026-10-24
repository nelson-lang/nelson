//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nflow_layoutBuiltin.hpp"
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
// nflow_layout(sys)
//   Auto-arrange the blocks of a (sub)system into deterministic left-to-right
//   layers by connectivity. sys is 'model' for the root diagram or
//   'model/Sub[/Inner]' for a nested subsystem; only that depth is arranged and
//   the graph semantics are unchanged.
//=============================================================================
ArrayOfVector
nflow_layoutBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    std::string modelName;
    NFlowModelDocument& doc = requireModel(argIn[0], modelName);
    std::string systemPath;
    if (argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray()) {
        std::string designator = argIn[0].asUtf8String();
        auto pos = designator.find('/');
        if (pos != std::string::npos && pos + 1 < designator.size()) {
            systemPath = designator.substr(pos + 1);
        }
    }
    std::string err;
    if (!doc.layoutSystem(systemPath, err)) {
        Error("Nelson:nflow_engine:layoutFailed", err);
    }
    return ArrayOfVector();
}
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
