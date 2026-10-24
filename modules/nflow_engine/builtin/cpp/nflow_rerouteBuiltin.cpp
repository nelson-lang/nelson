//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nflow_rerouteBuiltin.hpp"
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
// nflow_reroute(sys [, width, height])
//   Recompute the wire routes of a (sub)system with the router and store the
//   waypoints on each connection. sys is 'model' or
//   'model/Sub[/Inner]'. The optional canvas size defaults to 4000 x 4000.
//=============================================================================
ArrayOfVector
nflow_rerouteBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 3);
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
    int canvasWidth = 4000;
    int canvasHeight = 4000;
    if (argIn.size() >= 2) {
        canvasWidth = static_cast<int>(argIn[1].asDoubleScalar());
    }
    if (argIn.size() >= 3) {
        canvasHeight = static_cast<int>(argIn[2].asDoubleScalar());
    }
    std::string err;
    if (!doc.rerouteSystem(systemPath, canvasWidth, canvasHeight, err)) {
        Error("Nelson:nflow_engine:rerouteFailed", err);
    }
    return ArrayOfVector();
}
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
