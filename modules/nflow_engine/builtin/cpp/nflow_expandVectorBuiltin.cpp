//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nflow_expandVectorBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "characters_encoding.hpp"
#include "NFlowCodegenHelpers.hpp"
#include "SimEngine.hpp"
#include <nlohmann/json.hpp>
//=============================================================================
using namespace Nelson;
//=============================================================================
// __nflow_expand_vector__(diagramJson) -> expandedDiagramJson
// Vector -> scalar diagram expansion used by code generation. Exposed for tests:
// the expanded diagram must simulate identically to the vector input. Returns an
// "error" object on an unsupported block (e.g. a vector matmul).
//=============================================================================
ArrayOfVector
Nelson::NFlowGateway::nflow_expandVectorBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    std::string diagramJson = wstring_to_utf8(argIn[0].asWideString());
    Nelson::NFlow::ensureBuiltinsRegistered();
    nlohmann::json diagram;
    try {
        diagram = nlohmann::json::parse(diagramJson);
    } catch (const std::exception& ex) {
        std::string err = std::string("{\"error\":\"JSON parse error: ") + ex.what() + "\"}";
        ArrayOfVector retval;
        retval << ArrayOf::characterArray(utf8_to_wstring(err));
        return retval;
    }
    nflow::VectorExpandResult xr = nflow::expandVectorDiagram(diagram);
    std::string out;
    if (!xr.ok) {
        nlohmann::json e;
        e["error"] = xr.error;
        e["errorBlock"] = xr.errorBlockId;
        out = e.dump();
    } else {
        out = xr.diagram.dump();
    }
    ArrayOfVector retval;
    retval << ArrayOf::characterArray(utf8_to_wstring(out));
    return retval;
}
//=============================================================================
