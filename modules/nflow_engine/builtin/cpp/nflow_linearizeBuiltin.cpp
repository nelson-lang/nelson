//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nflow_linearizeBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "characters_encoding.hpp"
#include "SimEngine.hpp"
#include "nlohmann/json.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// __nflow_linearize__(diagramJson [, x0 [, u0]]) -> resultJson
// The diagram is passed as a raw JSON string. A jsondecode round trip would
// homogenize heterogeneous / nested block struct arrays and corrupt a subsystem's
// inner diagram. Optional operating-point vectors x0 and u0 are injected into
// the diagram with nlohmann before linearization.
ArrayOfVector
Nelson::NFlowGateway::nflow_linearizeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 3);
    std::string diagramJson = wstring_to_utf8(argIn[0].asWideString());
    const bool haveX0 = argIn.size() >= 2 && !argIn[1].isEmpty();
    const bool haveU0 = argIn.size() >= 3 && !argIn[2].isEmpty();
    if (haveX0 || haveU0) {
        try {
            nlohmann::json diagram = nlohmann::json::parse(diagramJson);
            if (haveX0) {
                ArrayOf x0 = argIn[1];
                x0.promoteType(NLS_DOUBLE);
                const double* p = (const double*)x0.dataPointer();
                nlohmann::json arr = nlohmann::json::array();
                for (indexType k = 0; k < x0.elementCount(); ++k) {
                    arr.push_back(p[k]);
                }
                diagram["x0"] = arr;
            }
            if (haveU0) {
                ArrayOf u0 = argIn[2];
                u0.promoteType(NLS_DOUBLE);
                const double* p = (const double*)u0.dataPointer();
                nlohmann::json arr = nlohmann::json::array();
                for (indexType k = 0; k < u0.elementCount(); ++k) {
                    arr.push_back(p[k]);
                }
                diagram["u0"] = arr;
            }
            diagramJson = diagram.dump();
        } catch (const std::exception&) {
            // Leave diagramJson unchanged; linearizeModel reports the parse error.
        }
    }
    std::string result = Nelson::NFlow::linearizeModel(diagramJson);
    ArrayOfVector retval;
    retval << ArrayOf::characterArray(utf8_to_wstring(result));
    return retval;
}
//=============================================================================
