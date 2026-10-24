//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "set_paramBuiltin.hpp"
#include "NFlowBuiltinHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <vector>
//=============================================================================
using namespace Nelson;
using namespace Nelson::NFlow;
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
ArrayOfVector
set_paramBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, -1);
    nargoutcheck(nLhs, 0, 0);
    if (((argIn.size() - 1) % 2) != 0) {
        Error("Nelson:nflow_engine:invalidArgument:setParamExpectsNameValuePairs",
            "set_param expects name/value pairs after the object.");
    }
    ParamTarget target = requireParamTarget(argIn[0]);

    // Pass 1 validates every key (and Position arity) before anything is mutated,
    // so a bad pair leaves the model untouched.
    std::vector<std::string> keys;
    std::vector<size_t> valueIndex;
    for (size_t i = 1; i + 1 < argIn.size(); i += 2) {
        std::string key = argIn[i].asUtf8String();
        if (target.isBlock) {
            if (key == "BlockType" || key == "Parent") {
                Error("Nelson:nflow_engine:readOnlyParameter", "Parameter '%s' is read-only.", key);
            }
            if (key == "Position") {
                ArrayOf pv(argIn[i + 1]);
                pv.promoteType(NLS_DOUBLE);
                if (pv.elementCount() != 4) {
                    Error("Nelson:nflow_engine:invalidValue",
                        "Position must be a 1x4 [x y x2 y2] vector.");
                }
            }
            if (key == "Commented") {
                std::string v = argIn[i + 1].asUtf8String();
                if (v != "off" && v != "on" && v != "through") {
                    Error("Nelson:nflow_engine:invalidValue:commentedOffThrough",
                        "Commented must be 'off', 'on' or 'through'.");
                }
            }
            if (key == "PortHandles" || key == "PortConnectivity" || key == "LineHandles") {
                Error("Nelson:nflow_engine:readOnlyParameter", "Parameter '%s' is read-only.", key);
            }
        } else {
            if (key == "Dirty") {
                Error("Nelson:nflow_engine:readOnlyParameter", "Parameter '%s' is read-only.",
                    "Dirty");
            }
            std::string tmp, e2;
            if (key != "Name" && !target.doc->getModelParam(key, tmp, e2)) {
                Error("Nelson:nflow_engine:unknownParameter", "Unknown model parameter '%s'.", key);
            }
        }
        keys.push_back(key);
        valueIndex.push_back(i + 1);
    }

    // Pass 2 applies. Block Position and non-scalar numeric values round-trip as
    // geometry/JSON; everything else stays a string.
    for (size_t j = 0; j < keys.size(); ++j) {
        const std::string& key = keys[j];
        const ArrayOf& value = argIn[valueIndex[j]];
        if (target.isBlock) {
            applyTypedBlockParam(*target.doc, target.blockName, key, value);
        } else {
            std::string err;
            if (!target.doc->setModelParam(key, paramValueToString(value), err)) {
                Error("Nelson:nflow_engine:setParamFailed", err);
            }
        }
    }
    return ArrayOfVector();
}
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
