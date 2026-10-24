//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "find_systemBuiltin.hpp"
#include "NFlowBuiltinHelpers.hpp"
#include "NFlowModelDocument.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <string>
#include <vector>
//=============================================================================
using namespace Nelson;
using namespace Nelson::NFlow;
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
static void
collectSystemPaths(const nlohmann::json& diagram, const std::string& prefix, int remaining,
    bool haveTypeFilter, const std::string& typeFilter, stringVector& out);
//=============================================================================
// find_system(sys)
//   Returns the model and every block under it (recursively into subsystems),
//   as a cell array of paths ('model', 'model/BlockName', 'model/Sub/BlockName',
//   ...) in a deterministic depth-first order.
// find_system(sys, 'BlockType', TYPE)
//   Returns only the paths of blocks whose type is TYPE (the model itself is
//   omitted), still searching every level.
// find_system(sys, 'SearchDepth', N)
//   Limits the search: 0 = the system only, 1 = its direct children, N = down to
//   depth N. Omitted = every level. 'Type'/'LookUnderMasks'/'FollowLinks' are
//   accepted for call-site compatibility; an unknown property is an error.
//=============================================================================
ArrayOfVector
find_systemBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, -1);
    nargoutcheck(nLhs, 0, 1);
    if (((argIn.size() - 1) % 2) != 0) {
        Error("Nelson:nflow_engine:invalidArgument:findSystemExpectsNameValuePairs",
            "find_system expects name/value pairs after the system.");
    }
    std::string modelName;
    NFlowModelDocument& doc = requireModel(argIn[0], modelName);

    std::string blockTypeFilter;
    bool haveTypeFilter = false;
    int searchDepth = -1; // -1 = unlimited
    for (size_t i = 1; i + 1 < argIn.size(); i += 2) {
        std::string key = argIn[i].asUtf8String();
        if (key == "BlockType" || key == "blocktype") {
            blockTypeFilter = argIn[i + 1].asUtf8String();
            haveTypeFilter = true;
        } else if (key == "SearchDepth" || key == "searchdepth") {
            const ArrayOf& v = argIn[i + 1];
            double d = 0;
            if (v.isNumeric() && !v.isEmpty()) {
                d = v.asDoubleScalar();
            } else {
                try {
                    d = std::stod(v.asUtf8String());
                } catch (...) {
                    Error("Nelson:nflow_engine:invalidValue:findSystemSearchDepthANonNegative",
                        "find_system: SearchDepth must be a non-negative integer.");
                }
            }
            if (d < 0) {
                Error("Nelson:nflow_engine:invalidValue:findSystemSearchDepthANonNegative",
                    "find_system: SearchDepth must be a non-negative integer.");
            }
            searchDepth = static_cast<int>(d);
        } else if (key == "Type" || key == "LookUnderMasks" || key == "FollowLinks") {
            // Accepted for call-site compatibility; no effect in this version.
        } else {
            Error("Nelson:nflow_engine:unknownProperty", "Unknown find_system property '%s'.", key);
        }
    }

    stringVector paths;
    if (!haveTypeFilter) {
        paths.push_back(modelName); // the system itself (depth 0)
    }
    collectSystemPaths(
        doc.diagram(), modelName, searchDepth, haveTypeFilter, blockTypeFilter, paths);

    ArrayOfVector retval;
    retval << ArrayOf::cellArrayFromArrayOfCharacterRowVectors(paths);
    return retval;
}
//=============================================================================
static void
collectSystemPaths(const nlohmann::json& diagram, const std::string& prefix, int remaining,
    bool haveTypeFilter, const std::string& typeFilter, stringVector& out)
{
    // remaining: number of block levels still allowed (-1 = unlimited); 0 stops.
    if (remaining == 0) {
        return;
    }
    if (!diagram.contains("blocks") || !diagram["blocks"].is_array()) {
        return;
    }
    const int childRemaining = (remaining < 0) ? -1 : (remaining - 1);
    for (const auto& b : diagram["blocks"]) {
        const std::string id = b.value("id", std::string());
        if (id.empty()) {
            continue;
        }
        const std::string path = prefix + "/" + id;
        if (!haveTypeFilter || b.value("type", std::string()) == typeFilter) {
            out.push_back(path);
        }
        // Recurse into a subsystem's nested diagram (always descends within the
        // depth budget, regardless of the type filter, to find deeper matches).
        if (b.contains("params") && b["params"].is_object() && b["params"].contains("subsystem")
            && b["params"]["subsystem"].is_object()) {
            collectSystemPaths(
                b["params"]["subsystem"], path, childRemaining, haveTypeFilter, typeFilter, out);
        }
    }
}
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
