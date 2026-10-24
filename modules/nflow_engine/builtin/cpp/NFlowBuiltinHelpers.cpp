//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NFlowBuiltinHelpers.hpp"
#include "NFlowModelRegistry.hpp"
#include "NFlowBlockCatalog.hpp"
#include "Dimensions.hpp"
#include "ModulePath.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <cctype>
#include <mutex>
#include <cstdio>
//=============================================================================
using namespace Nelson;
using namespace Nelson::NFlow;
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
void
raiseModelError(const std::string& message)
{
    Error("Nelson:nflow_engine:modelError", message);
}
//=============================================================================
void
ensureCatalogLoaded()
{
    static std::once_flag s_once;
    std::call_once(s_once, []() {
        std::wstring dir = ModulePath(L"nflow_blocks", GET_ROOT_PATH) + L"/libraries";
        NFlowBlockCatalog::instance().loadManifestDirectory(dir);
    });
}
//=============================================================================
static std::string
modelPartOf(const std::string& designator)
{
    auto pos = designator.find('/');
    return (pos == std::string::npos) ? designator : designator.substr(0, pos);
}
//=============================================================================
NFlowModelDocument&
requireModel(const ArrayOf& arg, std::string& modelName)
{
    auto& reg = NFlowModelRegistry::instance();
    if (arg.isNumeric() && !arg.isEmpty()) {
        double handle = arg.asDoubleScalar();
        NFlowModelDocument* doc = reg.resolveByHandle(handle);
        if (doc == nullptr) {
            Error("Nelson:nflow_engine:invalidHandle:invalidNflowModelHandle",
                "Invalid nflow model handle.");
        }
        modelName = doc->name();
        return *doc;
    }
    if (arg.isRowVectorCharacterArray() || arg.isScalarStringArray()) {
        std::string name = modelPartOf(arg.asUtf8String());
        NFlowModelDocument* doc = reg.resolveByName(name);
        if (doc == nullptr) {
            Error("Nelson:nflow_engine:modelNotLoaded", "Model not loaded: %s", name);
        }
        modelName = name;
        return *doc;
    }
    Error("Nelson:nflow_engine:invalidHandle:aModelHandleNameExpected",
        "A model handle or name is expected.");
    static NFlowModelDocument dummy("");
    return dummy;
}
//=============================================================================
ParamTarget
requireParamTarget(const ArrayOf& arg)
{
    ParamTarget target;
    auto& reg = NFlowModelRegistry::instance();
    // A numeric designator is a block handle (from gcbh) or a model handle. Block
    // handles take precedence so a handle round-trips through get_param/set_param
    // onto the block it names, not its parent model.
    if (arg.isNumeric() && !arg.isEmpty()) {
        std::string blockPath;
        if (reg.resolveBlockHandle(arg.asDoubleScalar(), blockPath)) {
            std::string modelName, blockName;
            splitModelBlock(blockPath, modelName, blockName);
            NFlowModelDocument* doc = reg.resolveByName(modelName);
            if (doc == nullptr) {
                Error("Nelson:nflow_engine:invalidHandle", "Invalid nflow block handle.");
            }
            target.doc = doc;
            target.modelName = modelName;
            target.isBlock = true;
            target.blockName = blockName;
            return target;
        }
    }
    std::string modelName;
    target.doc = &requireModel(arg, modelName);
    target.modelName = modelName;
    if (arg.isRowVectorCharacterArray() || arg.isScalarStringArray()) {
        std::string designator = arg.asUtf8String();
        auto pos = designator.find('/');
        if (pos != std::string::npos && pos + 1 < designator.size()) {
            target.isBlock = true;
            target.blockName = designator.substr(pos + 1);
        }
    }
    return target;
}
//=============================================================================
std::string
paramValueToString(const ArrayOf& v)
{
    if (v.isRowVectorCharacterArray() || v.isScalarStringArray()) {
        return v.asUtf8String();
    }
    if (v.isLogical()) {
        return v.asLogicalScalar() ? "on" : "off";
    }
    if (v.isNumeric() && !v.isEmpty()) {
        double d = v.asDoubleScalar();
        if (d == static_cast<long long>(d)) {
            return std::to_string(static_cast<long long>(d));
        }
        char buffer[64];
        std::snprintf(buffer, sizeof(buffer), "%.15g", d);
        return std::string(buffer);
    }
    Error("Nelson:nflow_engine:invalidParameterType", "Unsupported parameter value type.");
    return "";
}
//=============================================================================
bool
isNonScalarNumericValue(const ArrayOf& v)
{
    return v.isNumeric() && !v.isEmpty() && !v.isScalar();
}
//=============================================================================
nlohmann::json
numericArrayToJson(const ArrayOf& v)
{
    ArrayOf d(v);
    d.promoteType(NLS_DOUBLE);
    const double* p = static_cast<const double*>(d.dataPointer());
    const size_t rows = static_cast<size_t>(d.rows());
    const size_t cols = static_cast<size_t>(d.columns());
    nlohmann::json out = nlohmann::json::array();
    if (rows <= 1) {
        const size_t n = static_cast<size_t>(d.elementCount());
        for (size_t i = 0; i < n; ++i) {
            out.push_back(p[i]);
        }
        return out;
    }
    // Nelson data is column-major, element (r,c) at p[r + c*rows]; the JSON is row-major.
    for (size_t r = 0; r < rows; ++r) {
        nlohmann::json row = nlohmann::json::array();
        for (size_t c = 0; c < cols; ++c) {
            row.push_back(p[r + c * rows]);
        }
        out.push_back(row);
    }
    return out;
}
//=============================================================================
ArrayOf
jsonParamToArrayOf(const nlohmann::json& v)
{
    if (v.is_string()) {
        return ArrayOf::characterArray(v.get<std::string>());
    }
    if (v.is_boolean()) {
        return ArrayOf::logicalScalar(v.get<bool>());
    }
    if (v.is_number()) {
        return ArrayOf::doubleScalar(v.get<double>());
    }
    if (v.is_array()) {
        if (v.empty()) {
            return ArrayOf::doubleRowVector(std::vector<double> {});
        }
        bool allNumbers = true;
        bool allArrays = true;
        for (const auto& e : v) {
            allNumbers = allNumbers && e.is_number();
            allArrays = allArrays && e.is_array();
        }
        if (allNumbers) {
            std::vector<double> vals;
            vals.reserve(v.size());
            for (const auto& e : v) {
                vals.push_back(e.get<double>());
            }
            return ArrayOf::doubleRowVector(vals);
        }
        if (allArrays) {
            const size_t rows = v.size();
            const size_t cols = v[0].is_array() ? v[0].size() : 0;
            bool rectangular = cols > 0;
            for (const auto& row : v) {
                if (!row.is_array() || row.size() != cols) {
                    rectangular = false;
                    break;
                }
                for (const auto& e : row) {
                    if (!e.is_number()) {
                        rectangular = false;
                        break;
                    }
                }
                if (!rectangular) {
                    break;
                }
            }
            if (rectangular) {
                double* data
                    = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, rows * cols));
                for (size_t r = 0; r < rows; ++r) {
                    for (size_t c = 0; c < cols; ++c) {
                        data[r + c * rows] = v[r][c].get<double>();
                    }
                }
                return ArrayOf(NLS_DOUBLE, Dimensions(rows, cols), data);
            }
        }
    }
    return ArrayOf::characterArray(v.dump());
}
//=============================================================================
void
applyTypedBlockParam(NFlowModelDocument& doc, const std::string& blockName, const std::string& key,
    const ArrayOf& value)
{
    std::string err;
    bool ok = true;
    if (key == "Position") {
        ArrayOf pv(value);
        pv.promoteType(NLS_DOUBLE);
        if (pv.elementCount() != 4) {
            Error("Nelson:nflow_engine:invalidValue", "Position must be a 1x4 [x y x2 y2] vector.");
        }
        const double* p = static_cast<const double*>(pv.dataPointer());
        ok = doc.setBlockPosition(blockName, p[0], p[1], p[2] - p[0], p[3] - p[1], err);
    } else if (isNonScalarNumericValue(value)) {
        ok = doc.setBlockParamJson(blockName, key, numericArrayToJson(value), err);
    } else {
        ok = doc.setBlockParam(blockName, key, paramValueToString(value), err);
    }
    if (!ok) {
        Error("Nelson:nflow_engine:setParamFailed", err);
    }
}
//=============================================================================
std::string
toLowerCopy(const std::string& s)
{
    std::string out = s;
    std::transform(out.begin(), out.end(), out.begin(),
        [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    return out;
}
//=============================================================================
void
splitModelBlock(const std::string& path, std::string& model, std::string& block)
{
    auto pos = path.find('/');
    if (pos == std::string::npos || pos + 1 >= path.size()) {
        Error("Nelson:nflow_engine:invalidPath", "A 'model/block' path is expected: %s", path);
    }
    model = path.substr(0, pos);
    block = path.substr(pos + 1);
}
//=============================================================================
void
parseBlockPort(const std::string& ref, std::string& block, int& port)
{
    auto pos = ref.rfind('/');
    if (pos == std::string::npos || pos + 1 >= ref.size()) {
        Error("Nelson:nflow_engine:invalidPortReference",
            "Port reference must be of the form 'Block/port': %s", ref);
    }
    block = ref.substr(0, pos);
    try {
        port = std::stoi(ref.substr(pos + 1));
    } catch (...) {
        Error("Nelson:nflow_engine:invalidPortReference",
            "Port reference must be of the form 'Block/port': %s", ref);
    }
}
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
