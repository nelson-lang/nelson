//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "get_paramBuiltin.hpp"
#include "NFlowBuiltinHelpers.hpp"
#include "NFlowBlockCatalog.hpp"
#include "NFlowModelRegistry.hpp"
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
get_paramBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    ParamTarget target = requireParamTarget(argIn[0]);
    std::string key = argIn[1].asUtf8String();
    ArrayOfVector retval;

    if (!target.isBlock && key == "Blocks") {
        stringVector names = target.doc->blockNames();
        retval << ArrayOf::cellArrayFromArrayOfCharacterRowVectors(names);
        return retval;
    }
    // A block returns its handle from the table gcbh uses; a model returns the
    // handle assigned by new_system/load_system.
    if (key == "Handle") {
        auto& reg = NFlowModelRegistry::instance();
        if (target.isBlock) {
            if (!target.doc->hasBlock(target.blockName)) {
                RaiseError("No block named '" + target.blockName + "'.",
                    "Nelson:nflow_engine:blockNotFound");
            }
            retval << ArrayOf::doubleScalar(
                reg.handleForBlock(target.modelName + "/" + target.blockName));
        } else {
            double h = 0;
            if (!reg.handleOf(target.modelName, h)) {
                Error(
                    "Nelson:nflow_engine:modelNotLoaded", "Model not loaded: %s", target.modelName);
            }
            retval << ArrayOf::doubleScalar(h);
        }
        return retval;
    }
    if (target.isBlock && key == "Position") {
        double x = 0, y = 0, x2 = 0, y2 = 0;
        std::string e;
        if (!target.doc->getBlockPosition(target.blockName, x, y, x2, y2, e)) {
            Error("Nelson:nflow_engine:getParamFailed", e);
        }
        retval << ArrayOf::doubleRowVector(std::vector<double> { x, y, x2, y2 });
        return retval;
    }
    if (target.isBlock && key == "Commented") {
        std::string v, e;
        if (!target.doc->getBlockParam(target.blockName, key, v, e)) {
            v = "off";
        }
        retval << ArrayOf::characterArray(v);
        return retval;
    }
    // Port handles: 1-based Inport/Outport indices from the block's catalog port counts.
    if (target.isBlock && key == "PortHandles") {
        ensureCatalogLoaded();
        std::string type, e;
        if (!target.doc->getBlockParam(target.blockName, "BlockType", type, e)) {
            Error("Nelson:nflow_engine:getParamFailed", e);
        }
        const Nelson::NFlow::BlockCatalogEntry* entry
            = Nelson::NFlow::NFlowBlockCatalog::instance().find(type);
        const int nin = entry ? entry->inputCount : 0;
        const int nout = entry ? entry->outputCount : 0;
        std::vector<double> inp, outp;
        for (int i = 1; i <= nin; ++i) {
            inp.push_back(static_cast<double>(i));
        }
        for (int i = 1; i <= nout; ++i) {
            outp.push_back(static_cast<double>(i));
        }
        ArrayOfVector fieldValues;
        fieldValues << ArrayOf::doubleRowVector(inp);
        fieldValues << ArrayOf::doubleRowVector(outp);
        retval << ArrayOf::structScalar(stringVector { "Inport", "Outport" }, fieldValues);
        return retval;
    }

    // Parameters stored as a JSON array (vector/matrix) round-trip as a numeric
    // value; scalars and strings stay strings.
    if (target.isBlock) {
        nlohmann::json jv;
        std::string e;
        if (target.doc->getBlockParamJson(target.blockName, key, jv, e) && jv.is_array()) {
            retval << jsonParamToArrayOf(jv);
            return retval;
        }
    }

    std::string value, err;
    bool ok = target.isBlock ? target.doc->getBlockParam(target.blockName, key, value, err)
                             : target.doc->getModelParam(key, value, err);
    if (!ok) {
        Error("Nelson:nflow_engine:getParamFailed", err);
    }
    retval << ArrayOf::characterArray(value);
    return retval;
}
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
