//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "add_blockBuiltin.hpp"
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
add_blockBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, -1);
    nargoutcheck(nLhs, 0, 1);
    ensureCatalogLoaded();
    std::string source = argIn[0].asUtf8String();
    std::string destination = argIn[1].asUtf8String();
    std::string modelName, blockName;
    splitModelBlock(destination, modelName, blockName);
    std::string mn;
    NFlowModelDocument& doc = requireModel(ArrayOf::characterArray(modelName), mn);

    bool makeUnique = false;
    std::vector<std::string> paramKeys;
    std::vector<size_t> paramValueIndex;
    size_t i = 2;
    while (i + 1 < argIn.size()) {
        std::string key = argIn[i].asUtf8String();
        if (toLowerCopy(key) == "makenameunique") {
            makeUnique = toLowerCopy(argIn[i + 1].asUtf8String()) == "on";
        } else {
            paramKeys.push_back(key);
            paramValueIndex.push_back(i + 1);
        }
        i += 2;
    }

    std::string outName, err;
    if (!doc.addBlock(source, blockName, makeUnique, outName, err)) {
        Error("Nelson:nflow_engine:addBlockFailed", err);
    }
    // Same routing as set_param: Position and non-scalar numeric values round-trip
    // as geometry/JSON.
    for (size_t j = 0; j < paramKeys.size(); ++j) {
        applyTypedBlockParam(doc, outName, paramKeys[j], argIn[paramValueIndex[j]]);
    }
    ArrayOfVector retval;
    retval << ArrayOf::characterArray(modelName + "/" + outName);
    return retval;
}
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
